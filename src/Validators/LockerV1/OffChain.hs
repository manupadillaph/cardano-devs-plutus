{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE NumericUnderscores         #-}

--{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Validators.LockerV1.OffChain
    (
        StartParams (..), GetParams (..),
        ValidatorSchema,
        start, get,
        endpoints
    ) where

--Import Externos

import qualified Control.Monad                       as Monad (void)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
--import qualified Data.Either                         as DataEither (Either (Right))
import qualified Data.Map                            as DataMap
--import qualified Data.Maybe                          as DataMaybe
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified Data.Text                           as DataText (Text)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Ledger                              (PaymentPubKeyHash, getCardanoTxId)
import qualified Ledger.Ada                          as LedgerAda
import qualified Ledger.Constraints                  as LedgerConstraints
import qualified Ledger.Tx                           as LedgerTx (ChainIndexTxOut (..))
import qualified Playground.Contract                 (mkSchemaDefinitions)
import qualified Plutus.Contract                     as PlutusContract
import qualified Plutus.V1.Ledger.Api                as LedgerApiV1
import qualified Plutus.V1.Ledger.Interval           as LedgerIntervalV1 (from) -- contains, 
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Schema                              (ToSchema)
import qualified Text.Printf                         as TextPrintf (printf)

--Import Internos

import qualified Validators.LockerV1.Typos          as T
import qualified Validators.LockerV1.OnChain         as OnChain (typedValidator, codeValidator, addressValidator)

data StartParams = StartParams
    {
        spDeadline :: !LedgerApiV1.POSIXTime,
        spName     :: !Integer,
        spAdaQty   :: !Integer
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

data GetParams = GetParams
    {
        gpName   :: !Integer,
        gpAdaQty :: !Integer
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

type ValidatorSchema =
        PlutusContract.Endpoint "start" StartParams PlutusContract..\/
        PlutusContract.Endpoint "get" GetParams

start :: StartParams -> PlutusContract.Contract w s DataText.Text ()
start StartParams{..} = do
    pkh <- PlutusContract.ownFirstPaymentPubKeyHash
    orefMaybe <- findUtxoInValidator pkh spName
    case orefMaybe of
        Nothing -> do
            let a = T.ValidatorData
                    {
                        aCreator  = pkh,
                        aDeadline = spDeadline,
                        aName     = spName,
                        aAdaQty   = spAdaQty
                    }
                d = T.ValidatorDatum
                    {
                        dData     = a
                    }
                v = LedgerAda.lovelaceValueOf spAdaQty
                tx = LedgerConstraints.mustPayToTheScript d v
            ledgerTx <- PlutusContract.submitTxConstraints OnChain.typedValidator tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
            PlutusContract.logInfo @P.String $ TextPrintf.printf  "--------------------------- Start Endpoint - Submited - Datum: %s - Value: %s ---------------------------" (P.show a) (P.show v)
        _ -> PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Start Endpoint - Error - Ese nombre ya existe ---------------------------"

get :: forall w s. GetParams ->  PlutusContract.Contract w s DataText.Text ()
get GetParams{..} = do
    pkh <- PlutusContract.ownFirstPaymentPubKeyHash
    now <- PlutusContract.currentTime
    orefMaybe <- findUtxoInValidator pkh gpName
    case orefMaybe of
        Nothing ->PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Get Endpoint - Error - Ese nombre no existe ---------------------------"
        Just (oref, ciOut) -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Get Endpoint - Redeem Utxo: %s ---------------------------" (P.show oref)
            let
                vGet       = gpAdaQty

                Just dOld = getDatum (oref,ciOut)
      
                redeemerTipo1 = T.ValidatorRedeemer {
                    rTipo = T.aName $ T.dData dOld
                }
                r  = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData redeemerTipo1

                vChange       = T.aAdaQty (T.dData dOld) - vGet

                a = T.ValidatorData
                    {
                        aCreator  = pkh,
                        aDeadline = T.aDeadline $ T.dData dOld,
                        aName = T.aName $ T.dData dOld,
                        aAdaQty   = vChange
                    }
                d = T.ValidatorDatum
                    {
                        dData    = a
                    }
                
                --datumBuiltinData  = LedgerApiV1.Datum $ PlutusTx.toBuiltinData d

                vGetADA       = LedgerAda.lovelaceValueOf vGet
                vChangeADA       = LedgerAda.lovelaceValueOf vChange

                lookups = LedgerConstraints.plutusV1TypedValidatorLookups OnChain.typedValidator P.<>
                  LedgerConstraints.plutusV1OtherScript OnChain.codeValidator                P.<>
                  LedgerConstraints.unspentOutputs (DataMap.singleton oref ciOut)

                tx
                 | vChange >= T.minLovelace =
                                    LedgerConstraints.mustPayToPubKey pkh vGetADA  P.<>
                                    LedgerConstraints.mustValidateIn (LedgerIntervalV1.from now) P.<>
                                    LedgerConstraints.mustSpendScriptOutput oref r P.<>
                                    LedgerConstraints.mustPayToTheScript d vChangeADA

                 | otherwise =    LedgerConstraints.mustPayToPubKey pkh vGetADA  P.<>
                                    LedgerConstraints.mustValidateIn (LedgerIntervalV1.from now) P.<>
                                    LedgerConstraints.mustSpendScriptOutput oref r

            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Get Endpoint - Monto Anterior: %s - Get: %s - Cambio: %s ---------------------------" (P.show (T.aAdaQty (T.dData dOld))) (P.show vGet) (P.show vChange)

            ledgerTx <- PlutusContract.submitTxConstraintsWith lookups tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Get EndPoint - Submited -------------------------"


getDatum :: (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut) -> Maybe T.ValidatorDatum
getDatum (_, o) = do
    let
        datHashOrDatum = LedgerTx._ciTxOutScriptDatum o

    --PlutusContract.logInfo @P.String $ TextPrintf.printf "getDatum: _ciTxOutScriptDatum = datHashOrDatum: %s" (P.show datHashOrDatum)
    
    case snd datHashOrDatum of
        Just datum -> do
            let 
                LedgerApiV1.Datum e = datum

            --PlutusContract.logInfo @P.String $ TextPrintf.printf "getDatum: LedgerApiV1.Datum: %s" (P.show e)

            case (LedgerApiV1.fromBuiltinData e :: Maybe T.ValidatorDatum) of
                Nothing ->  Nothing
                d ->  d

        Nothing ->  Nothing

-- getDatum2 :: (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut) -> PlutusContract.Contract w s DataText.Text (Maybe T.ValidatorDatum)
-- getDatum2 (_, o) = do
--     let
--         datHashOrDatum = LedgerTx._ciTxOutScriptDatum o

--     PlutusContract.logInfo @P.String $ TextPrintf.printf "getDatum: _ciTxOutScriptDatum = datHashOrDatum: %s" (P.show datHashOrDatum)
    
--     case snd datHashOrDatum of
--         Just datum -> do
--             let 
--                 LedgerApiV1.Datum e = datum
--                 -- T.ValidatorDatum e1 = 

--             PlutusContract.logInfo @P.String $ TextPrintf.printf "getDatum: LedgerApiV1.Datum: %s" (P.show e)
            
            
--             PlutusContract.logInfo @P.String $ TextPrintf.printf "getDatum: PlutusTx.fromBuiltinData: %s" (P.show  (LedgerApiV1.unsafeFromBuiltinData e :: T.ValidatorDatum))

--             case  (LedgerApiV1.fromBuiltinData e :: Maybe T.ValidatorDatum) of
--                 Nothing -> do
--                     PlutusContract.logInfo @P.String $ TextPrintf.printf "getDatum: fromBuiltinData Nothing"
--                     return Nothing
--                 Just d -> do
--                     PlutusContract.logInfo @P.String $ TextPrintf.printf "getDatum: fromBuiltinData %s" (P.show d)
--                     return $ Just d

--         Nothing -> return Nothing

    -- case snd datHashOrDatum of
    --     Just datum -> do
    --         LedgerApiV1.Datum e 
    --         DataMaybe.fromMaybe Nothing (PlutusTx.fromBuiltinData datum)
    --     _ -> Nothing    

    --     LedgerApiV1.Datum e = snd datHashOrDatum 
    -- DataMaybe.fromMaybe P.Nothing (PlutusTx.fromBuiltinData e)


checkUTXO  :: (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut) -> Ledger.PaymentPubKeyHash -> Integer -> Bool
checkUTXO (oref,o)  ppkh name = do
    let
        datum = getDatum (oref,o) 

    case datum of
        Nothing -> False
        Just T.ValidatorDatum{..}
            | T.aCreator dData == ppkh && T.aName dData == name -> True
            | otherwise                                         -> False

findUTXO :: [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]  -> Ledger.PaymentPubKeyHash -> Integer -> PlutusContract.Contract w s DataText.Text (Maybe (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut))
findUTXO [] _ _ = return Nothing --do  
findUTXO [(oref,o)]  ppkh name  = do

    --a <- getDatum2 (oref,o) 

    --PlutusContract.logInfo @P.String $ TextPrintf.printf "findUTXO: oref: %s - datum: %s - datum: %s" (P.show oref) (P.show $ getDatum (oref,o)) (P.show a)
    
    PlutusContract.logInfo @P.String $ TextPrintf.printf "findUTXO: oref: %s - datum: %s" (P.show oref) (P.show $ getDatum (oref,o))


    if ( checkUTXO (oref, o) ppkh name) then
        return $ Just (oref, o)
    else
        return Nothing
findUTXO ((oref,o):xs) ppkh name
    | checkUTXO (oref ,o)  ppkh name = return $ Just (oref, o)
    | otherwise = findUTXO xs ppkh name

findUtxoInValidator :: Ledger.PaymentPubKeyHash -> Integer -> PlutusContract.Contract w s DataText.Text (Maybe (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut))
findUtxoInValidator ppkh name = do
    PlutusContract.logInfo @P.String $ TextPrintf.printf "findUtxoInValidator: ppkh: %s - name: %s" (P.show ppkh) (P.show name)
    utxos <- PlutusContract.utxosAt OnChain.addressValidator
    PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosAt: addressValidator: %s - utxos: %s" (P.show OnChain.addressValidator) (P.show utxos)
    let
        xs = [ (oref, o) | (oref, o) <- DataMap.toList utxos ]

    findUTXO xs ppkh name

endpoints :: PlutusContract.Contract () ValidatorSchema DataText.Text ()
endpoints = PlutusContract.awaitPromise (start' `PlutusContract.select` get') >> endpoints
  where
    start' = PlutusContract.endpoint @"start" start
    get' = PlutusContract.endpoint @"get" get

Playground.Contract.mkSchemaDefinitions ''ValidatorSchema

