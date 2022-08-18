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

module Validators.RedeemerV1.OffChain
    (
        StartParams (..), GetParams (..),
        ValidatorSchema,
        start, get,
        endpoints
    ) where

--Import Externos

import qualified Control.Monad                       as Monad (void)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified Data.Map                            as DataMap
import qualified Data.Text                           as DataText (Text)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Ledger                              (PaymentPubKeyHash, getCardanoTxId)
import qualified Ledger.Ada                          as LedgerAda    
import qualified Ledger.Constraints                  as LedgerConstraints
import qualified Ledger.Tx                           as LedgerTx (ChainIndexTxOut (..))       
import qualified Playground.Contract                 (mkSchemaDefinitions) 
import qualified Plutus.Contract                     as PlutusContract 
import qualified Plutus.V1.Ledger.Api                as LedgerApiV1
import qualified Plutus.V1.Ledger.Interval           as LedgerIntervalV1 (from)
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Schema                              (ToSchema)
import qualified Text.Printf                         as TextPrintf (printf)

--Import Internos

import qualified Validators.RedeemerV1.Typos         as T
import qualified Validators.RedeemerV1.OnChain       as OnChain (typedValidator, codeValidator, addressValidator)

--Modulo:

data StartParams = StartParams
    { 
        ppDeadline :: !LedgerApiV1.POSIXTime,
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
    oref <- findUtxoInValidator pkh spName
    case oref of
        P.Nothing -> do
            let a = T.ValidatorData
                    { 
                        aCreator  = pkh,
                        aDeadline = ppDeadline,
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
            Monad.void P.$ PlutusContract.awaitTxConfirmed P.$ Ledger.getCardanoTxId ledgerTx
            PlutusContract.logInfo @P.String P.$ TextPrintf.printf  "--------------------------- Start Endpoint - Submited - Datum: %s - Value: %s ---------------------------" (P.show a) (P.show v)
        _ -> PlutusContract.logInfo @P.String P.$ TextPrintf.printf "--------------------------- Start Endpoint - Error - Ese nombre ya existe ---------------------------" 

get :: forall w s. GetParams ->  PlutusContract.Contract w s DataText.Text ()
get GetParams{..} = do
    pkh <- PlutusContract.ownFirstPaymentPubKeyHash
    now <- PlutusContract.currentTime
    orefMaybe <- findUtxoInValidator pkh gpName
    case orefMaybe of
        P.Nothing ->PlutusContract.logInfo @P.String P.$ TextPrintf.printf "--------------------------- Get Endpoint - Error - Ese nombre no existe ---------------------------"
        P.Just oref -> do
            PlutusContract.logInfo @P.String P.$ TextPrintf.printf "--------------------------- Get Endpoint - Redeem Utxo: %s ---------------------------" (P.show oref)
            (oref2,o) <- getTxOutRefAndChainIndexTxOutFromTxOutRef oref
            let 
                vGet       = gpAdaQty
                
                P.Just dOld = getDatumm (oref2,o) 
                redeemerTipo1 = T.ValidatorRedeemer {
                    rTipo = T.aName P.$ T.dData dOld
                }
                r  = LedgerApiV1.Redeemer P.$ PlutusTx.toBuiltinData redeemerTipo1

                vChange       = T.aAdaQty (T.dData dOld) P.- vGet
                
                a = T.ValidatorData
                    { 
                        aCreator  = pkh,
                        aDeadline = T.aDeadline P.$ T.dData dOld,
                        aName = T.aName P.$ T.dData dOld,
                        aAdaQty   = vChange
                    }
                d = T.ValidatorDatum
                    { 
                        dData    = a
                    }

                vGetADA       = LedgerAda.lovelaceValueOf vGet
                vChangeADA       = LedgerAda.lovelaceValueOf vChange

                lookups = LedgerConstraints.plutusV1TypedValidatorLookups OnChain.typedValidator P.<>
                  LedgerConstraints.plutusV1OtherScript OnChain.codeValidator                P.<>
                  LedgerConstraints.unspentOutputs (DataMap.singleton oref2 o)

                tx
                 | vChange P.>= T.minLovelace = 
                                    LedgerConstraints.mustPayToPubKey pkh vGetADA  P.<>
                                    LedgerConstraints.mustValidateIn (LedgerIntervalV1.from now) P.<>
                                    LedgerConstraints.mustSpendScriptOutput oref2 r P.<>
                                    LedgerConstraints.mustPayToTheScript d vChangeADA

                 | P.otherwise =    LedgerConstraints.mustPayToPubKey pkh vGetADA  P.<>
                                    LedgerConstraints.mustValidateIn (LedgerIntervalV1.from now) P.<>
                                    LedgerConstraints.mustSpendScriptOutput oref2 r 

            PlutusContract.logInfo @P.String P.$ TextPrintf.printf "--------------------------- Get Endpoint - Monto Anterior: %s - Get: %s - Cambio: %s ---------------------------" (P.show (T.aAdaQty (T.dData dOld))) (P.show vGet) (P.show vChange)

            ledgerTx <- PlutusContract.submitTxConstraintsWith lookups tx
            Monad.void P.$ PlutusContract.awaitTxConfirmed P.$ Ledger.getCardanoTxId ledgerTx
            PlutusContract.logInfo @P.String P.$ TextPrintf.printf "--------------------------- Get EndPoint - Submited -------------------------"
   
getDatumm :: (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut) -> P.Maybe T.ValidatorDatum
getDatumm (_, o) = do
    let 
        datHashOrDatum = LedgerTx._ciTxOutScriptDatum o
    LedgerApiV1.Datum e <- P.snd datHashOrDatum
    case PlutusTx.fromBuiltinData e of
        P.Nothing -> P.Nothing
        P.Just d -> d


checkUTXO  :: (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut) -> Ledger.PaymentPubKeyHash -> Integer -> Bool
checkUTXO (oref,o)  ppkh name = do
    case getDatumm (oref,o) of
        P.Nothing -> P.False
        P.Just T.ValidatorDatum{..}
            | T.aCreator dData P.== ppkh P.&& T.aName dData P.== name -> P.True
            | P.otherwise                                                              -> P.False

findUTXO :: [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]  -> Ledger.PaymentPubKeyHash -> Integer -> P.Maybe LedgerApiV1.TxOutRef
findUTXO [] _ _ = P.Nothing --do  
findUTXO [(oref,o)]  ppkh name  = do
    if checkUTXO (oref, o) ppkh name then 
        P.return oref
    else 
        P.Nothing
findUTXO ((oref,o):xs) ppkh name  
    | checkUTXO (oref ,o)  ppkh name = P.return oref
    | P.otherwise = findUTXO xs   ppkh name

findUtxoInValidator :: Ledger.PaymentPubKeyHash -> Integer -> PlutusContract.Contract w s DataText.Text (P.Maybe LedgerApiV1.TxOutRef)
findUtxoInValidator ppkh name = do
    utxos <- PlutusContract.utxosAt OnChain.addressValidator
    let 
        xs = [ (oref, o) | (oref, o) <- DataMap.toList utxos ]
        out = findUTXO xs ppkh name
    P.return out
 
getTxOutRefAndChainIndexTxOutFromTxOutRef :: LedgerApiV1.TxOutRef -> PlutusContract.Contract w s DataText.Text (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)
getTxOutRefAndChainIndexTxOutFromTxOutRef get_oref= do
    utxos <- PlutusContract.utxosAt OnChain.addressValidator
    let 
        xs = [ (oref, o) | (oref, o) <- DataMap.toList utxos , get_oref P.== oref]
    case xs of
        [x] ->  P.return x
        
endpoints :: PlutusContract.Contract () ValidatorSchema DataText.Text ()
endpoints = PlutusContract.awaitPromise (start' `PlutusContract.select` get') P.>> endpoints
  where
    start' = PlutusContract.endpoint @"start" start
    get' = PlutusContract.endpoint @"get" get

Playground.Contract.mkSchemaDefinitions ''ValidatorSchema

