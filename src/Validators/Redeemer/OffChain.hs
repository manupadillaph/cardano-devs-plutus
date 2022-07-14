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

--{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Validators.Redeemer.OffChain
    (
      StartParams (..), GetParams (..)
    , ValidatorSchema
    , start, get
    , endpoints
    -- , schemas
    -- , ensureKnownCurrencies
    -- , printJson
    -- , printSchemas
    -- , stage
    ) where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           Data.String  
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada
import           Playground.Contract  (IO, ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (unless)
import qualified Prelude              as P 
import           Schema               (ToSchema)
import     qualified      Data.OpenApi.Schema         (ToSchema)
import           Text.Printf          (printf)
import Data.Typeable

import          Plutus.Trace.Emulator  as Emulator
import          Wallet.Emulator.Wallet
import          Data.Default
import          Ledger.TimeSlot 

--Import Internos
import  Validators.Redeemer.Typos        (ValidatorDatum  (..), ValidatorData  (..), ValidatorRedeemer (..))
import  Validators.Redeemer.OnChain     (typedValidator, codeValidator, addressValidator)

minLovelace :: Integer
minLovelace = 2000000

data StartParams = StartParams
    { 
        spDeadline :: !POSIXTime
        , spName :: !Integer
        , spAdaQty   :: !Integer
    } deriving (P.Eq, P.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema,  P.Show)

data GetParams = GetParams
    { 
        gpName :: !Integer
        , gpAdaQty   :: !Integer
    } deriving (P.Eq, P.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema, P.Show)

type ValidatorSchema =
        Endpoint "start" StartParams
        .\/ Endpoint "get" GetParams

start ::  StartParams -> Contract w s Text ()
start StartParams{..} = do
    pkh <- ownPaymentPubKeyHash
    oref <- findUtxoInValidator pkh spName
    case oref of
        Nothing -> do
            let a = ValidatorData
                    { 
                    aCreator   = pkh
                    , aDeadline = spDeadline
                    , aName = spName
                    , aAdaQty   = spAdaQty
                    }
                d = ValidatorDatum
                    { dData    = a
                    }
                v = Ada.lovelaceValueOf spAdaQty
                tx = Constraints.mustPayToTheScript d v
            ledgerTx <- submitTxConstraints typedValidator tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @P.String $ printf  "--------------------------- Started plazo fijo %s for token %s" (P.show a) (P.show v)
        _ -> logInfo @P.String $ printf "--------------------------- Plazo fijo con ese nombre ya existe" 

get :: forall w s. GetParams ->  Contract w s Text ()
get GetParams{..} = do
    pkh <- ownPaymentPubKeyHash
    now   <- currentTime
    oref <- findUtxoInValidator pkh gpName
    --logInfo @P.String $ printf "findUtxoInValidator 222222222222222222 plazo fijo utxo with datum %s" (P.show out)
    case oref of
        Nothing ->logInfo @P.String $ printf "--------------------------- Plazo Fijo NOT FOUND"
        --TxOutRef ->logInfo @P.String $ printf "FOUND 222222222222222222 plazo fijo utxo with datum %s" (P.show oref)
        Just oref -> do
            logInfo @P.String $ printf "--------------------------- Plazo Fijo FOUND utxo %s" (P.show oref)
            (oref2,o) <- getFromValidator oref
            let 
                vGet       = gpAdaQty
                
                
                
                Just dOld = getDatumm (oref2,o) 

                redeemerTipo1 = ValidatorRedeemer {
                    rTipo = aName $ dData dOld
                }

                r      = Redeemer $ PlutusTx.toBuiltinData redeemerTipo1

                vChange       = aAdaQty (dData dOld) - vGet
                
                
                a = ValidatorData
                    { 
                    aCreator   = pkh
                    , aDeadline = aDeadline $ dData dOld
                    , aName = aName $ dData dOld
                    , aAdaQty   = vChange
                    }
                d = ValidatorDatum
                    { 
                    dData    = a
                    }

                seller = pkh

                vGetADA       = Ada.lovelaceValueOf vGet
                vChangeADA       = Ada.lovelaceValueOf vChange

                lookups = Constraints.typedValidatorLookups typedValidator P.<>
                  Constraints.otherScript codeValidator                P.<>
                  Constraints.unspentOutputs (Map.singleton oref2 o)

                tx
                 |  vChange >= minLovelace = Constraints.mustPayToPubKey pkh vGetADA  <>
                                    --Constraints.mustValidateIn (from $ aDeadline adAuction)                    <>
                                   -- Constraints.mustValidateIn (from $ aDeadline $ dData dOld)<>
                                    Constraints.mustValidateIn (from now)<>
                                    Constraints.mustSpendScriptOutput oref2 r <>
                                    Constraints.mustPayToTheScript d vChangeADA
                 | otherwise = Constraints.mustPayToPubKey pkh vGetADA  <>
                                    --Constraints.mustValidateIn (from $ aDeadline $ dData dOld)<>
                                   Constraints.mustValidateIn (from now)<>
                                    Constraints.mustSpendScriptOutput oref2 r 

            logInfo @P.String $ printf "--------------------------- Monto Anterior: %s - Get: %s - Cambio: %s " (P.show (aAdaQty (dData dOld))) (P.show vGet) (P.show vChange)

            ledgerTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @P.String $ printf "--------------------------- Get Plazo Fijo "
   
getDatumm :: (TxOutRef, ChainIndexTxOut) -> Maybe ValidatorDatum
getDatumm (oref,o) = case _ciTxOutDatum o of
                        Left _          -> Nothing
                        Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                            Nothing -> Nothing
                            Just d@ValidatorDatum{..} -> Just d

checkUTXO  :: (TxOutRef, ChainIndexTxOut) -> PaymentPubKeyHash -> Integer -> Bool
checkUTXO (oref,o)  ppkh name = do
    case getDatumm (oref,o) of
        Nothing -> False
        Just d@ValidatorDatum{..}
            | aCreator dData == ppkh && aName dData == name -> True
            | otherwise                                           -> False

findUTXO :: [(TxOutRef, ChainIndexTxOut)]  -> PaymentPubKeyHash -> Integer -> Maybe TxOutRef
findUTXO [] _ _ = Nothing --do  
findUTXO [(oref,o)]  ppkh name  = do
    if checkUTXO (oref, o) ppkh name then 
        return oref
    else 
        Nothing
findUTXO ((oref,o):xs) ppkh name  
    | checkUTXO (oref ,o)  ppkh name = return oref
    | otherwise = findUTXO xs   ppkh name

findUtxoInValidator :: PaymentPubKeyHash -> Integer -> Contract w s Text (Maybe TxOutRef)
findUtxoInValidator ppkh name = do
    utxos <- utxosAt addressValidator
    let 
        xs = [ (oref, o) | (oref, o) <- Map.toList utxos ]
        out = findUTXO xs ppkh name
    return out
 
getFromValidator :: TxOutRef -> Contract w s Text (TxOutRef, ChainIndexTxOut)
getFromValidator get_oref= do
    utxos <- utxosAt addressValidator
    let 
        xs = [ (oref, o) | (oref, o) <- Map.toList utxos , get_oref == oref]
    case xs of
        [x] ->  return x
        
endpoints :: Contract () ValidatorSchema Text ()
endpoints = awaitPromise (start' `select` get') >> endpoints
  where
    start' = endpoint @"start" start
    get' = endpoint @"get" get

mkSchemaDefinitions ''ValidatorSchema

