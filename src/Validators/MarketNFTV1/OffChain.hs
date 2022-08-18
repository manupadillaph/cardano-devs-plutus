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

module Validators.MarketNFTV1.OffChain
    (
        SellNFTParams (..), BuyNFTParams (..), GetBackNFTParams (..),
        ValidatorSchema,
        sell, buy, getback,
        endpoints 
    ) where

--Import Externos

--import qualified Control.Lens
import qualified Control.Monad                       as Monad (void)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified Data.Map                            as DataMap
import qualified Data.Text                           as DataText (Text)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Ledger                              (getCardanoTxId, pubKeyHashAddress) --PaymentPubKeyHash, 
import qualified Ledger.Ada                          as LedgerAda
import qualified Ledger.Constraints                  as LedgerConstraints
import qualified Ledger.Tx                           as LedgerTx (ChainIndexTxOut (..))
import qualified Playground.Contract                 (mkSchemaDefinitions)
import qualified Plutus.Contract                     as PlutusContract
import qualified Plutus.V1.Ledger.Api                as LedgerApiV1
import qualified Plutus.V1.Ledger.Interval           as LedgerIntervalV1 (from) -- contains, 
import qualified Plutus.V1.Ledger.Value              as LedgerValueV1
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Schema                              (ToSchema)
import qualified Text.Printf                         as TextPrintf (printf)

--Import Internos:

import qualified Validators.MarketNFTV1.Typos        as T (ValidatorDatum  (..), ValidatorRedeemer (..), NFT)
import qualified Validators.MarketNFTV1.OnChain      as OnChain (typedValidator, codeValidator, addressValidator)

-- Modulo:

minLovelace :: Integer
minLovelace = 2000000

data SellNFTParams = SellNFTParams
    {
        spNFT   :: !T.NFT,
        spPrice :: !Integer
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

data BuyNFTParams = BuyNFTParams
    {
        bpNFT   :: !T.NFT,
        bpPrice :: !Integer
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

data GetBackNFTParams = GetBackNFTParams
    {
        gbpNFT   :: !T.NFT
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

type ValidatorSchema =
        PlutusContract.Endpoint "sell" SellNFTParams PlutusContract..\/
        PlutusContract.Endpoint "buy" BuyNFTParams PlutusContract..\/
        PlutusContract.Endpoint "getback" GetBackNFTParams

sell :: SellNFTParams -> PlutusContract.Contract w s DataText.Text ()
sell SellNFTParams{..} = do
    pkh <- PlutusContract.ownFirstPaymentPubKeyHash

    let d = T.ValidatorDatum
            {
                dSeller = pkh,
                dNFT = spNFT,
                dPrice = spPrice
            }

        (cs, tn) = LedgerValueV1.unAssetClass spNFT

        v = LedgerValueV1.singleton cs tn 1 P.<> LedgerAda.lovelaceValueOf minLovelace

        tx = LedgerConstraints.mustPayToTheScript d v

    ledgerTx <- PlutusContract.submitTxConstraints OnChain.typedValidator tx

    Monad.void P.$ PlutusContract.awaitTxConfirmed P.$ Ledger.getCardanoTxId ledgerTx

    PlutusContract.logInfo @P.String P.$ TextPrintf.printf  "--------------------------- Sell Endpoint - Submited - Datum: %s - Value: %s ---------------------------" (P.show d) (P.show v)

buy :: forall w s. BuyNFTParams ->  PlutusContract.Contract w s DataText.Text ()
buy BuyNFTParams{..} = do

    now <- PlutusContract.currentTime

    buyer <- PlutusContract.ownFirstPaymentPubKeyHash

    let
        buyerAdds = Ledger.pubKeyHashAddress buyer P.Nothing

    utxosAtBuyer <- PlutusContract.utxosAt buyerAdds
    
    utxo <- findUtxoInValidatorWithNFT bpNFT

    case utxo of
            
            P.Nothing -> PlutusContract.logInfo @P.String P.$ TextPrintf.printf "--------------------------- Buy Endpoint - Error - No Encontré NFT ---------------------------"
            
            P.Just (oref, chainIndexTxOut) -> do

                PlutusContract.logInfo @P.String P.$ TextPrintf.printf "--------------------------- Buy Endpoint - Redeem Utxo: %s ---------------------------" (P.show oref)
                
                let

                    P.Just dOld = getDatumm chainIndexTxOut

                    (cs, tn) = LedgerValueV1.unAssetClass bpNFT

                    vGetNFT     = LedgerValueV1.singleton cs tn 1 P.<> LedgerAda.lovelaceValueOf minLovelace

                    vGetADA     = LedgerAda.lovelaceValueOf P.$ T.dPrice dOld

                    redeemer = T.RedeemBuyerBuyNFT
                    redeemerLedger  = LedgerApiV1.Redeemer P.$ PlutusTx.toBuiltinData redeemer

                    lookups =
                        LedgerConstraints.plutusV1TypedValidatorLookups OnChain.typedValidator P.<>
                        LedgerConstraints.plutusV1OtherScript OnChain.codeValidator            P.<>
                        LedgerConstraints.unspentOutputs (DataMap.singleton oref chainIndexTxOut)                      P.<>
                        LedgerConstraints.unspentOutputs utxosAtBuyer

                    tx =
                        LedgerConstraints.mustPayToPubKey (T.dSeller dOld) vGetADA  P.<>
                        LedgerConstraints.mustPayToPubKey buyer vGetNFT  P.<>
                        LedgerConstraints.mustValidateIn (LedgerIntervalV1.from now)            P.<>
                        LedgerConstraints.mustSpendScriptOutput oref redeemerLedger

                --PlutusContract.logInfo @P.String P.$ TextPrintf.printf "--------------------------- Buy Endpoint ---------------------------" 

                ledgerTx <- PlutusContract.submitTxConstraintsWith lookups tx
                Monad.void P.$ PlutusContract.awaitTxConfirmed P.$ Ledger.getCardanoTxId ledgerTx
                PlutusContract.logInfo @P.String P.$ TextPrintf.printf "--------------------------- Buy EndPoint - Submited -------------------------"


getback :: forall w s. GetBackNFTParams ->  PlutusContract.Contract w s DataText.Text ()
getback GetBackNFTParams{..} = do

    now <- PlutusContract.currentTime

    seller <- PlutusContract.ownFirstPaymentPubKeyHash

    let
        sellerAdds = Ledger.pubKeyHashAddress seller P.Nothing

    utxosAtseller <- PlutusContract.utxosAt sellerAdds

    utxo <- findUtxoInValidatorWithNFT gbpNFT

    case utxo of

        P.Nothing ->PlutusContract.logInfo @P.String P.$ TextPrintf.printf "--------------------------- GetBack Endpoint - Error - No Encontré NFT ---------------------------"

        P.Just (oref, chainIndexTxOut) -> do
            PlutusContract.logInfo @P.String P.$ TextPrintf.printf "--------------------------- GetBack Endpoint - Redeem Utxo: %s ---------------------------" (P.show oref)

            let

                --P.Just dOld = getDatumm chainIndexTxOut

                (cs, tn) = LedgerValueV1.unAssetClass gbpNFT

                vGetNFT     = LedgerValueV1.singleton cs tn 1 P.<> LedgerAda.lovelaceValueOf minLovelace

                redeemer = T.RedeemSellerGetBackNFT
                redeemerLedger  = LedgerApiV1.Redeemer P.$ PlutusTx.toBuiltinData redeemer

                lookups =
                    LedgerConstraints.plutusV1TypedValidatorLookups OnChain.typedValidator P.<>
                    LedgerConstraints.plutusV1OtherScript OnChain.codeValidator            P.<>
                    LedgerConstraints.unspentOutputs (DataMap.singleton oref chainIndexTxOut)                      P.<>
                    LedgerConstraints.unspentOutputs utxosAtseller

                tx =
                    LedgerConstraints.mustPayToPubKey seller vGetNFT                        P.<>
                    LedgerConstraints.mustValidateIn (LedgerIntervalV1.from now)            P.<>
                    LedgerConstraints.mustSpendScriptOutput oref redeemerLedger

            --PlutusContract.logInfo @P.String P.$ TextPrintf.printf "--------------------------- Buy Endpoint ---------------------------" 

            ledgerTx <- PlutusContract.submitTxConstraintsWith lookups tx
            Monad.void P.$ PlutusContract.awaitTxConfirmed P.$ Ledger.getCardanoTxId ledgerTx
            PlutusContract.logInfo @P.String P.$ TextPrintf.printf "--------------------------- GetBack EndPoint - Submited -------------------------"

getDatumm :: LedgerTx.ChainIndexTxOut -> P.Maybe T.ValidatorDatum
getDatumm chainIndexTxOut = do
    let
        datHashOrDatum = LedgerTx._ciTxOutScriptDatum chainIndexTxOut

    LedgerApiV1.Datum e <- P.snd datHashOrDatum

    case PlutusTx.fromBuiltinData e of
        P.Nothing -> P.Nothing
        P.Just d -> d

getValueFromChainIndexTxOut :: LedgerTx.ChainIndexTxOut -> LedgerValueV1.Value
getValueFromChainIndexTxOut = LedgerTx._ciTxOutValue   

findUtxoInValidatorWithNFT :: T.NFT -> PlutusContract.Contract w s DataText.Text (P.Maybe (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut))
findUtxoInValidatorWithNFT nft = do
    utxos <- PlutusContract.utxosAt OnChain.addressValidator
    let
        xs = [(oref, o) |(oref, o) <- DataMap.toList utxos, LedgerValueV1.assetClassValueOf (getValueFromChainIndexTxOut o) nft P.== 1]
    case xs of
        [x] -> P.return P.$ P.Just x   
        _   -> P.return P.Nothing

-- checkUTXO  :: (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut) -> Ledger.PaymentPubKeyHash -> Integer -> Bool
-- checkUTXO (oref,o)  ppkh name = do
--     case getDatumm (oref,o) of
--         P.Nothing -> P.False
--         P.Just T.ValidatorDatum{..}
--             | T.aCreator dData P.== ppkh P.&& T.aName dData P.== name -> P.True
--             | P.otherwise                                                              -> P.False

-- findUTXO :: [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]  -> Ledger.PaymentPubKeyHash -> Integer -> P.Maybe LedgerApiV1.TxOutRef
-- findUTXO [] _ _ = P.Nothing --do  
-- findUTXO [(oref,o)]  ppkh name  = do
--     if checkUTXO (oref, o) ppkh name then
--         P.return oref
--     else
--         P.Nothing
-- findUTXO ((oref,o):xs) ppkh name
--     | checkUTXO (oref ,o)  ppkh name = P.return oref
--     | P.otherwise = findUTXO xs   ppkh name


-- getTxOutRefAndChainIndexTxOutFromTxOutRef :: LedgerApiV1.TxOutRef -> PlutusContract.Contract w s DataText.Text (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)
-- getTxOutRefAndChainIndexTxOutFromTxOutRef get_oref= do
--     utxos <- PlutusContract.utxosAt OnChain.addressValidator
--     let
--         xs = [ (oref, o) | (oref, o) <- DataMap.toList utxos , get_oref P.== oref]
--     case xs of
--         [x] ->  P.return x

endpoints :: PlutusContract.Contract () ValidatorSchema DataText.Text ()
endpoints = PlutusContract.awaitPromise (sell' `PlutusContract.select` buy' `PlutusContract.select` getback') P.>> endpoints
  where
    sell' = PlutusContract.endpoint @"sell" sell
    buy' = PlutusContract.endpoint @"buy" buy
    getback' = PlutusContract.endpoint @"getback" getback

Playground.Contract.mkSchemaDefinitions ''ValidatorSchema



