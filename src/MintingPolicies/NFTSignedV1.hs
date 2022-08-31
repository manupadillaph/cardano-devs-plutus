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

module MintingPolicies.NFTSignedV1 where

--Import Externos

import qualified Control.Monad                       as Monad (void)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.Map                            as DataMap
import qualified Data.Text                           as DataText (Text)
import qualified Data.Void                           as DataVoid (Void)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Ledger                              (TokenName, getCardanoTxId, pubKeyHashAddress, PaymentPubKeyHash, unPaymentPubKeyHash) --CurrencySymbol, 
import qualified Ledger.Constraints                  as LedgerConstraints
import qualified Playground.Contract                 (mkSchemaDefinitions) 
import qualified Plutus.Contract                     as PlutusContract
import qualified Plutus.Script.Utils.V1.Scripts      as UtilsScriptsV1 (scriptCurrencySymbol)
import qualified Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies as UtilsTypedScriptsMintingV1
import qualified Plutus.Trace.Emulator               as TraceEmulator
import qualified Plutus.V1.Ledger.Api                as LedgerApiV1
import qualified Plutus.V1.Ledger.Contexts           as LedgerContextsV1 (ScriptContext, TxInfo, scriptContextTxInfo, txSignedBy, ownCurrencySymbol) 
import qualified Plutus.V1.Ledger.Scripts            as LedgerScriptsV1
import qualified Plutus.V1.Ledger.Value              as LedgerValueV1
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P 
import qualified Schema                              (ToSchema)
import qualified Text.Printf                         as TextPrintf (printf)
import qualified Wallet.Emulator.Wallet              as WalletEmulator

{-# INLINABLE mkPolicy #-}
mkPolicy :: Ledger.PaymentPubKeyHash -> LedgerApiV1.TxOutRef -> Ledger.TokenName -> () -> LedgerContextsV1.ScriptContext -> Bool
mkPolicy master txOutRef tokenName () ctx = 
        traceIfFalse "Master's signature missing!!! " signedByMaster &&
        traceIfFalse "Minting Pool NFT Policy: UTxO not consumed" (hasInputUTxO && checkMinting || checkBurning) &&
        traceIfFalse "Minting Pool NFT Policy: Wrong Mint Amount" (checkMintedAmount && checkMinting || checkBurning) &&
        traceIfFalse "Minting Pool NFT Policy: Wrong Burn Amount" (checkBurnedAmount && checkBurning || checkMinting) 

    where

        info :: LedgerContextsV1.TxInfo
        info = LedgerContextsV1.scriptContextTxInfo ctx

        signedByMaster :: Bool
        signedByMaster =
            LedgerContextsV1.txSignedBy info (Ledger.unPaymentPubKeyHash master)

        getValueOfOwnCurrencySymbol :: Maybe (LedgerValueV1.CurrencySymbol, LedgerValueV1.TokenName, Integer) 
        getValueOfOwnCurrencySymbol = do
            let 
                sameCurrencySymbol (cs, _, _) = cs == LedgerContextsV1.ownCurrencySymbol ctx
            find sameCurrencySymbol [ (cs, tn, am) | (cs, tn, am) <- LedgerValueV1.flattenValue (LedgerApiV1.txInfoMint info) ]
            

        checkMinting:: Bool
        checkMinting = case getValueOfOwnCurrencySymbol of
            Just (_, tn, amt) -> tn == tokenName && amt > 0
            _                  -> False

        checkBurning:: Bool
        checkBurning = case getValueOfOwnCurrencySymbol of
            Just (_, tn, amt) -> tn == tokenName && amt < 0
            _                  -> False

        hasInputUTxO :: Bool
        hasInputUTxO = any (\i -> LedgerApiV1.txInInfoOutRef i == txOutRef) $ LedgerApiV1.txInfoInputs info

        checkMintedAmount :: Bool
        checkMintedAmount = case getValueOfOwnCurrencySymbol of
            Just (_, tn, amt) -> tn == tokenName && amt == 1
            _                  -> False

        checkBurnedAmount :: Bool
        checkBurnedAmount = case getValueOfOwnCurrencySymbol of
            Just (_, tn, amt) -> tn == tokenName && amt == -1
            _                  -> False


policy :: Ledger.PaymentPubKeyHash -> LedgerApiV1.TxOutRef -> Ledger.TokenName -> LedgerScriptsV1.MintingPolicy
policy master txOutRef tokenName = LedgerScriptsV1.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \master' txOutRef' tokenName' -> UtilsTypedScriptsMintingV1.mkUntypedMintingPolicy (mkPolicy master' txOutRef' tokenName') ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode master
    `PlutusTx.applyCode`
    PlutusTx.liftCode txOutRef
    `PlutusTx.applyCode`
    PlutusTx.liftCode tokenName

curSymbol :: Ledger.PaymentPubKeyHash -> LedgerApiV1.TxOutRef -> Ledger.TokenName -> LedgerValueV1.CurrencySymbol
curSymbol master txOutRef tokenName = UtilsScriptsV1.scriptCurrencySymbol $ policy master txOutRef tokenName


data MintParams = MintParams
    { 
        mpTokenName   :: Ledger.TokenName,
        mpQty   :: Integer
    } deriving (GHCGenerics.Generic, DataAeson.FromJSON, DataAeson.ToJSON, P.Show, Schema.ToSchema)

type MintSchema = PlutusContract.Endpoint "mint" MintParams PlutusContract..\/ 
                  PlutusContract.Endpoint "mintAndBurn" MintParams

mint :: MintParams -> PlutusContract.Contract w MintSchema DataText.Text ()
mint mp = do

    pkh <- PlutusContract.ownFirstPaymentPubKeyHash

    let 
        address = Ledger.pubKeyHashAddress pkh Nothing

    utxos <- PlutusContract.utxosAt address
    
    case DataMap.keys utxos of
        []       -> PlutusContract.logError @P.String "no utxo found"
        oref : _ -> do
            let tn      = mpTokenName mp
            let val     = LedgerValueV1.singleton (curSymbol pkh oref tn) tn (mpQty mp)
                lookups = LedgerConstraints.plutusV1MintingPolicy (policy pkh oref tn) P.<>
                          LedgerConstraints.unspentOutputs utxos
                tx      = LedgerConstraints.mustMintValue val P.<>
                          LedgerConstraints.mustSpendPubKeyOutput oref
            ledgerTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookups tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
            PlutusContract.logInfo @P.String $ TextPrintf.printf "forged %s" (P.show val)

mintAndBurn :: MintParams -> PlutusContract.Contract w MintSchema DataText.Text ()
mintAndBurn mp = do

    pkh <- PlutusContract.ownFirstPaymentPubKeyHash

    let 
        address = Ledger.pubKeyHashAddress pkh Nothing

    utxos <- PlutusContract.utxosAt address
    
    case DataMap.keys utxos of
        []       -> PlutusContract.logError @P.String "no utxo found"
        oref : _ -> do
            let tn      = mpTokenName mp
            let val     = LedgerValueV1.singleton (curSymbol pkh oref tn) tn (mpQty mp)
                lookups = LedgerConstraints.plutusV1MintingPolicy (policy pkh oref tn) P.<>
                          LedgerConstraints.unspentOutputs utxos
                tx      = LedgerConstraints.mustMintValue val P.<>
                          LedgerConstraints.mustSpendPubKeyOutput oref
            ledgerTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookups tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
            PlutusContract.logInfo @P.String $ TextPrintf.printf "forged %s" (P.show val)

            utxos <- PlutusContract.utxosAt address
            let
                lookups = LedgerConstraints.plutusV1MintingPolicy (policy pkh oref tn) P.<>
                          LedgerConstraints.unspentOutputs utxos
                tx      = LedgerConstraints.mustMintValue (negate val) 
            ledgerTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookups tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
            PlutusContract.logInfo @P.String $ TextPrintf.printf "burned %s" (P.show val)

endpoints :: PlutusContract.Contract () MintSchema DataText.Text ()
endpoints = PlutusContract.awaitPromise (mint' `PlutusContract.select` mintAndBurn') >> endpoints
  where
    mint' = PlutusContract.endpoint @"mint" mint
    mintAndBurn' = PlutusContract.endpoint @"mintAndBurn" mintAndBurn


Playground.Contract.mkSchemaDefinitions ''MintSchema

testWithTraceEmulator ::  P.IO ()
testWithTraceEmulator = TraceEmulator.runEmulatorTraceIO $ do

    h1 <- TraceEmulator.activateContractWallet (WalletEmulator.knownWallet 1) endpoints
    h2 <- TraceEmulator.activateContractWallet (WalletEmulator.knownWallet 2) endpoints
    TraceEmulator.callEndpoint @"mint" h1 $ MintParams
        { 
            mpTokenName = "Mint1",
            mpQty = 1
        }
    Monad.void $ TraceEmulator.waitNSlots 1
    TraceEmulator.callEndpoint @"mintAndBurn" h1 $ MintParams
        { 
            mpTokenName = "MintAndBurn",
            mpQty = 1
        }
    TraceEmulator.callEndpoint @"mint" h2 $ MintParams
        { 
            mpTokenName = "Mint2",
            mpQty = 2
        }
    TraceEmulator.callEndpoint @"mintAndBurn" h2 $ MintParams
        { 
            mpTokenName = "MintAndBurn2",
            mpQty = 2
        }
    Monad.void $ TraceEmulator.waitNSlots 1
