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

module MintingPolicies.TokensSignedV1 where

--Import Externos

import qualified Control.Lens                        as Lens    
import qualified Control.Monad                       as Monad (void)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.Map                            as DataMap
--import qualified Data.Maybe                          as DataMaybe (fromJust) --, fromMaybe
import qualified Data.Text                           as DataText (Text) --pack, 
import qualified Data.Void                           as DataVoid (Void)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Ledger                              (getCardanoTxId, TokenName, pubKeyHashAddress, PaymentPubKeyHash, unPaymentPubKeyHash) --CurrencySymbol,  
import qualified Ledger.Constraints                  as LedgerConstraints
--import qualified Ledger.Tx                           as LedgerTx
import qualified Playground.Contract                 (mkSchemaDefinitions) 
import qualified Plutus.Contract                     as PlutusContract
--import qualified Plutus.Contract.Wallet              as PlutusContractWallet (getUnspentOutput)
import qualified Plutus.Script.Utils.V1.Scripts      as UtilsScriptsV1 (scriptCurrencySymbol)
import qualified Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies as UtilsTypedScriptsMintingV1
--import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators       as UtilsTypedScriptsValidatorsV1 (TypedValidator, ValidatorTypes (DatumType, RedeemerType))
import qualified Plutus.Trace.Emulator               as TraceEmulator
import qualified Plutus.V1.Ledger.Api                as LedgerApiV1
import qualified Plutus.V1.Ledger.Address            as LedgerAddressV1
import qualified Plutus.V1.Ledger.Contexts           as LedgerContextsV1 (ScriptContext, TxInfo, scriptContextTxInfo, txSignedBy, ownCurrencySymbol) 
import qualified Ledger.Index                        as LedgerIndex
import qualified Plutus.V1.Ledger.Scripts            as LedgerScriptsV1
import qualified Plutus.V1.Ledger.Value              as LedgerValueV1
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P 
import qualified Schema                              (ToSchema)
import qualified Text.Printf                         as TextPrintf (printf)
import qualified Wallet.Emulator.Wallet              as WalletEmulator

--Import Internos

--import qualified Utils

{-# INLINABLE mkPolicy #-}
mkPolicy :: Ledger.PaymentPubKeyHash -> LedgerApiV1.TxOutRef -> Ledger.TokenName ->  Integer -> () -> LedgerContextsV1.ScriptContext -> Bool
mkPolicy master txOutRef tokenName amt' () ctx = 

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
            Just (_, tn, amt) -> tn == tokenName && amt == amt'
            _                  -> False

        checkBurnedAmount :: Bool
        checkBurnedAmount = case getValueOfOwnCurrencySymbol of
            Just (_, tn, amt) -> tn == tokenName && amt < 0
            _                  -> False



policy :: Ledger.PaymentPubKeyHash -> LedgerApiV1.TxOutRef -> Ledger.TokenName -> Integer -> LedgerScriptsV1.MintingPolicy
policy master txOutRef tokenName amt = LedgerScriptsV1.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \master' txOutRef' tokenName' amt' -> UtilsTypedScriptsMintingV1.mkUntypedMintingPolicy $ mkPolicy  master' txOutRef' tokenName' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode master
    `PlutusTx.applyCode`
    PlutusTx.liftCode txOutRef
    `PlutusTx.applyCode`
    PlutusTx.liftCode tokenName
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt


curSymbol :: Ledger.PaymentPubKeyHash -> LedgerApiV1.TxOutRef -> Ledger.TokenName -> Integer -> LedgerValueV1.CurrencySymbol
curSymbol master txOutRef tokenName  amt = UtilsScriptsV1.scriptCurrencySymbol $ policy master txOutRef tokenName  amt  

data PolicyParams = PolicyParams 
    {
        pppkh :: Ledger.PaymentPubKeyHash,
        pporef :: LedgerApiV1.TxOutRef,
        pptn :: Ledger.TokenName,
        ppamt :: Integer
    } deriving (GHCGenerics.Generic, DataAeson.FromJSON, DataAeson.ToJSON, P.Show, Schema.ToSchema)

data MintParams = MintParams
    { 
        mpPolicyParams :: PolicyParams,
        mpAmount    :: Integer
    } deriving (GHCGenerics.Generic, DataAeson.FromJSON, DataAeson.ToJSON, P.Show, Schema.ToSchema)

data BurnParams = BurnParams
    { 
        mabpPolicyParams :: PolicyParams,
        mabpAmount    :: Integer
    } deriving (GHCGenerics.Generic, DataAeson.FromJSON, DataAeson.ToJSON, P.Show, Schema.ToSchema)


type MintSchema = PlutusContract.Endpoint "mint" MintParams PlutusContract..\/ 
                  PlutusContract.Endpoint "burn" BurnParams


mint :: MintParams -> PlutusContract.Contract w MintSchema DataText.Text ()
mint mp = do
    PlutusContract.logDebug @P.String $ TextPrintf.printf "started minting: %s" $ P.show mp
    
    pkh <- PlutusContract.ownFirstPaymentPubKeyHash

    let 
        address = Ledger.pubKeyHashAddress pkh Nothing

    utxos <- PlutusContract.utxosAt address

    let 
        pkh'     = pppkh $ mpPolicyParams mp
        oref'    = pporef $ mpPolicyParams mp
        tn'      = pptn $ mpPolicyParams mp
        amt'     = ppamt $ mpPolicyParams mp

        amt     = mpAmount mp

        val     = LedgerValueV1.singleton (curSymbol pkh' oref' tn' amt') tn' amt

        lookups = LedgerConstraints.plutusV1MintingPolicy (policy pkh' oref' tn' amt') P.<>
                    LedgerConstraints.unspentOutputs utxos

        tx      = LedgerConstraints.mustMintValue val P.<>
                    LedgerConstraints.mustSpendPubKeyOutput oref'

    ledgerTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookups tx
    Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "forged %s" (P.show val)

burn :: BurnParams -> PlutusContract.Contract w MintSchema DataText.Text ()
burn mp = do
    PlutusContract.logDebug @P.String $ TextPrintf.printf "started minting: %s" $ P.show mp
    
    pkh <- PlutusContract.ownFirstPaymentPubKeyHash

    let 
        address = Ledger.pubKeyHashAddress pkh Nothing

    utxos <- PlutusContract.utxosAt address
    
    let 
        pkh'     = pppkh $ mabpPolicyParams mp
        oref'    = pporef $ mabpPolicyParams mp
        tn'      = pptn $ mabpPolicyParams mp
        amt'     = ppamt $ mabpPolicyParams mp

        amt     = mabpAmount mp

        val     = LedgerValueV1.singleton (curSymbol pkh' oref' tn' amt') tn' amt

        lookups = LedgerConstraints.plutusV1MintingPolicy (policy pkh' oref' tn' amt') P.<>
                  LedgerConstraints.unspentOutputs utxos

        tx      = LedgerConstraints.mustMintValue (negate val) 

    ledgerTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookups tx
    Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "burned %s" (P.show val)

endpoints :: PlutusContract.Contract () MintSchema DataText.Text ()
endpoints = PlutusContract.awaitPromise (mint' `PlutusContract.select` burn') >> endpoints
  where
    mint' = PlutusContract.endpoint @"mint" mint
    burn' = PlutusContract.endpoint @"burn" burn

Playground.Contract.mkSchemaDefinitions ''MintSchema

{- | Get the utxos in the address for use in the emulator trace. -}
getUtxoListInEmulator :: LedgerAddressV1.Address -> TraceEmulator.EmulatorTrace [(LedgerApiV1.TxOutRef, LedgerApiV1.TxOut)]
getUtxoListInEmulator addr = do
    state <- TraceEmulator.chainState
    let utxoIndex = LedgerIndex.getIndex $ state Lens.^. TraceEmulator.index 
        utxos     =  [(oref, o) | (oref, o) <- DataMap.toList utxoIndex, LedgerApiV1.txOutAddress o == addr]
    P.pure utxos   


testWithTraceEmulator ::  P.IO ()
testWithTraceEmulator = TraceEmulator.runEmulatorTraceIO $ do

    let 
        user1 = WalletEmulator.knownWallet 1
        user2 = WalletEmulator.knownWallet 2

    h1 <- TraceEmulator.activateContractWallet user1 endpoints
    h2 <- TraceEmulator.activateContractWallet user2 endpoints

    utxosAtUser1 <- getUtxoListInEmulator (WalletEmulator.mockWalletAddress user1)
    let 
        policyParams = PolicyParams{
            pppkh = WalletEmulator.mockWalletPaymentPubKeyHash user1,
            pporef = fst $ head utxosAtUser1,
            pptn = "Token",
            ppamt = 100
        }

    -- TraceEmulator.callEndpoint @"mint" h1 $ MintParams
    --     { 
    --         mpPolicyParams = policyParams,
    --         mpAmount    = 200
    --     }
    TraceEmulator.callEndpoint @"mint" h1 $ MintParams
        { 
            mpPolicyParams = policyParams,
            mpAmount    = 100
        }

    Monad.void $ TraceEmulator.waitNSlots 1

    TraceEmulator.callEndpoint @"mint" h1 $ MintParams
        { 
            mpPolicyParams = policyParams,
            mpAmount    = 10
        }

    Monad.void $ TraceEmulator.waitNSlots 1

    -- TraceEmulator.callEndpoint @"mint" h2 $ MintParams
    --     { 
    --         mpPolicyParams = policyParams,
    --         mpAmount    = 100
    --     }
    -- Monad.void $ TraceEmulator.waitNSlots 1

    TraceEmulator.callEndpoint @"burn" h1 $ BurnParams
        { 
            mabpPolicyParams = policyParams,
            mabpAmount = 50
        }
    Monad.void $ TraceEmulator.waitNSlots 1

    -- TraceEmulator.callEndpoint @"burn" h2 $ BurnParams
    --     { 
    --         mabpPolicyParams = policyParams,
    --         mabpAmount = 50
    --     }
    -- Monad.void $ TraceEmulator.waitNSlots 1