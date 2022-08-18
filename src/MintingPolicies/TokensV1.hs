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

module MintingPolicies.TokensV1 where

--Import Externos

import qualified Control.Monad                       as Monad (void)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.Map                            as DataMap
--import qualified Data.Maybe                          as DataMaybe (fromJust) --, fromMaybe
import qualified Data.Text                           as DataText (Text) --pack, 
import qualified Data.Void                           as DataVoid (Void)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Ledger                              (getCardanoTxId, TokenName, pubKeyHashAddress) --CurrencySymbol,  
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
import qualified Plutus.V1.Ledger.Contexts           as LedgerContextsV1 (ScriptContext, TxInfo, scriptContextTxInfo) --, txSignedBy
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
mkPolicy :: LedgerApiV1.TxOutRef -> Ledger.TokenName ->  Integer -> () -> LedgerContextsV1.ScriptContext -> Bool
mkPolicy oref tn amt () ctx = 

        traceIfFalse "UTxO not consumed"   hasInputUTxO           &&
        traceIfFalse "Wrong token name or amount minted" checkTokenNameAndMintedAmount

    where

        info :: LedgerContextsV1.TxInfo
        info = LedgerContextsV1.scriptContextTxInfo ctx

        hasInputUTxO :: Bool
        hasInputUTxO = any (\i -> LedgerApiV1.txInInfoOutRef i == oref) $ LedgerApiV1.txInfoInputs info

        checkTokenNameAndMintedAmount :: Bool
        checkTokenNameAndMintedAmount = case LedgerValueV1.flattenValue (LedgerApiV1.txInfoMint info) of
            [(_, tn', amt')] -> tn' == tn && 
                                amt' == amt
            _                -> False


policy :: LedgerApiV1.TxOutRef -> Ledger.TokenName -> Integer -> LedgerScriptsV1.MintingPolicy
policy oref tn amt = LedgerScriptsV1.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' amt' -> UtilsTypedScriptsMintingV1.mkUntypedMintingPolicy $ mkPolicy oref' tn' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt


curSymbol :: LedgerApiV1.TxOutRef -> Ledger.TokenName -> Integer -> LedgerValueV1.CurrencySymbol
curSymbol oref tn amt = UtilsScriptsV1.scriptCurrencySymbol $ policy oref tn amt  

data MintParams = MintParams
    { 
        mpTokenName :: Ledger.TokenName,
        mpAmount    :: Integer
    } deriving (GHCGenerics.Generic, DataAeson.FromJSON, DataAeson.ToJSON, P.Show, Schema.ToSchema)

type MintSchema = PlutusContract.Endpoint "mint" MintParams

mint :: MintParams -> PlutusContract.Contract w MintSchema DataText.Text ()
mint mp = do
    PlutusContract.logDebug @P.String $ TextPrintf.printf "started minting: %s" $ P.show mp
    
    pkh <- PlutusContract.ownFirstPaymentPubKeyHash

    let 
        address = Ledger.pubKeyHashAddress pkh Nothing

    utxos <- PlutusContract.utxosAt address
    
    case DataMap.keys utxos of
        []       -> PlutusContract.logError @P.String "no utxo found"
        oref : _ -> do
            let 
                tn      = mpTokenName mp
                amt     = mpAmount mp
            let val     = LedgerValueV1.singleton (curSymbol oref tn amt) tn amt
                lookups = LedgerConstraints.plutusV1MintingPolicy (policy oref tn amt) P.<>
                          LedgerConstraints.unspentOutputs utxos
                tx      = LedgerConstraints.mustMintValue val P.<>
                          LedgerConstraints.mustSpendPubKeyOutput oref
            ledgerTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookups tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
            PlutusContract.logInfo @P.String $ TextPrintf.printf "forged %s" (P.show val)

endpoints :: PlutusContract.Contract () MintSchema DataText.Text ()
endpoints = mint' >> endpoints
  where
    mint' = PlutusContract.awaitPromise $ PlutusContract.endpoint @"mint" mint

Playground.Contract.mkSchemaDefinitions ''MintSchema

test :: P.IO ()
test = TraceEmulator.runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- TraceEmulator.activateContractWallet (WalletEmulator.knownWallet 1) endpoints
    h2 <- TraceEmulator.activateContractWallet (WalletEmulator.knownWallet 2) endpoints
    TraceEmulator.callEndpoint @"mint" h1 $ MintParams
        { 
            mpTokenName = tn,
            mpAmount    = 555
        }
    TraceEmulator.callEndpoint @"mint" h2 $ MintParams
        { 
            mpTokenName = tn,
            mpAmount    = 444
        }
    Monad.void $ TraceEmulator.waitNSlots 1
    TraceEmulator.callEndpoint @"mint" h1 $ MintParams
        { 
            mpTokenName = tn,
            mpAmount    = -222
        }
    Monad.void $ TraceEmulator.waitNSlots 1
