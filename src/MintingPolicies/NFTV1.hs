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

module MintingPolicies.NFTV1 where

--Import Externos

import qualified Control.Monad                       as Monad (void)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.Map                            as DataMap
import qualified Data.Text                           as DataText (Text)
import qualified Data.Void                           as DataVoid (Void)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Ledger                              (TokenName, getCardanoTxId, pubKeyHashAddress) --CurrencySymbol, 
import qualified Ledger.Constraints                  as LedgerConstraints
import qualified Playground.Contract                 (mkSchemaDefinitions) 
import qualified Plutus.Contract                     as PlutusContract
import qualified Plutus.Script.Utils.V1.Scripts      as UtilsScriptsV1 (scriptCurrencySymbol)
import qualified Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies as UtilsTypedScriptsMintingV1
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

{-# INLINABLE mkPolicy #-}
mkPolicy :: LedgerApiV1.TxOutRef -> Ledger.TokenName -> () -> LedgerContextsV1.ScriptContext -> Bool
mkPolicy oref tn () ctx = 
    
        traceIfFalse "UTxO not consumed"   hasInputUTxO           &&
        traceIfFalse "Wrong amount minted" checkMintedAmount

    where
    
        info :: LedgerContextsV1.TxInfo
        info = LedgerContextsV1.scriptContextTxInfo ctx

        hasInputUTxO :: Bool
        hasInputUTxO = any (\i -> LedgerApiV1.txInInfoOutRef i == oref) $ LedgerApiV1.txInfoInputs info

        checkMintedAmount :: Bool
        checkMintedAmount = case LedgerValueV1.flattenValue (LedgerApiV1.txInfoMint info) of
            [(_, tn', amt)] -> tn' == tn && 
                               amt == 1
            _               -> False


policy :: LedgerApiV1.TxOutRef -> Ledger.TokenName -> LedgerScriptsV1.MintingPolicy
policy oref tn = LedgerScriptsV1.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> UtilsTypedScriptsMintingV1.mkUntypedMintingPolicy (mkPolicy oref' tn') ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn

curSymbol :: LedgerApiV1.TxOutRef -> Ledger.TokenName -> LedgerValueV1.CurrencySymbol
curSymbol oref tn = UtilsScriptsV1.scriptCurrencySymbol $ policy oref tn 

newtype MintParams = MintParams
    { 
        mpTokenName   :: Ledger.TokenName
    } deriving (GHCGenerics.Generic, DataAeson.FromJSON, DataAeson.ToJSON, P.Show, Schema.ToSchema)

type MintSchema = PlutusContract.Endpoint "mint" MintParams

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
            let val     = LedgerValueV1.singleton (curSymbol oref tn) tn 1
                lookups = LedgerConstraints.plutusV1MintingPolicy (policy oref tn) P.<>
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
            mpTokenName = tn
        }
    TraceEmulator.callEndpoint @"mint" h2 $ MintParams
        { 
            mpTokenName = tn
        }
    Monad.void $ TraceEmulator.waitNSlots 1
    TraceEmulator.callEndpoint @"mint" h1 $ MintParams
        { 
            mpTokenName = tn
        }
    Monad.void $ TraceEmulator.waitNSlots 1