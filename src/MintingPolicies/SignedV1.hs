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

module MintingPolicies.SignedV1 where

--Import Externos

import qualified Control.Monad                       as Monad (void)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
--import qualified Data.Map                            as DataMap
import qualified Data.Text                           as DataText (Text)
import qualified Data.Void                           as DataVoid (Void)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Ledger                              (TokenName, getCardanoTxId, PaymentPubKeyHash, unPaymentPubKeyHash) --CurrencySymbol, 
import qualified Ledger.Constraints                  as LedgerConstraints
import qualified Playground.Contract                 (mkSchemaDefinitions)
import qualified Plutus.Contract                     as PlutusContract
import qualified Plutus.Script.Utils.V1.Scripts      as UtilsScriptsV1 (scriptCurrencySymbol)
import qualified Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies as UtilsTypedScriptsMintingV1
import qualified Plutus.Trace.Emulator               as TraceEmulator
--import qualified Plutus.V1.Ledger.Api                as LedgerApiV1
import qualified Plutus.V1.Ledger.Contexts           as LedgerContextsV1 (ScriptContext, TxInfo, scriptContextTxInfo, txSignedBy)
import qualified Plutus.V1.Ledger.Scripts            as LedgerScriptsV1
import qualified Plutus.V1.Ledger.Value              as LedgerValueV1
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Schema                              (ToSchema)
import qualified Text.Printf                         as TextPrintf (printf)
import qualified Wallet.Emulator.Wallet              as WalletEmulator

mkPolicy :: Ledger.PaymentPubKeyHash -> () -> LedgerContextsV1.ScriptContext -> Bool
mkPolicy pkh () ctx =

        traceIfFalse  "Creator's signature missing!!! " signedByCreator

    where

        info :: LedgerContextsV1.TxInfo
        info = LedgerContextsV1.scriptContextTxInfo ctx

        signedByCreator :: Bool
        signedByCreator = do
            LedgerContextsV1.txSignedBy info $ Ledger.unPaymentPubKeyHash pkh


policy :: Ledger.PaymentPubKeyHash -> LedgerScriptsV1.MintingPolicy
policy pkh = LedgerScriptsV1.mkMintingPolicyScript $
 $$(PlutusTx.compile [|| UtilsTypedScriptsMintingV1.mkUntypedMintingPolicy . mkPolicy ||])
     `PlutusTx.applyCode`
    PlutusTx.liftCode pkh

curSymbol :: Ledger.PaymentPubKeyHash -> LedgerValueV1.CurrencySymbol
curSymbol pkh = UtilsScriptsV1.scriptCurrencySymbol  $ policy pkh

data MintParams = MintParams
    { 
        mpTokenName :: !Ledger.TokenName,
        mpAmount    :: !Integer
    } deriving (GHCGenerics.Generic, DataAeson.FromJSON, DataAeson.ToJSON, P.Show, Schema.ToSchema)

type MintSchema = PlutusContract.Endpoint "mint" MintParams

mint :: MintParams -> PlutusContract.Contract w MintSchema DataText.Text ()
mint mp = do
    pkh <- PlutusContract.ownFirstPaymentPubKeyHash
    let val     = LedgerValueV1.singleton (curSymbol pkh ) (mpTokenName mp) (mpAmount mp)
        lookups = LedgerConstraints.plutusV1MintingPolicy $ (policy pkh)
        tx      = LedgerConstraints.mustMintValue val
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
