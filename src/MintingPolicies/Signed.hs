{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module MintingPolicies.Signed where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import Ledger.Address
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import Text.Read
import           Wallet.Emulator.Wallet

import qualified Data.Map                    as Map
import           Data.Maybe                  (fromJust)
--import           Data.OpenApi.Schema         (ToSchema)
import           Plutus.Contract.Wallet      (getUnspentOutput)


import qualified Prelude


{-# INLINABLE mkPolicy #-}
mkPolicy :: Integer -> PaymentPubKeyHash -> () -> ScriptContext -> Bool
mkPolicy str pkh () ctx =
        traceIfFalse  "wrong secret number" checkMintedAmount &&
        traceIfFalse  "creator's signature missing!!! " signedByCreator 
        
    where 
        info :: TxInfo
        info = scriptContextTxInfo ctx

        signedByCreator :: Bool
        signedByCreator = do 
            txSignedBy info $ unPaymentPubKeyHash pkh


        checkMintedAmount :: Bool
        checkMintedAmount = (str == 111)


policy :: Integer -> PaymentPubKeyHash -> Scripts.MintingPolicy
policy tttt pkh = mkMintingPolicyScript $
 $$(PlutusTx.compile [|| \tttt' pkh' -> Scripts.wrapMintingPolicy $ mkPolicy tttt' pkh' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode tttt
     `PlutusTx.applyCode`
    PlutusTx.liftCode pkh


curSymbol :: Integer -> PaymentPubKeyHash -> CurrencySymbol
curSymbol str pkh = scriptCurrencySymbol (policy str pkh) 

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type FreeSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let val     = Value.singleton (curSymbol 5555 pkh ) (mpTokenName mp) (mpAmount mp)
        lookups = Constraints.mintingPolicy $ (policy 4444 pkh)
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

mkSchemaDefinitions ''FreeSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 555
        }
    callEndpoint @"mint" h2 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 444
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = -222
        }
    void $ Emulator.waitNSlots 1
