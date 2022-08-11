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

module Validators.Locker.OnChain
    ( 
      codeValidator,
      typedValidator
    , hashValidator
    , addressValidator
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
import qualified Data.OpenApi.Schema         (ToSchema)
import           Text.Printf          (printf)
import Data.Typeable

import          Plutus.Trace.Emulator  as Emulator
import          Wallet.Emulator.Wallet
import          Data.Default
import          Ledger.TimeSlot 

-- TODO: para hacer V2 scripts

-- import           Cardano.Api                    (PlutusScriptV1, PlutusScriptV2,
--                                                  writeFileTextEnvelope)
-- import           Cardano.Api.Shelley            (PlutusScript (..))
-- import qualified Plutus.Script.Utils.V1.Scripts as PSU.V1
-- import qualified Plutus.Script.Utils.V2.Scripts as PSU.V2
-- import qualified Plutus.V1.Ledger.Api           as PlutusV1
-- import qualified Plutus.V1.Ledger.Contexts      as PlutusV1
-- import qualified Plutus.V2.Ledger.Api           as PlutusV2
--import qualified Plutus.V2.Ledger.Contexts      as PlutusV2

--Import Internos
import  Validators.Locker.Typos        (ValidatorDatum (..) , ValidatorData (..) , ValidatorRedeemer (..))


data ScriptPlazoFijo
instance Scripts.ValidatorTypes ScriptPlazoFijo where
    type instance RedeemerType ScriptPlazoFijo = ValidatorRedeemer
    type instance DatumType ScriptPlazoFijo = ValidatorDatum

{-# INLINABLE mkValidator #-}
mkValidator :: ValidatorDatum -> ValidatorRedeemer -> ScriptContext -> Bool
mkValidator  datum redeemer ctx = 
    traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
    traceIfFalse "deadline not reached" deadlineReached &&
    traceIfFalse "distinto redeemer que datum" (rTipo redeemer == aName ( dData datum))
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ aCreator $ dData datum

    deadlineReached :: Bool
    deadlineReached = contains (from $ aDeadline $ dData datum) $ txInfoValidRange info

typedValidator :: Scripts.TypedValidator ScriptPlazoFijo
typedValidator = Scripts.mkTypedValidator @ScriptPlazoFijo
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @ValidatorDatum @ValidatorRedeemer

codeValidator :: Validator
codeValidator = Scripts.validatorScript typedValidator

hashValidator :: Ledger.ValidatorHash
hashValidator = Scripts.validatorHash typedValidator

addressValidator :: Ledger.Address
addressValidator = scriptHashAddress hashValidator
