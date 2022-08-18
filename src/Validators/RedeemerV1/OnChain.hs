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

module Validators.RedeemerV1.OnChain
  ( 
    codeValidator, 
    typedValidator, 
    hashValidator, 
    addressValidator
  ) where

--Import Externos

import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators as UtilsTypedScriptsValidatorsV1
import qualified Plutus.V1.Ledger.Address            as LedgerAddressV1
import qualified Plutus.V1.Ledger.Contexts           as LedgerContextsV1 (ScriptContext) --, TxInfo, scriptContextTxInfo
import qualified Plutus.V1.Ledger.Scripts            as LedgerScriptsV1
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)

--Import Internos

import qualified Validators.RedeemerV1.Typos         as T (ValidatorDatum (..), ValidatorData (..), ValidatorRedeemer (..))

--Modulo:

data ValidatorScriptV1
instance UtilsTypedScriptsValidatorsV1.ValidatorTypes ValidatorScriptV1 where
    type instance RedeemerType ValidatorScriptV1 = T.ValidatorRedeemer
    type instance DatumType ValidatorScriptV1 = T.ValidatorDatum

{-# INLINABLE mkValidator #-}
mkValidator :: T.ValidatorDatum -> T.ValidatorRedeemer -> LedgerContextsV1.ScriptContext -> Bool
mkValidator  datum redeemer _ = 
    traceIfFalse "Distinto redeemer que datum" (T.rTipo redeemer == T.aName (T.dData datum))

typedValidator :: UtilsTypedScriptsValidatorsV1.TypedValidator ValidatorScriptV1
typedValidator = UtilsTypedScriptsValidatorsV1.mkTypedValidator @ValidatorScriptV1
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = UtilsTypedScriptsValidatorsV1.mkUntypedValidator 

codeValidator :: LedgerScriptsV1.Validator
codeValidator = UtilsTypedScriptsValidatorsV1.validatorScript typedValidator

hashValidator :: LedgerScriptsV1.ValidatorHash
hashValidator = UtilsTypedScriptsValidatorsV1.validatorHash typedValidator

addressValidator :: LedgerAddressV1.Address
addressValidator = UtilsTypedScriptsValidatorsV1.validatorAddress typedValidator
