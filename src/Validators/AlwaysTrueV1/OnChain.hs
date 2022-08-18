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

module Validators.AlwaysTrueV1.OnChain
  ( 
    codeValidator, 
    typedValidator, 
    hashValidator, 
    addressValidator
  ) where

--Import Externos


import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators as UtilsTypedScriptsValidatorsV1
import qualified Plutus.V1.Ledger.Address            as LedgerAddressV1
import qualified Plutus.V1.Ledger.Contexts           as LedgerContextsV1  (ScriptContext)
import qualified Plutus.V1.Ledger.Scripts            as LedgerScriptsV1
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
--import qualified Prelude                             as P 

--Import Internos

import qualified Validators.AlwaysTrueV1.Typos       as T (ValidatorDatum  (..) , ValidatorRedeemer (..)) --, ValidatorData (..)

data ValidatorScriptV1
instance UtilsTypedScriptsValidatorsV1.ValidatorTypes ValidatorScriptV1 where
    type instance RedeemerType ValidatorScriptV1 = T.ValidatorRedeemer
    type instance DatumType ValidatorScriptV1 = T.ValidatorDatum

{-# INLINABLE mkValidator #-}
mkValidator :: T.ValidatorDatum -> T.ValidatorRedeemer -> LedgerContextsV1.ScriptContext -> Bool
mkValidator _ _ _ = True

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
