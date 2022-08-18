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

module Validators.DeadlineV1.PAB
    ( 
        ValidatorContracts (..)
    ) where

--Import Externos

import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Plutus.PAB.Effects.Contract.Builtin as PABEffectsContractBuiltin (Empty, HasDefinitions (..), SomeBuiltin (..), endpointsToSchemas)
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Prettyprinter                       (Pretty (..), viaShow)

--Import Internos

import qualified Validators.DeadlineV1.OffChain      as OffChain

data ValidatorContracts = Start OffChain.StartParams | Get OffChain.GetParams
    deriving (P.Eq, P.Ord, P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)

instance Prettyprinter.Pretty ValidatorContracts where
    pretty = Prettyprinter.viaShow

instance PABEffectsContractBuiltin.HasDefinitions ValidatorContracts where

    getDefinitions        = [Start exampleStartParams, Get exampleGetParams]

    getContract (Start sp) = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.start @() @PABEffectsContractBuiltin.Empty sp
    getContract (Get gp)   = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.get   @() @PABEffectsContractBuiltin.Empty gp

    getSchema = const $ PABEffectsContractBuiltin.endpointsToSchemas @PABEffectsContractBuiltin.Empty

exampleStartParams :: OffChain.StartParams
exampleStartParams = OffChain.StartParams
    { 
        OffChain.spDeadline = 1657143764000,
        OffChain.spName     = 0,
        OffChain.spAdaQty   = 0
    }

exampleGetParams :: OffChain.GetParams
exampleGetParams = OffChain.GetParams
    { 
        OffChain.gpName    = 0,
        OffChain.gpAdaQty  = 0
    }
