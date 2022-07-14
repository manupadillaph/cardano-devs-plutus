{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Validators.StakeSimple.PAB
    -- ( 
    --     ValidatorContracts (..)
    -- ) 
    where

import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.OpenApi.Schema                 (ToSchema)
import           GHC.Generics                        (Generic)
import           Ledger                              (Address)
import           Plutus.PAB.Effects.Contract.Builtin (Empty, HasDefinitions (..), SomeBuiltin (..), endpointsToSchemas)
import           Prettyprinter                       (Pretty (..), viaShow)
import           Wallet.Emulator.Wallet              (knownWallet, mockWalletAddress)

--Import Nuevos

--Import Internos
import  Validators.StakeSimple.OffChain   

-- data ValidatorContracts = Start StartParams | Get GetParams
--     deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, ToSchema)

-- instance Pretty ValidatorContracts where
--     pretty = viaShow

-- instance HasDefinitions ValidatorContracts where

--     getDefinitions        = [Start exampleStartParams, Get exampleGetParams]

--     getContract (Start sp)= SomeBuiltin $ start @() @Empty sp
--     getContract (Get gp) =  SomeBuiltin $ get @() @Empty gp

--     getSchema = const $ endpointsToSchemas @Empty

-- exampleStartParams :: StartParams
-- exampleStartParams = StartParams
--     { 
--         spDeadline = 1657143764000,
--         spName = 0,
--         spAdaQty  = 0
--     }

-- exampleGetParams :: GetParams
-- exampleGetParams = GetParams
--     { 
--         gpName = 0,
--         gpAdaQty  = 0
--     }

