{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Validators.Locker.PAB
    ( 
        ValidatorContracts (..)
    ) where

import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.OpenApi.Schema                 (ToSchema)
import           GHC.Generics                        (Generic)
import           Ledger                              (Address)
import           Plutus.PAB.Effects.Contract.Builtin (Empty, HasDefinitions (..), SomeBuiltin (..), endpointsToSchemas)
import           Prettyprinter                       (Pretty (..), viaShow)
import           Wallet.Emulator.Wallet              (knownWallet, mockWalletAddress)


--Import Internos
import qualified Validators.Locker.OffChain                     as ValidatorOffChain

data ValidatorContracts = Start ValidatorOffChain.StartParams | Get ValidatorOffChain.GetParams
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, ToSchema)

instance Pretty ValidatorContracts where
    pretty = viaShow

instance HasDefinitions ValidatorContracts where

    getDefinitions        = [Start exampleStartParams, Get exampleGetParams]

    getContract (Start sp)= SomeBuiltin $ ValidatorOffChain.start @() @Empty sp
    getContract (Get gp) =  SomeBuiltin $ ValidatorOffChain.get @() @Empty gp

    getSchema = const $ endpointsToSchemas @Empty

exampleStartParams :: ValidatorOffChain.StartParams
exampleStartParams = ValidatorOffChain.StartParams
    { 
        ValidatorOffChain.spDeadline = 1657143764000,
        ValidatorOffChain.spName = 0,
        ValidatorOffChain.spAdaQty  = 0
    }

exampleGetParams :: ValidatorOffChain.GetParams
exampleGetParams = ValidatorOffChain.GetParams
    { 
        ValidatorOffChain.gpName = 0,
        ValidatorOffChain.gpAdaQty  = 0
    }

