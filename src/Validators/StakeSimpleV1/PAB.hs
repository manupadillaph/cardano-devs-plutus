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

--Para poder usar varios deriving en lugar de uno solo largo
    -- deriving (P.Eq, P.Ord, P.Show, GHCGenerics.Generic)
    -- deriving anyclass DataOpenApiSchema.ToSchema
    -- deriving anyclass (DataAeson.ToJSON, DataAeson.FromJSON)
-- en cambio de:
 -- deriving (P.Eq, P.Ord, P.Show, GHC.Generics.GenericSchema.ToSchema, DataAeson.ToJSON, DataAeson.FromJSON)

{-# LANGUAGE DerivingStrategies  #-} 


module Validators.StakeSimpleV1.PAB
    ( 
        ValidatorContracts (..)
    ) 
    where

-- import           Data.Aeson                          (DataAeson.ToJSON, DataAeson.FromJSON)
-- import           Data.OpenApi.Schema                 (ToSchema)
-- import           GHC.Generics                        (Generic)
-- import           Ledger                              (Address)
-- import           Plutus.PAB.Effects.Contract.Builtin (Empty, PABEffectsContractBuiltin.HasDefinitions (..), SomeBuiltin (..), endpointsToSchemas)
-- import           Prettyprinter                       (Pretty (..), viaShow)
-- import qualified Wallet.Emulator.Wallet              as WalletEmulator              (knownWallet, mockWalletAddress)

-- --Import Nuevos

-- --Import Internos
-- import qualified Validators.StakeSimpleV1.OffChain   
-- import qualified Validators.StakeSimpleV1.Typos   

--Import Internos

import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Plutus.PAB.Effects.Contract.Builtin as PABEffectsContractBuiltin (Empty, HasDefinitions (..), SomeBuiltin (..), endpointsToSchemas)
--import qualified Plutus.V1.Ledger.Address            as LedgerAddressV1
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Prettyprinter                       (Pretty (..), viaShow)
--import qualified Wallet.Emulator.Wallet              as WalletEmulator (knownWallet, mockWalletAddress)

--Import Internos

import qualified Validators.StakeSimpleV1.OffChain    as OffChain
import qualified Validators.StakeSimpleV1.Typos       as T

--Modulo

data ValidatorContracts = 
    MasterCreatePool T.MasterCreatePoolParams |
    MasterFundPool T.MasterFundPoolParams |
    MasterGetBackFund T.MasterGetBackFundParams |
    UserInvest T.UserInvestParams |
    UserGetBackInvest T.UserGetBackInvestParams |
    UserGetRewards T.UserGetRewardsParams |
    UserInvestRewards T.UserInvestRewardsParams
    deriving (P.Eq, P.Ord, P.Show, GHCGenerics.Generic)
    deriving anyclass DataOpenApiSchema.ToSchema
    deriving anyclass (DataAeson.ToJSON, DataAeson.FromJSON)

instance Prettyprinter.Pretty ValidatorContracts where
    pretty = Prettyprinter.viaShow

instance PABEffectsContractBuiltin.HasDefinitions ValidatorContracts where

    getDefinitions        = [
            MasterCreatePool T.exampleMasterCreatePoolParams ,
            MasterFundPool T.exampleMasterFundPoolParams ,
            MasterGetBackFund T.exampleMasterGetBackFundParams ,
            UserInvest T.exampleUserInvestParams ,
            UserGetBackInvest T.exampleUserGetBackInvestParams ,
            UserGetRewards T.exampleUserGetRewardsParams ,
            UserInvestRewards T.exampleUserInvestRewardsParams

        ]

    getContract (MasterCreatePool mcpParams)=   PABEffectsContractBuiltin.SomeBuiltin $ OffChain.masterCreatePool @() @PABEffectsContractBuiltin.Empty mcpParams
    getContract (MasterFundPool mfpParams) =    PABEffectsContractBuiltin.SomeBuiltin $ OffChain.masterFundPool @() @PABEffectsContractBuiltin.Empty mfpParams
    getContract (MasterGetBackFund mgbfParams)= PABEffectsContractBuiltin.SomeBuiltin $ OffChain.masterGetBackFund @() @PABEffectsContractBuiltin.Empty mgbfParams
    getContract (UserInvest uiParams)=          PABEffectsContractBuiltin.SomeBuiltin $ OffChain.userInvest @() @PABEffectsContractBuiltin.Empty uiParams
    getContract (UserGetBackInvest ugbiParams)= PABEffectsContractBuiltin.SomeBuiltin $ OffChain.userGetBackInvest @() @PABEffectsContractBuiltin.Empty ugbiParams
    getContract (UserGetRewards ugrParams)=     PABEffectsContractBuiltin.SomeBuiltin $ OffChain.userGetRewards @() @PABEffectsContractBuiltin.Empty ugrParams
    getContract (UserInvestRewards uirParams)=  PABEffectsContractBuiltin.SomeBuiltin $ OffChain.userInvestRewards @() @PABEffectsContractBuiltin.Empty uirParams

    getSchema = const $ PABEffectsContractBuiltin.endpointsToSchemas @PABEffectsContractBuiltin.Empty
    