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

import           Data.Aeson                          (DataAeson.ToJSON, DataAeson.FromJSON)
import           Data.OpenApi.Schema                 (ToSchema)
import           GHC.Generics                        (Generic)
import           Ledger                              (Address)
import           Plutus.PAB.Effects.Contract.Builtin (Empty, PABEffectsContractBuiltin.HasDefinitions (..), SomeBuiltin (..), endpointsToSchemas)
import           Prettyprinter                       (Pretty (..), viaShow)
import qualified Wallet.Emulator.Wallet              as WalletEmulator              (knownWallet, mockWalletAddress)

--Import Nuevos

--Import Internos
import qualified Validators.StakeSimpleV1.OffChain   
import qualified Validators.StakeSimpleV1.Typos   

data ValidatorContracts = 
    MasterCreatePool MasterCreatePoolParams |
    MasterFundPool MasterFundPoolParams |
    MasterGetBackFund MasterGetBackFundParams |
    UserInvest UserInvestParams |
    UserGetBackInvest UserGetBackInvestParams |
    UserGetRewards UserGetRewardsParams |
    UserInvestRewards UserInvestRewardsParams
    deriving (P.Eq, P.Ord, P.Show, GHCGenerics.Generic)
    deriving anyclass DataOpenApiSchema.ToSchema
    deriving anyclass (DataAeson.ToJSON, DataAeson.FromJSON)

instance Prettyprinter.Pretty ValidatorContracts where
    pretty = Prettyprinter.viaShow

instance PABEffectsContractBuiltin.HasDefinitions ValidatorContracts where

    getDefinitions        = [
            MasterCreatePool exampleMasterCreatePoolParams ,
            MasterFundPool exampleMasterFundPoolParams ,
            MasterGetBackFund exampleMasterGetBackFundParams ,
            UserInvest exampleUserInvestParams ,
            UserGetBackInvest exampleUserGetBackInvestParams ,
            UserGetRewards exampleUserGetRewardsParams ,
            UserInvestRewards exampleUserInvestRewardsParams

        ]

    getContract (MasterCreatePool mcpParams)=   PABEffectsContractBuiltin.SomeBuiltin $ masterCreatePool @() @PABEffectsContractBuiltin.Empty mcpParams
    getContract (MasterFundPool mfpParams) =    PABEffectsContractBuiltin.SomeBuiltin $ masterFundPool @() @PABEffectsContractBuiltin.Empty mfpParams
    getContract (MasterGetBackFund mgbfParams)= PABEffectsContractBuiltin.SomeBuiltin $ masterGetBackFund @() @PABEffectsContractBuiltin.Empty mgbfParams
    getContract (UserInvest uiParams)=          PABEffectsContractBuiltin.SomeBuiltin $ userInvest @() @PABEffectsContractBuiltin.Empty uiParams
    getContract (UserGetBackInvest ugbiParams)= PABEffectsContractBuiltin.SomeBuiltin $ userGetBackInvest @() @PABEffectsContractBuiltin.Empty ugbiParams
    getContract (UserGetRewards ugrParams)=     PABEffectsContractBuiltin.SomeBuiltin $ userGetRewards @() @PABEffectsContractBuiltin.Empty ugrParams
    getContract (UserInvestRewards uirParams)=  PABEffectsContractBuiltin.SomeBuiltin $ userInvestRewards @() @PABEffectsContractBuiltin.Empty uirParams

    getSchema = const $ PABEffectsContractBuiltin.endpointsToSchemas @PABEffectsContractBuiltin.Empty
    