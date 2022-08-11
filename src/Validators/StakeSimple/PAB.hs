{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}


--Para poder usar varios deriving en lugar de uno solo largo
    -- deriving (Eq, Ord, Show, Generic)
    -- deriving anyclass ToSchema
    -- deriving anyclass (FromJSON, ToJSON)
-- en cambio de:
 -- deriving (Eq, Ord, Show, Generic, ToSchema, FromJSON, ToJSON)

{-# LANGUAGE DerivingStrategies  #-} 


module Validators.StakeSimple.PAB
    ( 
        ValidatorContracts (..)
    ) 
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
import  Validators.StakeSimple.Typos   

data ValidatorContracts = 
    MasterCreatePool MasterCreatePoolParams |
    MasterFundPool MasterFundPoolParams |
    MasterGetBackFund MasterGetBackFundParams |
    UserInvest UserInvestParams |
    UserGetBackInvest UserGetBackInvestParams |
    UserGetRewards UserGetRewardsParams |
    UserInvestRewards UserInvestRewardsParams
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass ToSchema
    deriving anyclass (FromJSON, ToJSON)

instance Pretty ValidatorContracts where
    pretty = viaShow

instance HasDefinitions ValidatorContracts where

    getDefinitions        = [
            MasterCreatePool exampleMasterCreatePoolParams ,
            MasterFundPool exampleMasterFundPoolParams ,
            MasterGetBackFund exampleMasterGetBackFundParams ,
            UserInvest exampleUserInvestParams ,
            UserGetBackInvest exampleUserGetBackInvestParams ,
            UserGetRewards exampleUserGetRewardsParams ,
            UserInvestRewards exampleUserInvestRewardsParams

        ]

    getContract (MasterCreatePool mcpParams)=   SomeBuiltin $ masterCreatePool @() @Empty mcpParams
    getContract (MasterFundPool mfpParams) =    SomeBuiltin $ masterFundPool @() @Empty mfpParams
    getContract (MasterGetBackFund mgbfParams)= SomeBuiltin $ masterGetBackFund @() @Empty mgbfParams
    getContract (UserInvest uiParams)=          SomeBuiltin $ userInvest @() @Empty uiParams
    getContract (UserGetBackInvest ugbiParams)= SomeBuiltin $ userGetBackInvest @() @Empty ugbiParams
    getContract (UserGetRewards ugrParams)=     SomeBuiltin $ userGetRewards @() @Empty ugrParams
    getContract (UserInvestRewards uirParams)=  SomeBuiltin $ userInvestRewards @() @Empty uirParams

    getSchema = const $ endpointsToSchemas @Empty
    