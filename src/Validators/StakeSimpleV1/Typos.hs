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

module Validators.StakeSimpleV1.Typos where

import           Control.Monad        hiding (fmap)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           Data.String  
import qualified GHC.Generics                        as GHCGenerics (Generic)
import           Ledger               hiding (singleton)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           LedgerValueV1.Value         as Value
import           Ledger.Ada           as Ada
import           Playground.Contract  (IO, ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (unless)
import qualified Prelude              as P 
import qualified Schema                              (ToSchema)
import qualified      Data.OpenApi.Schema         (ToSchema)
import           Text.Printf          (printf)
import Data.Typeable

import          Plutus.Trace.Emulator  as Emulator
import          Wallet.Emulator.Wallet
import          Data.Default
import          Ledger.TimeSlot 

--Import Nuevos

--Synonimus definition for clear writting

type Masters = [Ledger.PaymentPubKeyHash]
type Master = Ledger.PaymentPubKeyHash
type Fund = Integer

type Interest = Integer

type User = Ledger.PaymentPubKeyHash
type Invest = Integer
type Proffit = Integer
type Deadline = LedgerApiV1.POSIXTime
type NFT = Ledger.AssetClass
type PoolNFT = NFT
type UserNFT = NFT

--Type for parametrized validator

data PoolParams = PoolParams
    { 
        ppMasters :: Masters ,           
        ppInterest    :: Interest ,
        ppMinumunInvest    :: Invest ,  
        ppMinumunCompoundInvest    :: Invest , 
        ppDeadline    :: Deadline , 
        ppPoolNFT  :: NFT,
        ppPoolNFTTxOutRef :: LedgerApiV1.TxOutRef,
        ppCurSymbolForMintingNFTPolicy :: LedgerApiV1.CurrencySymbol,
        ppValidTimeRange :: LedgerApiV1.POSIXTime,
        ppMinimunClaim :: Proffit
        -- TODO:
        -- Minimut invest time: 
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema,  P.Show)
PlutusTx.makeLift ''PoolParams


--Types for Datums

data MasterFunder = MasterFunder{
        mfMaster :: Master,
        mfFund :: Fund
    }deriving (P.Eq, P.Show, GHCGenerics.Generic)
    deriving anyclass (DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq  MasterFunder where
    {-# INLINABLE (==) #-}
    mi1 == mi2 = mfMaster mi1 ==    mfMaster mi2 && 
                                    mfFund mi1 == mfFund mi2

PlutusTx.unstableMakeIsData ''MasterFunder
--PlutusTx.makeLift ''MastermasterFunderss

data PoolStateTypo  = PoolStateTypo { 
        psPoolNFT :: PoolNFT,
        psMasterFunders   :: [MasterFunder],
        psUsersNFT :: [UserNFT]
    } 
  deriving (P.Eq, P.Show, GHCGenerics.Generic)
  deriving anyclass (DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq  PoolStateTypo where
    {-# INLINABLE (==) #-}
    ps1 == ps2 = psPoolNFT ps1 ==   psPoolNFT  ps2 && 
                                    psMasterFunders ps1 == psMasterFunders ps2 && 
                                    psUsersNFT ps1 == psUsersNFT ps2

PlutusTx.unstableMakeIsData ''PoolStateTypo
--PlutusTx.makeLift ''PoolStateTypo

data UserStateTypo = UserStateTypo
    {
        usUser   :: User,
        usUserNFT   :: UserNFT,
        usInvest   :: Invest,
        usCreatedAt  :: LedgerApiV1.POSIXTime,
        usDeadline   :: Deadline,
        usChashedOut   :: Proffit,
        usRewardsNotClaimed   :: Proffit,
        usLastClaimAt   :: (Maybe LedgerApiV1.POSIXTime)
    } deriving (P.Eq, P.Show, GHCGenerics.Generic)
  deriving anyclass (DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq  UserStateTypo where
    {-# INLINABLE (==) #-}
    us1 == us2 =    usUser  us1 == usUser  us2 &&
                    usUserNFT us1 == usUserNFT us2 &&
                    usInvest us1 == usInvest us2 &&
                    usCreatedAt us1 == usCreatedAt us2  &&
                    usDeadline us1 == usDeadline us2  &&
                    usChashedOut us1 == usChashedOut us2  &&
                    usRewardsNotClaimed us1 == usRewardsNotClaimed us2 &&
                    usLastClaimAt us1 == usLastClaimAt us2


PlutusTx.unstableMakeIsData ''UserStateTypo
--PlutusTx.makeLift ''UserStateTypo

data ValidatorDatum = 
    PoolState PoolStateTypo | 
    UserState UserStateTypo
  deriving (P.Eq, P.Show, GHCGenerics.Generic)
  deriving anyclass (DataAeson.ToJSON, DataAeson.FromJSON)


instance Eq  ValidatorDatum where
    {-# INLINABLE (==) #-}
    PoolState ps1 == PoolState ps2 = ps1 == ps2
    UserState us1  == UserState us2 = us1 == us2
    _ == _ = False

PlutusTx.unstableMakeIsData ''ValidatorDatum
--PlutusTx.makeLift ''ValidatorDatum

--Usefull functions to create the different data types

mkMasterFunder :: Master -> Fund -> MasterFunder
mkMasterFunder master fund = MasterFunder { mfMaster = master , mfFund = fund}

mkPoolStateTypo ::  PoolNFT ->  [MasterFunder] -> [UserNFT] -> PoolStateTypo
mkPoolStateTypo  poolNFT masterFunders userNFTs = PoolStateTypo {psPoolNFT = poolNFT  ,psMasterFunders = masterFunders , psUsersNFT = userNFTs}

mkPoolState :: PoolNFT -> [MasterFunder] -> [UserNFT] -> ValidatorDatum
mkPoolState  poolNFT masterFunders userNFTs = PoolState $ mkPoolStateTypo  poolNFT masterFunders userNFTs



mkUserStateTypo :: User -> UserNFT -> Invest -> LedgerApiV1.POSIXTime -> T.Deadline -> Proffit -> Proffit -> Maybe LedgerApiV1.POSIXTime   -> UserStateTypo
mkUserStateTypo user userNFT invest createdat deadline cashedout rewardsNotClaimed  lastClaim = UserStateTypo { usUser = user, usUserNFT = userNFT , usInvest = invest ,usCreatedAt = createdat , usDeadline = deadline , usRewardsNotClaimed = rewardsNotClaimed , usChashedOut = cashedout, usLastClaimAt = lastClaim }


mkUserState:: User -> UserNFT ->  Invest -> LedgerApiV1.POSIXTime -> T.Deadline -> Proffit -> Proffit  -> Maybe LedgerApiV1.POSIXTime -> ValidatorDatum
mkUserState user userNFT invest createdat deadline  cashedout rewardsNotClaimed lastClaim = UserState $ mkUserStateTypo user userNFT invest createdat deadline cashedout  rewardsNotClaimed lastClaim



--Types for Redeemers

data RedeemMasterFundPoolTypo  = RedeemMasterFundPoolTypo { 
        rmfpPoolNFT :: PoolNFT ,
        rmfpMaster :: Master ,
        rmfpFund :: Fund
    } 
  deriving (P.Eq, P.Show, GHCGenerics.Generic)
  deriving anyclass (DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq  RedeemMasterFundPoolTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  rmfpPoolNFT r1 ==          rmfpPoolNFT  r2 && 
                rmfpMaster r1 ==           rmfpMaster  r2 && 
                rmfpFund r1 ==    	     rmfpFund  r2


PlutusTx.unstableMakeIsData ''RedeemMasterFundPoolTypo
PlutusTx.makeLift ''RedeemMasterFundPoolTypo

data RedeemMasterGetPoolTypo  = RedeemMasterGetPoolTypo { 
        rmgpPoolNFT :: PoolNFT ,
        rmgpPoolNFTTokenName :: LedgerValueV1.TokenName,
        rmgpPoolNFTTxOutRef :: LedgerApiV1.TxOutRef ,
        rmgpMaster :: Master ,
        rmgpFund :: Fund
    } 
  deriving (P.Eq, P.Show, GHCGenerics.Generic)
  deriving anyclass (DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq  RedeemMasterGetPoolTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  rmgpPoolNFT r1 ==          rmgpPoolNFT  r2 && 
                rmgpPoolNFTTokenName r1 == rmgpPoolNFTTokenName  r2 && 
                rmgpPoolNFTTxOutRef r1 ==  rmgpPoolNFTTxOutRef  r2 && 
                rmgpMaster r1 ==           rmgpMaster  r2 && 
                rmgpFund r1 ==    	        rmgpFund  r2


PlutusTx.unstableMakeIsData ''RedeemMasterGetPoolTypo
PlutusTx.makeLift ''RedeemMasterGetPoolTypo

data RedeemUserInvestTypo  = RedeemUserInvestTypo { 
        ruiPoolNFT :: PoolNFT ,
        ruiUserNFT :: UserNFT ,
        ruiUserNFTTokenName :: LedgerValueV1.TokenName,
        ruiUserNFTTxOutRef :: LedgerApiV1.TxOutRef ,
        ruiUser :: User ,
        ruiInvest :: Invest,
        ruiCreatedAt :: LedgerApiV1.POSIXTime,
        ruiDeadline :: Deadline
    }  
  deriving (P.Eq, P.Show, GHCGenerics.Generic)
  deriving anyclass (DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq  RedeemUserInvestTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  ruiPoolNFT r1 ==              ruiPoolNFT  r2 && 
                ruiUserNFT r1 ==              ruiUserNFT  r2 && 
                ruiUserNFTTokenName r1 ==     ruiUserNFTTokenName  r2 && 
                ruiUserNFTTxOutRef r1 ==      ruiUserNFTTxOutRef  r2 && 
                ruiUser r1 ==                 ruiUser  r2 && 
                ruiInvest r1 ==               ruiInvest  r2 && 
                ruiCreatedAt r1 ==            ruiCreatedAt  r2 && 
                ruiDeadline r1 ==             ruiDeadline  r2

PlutusTx.unstableMakeIsData ''RedeemUserInvestTypo
PlutusTx.makeLift ''RedeemUserInvestTypo


data RedeemUserGetInvestTypo  = RedeemUserGetInvestTypo { 
        rugiPoolNFT :: PoolNFT ,
        rugiUserNFT :: UserNFT ,
        rugiUserNFTTokenName :: LedgerValueV1.TokenName,
        rugiUserNFTTxOutRef :: LedgerApiV1.TxOutRef ,
        rugiUser :: User ,
        rugiInvest :: Invest,
        rugiCreatedAt :: LedgerApiV1.POSIXTime,
        rugiDeadline :: Deadline
    }  
  deriving (P.Eq, P.Show, GHCGenerics.Generic)
  deriving anyclass (DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq  RedeemUserGetInvestTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  rugiPoolNFT r1 ==              rugiPoolNFT  r2 && 
                rugiUserNFT r1 ==              rugiUserNFT  r2 && 
                rugiUserNFTTokenName r1 ==     rugiUserNFTTokenName  r2 && 
                rugiUserNFTTxOutRef r1 ==      rugiUserNFTTxOutRef  r2 && 
                rugiUser r1 ==                 rugiUser  r2 && 
                rugiInvest r1 ==               rugiInvest  r2 && 
                rugiCreatedAt r1 ==            rugiCreatedAt  r2 && 
                rugiDeadline r1 ==             rugiDeadline  r2

PlutusTx.unstableMakeIsData ''RedeemUserGetInvestTypo
PlutusTx.makeLift ''RedeemUserGetInvestTypo


data RedeemUserGetRewardsTypo  = RedeemUserGetRewardsTypo { 
        rugrPoolNFT :: PoolNFT ,
        rugrUserNFT :: UserNFT ,
        -- rugrUserNFTTokenName :: LedgerValueV1.TokenName,
        -- rugrUserNFTTxOutRef :: LedgerApiV1.TxOutRef ,
        rugrUser :: User ,
        rugrClaim :: Proffit,
        rugrClaimAt :: LedgerApiV1.POSIXTime
        -- rugrCreatedAt :: LedgerApiV1.POSIXTime,
        -- rugrDeadline :: Deadline
    }  
  deriving (P.Eq, P.Show, GHCGenerics.Generic)
  deriving anyclass (DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq  RedeemUserGetRewardsTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  rugrPoolNFT r1 ==              rugrPoolNFT  r2 && 
                rugrUserNFT r1 ==              rugrUserNFT  r2 && 
                -- rugrUserNFTTokenName r1 ==     rugrUserNFTTokenName  r2 && 
                -- rugrUserNFTTxOutRef r1 ==      rugrUserNFTTxOutRef  r2 && 
                rugrUser r1 ==                 rugrUser  r2 && 
                rugrClaim r1 ==                 rugrClaim  r2 
                -- rugrCreatedAt r1 ==            rugrCreatedAt  r2 && 
                -- rugrDeadline r1 ==             rugrDeadline  r2

PlutusTx.unstableMakeIsData ''RedeemUserGetRewardsTypo
PlutusTx.makeLift ''RedeemUserGetRewardsTypo


data RedeemUserInvestRewardsTypo  = RedeemUserInvestRewardsTypo { 
        ruirPoolNFT :: PoolNFT ,
        ruirUserNFT :: UserNFT ,
        ruirUserNFTTokenName :: LedgerValueV1.TokenName,
        ruirUserNFTTxOutRef :: LedgerApiV1.TxOutRef ,
        ruirUser :: User ,
        ruirInvest :: Invest,
        ruirCreatedAt :: LedgerApiV1.POSIXTime,
        ruirDeadline :: Deadline
    }  
  deriving (P.Eq, P.Show, GHCGenerics.Generic)
  deriving anyclass (DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq  RedeemUserInvestRewardsTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  ruirPoolNFT r1 ==              ruirPoolNFT  r2 && 
                ruirUserNFT r1 ==              ruirUserNFT  r2 && 
                ruirUserNFTTokenName r1 ==     ruirUserNFTTokenName  r2 && 
                ruirUserNFTTxOutRef r1 ==      ruirUserNFTTxOutRef  r2 && 
                ruirUser r1 ==                 ruirUser  r2 && 
                ruirInvest r1 ==               ruirInvest  r2 && 
                ruirCreatedAt r1 ==            ruirCreatedAt  r2 && 
                ruirDeadline r1 ==             ruirDeadline  r2

PlutusTx.unstableMakeIsData ''RedeemUserInvestRewardsTypo
PlutusTx.makeLift ''RedeemUserInvestRewardsTypo

--Redeemers Definition

data ValidatorRedeemer = 
    RedeemMasterFundPool !RedeemMasterFundPoolTypo | 
    RedeemMasterGetPool !RedeemMasterGetPoolTypo |  
    RedeemUserInvest !RedeemUserInvestTypo |   
    RedeemUserGetInvest !RedeemUserGetInvestTypo |  
    RedeemUserGetRewards !RedeemUserGetRewardsTypo |  
    RedeemUserInvestRewards !RedeemUserInvestRewardsTypo   
    deriving P.Show

instance Eq  ValidatorRedeemer where
    {-# INLINABLE (==) #-}
    RedeemMasterFundPool rmfp1  == RedeemMasterFundPool rmfp2   =  rmfp1 == rmfp2
    RedeemMasterGetPool rmgp1 == RedeemMasterGetPool  rmgp2 = rmgp1 == rmgp2
    RedeemUserInvest rui1 == RedeemUserInvest rui2 =  rui1 == rui2
    RedeemUserGetInvest  rugi1 == RedeemUserGetInvest rugi2 = rugi1 == rugi2
    RedeemUserGetRewards rugr1 == RedeemUserGetRewards rugr2 = rugr1 == rugr2
    RedeemUserInvestRewards ruir1 == RedeemUserInvestRewards ruir2 = ruir1 == ruir2
    _ == _ = False

PlutusTx.makeIsDataIndexed ''ValidatorRedeemer [ 
        ('RedeemMasterFundPool, 0),
        ('RedeemMasterGetPool, 1),
        ('RedeemUserInvest,  2),
        ('RedeemUserGetInvest, 3),
        ('RedeemUserGetRewards,  4),
        ('RedeemUserInvestRewards,  5)
    ]

PlutusTx.makeLift ''ValidatorRedeemer

--Redeemers helpers

mkRedeemMasterFundPool :: PoolNFT -> Master -> Fund -> LedgerApiV1.Redeemer
mkRedeemMasterFundPool poolNFT master fund = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData (RedeemMasterFundPool $ mkRedeemMasterFundPoolTypo poolNFT  master fund  )

mkRedeemMasterFundPoolTypo ::  PoolNFT -> Master -> Fund -> RedeemMasterFundPoolTypo
mkRedeemMasterFundPoolTypo   poolNFT master fund  = RedeemMasterFundPoolTypo {
        rmfpPoolNFT = poolNFT  ,
        rmfpMaster = master, 
        rmfpFund = fund
    }

mkRedeemMasterGetPool :: PoolNFT -> LedgerValueV1.TokenName -> LedgerApiV1.TxOutRef -> Master -> Fund -> LedgerApiV1.Redeemer
mkRedeemMasterGetPool poolNFT tn txoutref master fund = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData (RedeemMasterGetPool $ mkRedeemMasterGetPoolTypo poolNFT tn txoutref master fund  )

mkRedeemMasterGetPoolTypo ::  PoolNFT -> LedgerValueV1.TokenName -> LedgerApiV1.TxOutRef -> Master -> Fund -> RedeemMasterGetPoolTypo
mkRedeemMasterGetPoolTypo   poolNFT tn txoutref master fund  = RedeemMasterGetPoolTypo {
        rmgpPoolNFT = poolNFT  ,
        rmgpPoolNFTTokenName = tn , 
        rmgpPoolNFTTxOutRef = txoutref, 
        rmgpMaster = master, 
        rmgpFund = fund
    }

mkRedeemUserInvest :: PoolNFT -> UserNFT -> LedgerValueV1.TokenName -> LedgerApiV1.TxOutRef -> User -> Invest -> LedgerApiV1.POSIXTime -> T.Deadline -> LedgerApiV1.Redeemer 
mkRedeemUserInvest poolNFT userNFT tn txoutref user invest createdAt deadline = LedgerApiV1.Redeemer $  PlutusTx.toBuiltinData (RedeemUserInvest  $ mkRedeemUserInvestTypo poolNFT userNFT tn txoutref user invest  createdAt deadline)

mkRedeemUserInvestTypo ::  PoolNFT -> UserNFT -> LedgerValueV1.TokenName -> LedgerApiV1.TxOutRef -> User -> Invest -> LedgerApiV1.POSIXTime -> T.Deadline -> RedeemUserInvestTypo
mkRedeemUserInvestTypo  poolNFT userNFT tn txoutref user invest createdAt deadline = RedeemUserInvestTypo { 
        ruiPoolNFT =  poolNFT,
        ruiUserNFT =  userNFT ,
        ruiUserNFTTokenName =  tn ,
        ruiUserNFTTxOutRef =  txoutref ,
        ruiUser =  user ,
        ruiInvest = invest,
        ruiCreatedAt =  createdAt,
        ruiDeadline = deadline
    }  

mkRedeemUserGetInvest :: PoolNFT -> UserNFT -> LedgerValueV1.TokenName -> LedgerApiV1.TxOutRef -> User -> Invest -> LedgerApiV1.POSIXTime -> T.Deadline -> LedgerApiV1.Redeemer
mkRedeemUserGetInvest poolNFT userNFT tn txoutref user invest createdAt deadline = LedgerApiV1.Redeemer $  PlutusTx.toBuiltinData (RedeemUserGetInvest  $ mkRedeemUserGetInvestTypo poolNFT userNFT tn txoutref user invest  createdAt deadline)

mkRedeemUserGetInvestTypo ::  PoolNFT -> UserNFT -> LedgerValueV1.TokenName -> LedgerApiV1.TxOutRef -> User -> Invest -> LedgerApiV1.POSIXTime -> T.Deadline -> RedeemUserGetInvestTypo
mkRedeemUserGetInvestTypo    poolNFT userNFT tn txoutref user invest createdAt deadline  = RedeemUserGetInvestTypo {
        rugiPoolNFT =  poolNFT,
        rugiUserNFT =  userNFT ,
        rugiUserNFTTokenName =  tn ,
        rugiUserNFTTxOutRef =  txoutref ,
        rugiUser =  user ,
        rugiInvest = invest,
        rugiCreatedAt =  createdAt,
        rugiDeadline = deadline
    }

mkRedeemUserGetRewards ::PoolNFT -> UserNFT -> User -> Proffit -> LedgerApiV1.POSIXTime -> LedgerApiV1.Redeemer
mkRedeemUserGetRewards poolNFT userNFT user claim claimAt = LedgerApiV1.Redeemer $  PlutusTx.toBuiltinData (RedeemUserGetRewards  $ mkRedeemUserGetRewardsTypo poolNFT userNFT  user claim claimAt  )

mkRedeemUserGetRewardsTypo ::  PoolNFT -> UserNFT -> User -> Proffit-> LedgerApiV1.POSIXTime ->   RedeemUserGetRewardsTypo
mkRedeemUserGetRewardsTypo    poolNFT userNFT  user claim  claimAt = RedeemUserGetRewardsTypo {
        rugrPoolNFT =  poolNFT,
        rugrUserNFT =  userNFT ,
        rugrUser =  user ,
        rugrClaim = claim,
        rugrClaimAt = claimAt
    }

mkRedeemUserInvestRewards ::PoolNFT -> UserNFT -> LedgerValueV1.TokenName -> LedgerApiV1.TxOutRef -> User -> Invest -> LedgerApiV1.POSIXTime -> T.Deadline -> LedgerApiV1.Redeemer
mkRedeemUserInvestRewards poolNFT userNFT tn txoutref user invest createdAt deadline = LedgerApiV1.Redeemer $  PlutusTx.toBuiltinData (RedeemUserInvestRewards  $ mkRedeemUserInvestRewardsTypo poolNFT userNFT tn txoutref user invest  createdAt deadline)

mkRedeemUserInvestRewardsTypo ::  PoolNFT -> UserNFT -> LedgerValueV1.TokenName -> LedgerApiV1.TxOutRef -> User -> Invest -> LedgerApiV1.POSIXTime -> T.Deadline -> RedeemUserInvestRewardsTypo
mkRedeemUserInvestRewardsTypo   poolNFT userNFT tn txoutref user invest createdAt deadline  = RedeemUserInvestRewardsTypo {
        ruirPoolNFT =  poolNFT,
        ruirUserNFT =  userNFT ,
        ruirUserNFTTokenName =  tn ,
        ruirUserNFTTxOutRef =  txoutref ,
        ruirUser =  user ,
        ruirInvest = invest,
        ruirCreatedAt =  createdAt,
        ruirDeadline = deadline
    }



--Types for Minting Policy Redeemers

data MintingRedeemer = MintingRedeemer
    { 
        mrTokenName   :: LedgerValueV1.TokenName,
        mrTxOutRef    :: LedgerApiV1.TxOutRef 
    } deriving  P.Show

instance Eq  MintingRedeemer where
    {-# INLINABLE (==) #-}
    r1 == r2 =    
        mrTokenName  r1 == mrTokenName  r2  &&
        mrTxOutRef  r1 == mrTxOutRef  r2    

PlutusTx.unstableMakeIsData ''MintingRedeemer

mkMintingRedeemer :: LedgerValueV1.TokenName -> LedgerApiV1.TxOutRef -> LedgerApiV1.Redeemer
mkMintingRedeemer tn txoutref  = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData ( MintingRedeemer 
        {
            mrTokenName = tn, 
            mrTxOutRef= txoutref
        }
    )

--Types for endpoints parameters

data MasterCreatePoolParams = MasterCreatePoolParams
    { 
        mcpPoolParam :: PoolParams, 

        mcpPoolNFTTokenName :: LedgerValueV1.TokenName,
        mcpPoolNFTTxOutRef :: LedgerApiV1.TxOutRef,

        mcpFund    :: Invest
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema,  P.Show)

data MasterFundPoolParams = MasterFundPoolParams
    { 
        mspPoolParam :: PoolParams, 

        -- mspPoolNFTTokenName :: LedgerValueV1.TokenName,
        -- mspPoolNFTTxOutRef :: LedgerApiV1.TxOutRef,

        mspFund    :: Invest
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

newtype MasterGetBackFundParams = MasterGetBackFundParams
    { 
        mgpPoolParam :: PoolParams
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

data UserInvestParams = UserInvestParams
    { 
        uipPoolParam :: PoolParams, 

        uiUserNFTTokenName :: LedgerValueV1.TokenName,
        uiUserNFTTxOutRef :: LedgerApiV1.TxOutRef,

        uipCreatedAt   :: LedgerApiV1.POSIXTime, 
        uipDeadline    :: Deadline, 
        uipInvest    :: Invest
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

data UserGetBackInvestParams = UserGetBackInvestParams
    { 
        ugipPoolParam :: PoolParams, 
        ugipDeadline    :: Deadline
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

data UserGetRewardsParams = UserGetRewardsParams
    { 
        ugrpPoolParam :: PoolParams,
        ugrpUserNFTTokenName :: LedgerValueV1.TokenName,
        ugrpUserNFTTxOutRef :: LedgerApiV1.TxOutRef,
        ugrpClaim    :: Proffit 

    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

data UserInvestRewardsParams = UserInvestRewardsParams
    { 
        uirpPoolParam :: PoolParams, 
        uirpDeadline    :: Deadline
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)


-- Examples

examplePOSIXTime :: LedgerApiV1.POSIXTime
examplePOSIXTime = 1658172331000

exampleTxOutRef :: LedgerApiV1.TxOutRef
exampleTxOutRef = LedgerApiV1.TxOutRef {
            LedgerApiV1.txOutRefId = "aaafff",
            LedgerApiV1.txOutRefIdx = 0
        }

examplePoolParams :: PoolParams
examplePoolParams = PoolParams
    { 
        ppMasters = [] ,           
        ppInterest    = 1 ,
        ppMinumunInvest     = 1,
        ppMinumunCompoundInvest    = 1,
        ppDeadline    = examplePOSIXTime,
        ppPoolNFT  = LedgerValueV1.assetClass LedgerApiV1.adaSymbol LedgerApiV1.adaToken,
        ppPoolNFTTxOutRef = exampleTxOutRef,
        ppCurSymbolForMintingNFTPolicy = adaSymbol,
        ppValidTimeRange  = examplePOSIXTime,
        ppMinimunClaim  = 1
    }

exampleMasterCreatePoolParams :: MasterCreatePoolParams
exampleMasterCreatePoolParams = MasterCreatePoolParams
    { 
        mcpPoolParam = examplePoolParams, 

        mcpPoolNFTTokenName = LedgerApiV1.adaToken,
        mcpPoolNFTTxOutRef = exampleTxOutRef,

        mcpFund    = 100_000_000
    } 

exampleMasterFundPoolParams :: MasterFundPoolParams
exampleMasterFundPoolParams = MasterFundPoolParams
    { 
        mspPoolParam = examplePoolParams, 

        -- mspPoolNFTTokenName = LedgerApiV1.adaToken,
        -- mspPoolNFTTxOutRef = exampleTxOutRef,

        mspFund    = 100_000_000
    } 

exampleMasterGetBackFundParams :: MasterGetBackFundParams
exampleMasterGetBackFundParams = MasterGetBackFundParams
    { 
        mgpPoolParam = examplePoolParams
    } 

exampleUserInvestParams :: UserInvestParams
exampleUserInvestParams = UserInvestParams
    { 
        uipPoolParam = examplePoolParams, 

        uiUserNFTTokenName = LedgerApiV1.adaToken,
        uiUserNFTTxOutRef = exampleTxOutRef,

        uipCreatedAt   = examplePOSIXTime,
        uipDeadline   = examplePOSIXTime,
        uipInvest    = 3_000_000
    } 

exampleUserGetBackInvestParams :: UserGetBackInvestParams
exampleUserGetBackInvestParams = UserGetBackInvestParams
    { 
        ugipPoolParam = examplePoolParams, 
        ugipDeadline   = examplePOSIXTime
    } 

exampleUserGetRewardsParams :: UserGetRewardsParams
exampleUserGetRewardsParams = UserGetRewardsParams
    { 
        ugrpPoolParam = examplePoolParams, 
        ugrpUserNFTTokenName = LedgerApiV1.adaToken,
        ugrpUserNFTTxOutRef = exampleTxOutRef,
        ugrpClaim   = 3_000_000

    } 

exampleUserInvestRewardsParams :: UserInvestRewardsParams
exampleUserInvestRewardsParams = UserInvestRewardsParams
    { 
        uirpPoolParam = examplePoolParams, 
        uirpDeadline   = examplePOSIXTime
    } 
