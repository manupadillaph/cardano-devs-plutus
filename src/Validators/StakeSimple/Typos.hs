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
{-# LANGUAGE NumericUnderscores    #-}

--{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module  Validators.StakeSimple.Typos where

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
import qualified Prelude              as HASKELL 
import           Schema               (ToSchema)
import qualified      Data.OpenApi.Schema         (ToSchema)
import           Text.Printf          (printf)
import Data.Typeable

import          Plutus.Trace.Emulator  as Emulator
import          Wallet.Emulator.Wallet
import          Data.Default
import          Ledger.TimeSlot 

--Import Nuevos

--Synonimus definition for clear writting

type Masters = [PaymentPubKeyHash]
type Master = PaymentPubKeyHash
type Fund = Integer

type Interest = Integer

type User = PaymentPubKeyHash
type Invest = Integer
type Proffit = Integer
type Deadline = POSIXTime
type NFT = AssetClass
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
        ppPoolNFTTxOutRef :: TxOutRef,
        ppCurSymbolForMintingNFTPolicy :: CurrencySymbol,
        ppValidTimeRange :: POSIXTime,
        ppMinimunClaim :: Proffit
        -- TODO:
        -- Minimut invest time: 
    } deriving (HASKELL.Eq, HASKELL.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema,  HASKELL.Show)
PlutusTx.makeLift ''PoolParams


--Types for Datums

data MasterFunder = MasterFunder{
        mfMaster :: Master,
        mfFund :: Fund
    }deriving (HASKELL.Eq, HASKELL.Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Eq MasterFunder where
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
  deriving (HASKELL.Eq, HASKELL.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq PoolStateTypo where
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
        usCreatedAt  :: POSIXTime,
        usDeadline   :: Deadline,
        usChashedOut   :: Proffit,
        usRewardsNotClaimed   :: Proffit,
        usLastClaimAt   :: !(Maybe POSIXTime)
    } deriving (HASKELL.Eq, HASKELL.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq UserStateTypo where
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
  deriving (HASKELL.Eq, HASKELL.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


instance Eq ValidatorDatum where
    {-# INLINABLE (==) #-}
    PoolState ps1 == PoolState ps2 = ps1 == ps2
    UserState us1  == UserState us2 = us1 == us2
    _ == _ = False

PlutusTx.unstableMakeIsData ''ValidatorDatum
--PlutusTx.makeLift ''ValidatorDatum

--Usefull functions to create the different data types

mkMasterFunder :: Master -> Fund -> MasterFunder
mkMasterFunder master fund = MasterFunder { mfMaster = master , mfFund = fund}

mkPoolStateTypo ::  PoolNFT ->   [MasterFunder] -> [UserNFT] -> PoolStateTypo
mkPoolStateTypo  poolNFT masterFunders userNFTs = PoolStateTypo {psPoolNFT = poolNFT  ,psMasterFunders = masterFunders , psUsersNFT = userNFTs}

mkPoolState :: PoolNFT -> [MasterFunder] -> [UserNFT] -> ValidatorDatum
mkPoolState  poolNFT masterFunders userNFTs = PoolState $ mkPoolStateTypo  poolNFT masterFunders userNFTs



mkUserStateTypo :: User ->  UserNFT -> Invest -> POSIXTime ->  Deadline -> Proffit ->  Proffit -> Maybe POSIXTime   -> UserStateTypo
mkUserStateTypo user userNFT invest createdat deadline cashedout rewardsNotClaimed  lastClaim = UserStateTypo { usUser = user, usUserNFT = userNFT , usInvest = invest ,usCreatedAt = createdat , usDeadline = deadline , usRewardsNotClaimed = rewardsNotClaimed , usChashedOut = cashedout, usLastClaimAt = lastClaim }


mkUserState:: User ->  UserNFT ->   Invest ->  POSIXTime ->  Deadline -> Proffit ->  Proffit  -> Maybe POSIXTime -> ValidatorDatum
mkUserState user userNFT invest createdat deadline  cashedout rewardsNotClaimed lastClaim = UserState $ mkUserStateTypo user userNFT invest createdat deadline cashedout  rewardsNotClaimed lastClaim



--Types for Redeemers

data RedeemMasterFundPoolTypo  = RedeemMasterFundPoolTypo { 
        rmfpPoolNFT :: !PoolNFT ,
        rmfpMaster :: !Master ,
        rmfpFund :: !Fund
    } 
  deriving (HASKELL.Eq, HASKELL.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq RedeemMasterFundPoolTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  rmfpPoolNFT r1 ==          rmfpPoolNFT  r2 && 
                rmfpMaster r1 ==           rmfpMaster  r2 && 
                rmfpFund r1 ==    	     rmfpFund  r2


PlutusTx.unstableMakeIsData ''RedeemMasterFundPoolTypo
PlutusTx.makeLift ''RedeemMasterFundPoolTypo

data RedeemMasterGetPoolTypo  = RedeemMasterGetPoolTypo { 
        rmgpPoolNFT :: !PoolNFT ,
        rmgpPoolNFTTokenName :: !TokenName ,
        rmgpPoolNFTTxOutRef :: !TxOutRef ,
        rmgpMaster :: !Master ,
        rmgpFund :: !Fund
    } 
  deriving (HASKELL.Eq, HASKELL.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq RedeemMasterGetPoolTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  rmgpPoolNFT r1 ==          rmgpPoolNFT  r2 && 
                rmgpPoolNFTTokenName r1 == rmgpPoolNFTTokenName  r2 && 
                rmgpPoolNFTTxOutRef r1 ==  rmgpPoolNFTTxOutRef  r2 && 
                rmgpMaster r1 ==           rmgpMaster  r2 && 
                rmgpFund r1 ==    	        rmgpFund  r2


PlutusTx.unstableMakeIsData ''RedeemMasterGetPoolTypo
PlutusTx.makeLift ''RedeemMasterGetPoolTypo

data RedeemUserInvestTypo  = RedeemUserInvestTypo { 
        ruiPoolNFT :: !PoolNFT ,
        ruiUserNFT :: !UserNFT ,
        ruiUserNFTTokenName :: !TokenName ,
        ruiUserNFTTxOutRef :: !TxOutRef ,
        ruiUser :: !User ,
        ruiInvest :: !Invest,
        ruiCreatedAt :: !POSIXTime,
        ruiDeadline :: !Deadline
    }  
  deriving (HASKELL.Eq, HASKELL.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq RedeemUserInvestTypo where
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
        rugiPoolNFT :: !PoolNFT ,
        rugiUserNFT :: !UserNFT ,
        rugiUserNFTTokenName :: !TokenName ,
        rugiUserNFTTxOutRef :: !TxOutRef ,
        rugiUser :: !User ,
        rugiInvest :: !Invest,
        rugiCreatedAt :: !POSIXTime,
        rugiDeadline :: !Deadline
    }  
  deriving (HASKELL.Eq, HASKELL.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq RedeemUserGetInvestTypo where
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
        rugrPoolNFT :: !PoolNFT ,
        rugrUserNFT :: !UserNFT ,
        -- rugrUserNFTTokenName :: !TokenName ,
        -- rugrUserNFTTxOutRef :: !TxOutRef ,
        rugrUser :: !User ,
        rugrClaim :: !Proffit,
        rugrClaimAt :: !POSIXTime
        -- rugrCreatedAt :: !POSIXTime,
        -- rugrDeadline :: !Deadline
    }  
  deriving (HASKELL.Eq, HASKELL.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq RedeemUserGetRewardsTypo where
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
        ruirPoolNFT :: !PoolNFT ,
        ruirUserNFT :: !UserNFT ,
        ruirUserNFTTokenName :: !TokenName ,
        ruirUserNFTTxOutRef :: !TxOutRef ,
        ruirUser :: !User ,
        ruirInvest :: !Invest,
        ruirCreatedAt :: !POSIXTime,
        ruirDeadline :: !Deadline
    }  
  deriving (HASKELL.Eq, HASKELL.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq RedeemUserInvestRewardsTypo where
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
    deriving HASKELL.Show

instance Eq ValidatorRedeemer where
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

mkRedeemMasterFundPool :: PoolNFT ->  Master -> Fund -> Redeemer
mkRedeemMasterFundPool poolNFT master fund = Redeemer $ PlutusTx.toBuiltinData (RedeemMasterFundPool $ mkRedeemMasterFundPoolTypo poolNFT  master fund  )

mkRedeemMasterFundPoolTypo ::  PoolNFT ->  Master -> Fund ->  RedeemMasterFundPoolTypo
mkRedeemMasterFundPoolTypo   poolNFT master fund  = RedeemMasterFundPoolTypo {
        rmfpPoolNFT = poolNFT  ,
        rmfpMaster = master, 
        rmfpFund = fund
    }

mkRedeemMasterGetPool :: PoolNFT -> TokenName -> TxOutRef -> Master -> Fund -> Redeemer
mkRedeemMasterGetPool poolNFT tn txoutref master fund = Redeemer $ PlutusTx.toBuiltinData (RedeemMasterGetPool $ mkRedeemMasterGetPoolTypo poolNFT tn txoutref master fund  )

mkRedeemMasterGetPoolTypo ::  PoolNFT -> TokenName -> TxOutRef -> Master -> Fund ->  RedeemMasterGetPoolTypo
mkRedeemMasterGetPoolTypo   poolNFT tn txoutref master fund  = RedeemMasterGetPoolTypo {
        rmgpPoolNFT = poolNFT  ,
        rmgpPoolNFTTokenName = tn , 
        rmgpPoolNFTTxOutRef = txoutref, 
        rmgpMaster = master, 
        rmgpFund = fund
    }

mkRedeemUserInvest :: PoolNFT -> UserNFT -> TokenName -> TxOutRef -> User -> Invest ->  POSIXTime ->  Deadline -> Redeemer 
mkRedeemUserInvest poolNFT userNFT tn txoutref user invest createdAt deadline = Redeemer $  PlutusTx.toBuiltinData (RedeemUserInvest  $ mkRedeemUserInvestTypo poolNFT userNFT tn txoutref user invest  createdAt deadline)

mkRedeemUserInvestTypo ::  PoolNFT -> UserNFT -> TokenName -> TxOutRef -> User -> Invest ->  POSIXTime ->  Deadline ->  RedeemUserInvestTypo
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

mkRedeemUserGetInvest :: PoolNFT -> UserNFT -> TokenName -> TxOutRef -> User -> Invest ->  POSIXTime ->  Deadline ->  Redeemer
mkRedeemUserGetInvest poolNFT userNFT tn txoutref user invest createdAt deadline = Redeemer $  PlutusTx.toBuiltinData (RedeemUserGetInvest  $ mkRedeemUserGetInvestTypo poolNFT userNFT tn txoutref user invest  createdAt deadline)

mkRedeemUserGetInvestTypo ::  PoolNFT -> UserNFT -> TokenName -> TxOutRef -> User -> Invest ->  POSIXTime ->  Deadline -> RedeemUserGetInvestTypo
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

mkRedeemUserGetRewards ::PoolNFT -> UserNFT ->  User -> Proffit -> POSIXTime ->  Redeemer
mkRedeemUserGetRewards poolNFT userNFT user claim claimAt = Redeemer $  PlutusTx.toBuiltinData (RedeemUserGetRewards  $ mkRedeemUserGetRewardsTypo poolNFT userNFT  user claim claimAt  )

mkRedeemUserGetRewardsTypo ::  PoolNFT -> UserNFT ->  User -> Proffit-> POSIXTime ->    RedeemUserGetRewardsTypo
mkRedeemUserGetRewardsTypo    poolNFT userNFT  user claim  claimAt = RedeemUserGetRewardsTypo {
        rugrPoolNFT =  poolNFT,
        rugrUserNFT =  userNFT ,
        rugrUser =  user ,
        rugrClaim = claim,
        rugrClaimAt = claimAt
    }

mkRedeemUserInvestRewards ::PoolNFT -> UserNFT -> TokenName -> TxOutRef -> User -> Invest ->  POSIXTime ->  Deadline ->  Redeemer
mkRedeemUserInvestRewards poolNFT userNFT tn txoutref user invest createdAt deadline = Redeemer $  PlutusTx.toBuiltinData (RedeemUserInvestRewards  $ mkRedeemUserInvestRewardsTypo poolNFT userNFT tn txoutref user invest  createdAt deadline)

mkRedeemUserInvestRewardsTypo ::  PoolNFT -> UserNFT -> TokenName -> TxOutRef -> User -> Invest ->  POSIXTime ->  Deadline ->  RedeemUserInvestRewardsTypo
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
        mrTokenName   :: TokenName ,
        mrTxOutRef    :: TxOutRef 
    } deriving  HASKELL.Show

instance Eq MintingRedeemer where
    {-# INLINABLE (==) #-}
    r1 == r2 =    
        mrTokenName  r1 == mrTokenName  r2  &&
        mrTxOutRef  r1 == mrTxOutRef  r2    

PlutusTx.unstableMakeIsData ''MintingRedeemer

mkMintingRedeemer ::TokenName -> TxOutRef -> Redeemer
mkMintingRedeemer tn txoutref  = Redeemer $ PlutusTx.toBuiltinData ( MintingRedeemer 
        {
            mrTokenName = tn, 
            mrTxOutRef= txoutref
        }
    )

--Types for endpoints parameters

data MasterCreatePoolParams = MasterCreatePoolParams
    { 
        mcpPoolParam :: PoolParams, 

        mcpPoolNFTTokenName :: TokenName,
        mcpPoolNFTTxOutRef :: TxOutRef,

        mcpFund    :: Invest
    } deriving (HASKELL.Eq, HASKELL.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema,  HASKELL.Show)

data MasterFundPoolParams = MasterFundPoolParams
    { 
        mspPoolParam :: PoolParams, 

        -- mspPoolNFTTokenName :: TokenName,
        -- mspPoolNFTTxOutRef :: TxOutRef,

        mspFund    :: Invest
    } deriving (HASKELL.Eq, HASKELL.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema, HASKELL.Show)

newtype MasterGetBackFundParams = MasterGetBackFundParams
    { 
        mgpPoolParam :: PoolParams
    } deriving (HASKELL.Eq, HASKELL.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema, HASKELL.Show)

data UserInvestParams = UserInvestParams
    { 
        uipPoolParam :: PoolParams, 

        uiUserNFTTokenName :: TokenName,
        uiUserNFTTxOutRef :: TxOutRef,

        uipCreatedAt   :: POSIXTime, 
        uipDeadline    :: Deadline, 
        uipInvest    :: Invest
    } deriving (HASKELL.Eq, HASKELL.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema, HASKELL.Show)

data UserGetBackInvestParams = UserGetBackInvestParams
    { 
        ugipPoolParam :: PoolParams, 
        ugipDeadline    :: Deadline
    } deriving (HASKELL.Eq, HASKELL.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema, HASKELL.Show)

data UserGetRewardsParams = UserGetRewardsParams
    { 
        ugrpPoolParam :: PoolParams,
        ugrpUserNFTTokenName :: TokenName,
        ugrpUserNFTTxOutRef :: TxOutRef,
        ugrpClaim    :: Proffit 

    } deriving (HASKELL.Eq, HASKELL.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema, HASKELL.Show)

data UserInvestRewardsParams = UserInvestRewardsParams
    { 
        uirpPoolParam :: PoolParams, 
        uirpDeadline    :: Deadline
    } deriving (HASKELL.Eq, HASKELL.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema, HASKELL.Show)


-- Examples

examplePOSIXTime :: POSIXTime
examplePOSIXTime = 1658172331000

exampleTxOutRef :: TxOutRef
exampleTxOutRef = TxOutRef {
            txOutRefId = "aaafff",
            txOutRefIdx = 0
        }

examplePoolParams :: PoolParams
examplePoolParams = PoolParams
    { 
        ppMasters = [] ,           
        ppInterest    = 1 ,
        ppMinumunInvest     = 1,
        ppMinumunCompoundInvest    = 1,
        ppDeadline    = examplePOSIXTime,
        ppPoolNFT  = assetClass adaSymbol adaToken,
        ppPoolNFTTxOutRef = exampleTxOutRef,
        ppCurSymbolForMintingNFTPolicy = adaSymbol,
        ppValidTimeRange  = examplePOSIXTime,
        ppMinimunClaim  = 1
    }

exampleMasterCreatePoolParams :: MasterCreatePoolParams
exampleMasterCreatePoolParams = MasterCreatePoolParams
    { 
        mcpPoolParam = examplePoolParams, 

        mcpPoolNFTTokenName = adaToken,
        mcpPoolNFTTxOutRef = exampleTxOutRef,

        mcpFund    = 100_000_000
    } 

exampleMasterFundPoolParams :: MasterFundPoolParams
exampleMasterFundPoolParams = MasterFundPoolParams
    { 
        mspPoolParam = examplePoolParams, 

        -- mspPoolNFTTokenName = adaToken,
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

        uiUserNFTTokenName = adaToken,
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
        ugrpUserNFTTokenName = adaToken,
        ugrpUserNFTTxOutRef = exampleTxOutRef,
        ugrpClaim   = 3_000_000

    } 

exampleUserInvestRewardsParams :: UserInvestRewardsParams
exampleUserInvestRewardsParams = UserInvestRewardsParams
    { 
        uirpPoolParam = examplePoolParams, 
        uirpDeadline   = examplePOSIXTime
    } 
