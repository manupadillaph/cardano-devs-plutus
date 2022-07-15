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
import qualified Prelude              as P 
import           Schema               (ToSchema)
import     qualified      Data.OpenApi.Schema         (ToSchema)
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
        spMasters :: Masters ,           
        spInterest    :: Interest ,
        spMinumunInvest    :: Invest ,  
        spMinumunCompoundInvest    :: Invest , 
        spDeadline    :: Deadline , 
        spPoolNFT  :: NFT	,
        spCurSymbolForMintingNFTPolicy :: CurrencySymbol
        -- TODO:
        -- Minimut invest time: 
    } deriving (P.Eq, P.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema,  P.Show)
PlutusTx.makeLift ''PoolParams


--Types for Datums

data MasterFunder = MasterFunder{
        mfMaster :: Master,
        mfFund :: Fund
    }deriving (P.Eq, P.Show, Generic)
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
  deriving (P.Eq, P.Show, Generic)
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
        usTotal   :: Proffit,
        usChashedOut   :: Proffit,
        usLastClaim    :: !(Maybe POSIXTime)
    } deriving (P.Eq, P.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq UserStateTypo where
    {-# INLINABLE (==) #-}
    us1 == us2 =    usUser  us1 == usUser  us2 &&
                    usUserNFT us1 == usUserNFT us2 &&
                    usInvest us1 == usInvest us2 &&
                    usDeadline us1 == usDeadline us2  &&
                    usDeadline us1 == usDeadline us2  &&
                    usTotal us1 == usTotal us2 &&
                    usChashedOut us1 == usChashedOut us2 &&
                    usLastClaim us1 == usLastClaim us2


PlutusTx.unstableMakeIsData ''UserStateTypo
--PlutusTx.makeLift ''UserStateTypo

data ValidatorDatum = 
    PoolState PoolStateTypo | 
    UserState UserStateTypo
  deriving (P.Eq, P.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


instance Eq ValidatorDatum where
    {-# INLINABLE (==) #-}
    PoolState ps1 == PoolState ps2 = ps1 == ps2
    UserState us1  == UserState us2 = us1 == us2
    _ == _ = False

PlutusTx.unstableMakeIsData ''ValidatorDatum
--PlutusTx.makeLift ''ValidatorDatum

--Types for Redeemers

data RedeemMasterFundPoolTypo  = RedeemMasterFundPoolTypo { 
        rmfpPoolNFT :: !PoolNFT ,
        rmfpPoolNFTTokenName :: !TokenName ,
        rmfpPoolNFTTxOutRef :: !TxOutRef ,
        rmfpMaster :: !Master ,
        rmfpFund :: !Fund
    } 
  deriving (P.Eq, P.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq RedeemMasterFundPoolTypo where
    {-# INLINABLE (==) #-}
    rmf1 == rmf2 =  rmfpPoolNFT rmf1 ==          rmfpPoolNFT  rmf2 && 
                    rmfpPoolNFTTokenName rmf1 == rmfpPoolNFTTokenName  rmf2 && 
                    rmfpPoolNFTTxOutRef rmf1 ==  rmfpPoolNFTTxOutRef  rmf2 && 
                    rmfpMaster rmf1 ==           rmfpMaster  rmf2 && 
                    rmfpFund rmf1 ==    	     rmfpFund  rmf2


PlutusTx.unstableMakeIsData ''RedeemMasterFundPoolTypo
PlutusTx.makeLift ''RedeemMasterFundPoolTypo

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
  deriving (P.Eq, P.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq RedeemUserInvestTypo where
    {-# INLINABLE (==) #-}
    rmf1 == rmf2 =  ruiPoolNFT rmf1 ==              ruiPoolNFT  rmf2 && 
                    ruiUserNFT rmf1 ==              ruiUserNFT  rmf2 && 
                    ruiUserNFTTokenName rmf1 ==     ruiUserNFTTokenName  rmf2 && 
                    ruiUserNFTTxOutRef rmf1 ==      ruiUserNFTTxOutRef  rmf2 && 
                    ruiUser rmf1 ==                 ruiUser  rmf2 && 
                    ruiInvest rmf1 ==               ruiInvest  rmf2 && 
                    ruiCreatedAt rmf1 ==            ruiCreatedAt  rmf2 && 
                    ruiDeadline rmf1 ==             ruiDeadline  rmf2

PlutusTx.unstableMakeIsData ''RedeemUserInvestTypo
PlutusTx.makeLift ''RedeemUserInvestTypo


data ValidatorRedeemer = 
    --RedeemMasterFundPool !PoolNFT !TokenName !TxOutRef !Master !Fund | 
    RedeemMasterFundPool !RedeemMasterFundPoolTypo | 
    RedeemMasterGetPool |
    RedeemUserInvest !RedeemUserInvestTypo |   
    RedeemUserGetInvest | 
    RedeemUserGetRewards | 
    RedeemUserInvestRewards  
    deriving P.Show



instance Eq ValidatorRedeemer where
    {-# INLINABLE (==) #-}
    --RedeemMasterFundPool nft1 tk1 txout1 m1  am1  == RedeemMasterFundPool nft2 tk2 txout2 m2 am2   =  nft1 == nft2 &&  tk1 == tk2 &&  txout1 == txout2 && m1 == m2 && am1 == am2
    RedeemMasterFundPool rmfp1  == RedeemMasterFundPool rmfp2   =  rmfp1 == rmfp2
    
    RedeemMasterGetPool == RedeemMasterGetPool = True
    RedeemUserInvest rui1 == RedeemUserInvest rui2 =  rui1 == rui2
    RedeemUserGetInvest  == RedeemUserGetInvest = True
    RedeemUserGetRewards  == RedeemUserGetRewards = True 
    RedeemUserInvestRewards  == RedeemUserInvestRewards = True 
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


--Types for Minting Policy Redeemers


data MintingRedeemer = MintingRedeemer
    { 
        mrTokenName   :: TokenName ,
        mrTxOutRef    :: TxOutRef 
    } deriving  P.Show

instance Eq MintingRedeemer where
    {-# INLINABLE (==) #-}
    r1 == r2 =    
        mrTokenName  r1 == mrTokenName  r2  &&
        mrTxOutRef  r1 == mrTxOutRef  r2    

PlutusTx.unstableMakeIsData ''MintingRedeemer

--Types for endpoints parameters

data MasterCreatePoolParams = MasterCreatePoolParams
    { 
        mcpPoolParam :: PoolParams, 

        mcpPoolNFTTokenName :: TokenName,
        mcpPoolNFTTxOutRef :: TxOutRef,

        mcpFund    :: Invest
    } deriving (P.Eq, P.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema,  P.Show)

data MasterFundPoolParams = MasterFundPoolParams
    { 
        mspPoolParam :: PoolParams, 

        mspPoolNFTTokenName :: TokenName,
        mspPoolNFTTxOutRef :: TxOutRef,

        mspFund    :: Invest
    } deriving (P.Eq, P.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema, P.Show)

newtype MasterGetBackFundParams = MasterGetBackFundParams
    { 
        mgpPoolParam :: PoolParams
    } deriving (P.Eq, P.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema, P.Show)

data UserInvestParams = UserInvestParams
    { 
        uipPoolParam :: PoolParams, 

        uiUserNFTTokenName :: TokenName,
        uiUserNFTTxOutRef :: TxOutRef,

        uipDeadline    :: Deadline, 
        uipInvest    :: Invest
    } deriving (P.Eq, P.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema, P.Show)

data UserGetBackInvestParams = UserGetBackInvestParams
    { 
        ugipPoolParam :: PoolParams, 
        ugipDeadline    :: Deadline
    } deriving (P.Eq, P.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema, P.Show)

newtype UserGetRewardsParams = UserGetRewardsParams
    { 
        ugrpPoolParam :: PoolParams
    } deriving (P.Eq, P.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema, P.Show)

data UserInvestRewardsParams = UserInvestRewardsParams
    { 
        uirpPoolParam :: PoolParams, 
        uirpDeadline    :: Deadline
    } deriving (P.Eq, P.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema, P.Show)


--Redeemers Definitions

redeemMasterFundPool :: PoolNFT -> TokenName -> TxOutRef -> Master -> Fund -> Redeemer
redeemMasterFundPool poolNFT tn txoutref master fund = Redeemer $ PlutusTx.toBuiltinData (RedeemMasterFundPool $ mkRedeemMasterFundPoolTypo poolNFT tn txoutref master fund  )

mkRedeemMasterFundPoolTypo ::  PoolNFT -> TokenName -> TxOutRef -> Master -> Fund ->  RedeemMasterFundPoolTypo
mkRedeemMasterFundPoolTypo   poolNFT tn txoutref master fund  = RedeemMasterFundPoolTypo {
        rmfpPoolNFT = poolNFT  ,
        rmfpPoolNFTTokenName = tn , 
        rmfpPoolNFTTxOutRef = txoutref, 
        rmfpMaster = master, 
        rmfpFund = fund
    }

redeemMasterGetPool :: Redeemer
redeemMasterGetPool = Redeemer $ PlutusTx.toBuiltinData RedeemMasterGetPool


redeemUserInvest :: PoolNFT -> UserNFT -> TokenName -> TxOutRef -> User -> Invest ->  POSIXTime ->  Deadline -> Redeemer 
redeemUserInvest poolNFT userNFT tn txoutref user invest createdAt deadline = Redeemer $  PlutusTx.toBuiltinData (RedeemUserInvest  $ mkRedeemUserInvestTypo poolNFT userNFT tn txoutref user invest  createdAt deadline)

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

redeemUserGetInvest :: Redeemer
redeemUserGetInvest = Redeemer $  PlutusTx.toBuiltinData RedeemUserGetInvest

redeemUserGetRewards :: Redeemer
redeemUserGetRewards = Redeemer $  PlutusTx.toBuiltinData RedeemUserGetRewards

redeemUserInvestRewards :: Redeemer
redeemUserInvestRewards = Redeemer $  PlutusTx.toBuiltinData RedeemUserInvestRewards


-- mintingRedeemer ::TokenName -> TxOutRef -> Redeemer
-- mintingRedeemer tn txoutref  = Redeemer $ PlutusTx.toBuiltinData (MintingRedeemer { mrTokenName= tn, mrTxOutRef= txoutref}  )


--Usefull functions to create the different data types
-- | Smart constructors for the untyped redeemers.

mkMasterFunder :: Master -> Fund -> MasterFunder
mkMasterFunder master fund = MasterFunder { mfMaster = master , mfFund = fund}

mkPoolStateTypo ::  PoolNFT ->   [MasterFunder] -> [UserNFT] -> PoolStateTypo
mkPoolStateTypo  poolNFT masterFunders userNFTs = PoolStateTypo {psPoolNFT = poolNFT  ,psMasterFunders = masterFunders , psUsersNFT = userNFTs}

mkUserStateTypo :: User ->  UserNFT -> Invest -> POSIXTime ->  Deadline -> Proffit ->  Proffit -> Maybe POSIXTime   -> UserStateTypo
mkUserStateTypo user userNFT invest createdat deadline total cashedout lastClaim = UserStateTypo { usUser = user, usUserNFT = userNFT , usInvest = invest ,usCreatedAt = createdat , usDeadline = deadline , usTotal = total , usChashedOut = cashedout, usLastClaim = lastClaim }

mkPoolState :: PoolNFT -> [MasterFunder] -> [UserNFT] -> ValidatorDatum
mkPoolState  poolNFT masterFunders userNFTs = PoolState $ mkPoolStateTypo  poolNFT masterFunders userNFTs

mkUserState:: User ->  UserNFT ->   Invest ->  POSIXTime ->  Deadline -> Proffit ->  Proffit  -> Maybe POSIXTime -> ValidatorDatum
mkUserState user userNFT invest createdat deadline total cashedout lastClaim = UserState $ mkUserStateTypo user userNFT invest createdat deadline total cashedout lastClaim

