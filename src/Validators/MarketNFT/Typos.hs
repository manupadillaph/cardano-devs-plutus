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

module  Validators.MarketNFT.Typos where

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
        ppMaximunInvest    :: Invest ,   
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
        psUsersNFT :: [UserNFT],
        psCountTotalUtxoWithPoolState :: Integer,   
        psChashedOut   :: Proffit
    } 
  deriving (HASKELL.Eq, HASKELL.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq PoolStateTypo where
    {-# INLINABLE (==) #-}
    ps1 == ps2 = psPoolNFT ps1 ==   psPoolNFT  ps2 && 
                                    psMasterFunders ps1 == psMasterFunders ps2 && 
                                    psUsersNFT ps1 == psUsersNFT ps2 && 
                                    psCountTotalUtxoWithPoolState   ps1 == psCountTotalUtxoWithPoolState ps2 && 
                                    psChashedOut ps1 == psChashedOut ps2

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


mkPoolStateTypo ::  PoolNFT ->   [MasterFunder] -> [UserNFT] -> Proffit ->  Integer -> PoolStateTypo
mkPoolStateTypo  poolNFT masterFunders userNFTs cashedout countTotalUtxoWithPoolState = do
    let 
        compareMasterFunders :: MasterFunder -> MasterFunder -> Ordering
        compareMasterFunders masterFunder1 masterFunder2
            | mfMaster  masterFunder1 < mfMaster masterFunder2 = LT
            | otherwise = GT

        compareUserNFT :: UserNFT -> UserNFT -> Ordering
        compareUserNFT userNFT1 userNFT2
            | userNFT1 < userNFT2 = LT
            | otherwise = GT
        -- need to be in order so it can be used after for cheking equality
        masterFundersOrder = sortBy compareMasterFunders masterFunders
        userNFTsOrder = sortBy compareUserNFT userNFTs

    PoolStateTypo {psPoolNFT = poolNFT  ,psMasterFunders = masterFundersOrder , psUsersNFT = userNFTsOrder, psChashedOut = cashedout, psCountTotalUtxoWithPoolState = countTotalUtxoWithPoolState}

mkPoolState :: PoolNFT -> [MasterFunder] -> [UserNFT] -> Proffit -> Integer -> ValidatorDatum
mkPoolState  poolNFT masterFunders userNFTs cashedout countTotalUtxoWithPoolState = PoolState $ mkPoolStateTypo  poolNFT masterFunders userNFTs cashedout countTotalUtxoWithPoolState



mkUserStateTypo :: User ->  UserNFT -> Invest -> POSIXTime ->  Deadline -> Proffit ->  Proffit -> Maybe POSIXTime   -> UserStateTypo
mkUserStateTypo user userNFT invest createdat deadline cashedout rewardsNotClaimed  lastClaim = UserStateTypo { usUser = user, usUserNFT = userNFT , usInvest = invest ,usCreatedAt = createdat , usDeadline = deadline , usRewardsNotClaimed = rewardsNotClaimed , usChashedOut = cashedout, usLastClaimAt = lastClaim }


mkUserState:: User ->  UserNFT ->   Invest ->  POSIXTime ->  Deadline -> Proffit ->  Proffit  -> Maybe POSIXTime -> ValidatorDatum
mkUserState user userNFT invest createdat deadline  cashedout rewardsNotClaimed lastClaim = UserState $ mkUserStateTypo user userNFT invest createdat deadline cashedout  rewardsNotClaimed lastClaim



--Types for Redeemers

data RedeemMasterFundPoolTypo  = RedeemMasterFundPoolTypo { 
        rmfpPoolNFT :: !PoolNFT ,
        rmfpMaster :: !Master ,
        rmfpFund :: !Fund, 
        rmfpUsingUtxo :: !TxOutRef
    } 
  deriving (HASKELL.Eq, HASKELL.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq RedeemMasterFundPoolTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  rmfpPoolNFT r1 ==      rmfpPoolNFT  r2 && 
                rmfpMaster r1 ==       rmfpMaster  r2 && 
                rmfpFund r1 ==    	   rmfpFund  r2 && 
                rmfpUsingUtxo r1 ==    rmfpUsingUtxo  r2



PlutusTx.unstableMakeIsData ''RedeemMasterFundPoolTypo
PlutusTx.makeLift ''RedeemMasterFundPoolTypo

data RedeemMasterFundAndMergePoolTypo  = RedeemMasterFundAndMergePoolTypo { 
        rmfampPoolNFT :: !PoolNFT ,
        rmfampMaster :: !Master ,
        rmfampFund :: !Fund,
        rmfampUtxoToMerge :: ![TxOutRef]
    } 
  deriving (HASKELL.Eq, HASKELL.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq RedeemMasterFundAndMergePoolTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  rmfampPoolNFT r1 ==          rmfampPoolNFT  r2 && 
                rmfampMaster r1 ==           rmfampMaster  r2 && 
                rmfampFund r1 ==    	     rmfampFund  r2 && 
                rmfampUtxoToMerge r1 == rmfampUtxoToMerge r2


PlutusTx.unstableMakeIsData ''RedeemMasterFundAndMergePoolTypo
PlutusTx.makeLift ''RedeemMasterFundAndMergePoolTypo

data RedeemMasterGetPoolTypo  = RedeemMasterGetPoolTypo { 
        rmgpPoolNFT :: !PoolNFT ,
        rmgpMaster :: !Master ,
        rmgpGetFund :: !Fund
    } 
  deriving (HASKELL.Eq, HASKELL.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq RedeemMasterGetPoolTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  rmgpPoolNFT r1 ==          rmgpPoolNFT  r2 && 
                rmgpMaster r1 ==           rmgpMaster  r2 && 
                rmgpGetFund r1 ==    	   rmgpGetFund  r2


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
        rugiUser :: !User ,
        rugiGetInvest :: !Invest
    }  
  deriving (HASKELL.Eq, HASKELL.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq RedeemUserGetInvestTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  rugiPoolNFT r1 ==              rugiPoolNFT  r2 && 
                rugiUserNFT r1 ==              rugiUserNFT  r2 && 
                rugiUser r1 ==                 rugiUser  r2 && 
                rugiGetInvest r1 ==            rugiGetInvest  r2 

PlutusTx.unstableMakeIsData ''RedeemUserGetInvestTypo
PlutusTx.makeLift ''RedeemUserGetInvestTypo

data RedeemUserGetRewardsTypo  = RedeemUserGetRewardsTypo { 
        rugrPoolNFT :: !PoolNFT ,
        rugrUserNFT :: !UserNFT ,
        rugrUser :: !User ,
        rugrClaim :: !Proffit,
        rugrClaimAt :: !POSIXTime
    }  
  deriving (HASKELL.Eq, HASKELL.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq RedeemUserGetRewardsTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  rugrPoolNFT r1 ==              rugrPoolNFT  r2 && 
                rugrUserNFT r1 ==              rugrUserNFT  r2 && 
                rugrUser r1 ==                 rugrUser  r2 && 
                rugrClaim r1 ==                 rugrClaim  r2 

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
        ruirDeadline :: !Deadline,
        ruirClaim :: !Proffit,
        ruirClaimAt :: !POSIXTime
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
                ruirDeadline r1 ==             ruirDeadline  r2 && 
                ruirClaim r1 ==                ruirClaim  r2 && 
                ruirClaimAt r1 ==              ruirClaimAt  r2


PlutusTx.unstableMakeIsData ''RedeemUserInvestRewardsTypo
PlutusTx.makeLift ''RedeemUserInvestRewardsTypo

--Redeemers Definition

data ValidatorRedeemer = 
    RedeemMasterFundPool !RedeemMasterFundPoolTypo | 
    RedeemMasterFundAndMergePool !RedeemMasterFundAndMergePoolTypo | 
    RedeemMasterGetPool !RedeemMasterGetPoolTypo |  
    RedeemUserInvest !RedeemUserInvestTypo |   
    RedeemUserGetInvest !RedeemUserGetInvestTypo |  
    RedeemUserGetRewards !RedeemUserGetRewardsTypo |  
    RedeemUserInvestRewards !RedeemUserInvestRewardsTypo   
    deriving HASKELL.Show

instance Eq ValidatorRedeemer where
    {-# INLINABLE (==) #-}
    RedeemMasterFundPool rmfp1  == RedeemMasterFundPool rmfp2   =  rmfp1 == rmfp2
    RedeemMasterFundAndMergePool rmfamp1  == RedeemMasterFundAndMergePool rmfamp2   =  rmfamp1 == rmfamp2
    RedeemMasterGetPool rmgp1 == RedeemMasterGetPool  rmgp2 = rmgp1 == rmgp2
    RedeemUserInvest rui1 == RedeemUserInvest rui2 =  rui1 == rui2
    RedeemUserGetInvest  rugi1 == RedeemUserGetInvest rugi2 = rugi1 == rugi2
    RedeemUserGetRewards rugr1 == RedeemUserGetRewards rugr2 = rugr1 == rugr2
    RedeemUserInvestRewards ruir1 == RedeemUserInvestRewards ruir2 = ruir1 == ruir2
    _ == _ = False

PlutusTx.makeIsDataIndexed ''ValidatorRedeemer [ 
        ('RedeemMasterFundPool, 0),
        ('RedeemMasterFundAndMergePool, 1),
        ('RedeemMasterGetPool, 2),
        ('RedeemUserInvest,  3),
        ('RedeemUserGetInvest, 4),
        ('RedeemUserGetRewards,  5),
        ('RedeemUserInvestRewards,  6)
    ]

PlutusTx.makeLift ''ValidatorRedeemer

--Redeemers helpers

mkRedeemMasterFundPool :: PoolNFT ->  Master -> Fund ->  TxOutRef-> Redeemer
mkRedeemMasterFundPool poolNFT master fund txOutRef = Redeemer $ PlutusTx.toBuiltinData (RedeemMasterFundPool $ mkRedeemMasterFundPoolTypo poolNFT  master fund  txOutRef )

mkRedeemMasterFundPoolTypo ::  PoolNFT ->  Master -> Fund -> TxOutRef->  RedeemMasterFundPoolTypo
mkRedeemMasterFundPoolTypo   poolNFT master fund  txOutRef = RedeemMasterFundPoolTypo {
        rmfpPoolNFT = poolNFT  ,
        rmfpMaster = master, 
        rmfpFund = fund,
        rmfpUsingUtxo = txOutRef
    }


mkRedeemMasterFundAndMergePool :: PoolNFT ->  Master -> Fund ->  [TxOutRef] -> Redeemer
mkRedeemMasterFundAndMergePool poolNFT master fund txOutRefs = Redeemer $ PlutusTx.toBuiltinData (RedeemMasterFundAndMergePool $ mkRedeemMasterFundAndMergePoolTypo poolNFT  master fund txOutRefs )

mkRedeemMasterFundAndMergePoolTypo ::  PoolNFT ->  Master -> Fund -> [TxOutRef] -> RedeemMasterFundAndMergePoolTypo
mkRedeemMasterFundAndMergePoolTypo   poolNFT master fund  txOutRefs = RedeemMasterFundAndMergePoolTypo {
        rmfampPoolNFT = poolNFT  ,
        rmfampMaster = master, 
        rmfampFund = fund,
        rmfampUtxoToMerge = txOutRefs
    }

mkRedeemMasterGetPool :: PoolNFT -> TokenName -> TxOutRef -> Master -> Fund -> Redeemer
mkRedeemMasterGetPool poolNFT tn txoutref master getfund = Redeemer $ PlutusTx.toBuiltinData (RedeemMasterGetPool $ mkRedeemMasterGetPoolTypo poolNFT tn txoutref master getfund  )

mkRedeemMasterGetPoolTypo ::  PoolNFT -> TokenName -> TxOutRef -> Master -> Fund ->  RedeemMasterGetPoolTypo
mkRedeemMasterGetPoolTypo   poolNFT tn txoutref master getfund  = RedeemMasterGetPoolTypo {
        rmgpPoolNFT = poolNFT  ,
        rmgpMaster = master, 
        rmgpGetFund = getfund
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

mkRedeemUserGetInvest :: PoolNFT -> UserNFT -> User -> Invest ->   Redeemer
mkRedeemUserGetInvest poolNFT userNFT  user getinvest  = Redeemer $  PlutusTx.toBuiltinData (RedeemUserGetInvest  $ mkRedeemUserGetInvestTypo poolNFT userNFT  user getinvest )

mkRedeemUserGetInvestTypo ::  PoolNFT -> UserNFT -> User -> Invest -> RedeemUserGetInvestTypo
mkRedeemUserGetInvestTypo    poolNFT userNFT  user getinvest   = RedeemUserGetInvestTypo {
        rugiPoolNFT =  poolNFT,
        rugiUserNFT =  userNFT ,
        rugiUser =  user ,
        rugiGetInvest = getinvest
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

mkRedeemUserInvestRewards ::PoolNFT -> UserNFT -> TokenName -> TxOutRef -> User -> Invest ->  POSIXTime ->  Deadline -> Proffit-> POSIXTime -> Redeemer
mkRedeemUserInvestRewards poolNFT userNFT tn txoutref user invest createdAt deadline claim  claimAt= Redeemer $  PlutusTx.toBuiltinData (RedeemUserInvestRewards  $ mkRedeemUserInvestRewardsTypo poolNFT userNFT tn txoutref user invest  createdAt deadline  claim  claimAt)

mkRedeemUserInvestRewardsTypo ::  PoolNFT -> UserNFT -> TokenName -> TxOutRef -> User -> Invest ->  POSIXTime ->  Deadline -> Proffit-> POSIXTime -> RedeemUserInvestRewardsTypo
mkRedeemUserInvestRewardsTypo   poolNFT userNFT tn txoutref user invest createdAt deadline claim  claimAt  = RedeemUserInvestRewardsTypo {
        ruirPoolNFT =  poolNFT,
        ruirUserNFT =  userNFT ,
        ruirUserNFTTokenName =  tn ,
        ruirUserNFTTxOutRef =  txoutref ,
        ruirUser =  user ,
        ruirInvest = invest,
        ruirCreatedAt =  createdAt,
        ruirDeadline = deadline,
        ruirClaim = claim,
        ruirClaimAt = claimAt
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
        pmcpPoolParam :: PoolParams, 

        pmcpPoolNFTTokenName :: TokenName,
        pmcpPoolNFTTxOutRef :: TxOutRef,

        pmcpFund    :: Invest
    } deriving (HASKELL.Eq, HASKELL.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema,  HASKELL.Show)

data MasterFundPoolParams = MasterFundPoolParams
    { 
        pmfpPoolParam :: PoolParams, 
        pmfpFund    :: Invest
    } deriving (HASKELL.Eq, HASKELL.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema, HASKELL.Show)

data MasterFundAndMergePoolParams = MasterFundAndMergePoolParams
    { 
        pmfampPoolParam :: PoolParams, 
        pmfampUtxoToMerge :: [TxOutRef],
        pmfampFund    :: Invest
    } deriving (HASKELL.Eq, HASKELL.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema, HASKELL.Show)

newtype MasterGetBackFundParams = MasterGetBackFundParams
    { 
        pmgbfPoolParam :: PoolParams
    } deriving (HASKELL.Eq, HASKELL.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema, HASKELL.Show)

data UserInvestParams = UserInvestParams
    { 
        puiPoolParam :: PoolParams, 

        puiUserNFTTokenName :: TokenName,
        puiUserNFTTxOutRef :: TxOutRef,

        puiCreatedAt   :: POSIXTime, 
        puiDeadline    :: Deadline, 
        puiInvest    :: Invest
    } deriving (HASKELL.Eq, HASKELL.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema, HASKELL.Show)

data UserGetBackInvestParams = UserGetBackInvestParams
    { 
        pugbiPoolParam :: PoolParams, 
        pugbiDeadline    :: Deadline
    } deriving (HASKELL.Eq, HASKELL.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema, HASKELL.Show)

data UserGetRewardsParams = UserGetRewardsParams
    { 
        pugrPoolParam :: PoolParams,
        pugrUserNFTTokenName :: TokenName,
        pugrUserNFTTxOutRef :: TxOutRef,
        pugrClaim    :: Proffit 

    } deriving (HASKELL.Eq, HASKELL.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema, HASKELL.Show)

data UserInvestRewardsParams = UserInvestRewardsParams
    { 
        puirPoolParam :: PoolParams, 
        puirDeadline    :: Deadline
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
        pmcpPoolParam = examplePoolParams, 

        pmcpPoolNFTTokenName = adaToken,
        pmcpPoolNFTTxOutRef = exampleTxOutRef,

        pmcpFund    = 100_000_000
    } 

exampleMasterFundPoolParams :: MasterFundPoolParams
exampleMasterFundPoolParams = MasterFundPoolParams
    { 
        pmfpPoolParam = examplePoolParams, 

        -- mspPoolNFTTokenName = adaToken,
        -- mspPoolNFTTxOutRef = exampleTxOutRef,

        pmfpFund    = 100_000_000
    } 

exampleMasterFundAndMergePoolParams :: MasterFundAndMergePoolParams
exampleMasterFundAndMergePoolParams = MasterFundAndMergePoolParams
    { 
        pmfampPoolParam = examplePoolParams, 

        -- mspPoolNFTTokenName = adaToken,
        -- mspPoolNFTTxOutRef = exampleTxOutRef,
        pmfampUtxoToMerge = [exampleTxOutRef],

        pmfampFund    = 100_000_000
    } 



exampleMasterGetBackFundParams :: MasterGetBackFundParams
exampleMasterGetBackFundParams = MasterGetBackFundParams
    { 
        pmgbfPoolParam = examplePoolParams
    } 

exampleUserInvestParams :: UserInvestParams
exampleUserInvestParams = UserInvestParams
    { 
        puiPoolParam = examplePoolParams, 

        puiUserNFTTokenName = adaToken,
        puiUserNFTTxOutRef = exampleTxOutRef,

        puiCreatedAt   = examplePOSIXTime,
        puiDeadline   = examplePOSIXTime,
        puiInvest    = 3_000_000
    } 

exampleUserGetBackInvestParams :: UserGetBackInvestParams
exampleUserGetBackInvestParams = UserGetBackInvestParams
    { 
        pugbiPoolParam = examplePoolParams, 
        pugbiDeadline   = examplePOSIXTime
    } 

exampleUserGetRewardsParams :: UserGetRewardsParams
exampleUserGetRewardsParams = UserGetRewardsParams
    { 
        pugrPoolParam = examplePoolParams, 
        pugrUserNFTTokenName = adaToken,
        pugrUserNFTTxOutRef = exampleTxOutRef,
        pugrClaim   = 3_000_000

    } 

exampleUserInvestRewardsParams :: UserInvestRewardsParams
exampleUserInvestRewardsParams = UserInvestRewardsParams
    { 
        puirPoolParam = examplePoolParams, 
        puirDeadline   = examplePOSIXTime
    } 
