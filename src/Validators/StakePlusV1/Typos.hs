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

module Validators.StakePlusV1.Typos where

--Import Nuevos

import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Ledger                              
import qualified Plutus.V1.Ledger.Api                as LedgerApiV1
import qualified Plutus.V1.Ledger.Value              as LedgerValueV1
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Schema                              (ToSchema)

--Modulo:

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
        ppMaximunInvest    :: Invest ,   
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
        psUsersNFT :: [UserNFT],
        psCountTotalUtxoWithPoolState :: Integer,   
        psChashedOut   :: Proffit
    } 
  deriving (P.Eq, P.Show, GHCGenerics.Generic)
  deriving anyclass (DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq  PoolStateTypo where
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
        usCreatedAt  :: LedgerApiV1.POSIXTime,
        usDeadline   :: Deadline,
        usChashedOut   :: Proffit,
        usRewardsNotClaimed   :: Proffit,
        usLastClaimAt   :: (P.Maybe LedgerApiV1.POSIXTime)
    } deriving (P.Eq, P.Show, GHCGenerics.Generic)
  deriving anyclass (DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq  UserStateTypo where
    {-# INLINABLE (==) #-}
    us1 == us2 =    usUser  us1 == usUser  us2 &&
                    usUserNFT us1 == usUserNFT us2 &&
                    usInvest us1 == usInvest us2 &&
                    usCreatedAt us1 == usCreatedAt us2 &&
                    usDeadline us1 == usDeadline us2 &&
                    usChashedOut us1 == usChashedOut us2 &&
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


mkPoolStateTypo ::  PoolNFT ->  [MasterFunder] -> [UserNFT] -> Proffit -> Integer -> PoolStateTypo
mkPoolStateTypo  poolNFT masterFunders userNFTs cashedout countTotalUtxoWithPoolState = do
    let 
        compareMasterFunders :: MasterFunder -> MasterFunder -> Ordering
        compareMasterFunders masterFunder1 masterFunder2
            | mfMaster  masterFunder1 < mfMaster masterFunder2 = LT
            | otherwise = GT

        compareUserNFT :: UserNFT -> UserNFT -> Ordering
        compareUserNFT userNFT1 userNFT2
            | userNFT1 < userNFT2 = LT
            | otherwise = P.GT
        -- need to be in order so it can be used after for cheking equality
        masterFundersOrder = sortBy compareMasterFunders masterFunders
        userNFTsOrder = sortBy compareUserNFT userNFTs

    PoolStateTypo {psPoolNFT = poolNFT  ,psMasterFunders = masterFundersOrder , psUsersNFT = userNFTsOrder, psChashedOut = cashedout, psCountTotalUtxoWithPoolState = countTotalUtxoWithPoolState}

mkPoolState :: PoolNFT -> [MasterFunder] -> [UserNFT] -> Proffit -> Integer -> ValidatorDatum
mkPoolState  poolNFT masterFunders userNFTs cashedout countTotalUtxoWithPoolState = PoolState $ mkPoolStateTypo  poolNFT masterFunders userNFTs cashedout countTotalUtxoWithPoolState



mkUserStateTypo :: User -> UserNFT -> Invest -> LedgerApiV1.POSIXTime -> Deadline -> Proffit -> Proffit -> P.Maybe LedgerApiV1.POSIXTime   -> UserStateTypo
mkUserStateTypo user userNFT invest createdat deadline cashedout rewardsNotClaimed  lastClaim = UserStateTypo { usUser = user, usUserNFT = userNFT , usInvest = invest ,usCreatedAt = createdat , usDeadline = deadline , usRewardsNotClaimed = rewardsNotClaimed , usChashedOut = cashedout, usLastClaimAt = lastClaim }


mkUserState:: User -> UserNFT ->  Invest -> LedgerApiV1.POSIXTime -> Deadline -> Proffit -> Proffit  -> P.Maybe LedgerApiV1.POSIXTime -> ValidatorDatum
mkUserState user userNFT invest createdat deadline  cashedout rewardsNotClaimed lastClaim = UserState $ mkUserStateTypo user userNFT invest createdat deadline cashedout  rewardsNotClaimed lastClaim



--Types for Redeemers

data RedeemMasterFundPoolTypo  = RedeemMasterFundPoolTypo { 
        rmfpPoolNFT :: PoolNFT ,
        rmfpMaster :: Master ,
        rmfpFund :: Fund, 
        rmfpUsingUtxo :: LedgerApiV1.TxOutRef
    } 
  deriving (P.Eq, P.Show, GHCGenerics.Generic)
  deriving anyclass (DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq  RedeemMasterFundPoolTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  rmfpPoolNFT r1   == rmfpPoolNFT  r2 && 
                rmfpMaster r1    == rmfpMaster  r2 && 
                rmfpFund r1      == rmfpFund  r2 && 
                rmfpUsingUtxo r1 == rmfpUsingUtxo  r2



PlutusTx.unstableMakeIsData ''RedeemMasterFundPoolTypo
PlutusTx.makeLift ''RedeemMasterFundPoolTypo

data RedeemMasterFundAndMergePoolTypo  = RedeemMasterFundAndMergePoolTypo { 
        rmfampPoolNFT :: PoolNFT ,
        rmfampMaster :: Master ,
        rmfampFund :: Fund,
        rmfampUtxoToMerge :: [LedgerApiV1.TxOutRef]
    } 
  deriving (P.Eq, P.Show, GHCGenerics.Generic)
  deriving anyclass (DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq  RedeemMasterFundAndMergePoolTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  rmfampPoolNFT r1 == rmfampPoolNFT  r2 && 
                rmfampMaster r1 == rmfampMaster  r2 && 
                rmfampFund r1 == rmfampFund  r2 && 
                rmfampUtxoToMerge r1 == rmfampUtxoToMerge r2


PlutusTx.unstableMakeIsData ''RedeemMasterFundAndMergePoolTypo
PlutusTx.makeLift ''RedeemMasterFundAndMergePoolTypo

data RedeemMasterGetPoolTypo  = RedeemMasterGetPoolTypo { 
        rmgpPoolNFT :: PoolNFT ,
        rmgpMaster :: Master ,
        rmgpGetFund :: Fund
    } 
  deriving (P.Eq, P.Show, GHCGenerics.Generic)
  deriving anyclass (DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq  RedeemMasterGetPoolTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  rmgpPoolNFT r1 == rmgpPoolNFT  r2 && 
                rmgpMaster r1 == rmgpMaster  r2 && 
                rmgpGetFund r1 == rmgpGetFund  r2


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
        rugiUser :: User ,
        rugiGetInvest :: Invest
    }  
  deriving (P.Eq, P.Show, GHCGenerics.Generic)
  deriving anyclass (DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq  RedeemUserGetInvestTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  rugiPoolNFT r1 ==              rugiPoolNFT  r2 && 
                rugiUserNFT r1 ==              rugiUserNFT  r2 && 
                rugiUser r1 ==                 rugiUser  r2 && 
                rugiGetInvest r1 ==            rugiGetInvest  r2 

PlutusTx.unstableMakeIsData ''RedeemUserGetInvestTypo
PlutusTx.makeLift ''RedeemUserGetInvestTypo

data RedeemUserGetRewardsTypo  = RedeemUserGetRewardsTypo { 
        rugrPoolNFT :: PoolNFT ,
        rugrUserNFT :: UserNFT ,
        rugrUser :: User ,
        rugrClaim :: Proffit,
        rugrClaimAt :: LedgerApiV1.POSIXTime
    }  
  deriving (P.Eq, P.Show, GHCGenerics.Generic)
  deriving anyclass (DataAeson.ToJSON, DataAeson.FromJSON)

instance Eq  RedeemUserGetRewardsTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  rugrPoolNFT r1 ==              rugrPoolNFT  r2 && 
                rugrUserNFT r1 ==              rugrUserNFT  r2 && 
                rugrUser r1 ==                 rugrUser  r2 && 
                rugrClaim r1 ==                 rugrClaim  r2 

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
        ruirDeadline :: Deadline,
        ruirClaim :: Proffit,
        ruirClaimAt :: LedgerApiV1.POSIXTime
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
    deriving P.Show

instance Eq  ValidatorRedeemer where
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

mkRedeemMasterFundPool :: PoolNFT -> Master -> Fund -> LedgerApiV1.TxOutRef -> LedgerApiV1.Redeemer
mkRedeemMasterFundPool poolNFT master fund txOutRef = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData (RedeemMasterFundPool $ mkRedeemMasterFundPoolTypo poolNFT  master fund  txOutRef )

mkRedeemMasterFundPoolTypo ::  PoolNFT -> Master -> Fund -> LedgerApiV1.TxOutRef -> RedeemMasterFundPoolTypo
mkRedeemMasterFundPoolTypo   poolNFT master fund  txOutRef = RedeemMasterFundPoolTypo {
        rmfpPoolNFT = poolNFT  ,
        rmfpMaster = master, 
        rmfpFund = fund,
        rmfpUsingUtxo = txOutRef
    }


mkRedeemMasterFundAndMergePool :: PoolNFT -> Master -> Fund -> [LedgerApiV1.TxOutRef] -> LedgerApiV1.Redeemer
mkRedeemMasterFundAndMergePool poolNFT master fund txOutRefs = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData (RedeemMasterFundAndMergePool $ mkRedeemMasterFundAndMergePoolTypo poolNFT  master fund txOutRefs )

mkRedeemMasterFundAndMergePoolTypo ::  PoolNFT -> Master -> Fund -> [LedgerApiV1.TxOutRef] -> RedeemMasterFundAndMergePoolTypo
mkRedeemMasterFundAndMergePoolTypo   poolNFT master fund  txOutRefs = RedeemMasterFundAndMergePoolTypo {
        rmfampPoolNFT = poolNFT  ,
        rmfampMaster = master, 
        rmfampFund = fund,
        rmfampUtxoToMerge = txOutRefs
    }

mkRedeemMasterGetPool :: PoolNFT -> LedgerValueV1.TokenName -> LedgerApiV1.TxOutRef -> Master -> Fund -> LedgerApiV1.Redeemer
mkRedeemMasterGetPool poolNFT tn txoutref master getfund = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData (RedeemMasterGetPool $ mkRedeemMasterGetPoolTypo poolNFT tn txoutref master getfund  )

mkRedeemMasterGetPoolTypo ::  PoolNFT -> LedgerValueV1.TokenName -> LedgerApiV1.TxOutRef -> Master -> Fund -> RedeemMasterGetPoolTypo
mkRedeemMasterGetPoolTypo   poolNFT tn txoutref master getfund  = RedeemMasterGetPoolTypo {
        rmgpPoolNFT = poolNFT  ,
        rmgpMaster = master, 
        rmgpGetFund = getfund
    }

mkRedeemUserInvest :: PoolNFT -> UserNFT -> LedgerValueV1.TokenName -> LedgerApiV1.TxOutRef -> User -> Invest -> LedgerApiV1.POSIXTime -> Deadline -> LedgerApiV1.Redeemer 
mkRedeemUserInvest poolNFT userNFT tn txoutref user invest createdAt deadline = LedgerApiV1.Redeemer $  PlutusTx.toBuiltinData (RedeemUserInvest  $ mkRedeemUserInvestTypo poolNFT userNFT tn txoutref user invest  createdAt deadline)

mkRedeemUserInvestTypo ::  PoolNFT -> UserNFT -> LedgerValueV1.TokenName -> LedgerApiV1.TxOutRef -> User -> Invest -> LedgerApiV1.POSIXTime -> Deadline -> RedeemUserInvestTypo
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

mkRedeemUserGetInvest :: PoolNFT -> UserNFT -> User -> Invest -> LedgerApiV1.Redeemer
mkRedeemUserGetInvest poolNFT userNFT  user getinvest  = LedgerApiV1.Redeemer $  PlutusTx.toBuiltinData (RedeemUserGetInvest  $ mkRedeemUserGetInvestTypo poolNFT userNFT  user getinvest )

mkRedeemUserGetInvestTypo ::  PoolNFT -> UserNFT -> User -> Invest -> RedeemUserGetInvestTypo
mkRedeemUserGetInvestTypo    poolNFT userNFT  user getinvest   = RedeemUserGetInvestTypo {
        rugiPoolNFT =  poolNFT,
        rugiUserNFT =  userNFT ,
        rugiUser =  user ,
        rugiGetInvest = getinvest
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

mkRedeemUserInvestRewards ::PoolNFT -> UserNFT -> LedgerValueV1.TokenName -> LedgerApiV1.TxOutRef -> User -> Invest -> LedgerApiV1.POSIXTime -> Deadline -> Proffit-> LedgerApiV1.POSIXTime -> LedgerApiV1.Redeemer
mkRedeemUserInvestRewards poolNFT userNFT tn txoutref user invest createdAt deadline claim  claimAt = LedgerApiV1.Redeemer $  PlutusTx.toBuiltinData (RedeemUserInvestRewards  $ mkRedeemUserInvestRewardsTypo poolNFT userNFT tn txoutref user invest  createdAt deadline  claim  claimAt)

mkRedeemUserInvestRewardsTypo ::  PoolNFT -> UserNFT -> LedgerValueV1.TokenName -> LedgerApiV1.TxOutRef -> User -> Invest -> LedgerApiV1.POSIXTime -> Deadline -> Proffit-> LedgerApiV1.POSIXTime -> RedeemUserInvestRewardsTypo
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
        mrTokenName   :: LedgerValueV1.TokenName,
        mrTxOutRef    :: LedgerApiV1.TxOutRef 
    } deriving  P.Show

instance Eq  MintingRedeemer where
    {-# INLINABLE (==) #-}
    r1 == r2 =    
        mrTokenName  r1 == mrTokenName  r2 &&
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
        pmcpPoolParam :: PoolParams, 

        pmcpPoolNFTTokenName :: LedgerValueV1.TokenName,
        pmcpPoolNFTTxOutRef :: LedgerApiV1.TxOutRef,

        pmcpFund    :: Invest
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema,  P.Show)

data MasterFundPoolParams = MasterFundPoolParams
    { 
        pmfpPoolParam :: PoolParams, 
        pmfpFund    :: Invest
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

data MasterFundAndMergePoolParams = MasterFundAndMergePoolParams
    { 
        pmfampPoolParam :: PoolParams, 
        pmfampUtxoToMerge :: [LedgerApiV1.TxOutRef],
        pmfampFund    :: Invest
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

newtype MasterGetBackFundParams = MasterGetBackFundParams
    { 
        pmgbfPoolParam :: PoolParams
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

data UserInvestParams = UserInvestParams
    { 
        puiPoolParam :: PoolParams, 

        puiUserNFTTokenName :: LedgerValueV1.TokenName,
        puiUserNFTTxOutRef :: LedgerApiV1.TxOutRef,

        puiCreatedAt   :: LedgerApiV1.POSIXTime, 
        puiDeadline    :: Deadline, 
        puiInvest    :: Invest
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

data UserGetBackInvestParams = UserGetBackInvestParams
    { 
        pugbiPoolParam :: PoolParams, 
        pugbiDeadline    :: Deadline
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

data UserGetRewardsParams = UserGetRewardsParams
    { 
        pugrPoolParam :: PoolParams,
        pugrUserNFTTokenName :: LedgerValueV1.TokenName,
        pugrUserNFTTxOutRef :: LedgerApiV1.TxOutRef,
        pugrClaim    :: Proffit 

    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

data UserInvestRewardsParams = UserInvestRewardsParams
    { 
        puirPoolParam :: PoolParams, 
        puirDeadline    :: Deadline
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
        ppCurSymbolForMintingNFTPolicy = LedgerApiV1.adaSymbol,
        ppValidTimeRange  = examplePOSIXTime,
        ppMinimunClaim  = 1
    }

exampleMasterCreatePoolParams :: MasterCreatePoolParams
exampleMasterCreatePoolParams = MasterCreatePoolParams
    { 
        pmcpPoolParam = examplePoolParams, 

        pmcpPoolNFTTokenName = LedgerApiV1.adaToken,
        pmcpPoolNFTTxOutRef = exampleTxOutRef,

        pmcpFund    = 100_000_000
    } 

exampleMasterFundPoolParams :: MasterFundPoolParams
exampleMasterFundPoolParams = MasterFundPoolParams
    { 
        pmfpPoolParam = examplePoolParams, 

        -- mspPoolNFTTokenName = LedgerApiV1.adaToken,
        -- mspPoolNFTTxOutRef = exampleTxOutRef,

        pmfpFund    = 100_000_000
    } 

exampleMasterFundAndMergePoolParams :: MasterFundAndMergePoolParams
exampleMasterFundAndMergePoolParams = MasterFundAndMergePoolParams
    { 
        pmfampPoolParam = examplePoolParams, 

        -- mspPoolNFTTokenName = LedgerApiV1.adaToken,
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

        puiUserNFTTokenName = LedgerApiV1.adaToken,
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
        pugrUserNFTTokenName = LedgerApiV1.adaToken,
        pugrUserNFTTxOutRef = exampleTxOutRef,
        pugrClaim   = 3_000_000

    } 

exampleUserInvestRewardsParams :: UserInvestRewardsParams
exampleUserInvestRewardsParams = UserInvestRewardsParams
    { 
        puirPoolParam = examplePoolParams, 
        puirDeadline   = examplePOSIXTime
    } 
