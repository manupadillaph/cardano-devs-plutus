
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

{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE AllowAmbiguousTypes              #-}
{-# LANGUAGE NumericUnderscores    #-}

--{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Validators.StakePlus.Helpers where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           Data.String  
import           GHC.Generics         (Generic)
--import           Ledger               hiding (singleton)
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
import qualified Data.OpenApi.Schema         (ToSchema)
import           Text.Printf          (printf)
import           Data.Typeable
import           Plutus.Trace.Emulator  as Emulator
import           Wallet.Emulator.Wallet
import           Data.Default
--import          Ledger.TimeSlot 

--Import Nuevos

import          Control.Lens

import PlutusTx.Builtins

import qualified Data.Map as Map
import Ledger 
import Ledger.Index
import qualified Plutus.Trace.Emulator  as Trace
import qualified Data.List
import Data.List

import qualified Data.ByteString.Char8 as C

--import Ledger           ( POSIXTime(POSIXTime), Slot(Slot) )
import Ledger.TimeSlot  ( SlotConfig(SlotConfig), posixTimeToEnclosingSlot,slotToBeginPOSIXTime,slotToEndPOSIXTime )


--Import Internos
import  Validators.StakePlus.Typos 


-- minumun ada that a txout can hold
minLovelace :: Integer
minLovelace = 2000000

--Validators Helper Functions for On and OffChain code


{-# INLINABLE checkIntervalSize #-}
checkIntervalSize :: Interval POSIXTime -> POSIXTime -> Bool
checkIntervalSize iv len =
    case getLowerBoundFromInterval iv of
        Just t  -> interval t (t + len) `Ledger.contains` iv
        Nothing -> False


{-# INLINABLE getLowerBoundFromInterval #-}
getLowerBoundFromInterval :: Interval a -> Maybe a
getLowerBoundFromInterval iv = case ivFrom iv of
    LowerBound (Finite lBound) _ -> Just lBound
    _                            -> Nothing


{-# INLINABLE stringToBuiltinByteString #-}
stringToBuiltinByteString :: HASKELL.String -> BuiltinByteString
stringToBuiltinByteString = toBuiltin . C.pack

{- | Function to return the Just value from a Maybe. -}
{-# INLINABLE fromJust #-}
fromJust :: Maybe a -> a
fromJust (Just valueInfo) = valueInfo
fromJust Nothing = traceError
                   "fromJust Nothing"

{-# INLINABLE get1st #-}
get1st :: (t, t1, t2) -> t
get1st (x,_,_) = x

{-# INLINABLE get2nd #-}
get2nd :: (t, t1, t2) -> t1
get2nd (_,x,_) = x

{-# INLINABLE get3rd #-}
get3rd :: (t, t1, t2) -> t2
get3rd (_,_,x) = x

{-# INLINABLE removeItem #-}
removeItem  :: Eq a => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys





{- | Try to get the PoolState Datum from a generic Datum. -}
{-# INLINABLE getPoolStateFromMaybeDatum #-}
getPoolStateFromMaybeDatum ::  Maybe ValidatorDatum -> Maybe PoolStateTypo
getPoolStateFromMaybeDatum datum = case datum of
    Just (PoolState dPoolState) -> Just dPoolState  
    _ -> Nothing

{- | Try to get the PoolState Datum from a generic Datum. -}
{-# INLINABLE getPoolStateFromDatum #-}
getPoolStateFromDatum ::  ValidatorDatum -> Maybe PoolStateTypo
getPoolStateFromDatum datum = case datum of
    (PoolState dPoolState) -> Just dPoolState  
    _ -> Nothing


{- | Try to get the UserState Datum from a generic Datum. -}
{-# INLINABLE getUserStateFromMaybeDatum #-}
getUserStateFromMaybeDatum ::  Maybe ValidatorDatum -> Maybe UserStateTypo
getUserStateFromMaybeDatum datum = case datum of
    Just (UserState dUserState) -> Just dUserState  
    _ -> Nothing

{- | Try to get the UserState Datum from a generic Datum. -}
{-# INLINABLE getUserStateFromDatum #-}
getUserStateFromDatum ::   ValidatorDatum -> Maybe UserStateTypo
getUserStateFromDatum datum = case datum of
    (UserState dUserState) ->  Just dUserState  
    _ -> Nothing


{- | Check if the Datum is a PoolState. -}
{-# INLINABLE datumIsPoolState #-}
datumIsPoolState :: Maybe ValidatorDatum -> Bool
datumIsPoolState (Just (PoolState _)) = True
datumIsPoolState _             = False

{- | Check if the Datum is a UserState. -}
{-# INLINABLE datumIsUserState #-}
datumIsUserState :: Maybe ValidatorDatum -> Bool
datumIsUserState (Just (UserState _)) = True
datumIsUserState _             = False



{- | 
Creates a new PoolState Datum with all the old Datums. Se usa solo como dummy para juntar PoolState y ver cosas de a varios, 
como por ejemplo buscar un usuario dentro de la lista final de userNFTs
-}
{-# INLINABLE mkDummyPoolStateFromPoolStateList #-}
mkDummyPoolStateFromPoolStateList :: [PoolStateTypo] ->  PoolNFT  ->  ValidatorDatum
mkDummyPoolStateFromPoolStateList poolStateDatums poolNFT  = do
    let 
        userNFTs = PlutusTx.Prelude.concat [ psUsersNFT datum | datum <- poolStateDatums ]

        masterFunders = PlutusTx.Prelude.concat [ psMasterFunders datum | datum <- poolStateDatums ]
        masterFundersAll = joinSameMasters  (masterFunders)

        cashedout = PlutusTx.Prelude.sum [ psChashedOut datum | datum <- poolStateDatums ]
        countTotalUtxoWithPoolState = PlutusTx.Prelude.sum [ psCountTotalUtxoWithPoolState datum | datum <- poolStateDatums ]

        -- TODO: 
        --    poolNFT es un parametro que deberia chekear sea igual a todos los PoolState Datums que estoy juntando.
        --    countTotalUtxoWithPoolState: deberia actualizarse con el nuevo total real. Esta suma es solo para crear un datum dummy

    PoolState $ mkPoolStateTypo poolNFT masterFundersAll userNFTs cashedout countTotalUtxoWithPoolState

{- | Creates a new PoolState Datum using all the old Datums and adding the cashedout. -}
{-# INLINABLE mkDummyPoolStateWithNewClaimRewardsFromPoolStateList #-}
mkDummyPoolStateWithNewClaimRewardsFromPoolStateList :: [PoolStateTypo] ->  PoolNFT  -> Proffit ->  ValidatorDatum
mkDummyPoolStateWithNewClaimRewardsFromPoolStateList poolStateDatums poolNFT cashedout  = do
    let 
        userNFTs = PlutusTx.Prelude.concat [ psUsersNFT datum | datum <- poolStateDatums ]

        masterFunders = PlutusTx.Prelude.concat [ psMasterFunders datum | datum <- poolStateDatums ]
        masterFundersAll = joinSameMasters  (masterFunders)

        cashedoutOld = PlutusTx.Prelude.sum [ psChashedOut datum | datum <- poolStateDatums ]
        countTotalUtxoWithPoolState = PlutusTx.Prelude.sum [ psCountTotalUtxoWithPoolState datum | datum <- poolStateDatums ]

    PoolState $ mkPoolStateTypo poolNFT masterFundersAll userNFTs (cashedoutOld + cashedout) countTotalUtxoWithPoolState

{- | Creates a new PoolState Datum using all the old Datums and adding the cashedout. -}
{-# INLINABLE mkPoolStateWithNewClaimRewardsFromPoolState #-}
mkPoolStateWithNewClaimRewardsFromPoolState :: PoolStateTypo ->  PoolNFT  -> Proffit ->  ValidatorDatum
mkPoolStateWithNewClaimRewardsFromPoolState poolStateDatum poolNFT cashedout  = 
    PoolState $ mkPoolStateTypo poolNFT (psMasterFunders poolStateDatum) (psUsersNFT poolStateDatum) (psChashedOut poolStateDatum + cashedout ) (psCountTotalUtxoWithPoolState poolStateDatum )


{- | Creates a new PoolState Datum using the old Datum and adding the new count fund. -}
{-# INLINABLE mkPoolStateWithNewCountFundsFromPoolState #-}
mkPoolStateWithNewCountFundsFromPoolState :: PoolStateTypo -> PoolNFT ->  ValidatorDatum
mkPoolStateWithNewCountFundsFromPoolState poolStateDatum poolNFT  = do
    PoolState $ mkPoolStateTypo poolNFT (psMasterFunders poolStateDatum) (psUsersNFT poolStateDatum) (psChashedOut poolStateDatum) (psCountTotalUtxoWithPoolState poolStateDatum + 1)




-- {- | Creates a new PoolState Datum using all the old Datums and adding the new fund to the specific master masterFunder. -}
-- {-# INLINABLE mkPoolStateWithNewFundFromPoolStateList #-}
-- mkPoolStateWithNewFundFromPoolStateList :: [PoolStateTypo] -> PoolNFT -> Master -> Fund ->  Integer ->  ValidatorDatum
-- mkPoolStateWithNewFundFromPoolStateList poolStateDatums poolNFT master fund  countTotalUtxoWithPoolState = do
--     let 



--         userNFTs = PlutusTx.Prelude.concat [ psUsersNFT datum | datum <- poolStateDatums ]

--         masterFunders = PlutusTx.Prelude.concat [ psMasterFunders datum | datum <- poolStateDatums ]

        

--         masterFunder_others = [ masterFunder | masterFunder <- masterFunders ,  mfMaster masterFunder /= master]
--         masterFunderOld =  PlutusTx.Prelude.find (\masterFunder -> mfMaster masterFunder == master) masterFunders

--         masterFunderNew = case masterFunderOld of
--             Nothing -> 
--                 -- traceError "Can't Find Master Funder In Datums"
--                 -- TODO: deberia dejar el error de arriba, pero como quiero ver como funciona los controles OnChain dejo que cree un nuevo PoolState Datum con un master que no existe
--                 mkMasterFunder master fund
--             Just MasterFunder{..}  -> 
--                 mkMasterFunder master (mfFund  + fund)

--         cashedout = PlutusTx.Prelude.sum [ psChashedOut datum | datum <- poolStateDatums ]

--         --countTotalUtxoWithPoolState = PlutusTx.Prelude.sum [ psCountTotalUtxoWithPoolState datum | datum <- poolStateDatums ]  - lenght poolStateDatums

--     PoolState $ mkPoolStateTypo poolNFT (masterFunderNew:masterFunder_others) userNFTs cashedout countTotalUtxoWithPoolState

{- |  -}
{-# INLINABLE joinSameMasters #-}
joinSameMasters :: [MasterFunder]  ->  [MasterFunder]  
joinSameMasters masterFunders = do
    let
        deleteMaster masterFunder masterFunders = [ masterFunder' | masterFunder' <- masterFunders ,  mfMaster masterFunder' /=  mfMaster masterFunder]    

        findAndJoinMaster masterFunder masterFunders = do
            let
                --[ masterFunder' | masterFunder' <- masterFunders ,  mfMaster masterFunder' /=  mfMaster masterFunder]    
                masterFundersOld =  PlutusTx.Prelude.filter  (\masterFunder' -> mfMaster masterFunder' == mfMaster masterFunder) masterFunders

            case masterFundersOld of
                [] ->  masterFunder
                _ -> mkMasterFunder (mfMaster masterFunder) (mfFund masterFunder  + PlutusTx.Prelude.sum [ mfFund masterFunder'| masterFunder' <-  masterFundersOld  ] )

    case masterFunders of
        [] -> []
        _ -> do
            let 
                headMasterFunder = PlutusTx.Prelude.head masterFunders
            findAndJoinMaster headMasterFunder (PlutusTx.Prelude.tail masterFunders) :joinSameMasters (deleteMaster headMasterFunder masterFunders)

    --     joinedHeadMasters = findAndJoinMaster 
    -- masterFunderOld =  PlutusTx.Prelude.find (\masterFunder -> mfMaster masterFunder == headMaster) masters

{- | Creates a new PoolState Datum using all the old Datums and adding the new fund to the specific master masterFunder. -}
{-# INLINABLE mkPoolStateWithNewFundFromPoolStateList #-}
mkPoolStateWithNewFundFromPoolStateList :: [PoolStateTypo] -> PoolNFT -> Master -> Fund ->  Integer ->  ValidatorDatum
mkPoolStateWithNewFundFromPoolStateList poolStateDatums poolNFT master fund  countTotalUtxoWithPoolState = do
    let 
        

        userNFTs = PlutusTx.Prelude.concat [ psUsersNFT datum | datum <- poolStateDatums ]

        masterFunders = PlutusTx.Prelude.concat [ psMasterFunders datum | datum <- poolStateDatums ]
        newMaster = mkMasterFunder master fund

        masterFundersAll = joinSameMasters  (newMaster:masterFunders)

        -- masterFunder_others = [ masterFunder | masterFunder <- masterFunders ,  mfMaster masterFunder /= master]
        -- masterFunderOld =  PlutusTx.Prelude.find (\masterFunder -> mfMaster masterFunder == master) masterFunders

        -- masterFunderNew = case masterFunderOld of
        --     Nothing -> 
        --         -- traceError "Can't Find Master Funder In Datums"
        --         -- TODO: deberia dejar el error de arriba, pero como quiero ver como funciona los controles OnChain dejo que cree un nuevo PoolState Datum con un master que no existe
        --         mkMasterFunder master fund
        --     Just MasterFunder{..}  -> 
        --         mkMasterFunder master (mfFund  + fund)

        cashedout = PlutusTx.Prelude.sum [ psChashedOut datum | datum <- poolStateDatums ]

        --countTotalUtxoWithPoolState = PlutusTx.Prelude.sum [ psCountTotalUtxoWithPoolState datum | datum <- poolStateDatums ]  - lenght poolStateDatums

    PoolState $ mkPoolStateTypo poolNFT masterFundersAll userNFTs cashedout countTotalUtxoWithPoolState






{- | Creates a new PoolState Datum using all the old Datums and adding the new user NFT. -}
{-# INLINABLE mkPoolStateWithNewUserInvestFromPoolState #-}
mkPoolStateWithNewUserInvestFromPoolState :: PoolStateTypo ->  PoolNFT  -> UserNFT ->  ValidatorDatum
mkPoolStateWithNewUserInvestFromPoolState poolStateDatum poolNFT userNFT  = do
    -- let 
        -- userNFTs = PlutusTx.Prelude.concat [ psUsersNFT datum | datum <- poolStateDatums ]
        -- masterFunders = PlutusTx.Prelude.concat [ psMasterFunders datum | datum <- poolStateDatums ]
        -- cashedout = PlutusTx.Prelude.sum [ psChashedOut datum | datum <- poolStateDatums ]

    PoolState $ mkPoolStateTypo poolNFT (psMasterFunders poolStateDatum) (userNFT:psUsersNFT poolStateDatum) (psChashedOut poolStateDatum) (psCountTotalUtxoWithPoolState poolStateDatum)


-- {- | Creates a new PoolState Datum using all the old Datums and adding the new user NFT. -}
-- {-# INLINABLE mkPoolStateWithNewUserInvestFromPoolStateList #-}
-- mkPoolStateWithNewUserInvestFromPoolStateList :: [PoolStateTypo] ->  PoolNFT  -> UserNFT ->  ValidatorDatum
-- mkPoolStateWithNewUserInvestFromPoolStateList poolStateDatums poolNFT userNFT  = do
--     let 
--         userNFTs = PlutusTx.Prelude.concat [ psUsersNFT datum | datum <- poolStateDatums ]
--         masterFunders = PlutusTx.Prelude.concat [ psMasterFunders datum | datum <- poolStateDatums ]
--         cashedout = PlutusTx.Prelude.sum [ psChashedOut datum | datum <- poolStateDatums ]

--     PoolState $ mkPoolStateTypo poolNFT masterFunders (userNFT:userNFTs) cashedout


-- Definition of Currency Symbol for the NFT

{-# INLINABLE curSymbol #-}
curSymbol :: Scripts.MintingPolicy  -> CurrencySymbol
curSymbol  = scriptCurrencySymbol 

-- Helpers for conversion of time and slots

{-# INLINABLE timeWhenSlotChangedTo1Sec #-}
timeWhenSlotChangedTo1Sec :: POSIXTime
timeWhenSlotChangedTo1Sec = POSIXTime 1595967616000  -- 2020/07/28 20:20:16 - epoch:74 - slot:1598400 - block:1597133  

{-# INLINABLE slotWhenSlotChangedTo1Sec #-}
slotWhenSlotChangedTo1Sec :: Slot
slotWhenSlotChangedTo1Sec = Slot 1598400

{-# INLINABLE testnetConf #-}
testnetConf :: SlotConfig
testnetConf = SlotConfig 1000 timeWhenSlotChangedTo1Sec 

{-# INLINABLE slotTestnetToPosixTimeToSConverter #-}
slotTestnetToPosixTimeToSConverter :: Slot -> POSIXTime
slotTestnetToPosixTimeToSConverter = slotToEndPOSIXTime testnetConf
-- TODO: tambien puede ser convertido al comienzo del slot: slotToEndPOSIXTime

{-# INLINABLE posixTimeToSlotTestnetConverter #-}
posixTimeToSlotTestnetConverter :: POSIXTime -> Slot
posixTimeToSlotTestnetConverter time = slotWhenSlotChangedTo1Sec + posixTimeToEnclosingSlot testnetConf time


-- miliseconds per day
{-# INLINABLE msPerDay #-}
msPerDay :: Integer
msPerDay = 1000 * 60 * 60 * 24

-- miliseconds per year
{-# INLINABLE msPerYear #-}
msPerYear :: Integer
msPerYear = msPerDay * 365

-- day number to POSIXTime
{-# INLINABLE days #-}
days :: Integer -> POSIXTime
days n = POSIXTime (n * msPerDay)

-- 1% o 2% proffit 
{-# INLINABLE levelAPR #-}
levelAPR :: Integer -> Integer
levelAPR n | n == 1 = 1
           | n == 2 = 2

-- 25% o 50% depends in the deposit time
{-# INLINABLE depositLevel #-}
depositLevel :: POSIXTime -> POSIXTime -> Integer
depositLevel depositTime now =
    if (now - depositTime) < days 90
    then 1
    else 2

msPerYearMi :: Integer
msPerYearMi = msPerYear * 1_000_000

{-# INLINABLE getRewardsPerInvest #-}
getRewardsPerInvest :: Maybe POSIXTime -> POSIXTime -> POSIXTime ->  Invest  ->  Proffit
getRewardsPerInvest lastClaim now depTime invest =
    case lastClaim of
        Nothing -> getRewards $ getPOSIXTime (now - depTime)
        Just lClaim ->
            getRewards $ getPOSIXTime (now - lClaim)
            -- if lClaim < depTime
            -- then getRewards $ getPOSIXTime (now - depTime)
            -- else getRewards $ getPOSIXTime (now - lClaim)
  where
    getRewards :: Integer -> Proffit
    -- TODO: deberia calcular 1% por slot, o sea por segundo, o sea por 1000ms
    -- getlevel: 1%
    -- 1% * 10000ms * 10_000_000    divide 100% * 1000ms * 
    -- 0.01 * 10s     * 10 ADA                               = 
    --getRewards duration = (getLevel * duration * invest) `PlutusTx.Prelude.divide` (1000 * 100)

    --1% cada decima de segundo, o sea 1s/100
    getRewards duration = (getLevel * duration * invest) `PlutusTx.Prelude.divide` (100 * 100)

    --getRewards duration = (getLevel * duration * invest) -`PlutusTx.Prelude.divide` msPerYearMi

    getLevel :: Integer
    getLevel = levelAPR $ depositLevel depTime now


