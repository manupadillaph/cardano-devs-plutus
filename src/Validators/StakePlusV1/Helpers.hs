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

module Validators.StakePlusV1.Helpers where

--Import Externos

import qualified Data.ByteString.Char8               as DataByteStringChar8
import qualified Ledger                              ( Slot(Slot) ) --POSIXTime(POSIXTime), 
import qualified Ledger.TimeSlot                     as LedgerTimeSlot ( SlotConfig(SlotConfig), posixTimeToEnclosingSlot, slotToEndPOSIXTime) --,slotToBeginPOSIXTime
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Plutus.Script.Utils.V1.Scripts                as UtilsScriptsV1
import qualified Plutus.V1.Ledger.Api                as LedgerApiV1
import qualified Plutus.V1.Ledger.Interval           as LedgerIntervalV1 (contains, interval) --from, 
import qualified Plutus.V1.Ledger.Scripts            as LedgerScriptsV1
import qualified Plutus.V1.Ledger.Value              as LedgerValueV1
import qualified PlutusTx.Builtins                   as TxBuiltins 

--Import Internos

import qualified Validators.StakePlusV1.Typos        as T

-- Modulo:

-- minumun ada that a txout can hold
minLovelace :: Integer
minLovelace = 2000000

--Validators Helper Functions for On and OffChain code

{-# INLINABLE checkIntervalSize #-}
checkIntervalSize :: LedgerApiV1.Interval LedgerApiV1.POSIXTime -> LedgerApiV1.POSIXTime -> Bool
checkIntervalSize iv len =
    case getLowerBoundFromInterval iv of
        Just t  -> LedgerIntervalV1.interval t (t + len) `LedgerIntervalV1.contains` iv
        Nothing -> False


{-# INLINABLE getLowerBoundFromInterval #-}
getLowerBoundFromInterval :: LedgerApiV1.Interval a -> Maybe a
getLowerBoundFromInterval iv = case LedgerApiV1.ivFrom iv of
    LedgerApiV1.LowerBound (LedgerApiV1.Finite lBound) _ -> Just lBound
    _                            -> Nothing


{-# INLINABLE stringToBuiltinByteString #-}
stringToBuiltinByteString :: P.String -> TxBuiltins.BuiltinByteString
stringToBuiltinByteString = TxBuiltins.toBuiltin . DataByteStringChar8.pack

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
getPoolStateFromMaybeDatum ::  Maybe T.ValidatorDatum -> Maybe T.PoolStateTypo
getPoolStateFromMaybeDatum datum = case datum of
    Just (T.PoolState dPoolState) -> Just dPoolState
    _ -> Nothing

{- | Try to get the PoolState Datum from a generic Datum. -}
{-# INLINABLE getPoolStateFromDatum #-}
getPoolStateFromDatum :: T.ValidatorDatum -> Maybe T.PoolStateTypo
getPoolStateFromDatum datum = case datum of
    (T.PoolState dPoolState) -> Just dPoolState
    _ -> Nothing


{- | Try to get the UserState Datum from a generic Datum. -}
{-# INLINABLE getUserStateFromMaybeDatum #-}
getUserStateFromMaybeDatum ::  Maybe T.ValidatorDatum -> Maybe T.UserStateTypo
getUserStateFromMaybeDatum datum = case datum of
    Just (T.UserState dUserState) -> Just dUserState
    _ -> Nothing

{- | Try to get the UserState Datum from a generic Datum. -}
{-# INLINABLE getUserStateFromDatum #-}
getUserStateFromDatum ::   T.ValidatorDatum -> Maybe T.UserStateTypo
getUserStateFromDatum datum = case datum of
    (T.UserState dUserState) -> Just dUserState
    _ -> Nothing


{- | Check if the Datum is a T.PoolState. -}
{-# INLINABLE datumIsPoolState #-}
datumIsPoolState :: Maybe T.ValidatorDatum -> Bool
datumIsPoolState (Just (T.PoolState _)) = True
datumIsPoolState _             = False

{- | Check if the Datum is a UserState. -}
{-# INLINABLE datumIsUserState #-}
datumIsUserState :: Maybe T.ValidatorDatum -> Bool
datumIsUserState (Just (T.UserState _)) = True
datumIsUserState _             = False



{- | 
Creates a new PoolState Datum with all the old Datums. Se usa solo como dummy para juntar PoolState y ver cosas de a varios, 
como por ejemplo buscar un usuario dentro de la lista final de userNFTs
-}
{-# INLINABLE mkDummyPoolStateFromPoolStateList #-}
mkDummyPoolStateFromPoolStateList :: [T.PoolStateTypo] -> T.PoolNFT  -> T.ValidatorDatum
mkDummyPoolStateFromPoolStateList poolStateDatums poolNFT  = do
    let
        userNFTs = concat [ T.psUsersNFT datum | datum <- poolStateDatums ]

        masterFunders = concat [ T.psMasterFunders datum | datum <- poolStateDatums ]
        masterFundersAll = joinSameMasters  (masterFunders)

        cashedout = sum [ T.psChashedOut datum | datum <- poolStateDatums ]
        countTotalUtxoWithPoolState = sum [ T.psCountTotalUtxoWithPoolState datum | datum <- poolStateDatums ]

        -- TODO: 
        --    poolNFT es un parametro que deberia chekear sea igual a todos los PoolState Datums que estoy juntando.
        --    countTotalUtxoWithPoolState: deberia actualizarse con el nuevo total real. Esta suma es solo para crear un datum dummy

    T.PoolState $ T.mkPoolStateTypo poolNFT masterFundersAll userNFTs cashedout countTotalUtxoWithPoolState

{- | Creates a new PoolState Datum using all the old Datums and adding the cashedout. -}
{-# INLINABLE mkDummyPoolStateWithNewClaimRewardsFromPoolStateList #-}
mkDummyPoolStateWithNewClaimRewardsFromPoolStateList :: [T.PoolStateTypo] -> T.PoolNFT  -> T.Proffit -> T.ValidatorDatum
mkDummyPoolStateWithNewClaimRewardsFromPoolStateList poolStateDatums poolNFT cashedout  = do
    let
        userNFTs = concat [ T.psUsersNFT datum | datum <- poolStateDatums ]

        masterFunders = concat [ T.psMasterFunders datum | datum <- poolStateDatums ]
        masterFundersAll = joinSameMasters  (masterFunders)

        cashedoutOld = sum [ T.psChashedOut datum | datum <- poolStateDatums ]
        countTotalUtxoWithPoolState = sum [ T.psCountTotalUtxoWithPoolState datum | datum <- poolStateDatums ]

    T.PoolState $ T.mkPoolStateTypo poolNFT masterFundersAll userNFTs (cashedoutOld + cashedout) countTotalUtxoWithPoolState

{- | Creates a new PoolState Datum using all the old Datums and adding the cashedout. -}
{-# INLINABLE mkPoolStateWithNewClaimRewardsFromPoolState #-}
mkPoolStateWithNewClaimRewardsFromPoolState :: T.PoolStateTypo -> T.PoolNFT  -> T.Proffit -> T.ValidatorDatum
mkPoolStateWithNewClaimRewardsFromPoolState poolStateDatum poolNFT cashedout  =
    T.PoolState $ T.mkPoolStateTypo poolNFT (T.psMasterFunders poolStateDatum) (T.psUsersNFT poolStateDatum) (T.psChashedOut poolStateDatum + cashedout ) (T.psCountTotalUtxoWithPoolState poolStateDatum )


{- | Creates a new PoolState Datum using the old Datum and adding the new count fund. -}
{-# INLINABLE mkPoolStateWithNewCountFundsFromPoolState #-}
mkPoolStateWithNewCountFundsFromPoolState :: T.PoolStateTypo -> T.PoolNFT -> T.ValidatorDatum
mkPoolStateWithNewCountFundsFromPoolState poolStateDatum poolNFT  = do
    T.PoolState $ T.mkPoolStateTypo poolNFT (T.psMasterFunders poolStateDatum) (T.psUsersNFT poolStateDatum) (T.psChashedOut poolStateDatum) (T.psCountTotalUtxoWithPoolState poolStateDatum + 1)




-- {- | Creates a new PoolState Datum using all the old Datums and adding the new fund to the specific master masterFunder. -}
-- {-# INLINABLE mkPoolStateWithNewFundFromPoolStateList #-}
-- mkPoolStateWithNewFundFromPoolStateList :: [T.PoolStateTypo] -> T.PoolNFT -> T.Master -> T.Fund -> Integer -> T.ValidatorDatum
-- mkPoolStateWithNewFundFromPoolStateList poolStateDatums poolNFT master fund  countTotalUtxoWithPoolState = do
--     let 



--         userNFTs = concat [ T.psUsersNFT datum | datum <- poolStateDatums ]

--         masterFunders = concat [ T.psMasterFunders datum | datum <- poolStateDatums ]



--         masterFunder_others = [ masterFunder | masterFunder <- masterFunders ,  T.mfMaster masterFunder /= master]
--         masterFunderOld =  find (\masterFunder -> T.mfMaster masterFunder == master) masterFunders

--         masterFunderNew = case masterFunderOld of
--             Nothing -> 
--                 -- traceError "Can't Find Master Funder In Datums"
--                 -- TODO: deberia dejar el error de arriba, pero como quiero ver como funciona los controles OnChain dejo que cree un nuevo T.PoolState Datum con un master que no existe
--                 T.mkMasterFunder master fund
--             Just T.MasterFunder{..}  -> 
--                 T.mkMasterFunder master (T.mfFund  + fund)

--         cashedout = sum [ T.psChashedOut datum | datum <- poolStateDatums ]

--         --countTotalUtxoWithPoolState = sum [ T.psCountTotalUtxoWithPoolState datum | datum <- poolStateDatums ]  - lenght poolStateDatums

--     T.PoolState $ T.mkPoolStateTypo poolNFT (masterFunderNew:masterFunder_others) userNFTs cashedout countTotalUtxoWithPoolState

{- |  -}
{-# INLINABLE joinSameMasters #-}
joinSameMasters :: [T.MasterFunder]  -> [T.MasterFunder]
joinSameMasters masterFunders = do
    let
        deleteMaster mF mFs = [ mF' | mF' <- mFs ,  T.mfMaster mF' /=  T.mfMaster mF]

        findAndJoinMaster mF mFs = do
            let
                --[ mF' | mF' <- mFs ,  T.mfMaster mF' /=  T.mfMaster mF]    
                masterFundersOld =  filter  (\mF' -> T.mfMaster mF' == T.mfMaster mF) mFs

            case masterFundersOld of
                [] -> mF
                _ -> T.mkMasterFunder (T.mfMaster mF) (T.mfFund mF  + sum [ T.mfFund mF'| mF' <-  masterFundersOld  ] )

    case masterFunders of
        [] -> []
        _ -> do
            let
                headMasterFunder = head masterFunders
            findAndJoinMaster headMasterFunder (tail masterFunders) :joinSameMasters (deleteMaster headMasterFunder masterFunders)

    --     joinedHeadMasters = findAndJoinMaster 
    -- masterFunderOld =  find (\masterFunder -> T.mfMaster masterFunder == headMaster) masters

{- | Creates a new PoolState Datum using all the old Datums and adding the new fund to the specific master masterFunder. -}
{-# INLINABLE mkPoolStateWithNewFundFromPoolStateList #-}
mkPoolStateWithNewFundFromPoolStateList :: [T.PoolStateTypo] -> T.PoolNFT -> T.Master -> T.Fund -> Integer -> T.ValidatorDatum
mkPoolStateWithNewFundFromPoolStateList poolStateDatums poolNFT master fund  countTotalUtxoWithPoolState = do
    let


        userNFTs = concat [ T.psUsersNFT datum | datum <- poolStateDatums ]

        masterFunders = concat [ T.psMasterFunders datum | datum <- poolStateDatums ]
        newMaster = T.mkMasterFunder master fund

        masterFundersAll = joinSameMasters  (newMaster:masterFunders)

        -- masterFunder_others = [ masterFunder | masterFunder <- masterFunders ,  T.mfMaster masterFunder /= master]
        -- masterFunderOld =  find (\masterFunder -> T.mfMaster masterFunder == master) masterFunders

        -- masterFunderNew = case masterFunderOld of
        --     Nothing -> 
        --         -- traceError "Can't Find Master Funder In Datums"
        --         -- TODO: deberia dejar el error de arriba, pero como quiero ver como funciona los controles OnChain dejo que cree un nuevo T.PoolState Datum con un master que no existe
        --         T.mkMasterFunder master fund
        --     Just T.MasterFunder{..}  -> 
        --         T.mkMasterFunder master (T.mfFund  + fund)

        cashedout = sum [ T.psChashedOut datum | datum <- poolStateDatums ]

        --countTotalUtxoWithPoolState = sum [ T.psCountTotalUtxoWithPoolState datum | datum <- poolStateDatums ]  - lenght poolStateDatums

    T.PoolState $ T.mkPoolStateTypo poolNFT masterFundersAll userNFTs cashedout countTotalUtxoWithPoolState






{- | Creates a new PoolState Datum using all the old Datums and adding the new user NFT. -}
{-# INLINABLE mkPoolStateWithNewUserInvestFromPoolState #-}
mkPoolStateWithNewUserInvestFromPoolState :: T.PoolStateTypo -> T.PoolNFT  -> T.UserNFT -> T.ValidatorDatum
mkPoolStateWithNewUserInvestFromPoolState poolStateDatum poolNFT userNFT  = do
    -- let 
        -- userNFTs = concat [ T.psUsersNFT datum | datum <- poolStateDatums ]
        -- masterFunders = concat [ T.psMasterFunders datum | datum <- poolStateDatums ]
        -- cashedout = sum [ T.psChashedOut datum | datum <- poolStateDatums ]

    T.PoolState $ T.mkPoolStateTypo poolNFT (T.psMasterFunders poolStateDatum) (userNFT:T.psUsersNFT poolStateDatum) (T.psChashedOut poolStateDatum) (T.psCountTotalUtxoWithPoolState poolStateDatum)


-- {- | Creates a new PoolState Datum using all the old Datums and adding the new user NFT. -}
-- {-# INLINABLE mkPoolStateWithNewUserInvestFromPoolStateList #-}
-- mkPoolStateWithNewUserInvestFromPoolStateList :: [T.PoolStateTypo] -> T.PoolNFT  -> T.UserNFT -> T.ValidatorDatum
-- mkPoolStateWithNewUserInvestFromPoolStateList poolStateDatums poolNFT userNFT  = do
--     let 
--         userNFTs = concat [ T.psUsersNFT datum | datum <- poolStateDatums ]
--         masterFunders = concat [ T.psMasterFunders datum | datum <- poolStateDatums ]
--         cashedout = sum [ T.psChashedOut datum | datum <- poolStateDatums ]

--     T.PoolState $ T.mkPoolStateTypo poolNFT masterFunders (userNFT:userNFTs) cashedout


-- Definition of Currency Symbol for the NFT

{-# INLINABLE curSymbol #-}
curSymbol :: LedgerScriptsV1.MintingPolicy  -> LedgerValueV1.CurrencySymbol
curSymbol  = UtilsScriptsV1.scriptCurrencySymbol

-- Helpers for conversion of time and slots

{-# INLINABLE timeWhenSlotChangedTo1Sec #-}
timeWhenSlotChangedTo1Sec :: LedgerApiV1.POSIXTime
timeWhenSlotChangedTo1Sec = LedgerApiV1.POSIXTime 1595967616000  -- 2020/07/28 20:20:16 - epoch:74 - slot:1598400 - block:1597133  

{-# INLINABLE slotWhenSlotChangedTo1Sec #-}
slotWhenSlotChangedTo1Sec :: Ledger.Slot
slotWhenSlotChangedTo1Sec = Ledger.Slot 1598400

{-# INLINABLE testnetConf #-}
testnetConf :: LedgerTimeSlot.SlotConfig
testnetConf = LedgerTimeSlot.SlotConfig 1000 timeWhenSlotChangedTo1Sec

{-# INLINABLE slotTestnetToPosixTimeToSConverter #-}
slotTestnetToPosixTimeToSConverter :: Ledger.Slot -> LedgerApiV1.POSIXTime
slotTestnetToPosixTimeToSConverter = LedgerTimeSlot.slotToEndPOSIXTime testnetConf
-- TODO: tambien puede ser convertido al comienzo del slot: LedgerTimeSlot.slotToEndPOSIXTime

{-# INLINABLE posixTimeToSlotTestnetConverter #-}
posixTimeToSlotTestnetConverter :: LedgerApiV1.POSIXTime -> Ledger.Slot
posixTimeToSlotTestnetConverter time = slotWhenSlotChangedTo1Sec + LedgerTimeSlot.posixTimeToEnclosingSlot testnetConf time


-- miliseconds per day
{-# INLINABLE msPerDay #-}
msPerDay :: Integer
msPerDay = 1000 * 60 * 60 * 24

-- miliseconds per year
{-# INLINABLE msPerYear #-}
msPerYear :: Integer
msPerYear = msPerDay * 365

-- day number to LedgerApiV1.POSIXTime
{-# INLINABLE days #-}
days :: Integer -> LedgerApiV1.POSIXTime
days n = LedgerApiV1.POSIXTime (n * msPerDay)

-- 1% o 2% proffit 
{-# INLINABLE levelAPR #-}
levelAPR :: Integer -> Integer
levelAPR n | n == 1 = 1
           | n == 2 = 2

-- 25% o 50% depends in the deposit time
{-# INLINABLE depositLevel #-}
depositLevel :: LedgerApiV1.POSIXTime -> LedgerApiV1.POSIXTime -> Integer
depositLevel depositTime now =
    if (now - depositTime) < days 90
    then 1
    else 2

msPerYearMi :: Integer
msPerYearMi = msPerYear * 1_000_000

{-# INLINABLE getRewardsPerInvest #-}
getRewardsPerInvest :: Maybe LedgerApiV1.POSIXTime -> LedgerApiV1.POSIXTime -> LedgerApiV1.POSIXTime -> T.Invest  -> T.Proffit
getRewardsPerInvest lastClaim now depTime invest =
    case lastClaim of
        Nothing -> getRewards $ LedgerApiV1.getPOSIXTime (now - depTime)
        Just lClaim ->
            getRewards $ LedgerApiV1.getPOSIXTime (now - lClaim)
            -- if lClaim < depTime
            -- then getRewards $ LedgerApiV1.getPOSIXTime (now - depTime)
            -- else getRewards $ LedgerApiV1.getPOSIXTime (now - lClaim)
  where
    getRewards :: Integer -> T.Proffit
    -- TODO: deberia calcular 1% por slot, o sea por segundo, o sea por 1000ms
    -- getlevel: 1%
    -- 1% * 10000ms * 10_000_000    divide 100% * 1000ms * 
    -- 0.01 * 10s     * 10 ADA                               = 
    --getRewards duration = (getLevel * duration * invest) `divide` (1000 * 100)

    --1% cada decima de segundo, o sea 1s/100
    getRewards duration = (getLevel * duration * invest) `divide` (100 * 100)

    --getRewards duration = (getLevel * duration * invest) -`divide` msPerYearMi

    getLevel :: Integer
    getLevel = levelAPR $ depositLevel depTime now


