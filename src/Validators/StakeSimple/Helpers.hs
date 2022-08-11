
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

module Validators.StakeSimple.Helpers where

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


import qualified Data.ByteString.Char8 as C

--import Ledger           ( POSIXTime(POSIXTime), Slot(Slot) )
import Ledger.TimeSlot  ( SlotConfig(SlotConfig), posixTimeToEnclosingSlot,slotToBeginPOSIXTime,slotToEndPOSIXTime )


--Import Internos
import  Validators.StakeSimple.Typos 

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
                   

{- | Try to get the PoolState Datum from a generic Datum. -}
{-# INLINABLE getPoolStateFromDatum #-}
getPoolStateFromDatum ::  Maybe ValidatorDatum -> Maybe PoolStateTypo
getPoolStateFromDatum datum = case datum of
    Just (PoolState dPoolState) ->  do
        -- logInfo @HASKELL.String $ printf "Encontrado Datumm PoolState: %s" (HASKELL.show dPoolState)
        Just dPoolState  
    _ -> Nothing

{- | Try to get the UserState Datum from a generic Datum. -}
{-# INLINABLE getUserStateFromDatum #-}
getUserStateFromDatum ::  Maybe ValidatorDatum -> Maybe UserStateTypo
getUserStateFromDatum datum = case datum of
    Just (UserState dUserState) ->  do
        -- logInfo @HASKELL.String $ printf "Encontrado Datumm UserState: %s" (HASKELL.show dUserState)
        Just dUserState  
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


{- | Creates a new PoolState Datum using all the old Datums and adding the new fund to the specific master masterFunder. -}
{-# INLINABLE mkPoolStateWithNewFundFromPoolStateList #-}
mkPoolStateWithNewFundFromPoolStateList :: [PoolStateTypo] -> PoolNFT -> Master -> Fund -> ValidatorDatum
mkPoolStateWithNewFundFromPoolStateList poolStateDatums poolNFT master fund  = do
    let 
        userNFTs = PlutusTx.Prelude.concat [ psUsersNFT datum | datum <- poolStateDatums ]
        masterFunders = PlutusTx.Prelude.concat [ psMasterFunders datum | datum <- poolStateDatums ]
        masterFunder_others = [ masterFunder | masterFunder <- masterFunders ,  mfMaster masterFunder /= master]
        masterFunderOld =  PlutusTx.Prelude.find (\masterFunder -> mfMaster masterFunder == master) masterFunders

        masterFunderNew = case masterFunderOld of
            Nothing -> 
                -- traceError "Can't Find Master Funder In Datums"
                -- TODO: deberia dejar el error de arriba, pero como quiero ver como funciona los controles OnChain dejo que cree un nuevo PoolState Datum con un master que no existe
                mkMasterFunder master fund
            Just MasterFunder{..}  -> 
                mkMasterFunder master (mfFund  + fund)

    PoolState $ mkPoolStateTypo poolNFT (masterFunderNew:masterFunder_others) userNFTs



{- | Creates a new PoolState Datum with all the old Datums. -}
{-# INLINABLE mkPoolStateFromPoolStateList #-}
mkPoolStateFromPoolStateList :: [PoolStateTypo] ->  PoolNFT  ->  ValidatorDatum
mkPoolStateFromPoolStateList poolStateDatums poolNFT  = do
    let 
        userNFTs = PlutusTx.Prelude.concat [ psUsersNFT datum | datum <- poolStateDatums ]
        masterFunders = PlutusTx.Prelude.concat [ psMasterFunders datum | datum <- poolStateDatums ]
        -- TODO: poolNFT es un parametro que deberia chekear sea igual a todos los PoolState Datums que estoy juntando.

    PoolState $ mkPoolStateTypo poolNFT masterFunders userNFTs


{- | Creates a new PoolState Datum sing all the old Datums and adding the new user NFT. -}
{-# INLINABLE mkPoolStateWithNewUserInvestFromPoolStateList #-}
mkPoolStateWithNewUserInvestFromPoolStateList :: [PoolStateTypo] ->  PoolNFT  -> UserNFT ->  ValidatorDatum
mkPoolStateWithNewUserInvestFromPoolStateList poolStateDatums poolNFT userNFT  = do
    let 
        userNFTs = PlutusTx.Prelude.concat [ psUsersNFT datum | datum <- poolStateDatums ]
        masterFunders = PlutusTx.Prelude.concat [ psMasterFunders datum | datum <- poolStateDatums ]

    PoolState $ mkPoolStateTypo poolNFT masterFunders (userNFT:userNFTs)


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


--Getting Inputs and Output with PoolState or UserState Datums

{- | Get the Outputs going again to the script address. -}
{-# INLINABLE getOnwOutputs #-}
getOnwOutputs :: ScriptContext -> [TxOut]
getOnwOutputs = getContinuingOutputs

{- | Gets the datum attached to a utxo. -}
{-# INLINABLE getDatumFromTxOut #-}
getDatumFromTxOut :: PlutusTx.FromData ValidatorDatum => TxOut -> ScriptContext -> Maybe ValidatorDatum
getDatumFromTxOut o ctx = txOutDatum o >>= (`findDatum` scriptContextTxInfo ctx)
                   >>= PlutusTx.fromBuiltinData . getDatum :: Maybe ValidatorDatum


{- | Gets the input PoolState Datums of the tx ScriptContext. -}
{-# INLINABLE getInputPoolStateDatums #-}
getInputPoolStateDatums :: ScriptContext -> [(TxOut,PoolStateTypo)]
getInputPoolStateDatums ctx = do
    let
        txInInfoResolveds = [txInInfoResolved  txInfoInput | txInfoInput <- txInfoInputs (scriptContextTxInfo ctx)]

        txOutAndDatums = [  (txInInfoResolved, getDatumFromTxOut txInInfoResolved ctx) | txInInfoResolved <- txInInfoResolveds]

        txOutAndJustDatums = [  (txtout, dat) | (txtout, dat) <- txOutAndDatums, isJust dat ]

        txOutAndPoolState =  [  (txtout, fromJust $ getPoolStateFromDatum dat) | (txtout, dat) <- txOutAndJustDatums,  datumIsPoolState dat ] 

    txOutAndPoolState

{-# INLINABLE getInputUserStateDatums #-}
getInputUserStateDatums :: ScriptContext -> [(TxOut,UserStateTypo)]
getInputUserStateDatums ctx = do
    let
        txInInfoResolveds = [txInInfoResolved  txInfoInput | txInfoInput <- txInfoInputs (scriptContextTxInfo ctx)]

        txOutAndDatums = [  (txInInfoResolved, getDatumFromTxOut txInInfoResolved ctx) | txInInfoResolved <- txInInfoResolveds]

        txOutAndJustDatums = [  (txtout, dat) | (txtout, dat) <- txOutAndDatums, isJust dat ]

        txOutAndUserState =  [  (txtout, fromJust $ getUserStateFromDatum dat) | (txtout, dat) <- txOutAndJustDatums,  datumIsUserState dat ] 

    txOutAndUserState

{-# INLINABLE getSingleOwnOutputPoolStateDatum #-}
getSingleOwnOutputPoolStateDatum :: ScriptContext -> (TxOut,PoolStateTypo)
getSingleOwnOutputPoolStateDatum ctx = 
    case getOnwOutputs ctx of
        [txOut] -> case getDatumFromTxOut txOut ctx of
            Just (PoolState dPoolState)  -> (txOut, dPoolState)
            Nothing   -> traceError "Wrong output PoolState Datum type."
        _   -> traceError "Expected exactly one output with PoolState Datum."

{-# INLINABLE getDoubleOwnOutputsPoolStateAndUSerStateDatum #-}
getDoubleOwnOutputsPoolStateAndUSerStateDatum :: ScriptContext -> ((TxOut,PoolStateTypo),(TxOut,UserStateTypo))
getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx = 
    case getOnwOutputs ctx of
        [txOut1,txOut2]-> case (getDatumFromTxOut txOut1 ctx, getDatumFromTxOut txOut2 ctx) of
            (Just (PoolState dPoolState), Just (UserState dUserState) ) -> ((txOut1, dPoolState),(txOut2, dUserState))
            (Just (UserState dUserState), Just (PoolState dPoolState) ) -> ((txOut2, dPoolState),(txOut1, dUserState))
            (Nothing,_)   -> traceError "Wrong output PoolState Datum or UserState Datum type."
            (_,Nothing)   -> traceError "Wrong output PoolState Datum or UserState Datum type."
        _   -> traceError "Expected exactly two output. One with PoolState Datum and other with UserState Datum"





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
    getRewards duration = (getLevel * duration * invest) `PlutusTx.Prelude.divide` (1000 * 100)

    --getRewards duration = (getLevel * duration * invest) -`PlutusTx.Prelude.divide` msPerYearMi

    getLevel :: Integer
    getLevel = levelAPR $ depositLevel depTime now


