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

module Validators.StakeSimpleV1.Helpers where

import           Control.Monad        hiding (fmap)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           Data.String  
import qualified GHC.Generics                        as GHCGenerics (Generic)
--import           Ledger               hiding (singleton)
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
import qualified Data.OpenApi.Schema         (ToSchema)
import           Text.Printf          (printf)
import           Data.Typeable
import           Plutus.Trace.Emulator  as Emulator
import qualified Wallet.Emulator.Wallet              as WalletEmulator
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

--import Ledger           ( LedgerApiV1.POSIXTime(LedgerApiV1.POSIXTime), Ledger.Slot(Slot) )
import Ledger.TimeSlot  ( LedgerTimeSlot.SlotConfig(SlotConfig), LedgerTimeSlot.posixTimeToEnclosingSlot,slotToBeginPOSIXTime,LedgerTimeSlot.slotToEndPOSIXTime )


--Import Internos
import qualified Validators.StakeSimpleV1.Typos 

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
stringToBuiltinByteString :: P.String -> BuiltinByteString
stringToBuiltinByteString = toBuiltin . DataText.pack

{- | Function to return the Just value from a Maybe. -}
{-# INLINABLE Helpers.fromJust #-}
Helpers.fromJust :: Maybe a -> a
Helpers.fromJust (Just valueInfo) = valueInfo
Helpers.fromJust Nothing = traceError
                   "Helpers.fromJust Nothing"
                   

{- | Try to get the PoolState Datum from a generic Datum. -}
{-# INLINABLE getPoolStateFromDatum #-}
getPoolStateFromDatum ::  Maybe ValidatorDatum -> Maybe PoolStateTypo
getPoolStateFromDatum datum = case datum of
    Just (PoolState dPoolState) -> do
        -- PlutusContract.logInfo @P.String $ TextPrintf.printf "Encontrado Datumm PoolState: %s" (P.show dPoolState)
        Just dPoolState  
    _ -> Nothing

{- | Try to get the UserState Datum from a generic Datum. -}
{-# INLINABLE getUserStateFromDatum #-}
getUserStateFromDatum ::  Maybe ValidatorDatum -> Maybe UserStateTypo
getUserStateFromDatum datum = case datum of
    Just (UserState dUserState) -> do
        -- PlutusContract.logInfo @P.String $ TextPrintf.printf "Encontrado Datumm UserState: %s" (P.show dUserState)
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
{-# INLINABLE T.mkPoolStateWithNewFundFromPoolStateList #-}
T.mkPoolStateWithNewFundFromPoolStateList :: [PoolStateTypo] -> PoolNFT -> Master -> Fund -> ValidatorDatum
T.mkPoolStateWithNewFundFromPoolStateList poolStateDatums poolNFT master fund  = do
    let 
        userNFTs = concat [ T.psUsersNFT datum | datum <- poolStateDatums ]
        masterFunders = concat [ T.psMasterFunders datum | datum <- poolStateDatums ]
        masterFunder_others = [ masterFunder | masterFunder <- masterFunders ,  T.mfMaster masterFunder /= master]
        masterFunderOld =  find (\masterFunder -> T.mfMaster masterFunder == master) masterFunders

        masterFunderNew = case masterFunderOld of
            Nothing -> 
                -- traceError "Can't Find Master Funder In Datums"
                -- TODO: deberia dejar el error de arriba, pero como quiero ver como funciona los controles OnChain dejo que cree un nuevo PoolState Datum con un master que no existe
                T.mkMasterFunder master fund
            Just MasterFunder{..}  -> 
                T.mkMasterFunder master (T.mfFund  + fund)

    PoolState $ T.mkPoolStateTypo poolNFT (masterFunderNew:masterFunder_others) userNFTs



{- | Creates a new PoolState Datum with all the old Datums. -}
{-# INLINABLE T.mkPoolStateFromPoolStateList #-}
T.mkPoolStateFromPoolStateList :: [PoolStateTypo] -> PoolNFT  -> ValidatorDatum
T.mkPoolStateFromPoolStateList poolStateDatums poolNFT  = do
    let 
        userNFTs = concat [ T.psUsersNFT datum | datum <- poolStateDatums ]
        masterFunders = concat [ T.psMasterFunders datum | datum <- poolStateDatums ]
        -- TODO: poolNFT es un parametro que deberia chekear sea igual a todos los PoolState Datums que estoy juntando.

    PoolState $ T.mkPoolStateTypo poolNFT masterFunders userNFTs


{- | Creates a new PoolState Datum sing all the old Datums and adding the new user NFT. -}
{-# INLINABLE T.mkPoolStateWithNewUserInvestFromPoolStateList #-}
T.mkPoolStateWithNewUserInvestFromPoolStateList :: [PoolStateTypo] -> PoolNFT  -> UserNFT -> ValidatorDatum
T.mkPoolStateWithNewUserInvestFromPoolStateList poolStateDatums poolNFT userNFT  = do
    let 
        userNFTs = concat [ T.psUsersNFT datum | datum <- poolStateDatums ]
        masterFunders = concat [ T.psMasterFunders datum | datum <- poolStateDatums ]

    PoolState $ T.mkPoolStateTypo poolNFT masterFunders (userNFT:userNFTs)


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


--Getting Inputs and Output with PoolState or UserState Datums

{- | Get the Outputs going again to the script address. -}
{-# INLINABLE getOnwOutputs #-}
getOnwOutputs :: LedgerContextsV1.ScriptContext -> [LedgerApiV1.TxOut]
getOnwOutputs = LedgerContextsV1.getContinuingOutputs

{- | Gets the datum attached to a utxo. -}
{-# INLINABLE getDatumFromTxOut #-}
getDatumFromTxOut :: PlutusTx.FromData ValidatorDatum => LedgerApiV1.TxOut -> LedgerContextsV1.ScriptContext -> Maybe ValidatorDatum
getDatumFromTxOut o ctx = LedgerApiV1.txOutValue o >>= (`LedgerContextsV1.findDatum` LedgerContextsV1.scriptContextTxInfo ctx)
                   >>= PlutusTx.fromBuiltinData . LedgerScriptsV1.getDatum :: Maybe ValidatorDatum


{- | Gets the input PoolState Datums of the tx ScriptContext. -}
{-# INLINABLE getInputPoolStateDatums #-}
getInputPoolStateDatums :: LedgerContextsV1.ScriptContext -> [(LedgerApiV1.TxOut, T.PoolStateTypo)]
getInputPoolStateDatums ctx = do
    let
        txInInfoResolveds = [LedgerApiV1.txInInfoResolved  txInfoInput | txInfoInput <- LedgerApiV1.txInfoInputs (LedgerContextsV1.scriptContextTxInfo ctx)]

        txOutAndDatums = [  (LedgerApiV1.txInInfoResolved, getDatumFromTxOut LedgerApiV1.txInInfoResolved ctx) | LedgerApiV1.txInInfoResolved <- txInInfoResolveds]

        txOutAndJustDatums = [  (txtout, dat) | (txtout, dat) <- txOutAndDatums, isJust dat ]

        txOutAndPoolState =  [  (txtout, Helpers.fromJust $ getPoolStateFromDatum dat) | (txtout, dat) <- txOutAndJustDatums,  datumIsPoolState dat ] 

    txOutAndPoolState

{-# INLINABLE getInputUserStateDatums #-}
getInputUserStateDatums :: LedgerContextsV1.ScriptContext -> [(LedgerApiV1.TxOut, UserStateTypo)]
getInputUserStateDatums ctx = do
    let
        txInInfoResolveds = [LedgerApiV1.txInInfoResolved  txInfoInput | txInfoInput <- LedgerApiV1.txInfoInputs (LedgerContextsV1.scriptContextTxInfo ctx)]

        txOutAndDatums = [  (LedgerApiV1.txInInfoResolved, getDatumFromTxOut LedgerApiV1.txInInfoResolved ctx) | LedgerApiV1.txInInfoResolved <- txInInfoResolveds]

        txOutAndJustDatums = [  (txtout, dat) | (txtout, dat) <- txOutAndDatums, isJust dat ]

        txOutAndUserState =  [  (txtout, Helpers.fromJust $ getUserStateFromDatum dat) | (txtout, dat) <- txOutAndJustDatums,  datumIsUserState dat ] 

    txOutAndUserState

{-# INLINABLE getSingleOwnOutputPoolStateDatum #-}
getSingleOwnOutputPoolStateDatum :: LedgerContextsV1.ScriptContext -> (LedgerApiV1.TxOut, T.PoolStateTypo)
getSingleOwnOutputPoolStateDatum ctx = 
    case getOnwOutputs ctx of
        [txOut] -> case getDatumFromTxOut txOut ctx of
            Just (PoolState dPoolState)  -> (txOut, dPoolState)
            Nothing   -> traceError "Wrong output PoolState Datum type."
        _   -> traceError "Expected exactly one output with PoolState Datum."

{-# INLINABLE getDoubleOwnOutputsPoolStateAndUSerStateDatum #-}
getDoubleOwnOutputsPoolStateAndUSerStateDatum :: LedgerContextsV1.ScriptContext -> ((LedgerApiV1.TxOut, T.PoolStateTypo),(LedgerApiV1.TxOut, UserStateTypo))
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
getRewardsPerInvest :: Maybe LedgerApiV1.POSIXTime -> LedgerApiV1.POSIXTime -> LedgerApiV1.POSIXTime -> Invest  -> Proffit
getRewardsPerInvest lastClaim now depTime invest =
    case lastClaim of
        Nothing -> getRewards $ LedgerApiV1.getPOSIXTime (now - depTime)
        Just lClaim ->
            getRewards $ LedgerApiV1.getPOSIXTime (now - lClaim)
            -- if lClaim < depTime
            -- then getRewards $ LedgerApiV1.getPOSIXTime (now - depTime)
            -- else getRewards $ LedgerApiV1.getPOSIXTime (now - lClaim)
  where
    getRewards :: Integer -> Proffit
    -- TODO: deberia calcular 1% por slot, o sea por segundo, o sea por 1000ms
    -- getlevel: 1%
    -- 1% * 10000ms * 10_000_000    divide 100% * 1000ms * 
    -- 0.01 * 10s     * 10 ADA                               = 
    getRewards duration = (getLevel * duration * invest) `divide` (1000 * 100)

    --getRewards duration = (getLevel * duration * invest) -`divide` msPerYearMi

    getLevel :: Integer
    getLevel = levelAPR $ depositLevel depTime now


