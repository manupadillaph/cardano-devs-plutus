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

module Validators.StakeSimple.OffChainHelpers where

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
import     qualified      Data.OpenApi.Schema         (ToSchema)
import           Text.Printf          (printf)
import Data.Typeable

import          Plutus.Trace.Emulator  as Emulator
import          Wallet.Emulator.Wallet
import          Data.Default
import          Ledger.TimeSlot 

--Import Nuevos

import          Control.Lens

import PlutusTx.Builtins

import qualified Data.Map as Map
import Ledger 
import Ledger.Index
import qualified Plutus.Trace.Emulator  as Trace
import qualified Data.List

--Import Internos
import  Validators.StakeSimple.Typos 
--import  Validators.StakeSimple.OnChainHelpers 
import  Validators.StakeSimple.Helpers     

{- | Get the value from a ChainIndexTxOut. -}
getValueFromChainIndexTxOut :: ChainIndexTxOut -> Value
getValueFromChainIndexTxOut scriptChainIndexTxOut = scriptChainIndexTxOut ^. ciTxOutValue

{- | Try to get the generic Datum from a ChainIndexTxOut. -}
getDatumFromChainIndexTxOut :: ChainIndexTxOut ->  Maybe ValidatorDatum
getDatumFromChainIndexTxOut scriptChainIndexTxOut = do
    -- logInfo @HASKELL.String $ printf "getDatumFromChainIndexTxOut de: %s " (HASKELL.show $ _ciTxOutDatum o)
    case _ciTxOutDatum scriptChainIndexTxOut of
        Left _          -> do
            -- logInfo @HASKELL.String $ printf "Left " 
            Nothing
        Right datum -> do
            let 
                validatorDatum = PlutusTx.fromBuiltinData (getDatum datum) :: Maybe ValidatorDatum
            case validatorDatum of
                Nothing ->  do
                    -- logInfo @HASKELL.String $ printf "Nothing "
                    Nothing    
                Just (PoolState dPoolState) ->  do
                    -- logInfo @HASKELL.String $ printf "Encontrado Datumm Master: %s" (HASKELL.show dPoolState)
                    Just (PoolState dPoolState)   
                Just (UserState validatorUserState) ->  do
                    -- logInfo @HASKELL.String $ printf "Encontrado Datumm User: %s" (HASKELL.show validatorUserState)
                    Just (UserState validatorUserState)   


{- | Get the list of PoolState Datums from a list of Utxos -}
getPoolStateListFromUtxoList :: [(TxOutRef, ChainIndexTxOut)] -> [PoolStateTypo]
getPoolStateListFromUtxoList utxosWithPoolState  = do
    [ getPoolStateFromUtxo (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- utxosWithPoolState, datumIsPoolState  (getDatumFromChainIndexTxOut  scriptChainIndexTxOut) ]

{- | Get PoolState Datum from a Utxo -}
getPoolStateFromUtxo :: (TxOutRef, ChainIndexTxOut) -> PoolStateTypo
getPoolStateFromUtxo utxoWithPoolState  = do
    let 
        scriptChainIndexTxOut = snd utxoWithPoolState
    fromJust (getPoolStateFromDatum (getDatumFromChainIndexTxOut  scriptChainIndexTxOut))
    
{- | Get the list of UserState Datums from a list of Utxos -}
getUserStateListFromUtxoList :: [(TxOutRef, ChainIndexTxOut)] -> [UserStateTypo]
getUserStateListFromUtxoList utxosWithUserState  = do
    [  getUserStateFromUtxo (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- utxosWithUserState, datumIsUserState  (getDatumFromChainIndexTxOut  scriptChainIndexTxOut) ]

{- | Get UserState Datum from a Utxos -}
getUserStateFromUtxo :: (TxOutRef, ChainIndexTxOut) -> UserStateTypo
getUserStateFromUtxo utxoWithUserState  = do
    let 
        scriptChainIndexTxOut = snd utxoWithUserState
    fromJust (getUserStateFromDatum (getDatumFromChainIndexTxOut  scriptChainIndexTxOut))


{- | Creates a new PoolState Datum using a list of Utxo whit PoolState Datums and adding the new fund to the specific master masterFunder. -}
mkPoolStateWithNewFundFromUtxoList :: [(TxOutRef, ChainIndexTxOut)]  ->  PoolNFT -> Master -> Fund -> ValidatorDatum
mkPoolStateWithNewFundFromUtxoList utxosWithPoolState poolNFT master fund  = do
    let 
        poolStateDatums = getPoolStateListFromUtxoList utxosWithPoolState
    
    mkPoolStateWithNewFundFromPoolStateList poolStateDatums poolNFT master fund 

{- | Creates a new PoolState Datum using a list of Utxo with PoolState Datums and adding the new user NFT. -}
mkPoolStateWithNewUserInvestFromUtxoList :: [(TxOutRef, ChainIndexTxOut)]  -> PoolNFT  ->  UserNFT  -> ValidatorDatum
mkPoolStateWithNewUserInvestFromUtxoList utxosWithPoolState poolNFT userNFT   = do
    let 
        poolStateDatums = getPoolStateListFromUtxoList utxosWithPoolState
    
    mkPoolStateWithNewUserInvestFromPoolStateList poolStateDatums poolNFT userNFT 
   
{- | Get the list of utxos with valid PoolState datum in the script address. -}
getUtxoListWithValidPoolStateInScript :: Ledger.Address -> Contract w s Text [(TxOutRef, ChainIndexTxOut)]
getUtxoListWithValidPoolStateInScript addressValidator  = do
    utxos <- utxosAt addressValidator
    logInfo @HASKELL.String $ printf "utxosAt: %s" (HASKELL.show utxos)
    let 
        utxosListValid = [ (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- Map.toList utxos, datumIsPoolState (getDatumFromChainIndexTxOut  scriptChainIndexTxOut) ]
        -- TODO: revisar lista de utxo, comprobar datum correcto, tienen que contener los mismos invertsores que el pool, 
        -- el mismo NFT y el NFT tiene que estar en el pool

        utxosRef = [ txOutRef | (txOutRef, scriptChainIndexTxOut) <- utxosListValid ]

    logInfo @HASKELL.String $ printf "Utxos List with Valid PoolState: %s" (HASKELL.show utxosRef)
    return utxosListValid

{- | Get the list of utxos with valid UserState datum in the script address. -}
getUtxoListWithValidUserStateInScript :: Ledger.Address -> UserNFT -> Contract w s Text [(TxOutRef, ChainIndexTxOut)]
getUtxoListWithValidUserStateInScript addressValidator userNFT  = do
    utxos <- utxosAt addressValidator
    logInfo @HASKELL.String $ printf "utxosAt: %s" (HASKELL.show utxos)
    let 
        utxosListValidWithDatum = [ (txOutRef, scriptChainIndexTxOut, fromJust (getUserStateFromDatum (getDatumFromChainIndexTxOut  scriptChainIndexTxOut))) | (txOutRef, scriptChainIndexTxOut) <- Map.toList utxos, datumIsUserState (getDatumFromChainIndexTxOut  scriptChainIndexTxOut) ]
            

        utxosListValid = [ (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut, dUserState) <- utxosListValidWithDatum, usUserNFT dUserState == userNFT]
        
        -- TODO: revisar lista de utxo, comprobar datum correcto, tienen que contener usuario registrado en el pool

        utxosRef = [ txOutRef | (txOutRef, scriptChainIndexTxOut) <- utxosListValid ]

    logInfo @HASKELL.String $ printf "Utxos List Valid UserState: %s" (HASKELL.show utxosRef)
    return utxosListValid


{- | Get the utxos in the address for use in the emulator trace. -}
getUtxoListInEmulator :: Ledger.Address -> Trace.EmulatorTrace [(TxOutRef, TxOut)]
getUtxoListInEmulator addr = do
    state <- Trace.chainState
    let utxoIndex = getIndex $ state ^. Trace.index 
        utxos     =  [(oref, o) | (oref, o) <- Map.toList utxoIndex, txOutAddress o == addr]
    HASKELL.pure utxos   







-- getDatumContract :: [(TxOutRef, ChainIndexTxOut)]  -> Contract w s Text ()
-- getDatumContract [(txOutRef, scriptChainIndexTxOut)] = do
--     logInfo @String $ printf "GetDatum %s" (HASKELL.show txOutRef)
--     let 
--         dat = getDatumFromChainIndexTxOut scriptChainIndexTxOut
--     logInfo @String $ printf "Datum %s" (HASKELL.show dat)
--     return ()
-- getDatumContract [] = return ()
-- getDatumContract ((txOutRef, scriptChainIndexTxOut):xs) = do
--     getDatumContract [(txOutRef, scriptChainIndexTxOut)]
--     getDatumContract xs



-- checkUtxoMaster  ::  ChainIndexTxOut -> Master -> Bool
-- checkUtxoMaster scriptChainIndexTxOut  master  = 
--     case getDatumFromChainIndexTxOut scriptChainIndexTxOut of
--         Nothing -> False
--         Just (PoolState dPoolState) -> PlutusTx.Prelude.any (master==) [mfMaster masterFunder | masterFunder <- psMasterFunders dPoolState]
--         _ -> False


-- checkUtxoUserWithDeadline  :: ChainIndexTxOut-> User -> Deadline-> Bool
-- checkUtxoUserWithDeadline scriptChainIndexTxOut user deadline = 
--     case getDatumFromChainIndexTxOut scriptChainIndexTxOut of
--         Nothing -> False
--         Just (UserState validatorUserState) -> usUser validatorUserState == user && deadline >= usDeadline validatorUserState  
--         _ -> False


-- findUtxosMaster :: [(TxOutRef, ChainIndexTxOut)]  -> Master  ->  [(TxOutRef, ChainIndexTxOut)]
-- findUtxosMaster [] _ = []  
-- findUtxosMaster [(txOutRef, scriptChainIndexTxOut)]  master  
--     | checkUtxoMaster scriptChainIndexTxOut master = [(txOutRef, scriptChainIndexTxOut)]
--     | otherwise = []
-- findUtxosMaster ((txOutRef, scriptChainIndexTxOut):xs) master  
--     | checkUtxoMaster scriptChainIndexTxOut master = (txOutRef, scriptChainIndexTxOut):findUtxosMaster xs master
--     | otherwise = findUtxosMaster xs master


-- findUtxosUserWithDeadline :: [(TxOutRef, ChainIndexTxOut)]  -> User  -> Deadline ->  [(TxOutRef, ChainIndexTxOut)]
-- findUtxosUserWithDeadline [] _ _ = []  
-- findUtxosUserWithDeadline [(txOutRef, scriptChainIndexTxOut)]  user  deadline
--     | checkUtxoUserWithDeadline scriptChainIndexTxOut user deadline= [(txOutRef, scriptChainIndexTxOut)]
--     | otherwise = []
-- findUtxosUserWithDeadline ((txOutRef, scriptChainIndexTxOut):xs) user  deadline
--     | checkUtxoUserWithDeadline scriptChainIndexTxOut  user deadline = (txOutRef, scriptChainIndexTxOut):findUtxosUserWithDeadline xs user deadline
--     | otherwise = findUtxosUserWithDeadline xs user deadline


-- findUtxosFromMasters :: Ledger.Address -> Master -> Contract w s Text [(TxOutRef, ChainIndexTxOut)]
-- findUtxosFromMasters addressValidator master = do
--     utxos <- utxosAt addressValidator
--     logInfo @HASKELL.String $ printf "utxosAt: %s" (HASKELL.show utxos)
--     let 
--         xs = [ (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- Map.toList utxos ]
--         utxosFromMaster = findUtxosMaster xs master 
--         utxosRef = [ txOutRef | (txOutRef, scriptChainIndexTxOut) <- utxosFromMaster ]
--     getDatumContract $ Map.toList utxos
--     logInfo @HASKELL.String $ printf "utxosFromMaster: %s" (HASKELL.show utxosRef)
--     return utxosFromMaster



-- findUtxosFromUserWithDeadline :: Ledger.Address -> User  -> Deadline -> Contract w s Text [(TxOutRef, ChainIndexTxOut)]
-- findUtxosFromUserWithDeadline addressValidator user deadline = do
--     utxos <- utxosAt addressValidator
--     logInfo @HASKELL.String $ printf "utxosAt: %s" (HASKELL.show utxos)
--     let 
--         xs = [ (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- Map.toList utxos ]
--         utxosFromUserWithDeadline = findUtxosUserWithDeadline xs user deadline
--     getDatumContract $ Map.toList utxos
--     -- logInfo @HASKELL.String $ printf "utxosFromMaster: %s" (HASKELL.show utxosFromMaster)
--     return utxosFromUserWithDeadline



-- getUtxoAndChainIndexFromUtxo :: Ledger.Address -> TxOutRef -> Contract w s Text (TxOutRef, ChainIndexTxOut)
-- getUtxoAndChainIndexFromUtxo addrs get_oref = do
--     utxos <- utxosAt addrs
--     let 
--         xs = [ (oref, o) | (oref, o) <- Map.toList utxos , get_oref == oref]
--     case xs of
--         [x] ->  return x





-- manager :: PubKeyHash
-- manager = let (Just pkh) = PlutusTx.fromBuiltinData (mkB "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e") in pkh 
-- pk = PaymentPubKeyHash manager
-- dm = mkPoolState pk
-- datadm = PlutusTx.toData dm
