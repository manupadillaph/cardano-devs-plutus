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

module Validators.StakeSimpleV1.OffChainHelpers where

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
import qualified Validators.StakeSimpleV1.Typos 
--import qualified Validators.StakeSimpleV1.OnChainHelpers 
import qualified Validators.StakeSimpleV1.Helpers     

{- | Get the value from a ChainIndexTxOut. -}
getValueFromChainIndexTxOut :: LedgerTx.ChainIndexTxOut -> LedgerValueV1.Value
getValueFromChainIndexTxOut scriptChainIndexTxOut = scriptChainIndexTxOut Lens.^. LedgerApiV1.txOutValue 

{- | Try to get the generic Datum from a ChainIndexTxOut. -}
getDatumFromChainIndexTxOut :: LedgerTx.ChainIndexTxOut -> Maybe ValidatorDatum
getDatumFromChainIndexTxOut scriptChainIndexTxOut = do
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "getDatumFromChainIndexTxOut de: %s " (P.show $ _ciTxOutDatum o)
    case _ciTxOutDatum scriptChainIndexTxOut of
        Left _          -> do
            -- PlutusContract.logInfo @P.String $ TextPrintf.printf "Left " 
            Nothing
        Right datum -> do
            let 
                validatorDatum = PlutusTx.fromBuiltinData (LedgerScriptsV1.getDatum datum) :: Maybe ValidatorDatum
            case validatorDatum of
                Nothing -> do
                    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "Nothing "
                    Nothing    
                Just (PoolState dPoolState) -> do
                    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "Encontrado Datumm Master: %s" (P.show dPoolState)
                    Just (PoolState dPoolState)   
                Just (UserState validatorUserState) -> do
                    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "Encontrado Datumm User: %s" (P.show validatorUserState)
                    Just (UserState validatorUserState)   


{- | Get the list of PoolState Datums from a list of Utxos -}
getPoolStateListFromUtxoList :: [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)] -> [PoolStateTypo]
getPoolStateListFromUtxoList utxosWithPoolState  = do
    [ getPoolStateFromUtxo (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- utxosWithPoolState, Helpers.datumIsPoolState  (getDatumFromChainIndexTxOut  scriptChainIndexTxOut) ]

{- | Get PoolState Datum from a Utxo -}
getPoolStateFromUtxo :: (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut) -> PoolStateTypo
getPoolStateFromUtxo utxoWithPoolState  = do
    let 
        scriptChainIndexTxOut = snd utxoWithPoolState
    Helpers.fromJust (Helpers.getPoolStateFromDatum (getDatumFromChainIndexTxOut  scriptChainIndexTxOut))
    
{- | Get the list of UserState Datums from a list of Utxos -}
getUserStateListFromUtxoList :: [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)] -> [UserStateTypo]
getUserStateListFromUtxoList utxosWithUserState  = do
    [  getUserStateFromUtxo (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- utxosWithUserState, Helpers.datumIsUserState  (getDatumFromChainIndexTxOut  scriptChainIndexTxOut) ]

{- | Get UserState Datum from a Utxos -}
getUserStateFromUtxo :: (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut) -> UserStateTypo
getUserStateFromUtxo utxoWithUserState  = do
    let 
        scriptChainIndexTxOut = snd utxoWithUserState
    Helpers.fromJust (getUserStateFromDatum (getDatumFromChainIndexTxOut  scriptChainIndexTxOut))


{- | Creates a new PoolState Datum using a list of Utxo whit PoolState Datums and adding the new fund to the specific master masterFunder. -}
T.mkPoolStateWithNewFundFromUtxoList :: [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]  -> PoolNFT -> Master -> Fund -> ValidatorDatum
T.mkPoolStateWithNewFundFromUtxoList utxosWithPoolState poolNFT master fund  = do
    let 
        poolStateDatums = getPoolStateListFromUtxoList utxosWithPoolState
    
    T.Helpers.mkPoolStateWithNewFundFromPoolStateList poolStateDatums poolNFT master fund 

{- | Creates a new PoolState Datum using a list of Utxo with PoolState Datums and adding the new user NFT. -}
T.mkPoolStateWithNewUserInvestFromUtxoList :: [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]  -> PoolNFT  -> UserNFT  -> ValidatorDatum
T.mkPoolStateWithNewUserInvestFromUtxoList utxosWithPoolState poolNFT userNFT   = do
    let 
        poolStateDatums = getPoolStateListFromUtxoList utxosWithPoolState
    
    T.mkPoolStateWithNewUserInvestFromPoolStateList poolStateDatums poolNFT userNFT 
   
{- | Get the list of utxos with valid PoolState datum in the script address. -}
getUtxoListWithValidPoolStateInScript :: LedgerAddressV1.Address -> PlutusContract.Contract w s DataText.Text [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]
getUtxoListWithValidPoolStateInScript addressValidator = do
    utxos <- PlutusContract.utxosAt OnChain.OnChain.addressValidator
    PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosAt: %s" (P.show utxos)
    let 
        utxosListValid = [ (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- DataMap.toList utxos, Helpers.datumIsPoolState (getDatumFromChainIndexTxOut  scriptChainIndexTxOut) ]
        -- TODO: revisar lista de utxo, comprobar datum correcto, tienen que contener los mismos invertsores que el pool, 
        -- el mismo NFT y el NFT tiene que estar en el pool

        utxosRef = [ txOutRef | (txOutRef, scriptChainIndexTxOut) <- utxosListValid ]

    PlutusContract.logInfo @P.String $ TextPrintf.printf "Utxos List with Valid PoolState: %s" (P.show utxosRef)
    return utxosListValid

{- | Get the list of utxos with valid UserState datum in the script address. -}
getUtxoListWithValidUserStateInScript :: LedgerAddressV1.Address -> UserNFT -> PlutusContract.Contract w s DataText.Text [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]
getUtxoListWithValidUserStateInScript addressValidator userNFT  = do
    utxos <- PlutusContract.utxosAt OnChain.OnChain.addressValidator
    PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosAt: %s" (P.show utxos)
    let 
        utxosListValidWithDatum = [ (txOutRef, scriptChainIndexTxOut, Helpers.fromJust (getUserStateFromDatum (getDatumFromChainIndexTxOut  scriptChainIndexTxOut))) | (txOutRef, scriptChainIndexTxOut) <- DataMap.toList utxos, Helpers.datumIsUserState (getDatumFromChainIndexTxOut  scriptChainIndexTxOut) ]
            

        utxosListValid = [ (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut, dUserState) <- utxosListValidWithDatum, T.usUserNFT dUserState == userNFT]
        
        -- TODO: revisar lista de utxo, comprobar datum correcto, tienen que contener usuario registrado en el pool

        utxosRef = [ txOutRef | (txOutRef, scriptChainIndexTxOut) <- utxosListValid ]

    PlutusContract.logInfo @P.String $ TextPrintf.printf "Utxos List Valid UserState: %s" (P.show utxosRef)
    return utxosListValid


{- | Get the utxos in the address for use in the emulator trace. -}
getUtxoListInEmulator :: LedgerAddressV1.Address -> TraceEmulator.EmulatorTrace [(LedgerApiV1.TxOutRef, LedgerApiV1.TxOut)]
getUtxoListInEmulator addr = do
    state <- TraceEmulator.chainState
    let utxoIndex = LedgerIndex.getIndex $ state Lens.^. TraceEmulator.index 
        utxos     =  [(oref, o) | (oref, o) <- DataMap.toList utxoIndex, LedgerApiV1.txOutAddress o == addr]
    P.pure utxos   







-- getDatumContract :: [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]  -> PlutusContract.Contract w s DataText.Text ()
-- getDatumContract [(txOutRef, scriptChainIndexTxOut)] = do
--     logInfo @String $ TextPrintf.printf "GetDatum %s" (P.show txOutRef)
--     let 
--         dat = getDatumFromChainIndexTxOut scriptChainIndexTxOut
--     logInfo @String $ TextPrintf.printf "Datum %s" (P.show dat)
--     return ()
-- getDatumContract [] = return ()
-- getDatumContract ((txOutRef, scriptChainIndexTxOut):xs) = do
--     getDatumContract [(txOutRef, scriptChainIndexTxOut)]
--     getDatumContract xs



-- checkUtxoMaster  ::  ChainIndexTxOut -> Master -> Bool
-- checkUtxoMaster scriptChainIndexTxOut  master  = 
--     case getDatumFromChainIndexTxOut scriptChainIndexTxOut of
--         Nothing -> False
--         Just (PoolState dPoolState) -> any (master==) [T.mfMaster masterFunder | masterFunder <- T.psMasterFunders dPoolState]
--         _ -> False


-- checkUtxoUserWithDeadline  :: LedgerTx.ChainIndexTxOut-> User -> Deadline-> Bool
-- checkUtxoUserWithDeadline scriptChainIndexTxOut user deadline = 
--     case getDatumFromChainIndexTxOut scriptChainIndexTxOut of
--         Nothing -> False
--         Just (UserState validatorUserState) -> T.usUser validatorUserState == user && deadline >= T.usDeadline validatorUserState  
--         _ -> False


-- findUtxosMaster :: [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]  -> Master  -> [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]
-- findUtxosMaster [] _ = []  
-- findUtxosMaster [(txOutRef, scriptChainIndexTxOut)]  master  
--     | checkUtxoMaster scriptChainIndexTxOut master = [(txOutRef, scriptChainIndexTxOut)]
--     | otherwise = []
-- findUtxosMaster ((txOutRef, scriptChainIndexTxOut):xs) master  
--     | checkUtxoMaster scriptChainIndexTxOut master = (txOutRef, scriptChainIndexTxOut):findUtxosMaster xs master
--     | otherwise = findUtxosMaster xs master


-- findUtxosUserWithDeadline :: [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]  -> User  -> Deadline -> [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]
-- findUtxosUserWithDeadline [] _ _ = []  
-- findUtxosUserWithDeadline [(txOutRef, scriptChainIndexTxOut)]  user  deadline
--     | checkUtxoUserWithDeadline scriptChainIndexTxOut user deadline= [(txOutRef, scriptChainIndexTxOut)]
--     | otherwise = []
-- findUtxosUserWithDeadline ((txOutRef, scriptChainIndexTxOut):xs) user  deadline
--     | checkUtxoUserWithDeadline scriptChainIndexTxOut  user deadline = (txOutRef, scriptChainIndexTxOut):findUtxosUserWithDeadline xs user deadline
--     | otherwise = findUtxosUserWithDeadline xs user deadline


-- findUtxosFromMasters :: LedgerAddressV1.Address -> Master -> PlutusContract.Contract w s DataText.Text [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]
-- findUtxosFromMasters addressValidator master = do
--     utxos <- PlutusContract.utxosAt OnChain.OnChain.addressValidator
--     PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosAt: %s" (P.show utxos)
--     let 
--         xs = [ (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- DataMap.toList utxos ]
--         utxosFromMaster = findUtxosMaster xs master 
--         utxosRef = [ txOutRef | (txOutRef, scriptChainIndexTxOut) <- utxosFromMaster ]
--     getDatumContract $ DataMap.toList utxos
--     PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosFromMaster: %s" (P.show utxosRef)
--     return utxosFromMaster



-- findUtxosFromUserWithDeadline :: LedgerAddressV1.Address -> User  -> Deadline -> PlutusContract.Contract w s DataText.Text [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]
-- findUtxosFromUserWithDeadline addressValidator user deadline = do
--     utxos <- PlutusContract.utxosAt OnChain.OnChain.addressValidator
--     PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosAt: %s" (P.show utxos)
--     let 
--         xs = [ (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- DataMap.toList utxos ]
--         utxosFromUserWithDeadline = findUtxosUserWithDeadline xs user deadline
--     getDatumContract $ DataMap.toList utxos
--     -- PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosFromMaster: %s" (P.show utxosFromMaster)
--     return utxosFromUserWithDeadline



-- getUtxoAndChainIndexFromUtxo :: LedgerAddressV1.Address -> LedgerApiV1.TxOutRef -> PlutusContract.Contract w s DataText.Text (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)
-- getUtxoAndChainIndexFromUtxo addrs get_oref = do
--     utxos <- PlutusContract.utxosAt addrs
--     let 
--         xs = [ (oref, o) | (oref, o) <- DataMap.toList utxos , get_oref == oref]
--     case xs of
--         [x] -> return x





-- manager :: PubKeyHash
-- manager = let (Just pkh) = PlutusTx.fromBuiltinData (mkB "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e") in pkh 
-- pk = Ledger.PaymentPubKeyHash manager
-- dm = T.mkPoolState pk
-- datadm = PlutusTx.toData dm
