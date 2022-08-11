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

module Validators.MarketNFT.OffChainHelpers where

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
import  Validators.MarketNFT.Typos 
--import  Validators.MarketNFT.OnChainHelpers 
import  Validators.MarketNFT.Helpers     

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
    fromJust (getPoolStateFromMaybeDatum (getDatumFromChainIndexTxOut  scriptChainIndexTxOut))
    
{- | Get the list of UserState Datums from a list of Utxos -}
getUserStateListFromUtxoList :: [(TxOutRef, ChainIndexTxOut)] -> [UserStateTypo]
getUserStateListFromUtxoList utxosWithUserState  = do
    [  getUserStateFromUtxo (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- utxosWithUserState, datumIsUserState  (getDatumFromChainIndexTxOut  scriptChainIndexTxOut) ]

{- | Get UserState Datum from a Utxos -}
getUserStateFromUtxo :: (TxOutRef, ChainIndexTxOut) -> UserStateTypo
getUserStateFromUtxo utxoWithUserState  = do
    let 
        scriptChainIndexTxOut = snd utxoWithUserState
    fromJust (getUserStateFromMaybeDatum (getDatumFromChainIndexTxOut  scriptChainIndexTxOut))


-- mkPoolStateFromUtxoList:: [(TxOutRef, ChainIndexTxOut)]  ->  PoolNFT  -> ValidatorDatum
-- mkPoolStateFromUtxoList utxosWithPoolState poolNFT   = do
--     let 
--         poolStateDatums = getPoolStateListFromUtxoList utxosWithPoolState

--     mkPoolStateFromPoolStateList poolStateDatums poolNFT



{- | Creates a new PoolState Datum using a Utxo whit PoolState Datum and adding the new fund to the total count of funds -}
mkPoolStateWithNewCountFundsFromUtxo :: (TxOutRef, ChainIndexTxOut)  ->  PoolNFT -> ValidatorDatum
mkPoolStateWithNewCountFundsFromUtxo utxoWithPoolState poolNFT  = do
    let 
        poolStateDatum = getPoolStateFromUtxo utxoWithPoolState
    
    mkPoolStateWithNewCountFundsFromPoolState poolStateDatum poolNFT 

{- | Creates a new PoolState Datum using a list of Utxo whit PoolState Datums and adding the new fund to the specific master masterFunder. -}
mkPoolStateWithNewFundFromUtxoList :: [(TxOutRef, ChainIndexTxOut)]  ->  PoolNFT -> Master -> Fund -> Integer ->  ValidatorDatum
mkPoolStateWithNewFundFromUtxoList utxosWithPoolState poolNFT master fund countTotalUtxoWithPoolState  = do
    let 
        poolStateDatums = getPoolStateListFromUtxoList utxosWithPoolState
    
    mkPoolStateWithNewFundFromPoolStateList poolStateDatums poolNFT master fund countTotalUtxoWithPoolState

-- {- | Creates a new PoolState Datum using a list of Utxo with PoolState Datums and adding the new user NFT. -}
mkPoolStateWithNewUserInvestFromUtxo :: (TxOutRef, ChainIndexTxOut)  -> PoolNFT  ->  UserNFT  -> ValidatorDatum
mkPoolStateWithNewUserInvestFromUtxo utxosWithPoolState poolNFT userNFT   = do
    let 
        poolStateDatum = getPoolStateFromUtxo utxosWithPoolState
    
    mkPoolStateWithNewUserInvestFromPoolState poolStateDatum poolNFT userNFT 
   

-- {- | Creates a new PoolState Datum using a list of Utxo with PoolState Datums and adding the new user NFT. -}
-- mkPoolStateWithNewUserInvestFromUtxoList :: [(TxOutRef, ChainIndexTxOut)]  -> PoolNFT  ->  UserNFT  -> ValidatorDatum
-- mkPoolStateWithNewUserInvestFromUtxoList utxosWithPoolState poolNFT userNFT   = do
--     let 
--         poolStateDatums = getPoolStateListFromUtxoList utxosWithPoolState
    
--     mkPoolStateWithNewUserInvestFromPoolStateList poolStateDatums poolNFT userNFT 
   
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
        utxosListValidWithDatum = [ (txOutRef, scriptChainIndexTxOut, fromJust (getUserStateFromMaybeDatum (getDatumFromChainIndexTxOut  scriptChainIndexTxOut))) | (txOutRef, scriptChainIndexTxOut) <- Map.toList utxos, datumIsUserState (getDatumFromChainIndexTxOut  scriptChainIndexTxOut) ]
            

        utxosListValid = [ (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut, dUserState) <- utxosListValidWithDatum, usUserNFT dUserState == userNFT]
        
        -- TODO: revisar lista de utxo, comprobar datum correcto, tienen que contener usuario registrado en el pool

        utxosRef = [ txOutRef | (txOutRef, scriptChainIndexTxOut) <- utxosListValid ]

    logInfo @HASKELL.String $ printf "Utxos List Valid UserState: %s" (HASKELL.show utxosRef)
    return utxosListValid

getUtxoWithNFTInList :: [(TxOutRef, ChainIndexTxOut)] -> PoolNFT -> Maybe (TxOutRef, ChainIndexTxOut)
getUtxoWithNFTInList utxos poolNFT  = do
    let
        utxoWithNFT = [utxo  | utxo <- utxos , assetClassValueOf  (getValueFromChainIndexTxOut $ snd utxo) poolNFT > 0 ]

    case utxoWithNFT of
        [x] -> Just x
        _ -> Nothing



{- | Get the utxos in the address for use in the emulator trace. -}
getUtxoListInEmulator :: Ledger.Address -> Trace.EmulatorTrace [(TxOutRef, TxOut)]
getUtxoListInEmulator addr = do
    state <- Trace.chainState
    let utxoIndex = getIndex $ state ^. Trace.index 
        utxos     =  [(oref, o) | (oref, o) <- Map.toList utxoIndex, txOutAddress o == addr]
    HASKELL.pure utxos   


{-# INLINABLE removeUxtoByTxOutRef #-}
removeUxtoByTxOutRef  ::  TxOutRef -> [(TxOutRef, ChainIndexTxOut)] -> [(TxOutRef, ChainIndexTxOut)]
removeUxtoByTxOutRef _ []                 = []
removeUxtoByTxOutRef x (y:ys) 
            | x == fst y      = removeUxtoByTxOutRef x ys
            | otherwise       =  y : removeUxtoByTxOutRef x ys


-- manager :: PubKeyHash
-- manager = let (Just pkh) = PlutusTx.fromBuiltinData (mkB "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e") in pkh 
-- pk = PaymentPubKeyHash manager
-- dm = mkPoolState pk
-- datadm = PlutusTx.toData dm
