
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

import          Control.Lens

import PlutusTx.Builtins

import qualified Data.Map as Map
import Ledger 
import Ledger.Index
import qualified Plutus.Trace.Emulator  as Trace
import qualified Data.List


import qualified Data.ByteString.Char8 as C


--Import Internos
import  Validators.StakeSimple.Typos 

--Validators Helper Functions for On and OffChain code

{-# INLINABLE clearString #-}
clearString :: P.String -> BuiltinByteString
clearString = toBuiltin . C.pack

-- | Function to return the Just value from a Maybe
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
        -- logInfo @P.String $ printf "Encontrado Datumm PoolState: %s" (P.show dPoolState)
        Just dPoolState  
    _ -> Nothing

{- | Try to get the UserState Datum from a generic Datum. -}
{-# INLINABLE getUserStateFromDatum #-}
getUserStateFromDatum ::  Maybe ValidatorDatum -> Maybe UserStateTypo
getUserStateFromDatum datum = case datum of
    Just (UserState dUserState) ->  do
        -- logInfo @P.String $ printf "Encontrado Datumm UserState: %s" (P.show dUserState)
        Just dUserState  
    _ -> Nothing


{-# INLINABLE datumIsPoolState #-}
datumIsPoolState :: Maybe ValidatorDatum -> Bool
datumIsPoolState (Just (PoolState _)) = True
datumIsPoolState _             = False

{-# INLINABLE datumIsUserState #-}
datumIsUserState :: Maybe ValidatorDatum -> Bool
datumIsUserState (Just (UserState _)) = True
datumIsUserState _             = False


--mkPoolStateWithNewFund :: [(TxOutRef, ChainIndexTxOut)] -> NFT -> Master -> Fund -> ValidatorDatum
{- | Creates a new PoolState Datum using all the old Datums, adding the new fund to the specific master masterFunder. -}
{-# INLINABLE mkPoolStateWithNewFundFromPoolStateList #-}
mkPoolStateWithNewFundFromPoolStateList :: [PoolStateTypo] -> PoolNFT -> Master -> Fund -> ValidatorDatum
mkPoolStateWithNewFundFromPoolStateList poolStateDatums poolNFT master fund  = do
    let 
        userNFTs = PlutusTx.Prelude.concat [ psUsersNFT datum | datum <- poolStateDatums ]
        masterFunders = PlutusTx.Prelude.concat [ psMasterFunders datum | datum <- poolStateDatums ]
        masterFunder_others = [ masterFunder | masterFunder <- masterFunders ,  mfMaster masterFunder /= master]
        --masterFunderOld = fromJust (find (> 10) masterFunders)
        --masterFunderOld = fromJust (PlutusTx.Prelude.find (\masterFunder -> mfMaster masterFunder == master) masterFunders)
        masterFunderOld =  PlutusTx.Prelude.find (\masterFunder -> mfMaster masterFunder == master) masterFunders

        masterFunderNew = case masterFunderOld of
            Nothing -> 
                -- traceError "Can't Find Master Funder In Datums"
                -- TODO: deberia dejar el error, pero como quiero ver como funciona el OnChain dejo que cree un nuevo PoolState Datum con un master que no existe
                mkMasterFunder master fund
            Just MasterFunder{..}  -> 
                mkMasterFunder master (mfFund  + fund)

    PoolState $ mkPoolStateTypo poolNFT (masterFunderNew:masterFunder_others) userNFTs


{-# INLINABLE mkPoolStateWithNewUserInvestFromPoolStateList #-}
mkPoolStateWithNewUserInvestFromPoolStateList :: [PoolStateTypo] ->  PoolNFT  -> UserNFT ->  ValidatorDatum
mkPoolStateWithNewUserInvestFromPoolStateList poolStateDatums poolNFT userNFT  = do
    let 
        userNFTs = PlutusTx.Prelude.concat [ psUsersNFT datum | datum <- poolStateDatums ]
        masterFunders = PlutusTx.Prelude.concat [ psMasterFunders datum | datum <- poolStateDatums ]

    PoolState $ mkPoolStateTypo poolNFT masterFunders (userNFT:userNFTs)


-- Definition of Symbol for the NFT

{-# INLINABLE curSymbol #-}
curSymbol :: Scripts.MintingPolicy  -> CurrencySymbol
curSymbol  = scriptCurrencySymbol 





