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

module Validators.StakeSimpleV1.TestWithPABSimulator
    where

-- import           Control.Monad        hiding (fmap)
-- import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
-- import           Data.List.NonEmpty   (NonEmpty (..))
-- import           Data.Map             as Map
-- import           Data.Text            (pack, Text)
-- import           Data.String  
-- import qualified GHC.Generics                        as GHCGenerics (Generic)
-- import           Ledger               hiding (singleton)
-- import qualified Ledger.Constraints   as Constraints
-- import qualified Ledger.Typed.Scripts as Scripts
-- import           LedgerValueV1.Value         as Value
-- import           Ledger.Ada           as Ada
-- import           Playground.Contract  (IO, ensureKnownCurrencies, printSchemas, stage, printJson)
-- import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
-- import           Playground.Types     (KnownCurrency (..))
-- import           Plutus.Contract
-- import qualified PlutusTx
-- import           PlutusTx.Prelude     hiding (unless)
-- import qualified Prelude              as P 
-- import qualified Schema                              (ToSchema)
-- import qualified Data.OpenApi.Schema         (ToSchema)
-- import           Text.Printf          (printf)
-- import           Data.Typeable

-- import           Plutus.Trace.Emulator  as Emulator
-- import qualified Wallet.Emulator.Wallet              as WalletEmulator
-- --import          Data.Default
-- import           Ledger.TimeSlot 

--Import Nuevos

--import           Control.Monad.Freer.Extras as Extras
--import           Data.Void


import           Control.Monad                       (void)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (Result (..), fromJSON)
import qualified Data.Default                        (def)
import qualified Data.Monoid                         as Monoid
import           LedgerAddressV1.Address                      (Address, Ledger.PaymentPubKeyHash, pubKeyHashAddress)
import           Ledger.CardanoWallet   qualified as CW
import qualified Ledger.TimeSlot                     as TimeSlot
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler(contractHandler))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import qualified Plutus.V1.Ledger.Slot               as Ledger.Slot
import qualified Wallet.Emulator.Wallet              as WalletEmulator              (Wallet, knownWallet)


import           Playground.Contract  (IO)
import qualified Prelude                             as P
import           PlutusTx.Prelude                    hiding (unless)
import           Ledger                              hiding (singleton)
import           Plutus.V1.Ledger.Value
import qualified Plutus.Trace.Emulator  as Trace
import           Data.Map             as Map
import           Control.Lens
import           Plutus.Contract
import           Data.Text            (pack, Text)


import Wallet.TraceEmulator.Chain qualified as Chain
import Wallet.TraceEmulator.Chain (ChainControlEffect, ChainState)

import Control.Concurrent.STM (STM, TMVar, TVar)
import Control.Concurrent.STM qualified as STM
import Ledger (Address, Ledger.Slot, TxId, TxOutRef)

import Data.Time.Clock.POSIX
import Data.Time.Format
import System.Locale
import Data.Time.Clock
import Data.Fixed
import Data.Time.Format.ISO8601

import Control.Monad.Freer.Internal
import Plutus.PAB.Core

import System.Directory
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.String.Utils
import Text.Read

--Import Internos
import          Validators.StakeSimpleV1.OffChain
import          Validators.StakeSimpleV1.Typos
import          Validators.StakeSimpleV1.OffChainHelpers
import          Validators.StakeSimpleV1.OnChainNFT     (mintingNFTPolicy)
import          Validators.StakeSimpleV1.Helpers
import          Validators.StakeSimpleV1.PAB

handlers :: SimulatorEffectHandlers (Builtin PAB.ValidatorContracts)
handlers = Simulator.mkSimulatorHandlers Data.Default.def  P.$ interpret (contractHandler Builtin.handleBuiltin)

getWallet :: Integer -> Wallet
getWallet = WalletEmulator.knownWallet

walletPaymentPubKeyHash :: Integer -> Ledger.PaymentPubKeyHash
walletPaymentPubKeyHash walletNumber = CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber walletNumber)

walletPaymentPubKeyHashAddress ::  Integer -> Address
walletPaymentPubKeyHashAddress walletNumber = pubKeyHashAddress (walletPaymentPubKeyHash walletNumber) P.Nothing

getUtxosListInPABSimulator :: Blockchain -> LedgerAddressV1.Address -> [LedgerApiV1.TxOutRef]
getUtxosListInPABSimulator blockchain addr =  do
    let

        unspentOutputList = unspentOutputs blockchain

        utxos =  [txOutRef    | (txOutRef, txOut)    <- DataMap.toList  unspentOutputList , LedgerApiV1.txOutAddress  txOut == addr]
    utxos


getFormatTime :: LedgerApiV1.POSIXTime -> P.String
getFormatTime posixTime = do
    let
        milisegundosFixedPico :: Pico
        milisegundosFixedPico = MkFixed  (LedgerApiV1.getPOSIXTime posixTime * 1000000000)
        seconds = Data.Time.Clock.secondsToNominalDiffTime milisegundosFixedPico

    formatTime Data.Time.Format.defaultTimeLocale  "%c" $ posixSecondsToUTCTime seconds
    --iso8601Show $ posixSecondsToUTCTime $ seconds



getJSON :: P.String -> IO B.ByteString
getJSON file = B.readFile $ path ++ file

writeJSON :: P.String -> B.ByteString -> P.IO  ()
writeJSON file   = B.writeFile $  path ++ file


-- stringEq :: Eq a => P.String a -> P.String a
-- stringEq a = a


--path2 = "~/source/cardano-falcon-stakepool-devs/cardano-falcon-stakepol-devs-haskell/"
path = ""

mainLoop :: P.Maybe Integer -> P.Maybe PoolParams -> Control.Monad.Freer.Internal.Eff (Plutus.PAB.Core.PABEffects (Builtin PAB.ValidatorContracts) (Simulator.SimulatorState (Builtin PAB.ValidatorContracts))) () -> Control.Monad.Freer.Internal.Eff (Plutus.PAB.Core.PABEffects (Builtin PAB.ValidatorContracts) (Simulator.SimulatorState (Builtin PAB.ValidatorContracts))) ()
mainLoop walletNro pParams shutdown = do

    Simulator.logString @(Builtin PAB.ValidatorContracts) "OPERACIONES:"

    case walletNro of
        P.Nothing ->
            Simulator.logString @(Builtin PAB.ValidatorContracts) "1 - Elegir Wallet"
        P.Just walletNro ->
            Simulator.logString @(Builtin PAB.ValidatorContracts) $ "1 - Elegir Wallet (" ++ P.show walletNro ++ ")"

    Simulator.logString @(Builtin PAB.ValidatorContracts) "21 - Crear Pool Params"

    case pParams of
        P.Nothing ->
            Simulator.logString @(Builtin PAB.ValidatorContracts) "22 - Elegir Pool Params"
        P.Just pParams ->
            Simulator.logString @(Builtin PAB.ValidatorContracts) $ "22 - Elegir Pool Params(" ++ P.show (T.ppPoolNFT pParams ) ++ ")"

    
    Simulator.logString @(Builtin PAB.ValidatorContracts) "31 - Crear Pool"
    Simulator.logString @(Builtin PAB.ValidatorContracts) "32 - Fund Pool"

    Simulator.logString @(Builtin PAB.ValidatorContracts) "41 - Invertir en Pool"
    Simulator.logString @(Builtin PAB.ValidatorContracts) "42 - Claim Rewards de Pool"

    Simulator.logString @(Builtin PAB.ValidatorContracts) "99 - Salir"

    Simulator.logString @(Builtin PAB.ValidatorContracts) "Ingrese una opción:"
    opcion <- liftIO P.getLine

    blockchain <- Simulator.blockchain

    case readMaybe opcion of

        P.Just 1 -> do

            Simulator.logString @(Builtin PAB.ValidatorContracts) "Ingrese una wallet:"

            Simulator.logString @(Builtin PAB.ValidatorContracts) "1 - Master 1"
            Simulator.logString @(Builtin PAB.ValidatorContracts) "2 - Master 2"
            Simulator.logString @(Builtin PAB.ValidatorContracts) "3 - Master 3"
            Simulator.logString @(Builtin PAB.ValidatorContracts) "4 - User 1"
            Simulator.logString @(Builtin PAB.ValidatorContracts) "5 - User 2"
            opcionWallet <- liftIO P.getLine

            mainLoop (readMaybe opcionWallet)  pParams shutdown

        P.Just 21 -> do

            let
                master1 = 1
                master2 = 2

                uTxOutRefAt =  getUtxosListInPABSimulator blockchain (walletPaymentPubKeyHashAddress (Helpers.fromJust walletNro))

                poolNFTTxOutRef = head uTxOutRefAt
                idTxOut = LedgerApiV1.txOutRefId poolNFTTxOutRef
                indexTxOut = LedgerApiV1.txOutRefIdx  poolNFTTxOutRef

                poolNFTTokenName =  LedgerValueV1.TokenName (indexTxOut `consByteString`  LedgerApiV1.getTxId  idTxOut  )
                poolNFTCurrencySymbol = Helpers.curSymbol OnChainNFT.mintingNFTPolicy

                poolNFT = LedgerValueV1.assetClass poolNFTCurrencySymbol  poolNFTTokenName

                deadlinePool  = LedgerTimeSlot.slotToEndPOSIXTime Data.Default.def 500

                pParams = PoolParams
                    {
                        T.ppMasters = [walletPaymentPubKeyHash master1, walletPaymentPubKeyHash master2] ,
                        T.ppInterest = 10 ,
                        T.ppMinumunInvest   = 5_000_000 ,
                        T.ppMinumunCompoundInvest    = 3_000_000 ,
                        T.ppDeadline  = deadlinePool ,
                        T.ppPoolNFT = poolNFT ,
                        T.ppPoolNFTTxOutRef  = poolNFTTxOutRef ,
                        T.ppCurSymbolForMintingNFTPolicy = poolNFTCurrencySymbol,
                        T.ppValidTimeRange = 10_000,
                        T.ppMinimunClaim = 3_000_000
                    }

            Simulator.logString @(Builtin PAB.ValidatorContracts) $ "poolNFTTxOutRef: " ++ P.show poolNFTTxOutRef
            Simulator.logString @(Builtin PAB.ValidatorContracts) $ "poolNFTTokenName: " ++ P.show poolNFTTokenName
            Simulator.logString @(Builtin PAB.ValidatorContracts) $ "poolNFTCurrencySymbol: " ++ P.show poolNFTCurrencySymbol
            Simulator.logString @(Builtin PAB.ValidatorContracts) $ "poolNFT: " ++ P.show poolNFT

            Simulator.logString @(Builtin PAB.ValidatorContracts) "Ingrese un nombre para guardar el Pool:"
            opcionPool <- liftIO P.getLine
            liftIO $ writeJSON ("files/stakeSimple/" ++ opcionPool ++ ".json" ) (encode pParams)

            Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
            void $ liftIO P.getLine

            mainLoop walletNro (P.Just pParams) shutdown

        P.Just 22 -> do

            files <- liftIO $ getDirectoryContents "files/stakeSimple"
            
           -- validFIles <- [file | file  <- files ] 

            Simulator.logString @(Builtin PAB.ValidatorContracts) "Ingrese un Pool:"

            mapM_ ( Simulator.logString @(Builtin PAB.ValidatorContracts)   )  files

            --Simulator.logString @(Builtin PAB.ValidatorContracts) "33" >> 

            opcionPool <- liftIO P.getLine

            jsonFile <- liftIO $ getJSON ("files/stakeSimple/" ++ opcionPool ++ ".json" )

            let
                pParams =  decode jsonFile

            mainLoop walletNro pParams shutdown

        P.Just 31 -> do
            
            case (walletNro,  pParams) of
                (P.Just walletNro, P.Just pParams) -> do
                    let 

                        poolNFT = T.ppPoolNFT pParams

                        (_, poolNFTTokenName) = LedgerValueV1.unAssetClass poolNFT   
                        
                        -- idTxOut = LedgerApiV1.txOutRefId poolNFTTxOutRef
                        -- indexTxOut = LedgerApiV1.txOutRefIdx  poolNFTTxOutRef

                        -- poolNFTTokenName =  LedgerValueV1.TokenName (indexTxOut `consByteString`  LedgerApiV1.getTxId  idTxOut  )

                        -- poolNFTTxOutRef = LedgerApiV1.TxOutRef {
                        --         LedgerApiV1.txOutRefId  = unTokenName poolNFTTokenName ,
                        --         LedgerApiV1.txOutRefIdx  = unTokenName poolNFTTokenName ,
                        --     }

                        poolNFTTxOutRef = T.ppPoolNFTTxOutRef pParams

                    cMasterCreatePool_Master  <- Simulator.activateContract (getWallet  walletNro) (PAB.MasterCreatePool MasterCreatePoolParams{
                                mcpPoolParam = pParams,
                                mcpPoolNFTTokenName = poolNFTTokenName,
                                mcpPoolNFTTxOutRef = poolNFTTxOutRef,
                                mcpFund   = 100_000_000
                            })

                    Simulator.waitUntilFinished  cMasterCreatePool_Master

                    slot <- Simulator.currentSlot >>= liftIO . STM.atomically
                    let posixTime = LedgerTimeSlot.slotToEndPOSIXTime Data.Default.def slot
                    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
                    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
                    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
                    void $ liftIO P.getLine

                    mainLoop (P.Just walletNro) (P.Just pParams) shutdown

                (_, P.Just pParams) -> do

                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Elija Wallet"
                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
                    void $ liftIO P.getLine

                    mainLoop P.Nothing (P.Just pParams) shutdown

                (P.Just walletNro, _) -> do

                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Elija Pool Params"
                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
                    void $ liftIO P.getLine

                    mainLoop (P.Just walletNro) P.Nothing shutdown

                (_, _) -> do

                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Elija Wallet y Pool Params"
                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
                    void $ liftIO P.getLine

                    mainLoop P.Nothing P.Nothing shutdown

        P.Just 32 -> do
            case (walletNro,  pParams) of
                (P.Just walletNro, P.Just pParams) -> do

                    cMasterFundPool_Master  <- Simulator.activateContract (getWallet walletNro) (PAB.MasterFundPool MasterFundPoolParams{
                            mspPoolParam =  pParams,
                            mspFund   = 50_000_000
                        })

                    Simulator.waitUntilFinished  cMasterFundPool_Master

                    slot <- Simulator.currentSlot >>= liftIO . STM.atomically
                    let posixTime = LedgerTimeSlot.slotToEndPOSIXTime Data.Default.def slot
                    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
                    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
                    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
                    void $ liftIO P.getLine
                    mainLoop (P.Just walletNro) (P.Just pParams) shutdown

                (_, P.Just pParams) -> do

                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Elija Wallet"
                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
                    void $ liftIO P.getLine

                    mainLoop P.Nothing (P.Just pParams) shutdown

                (P.Just walletNro, _) -> do

                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Elija Pool Params"
                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
                    void $ liftIO P.getLine

                    mainLoop (P.Just walletNro) P.Nothing shutdown

                (_, _) -> do

                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Elija Wallet y Pool Params"
                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
                    void $ liftIO P.getLine

                    mainLoop P.Nothing P.Nothing shutdown

        P.Just 41 -> do
            
            case (walletNro,  pParams) of
                (P.Just walletNro, P.Just pParams) -> do

                    let
                        uTxOutRefAt =  getUtxosListInPABSimulator blockchain (walletPaymentPubKeyHashAddress walletNro)

                        user1NFTTxOutRef = head uTxOutRefAt
                        user1IdTxOut = LedgerApiV1.txOutRefId user1NFTTxOutRef
                        user1IndexTxOut = LedgerApiV1.txOutRefIdx  user1NFTTxOutRef

                        user1NFTTokenName =  LedgerValueV1.TokenName (user1IndexTxOut `consByteString`  LedgerApiV1.getTxId  user1IdTxOut  )
                        user1NFTCurrencySymbol = Helpers.curSymbol OnChainNFT.mintingNFTPolicy

                        userNFT = LedgerValueV1.assetClass user1NFTCurrencySymbol user1NFTTokenName

                    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "user1NFTTxOutRef: " ++ P.show user1NFTTxOutRef
                    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "user1NFTTokenName: " ++ P.show user1NFTTokenName
                    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "user1NFTCurrencySymbol: " ++ P.show user1NFTCurrencySymbol
                    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "userNFT: " ++ P.show userNFT

                    slot <- Simulator.currentSlot >>= liftIO . STM.atomically

                    let

                        createdAtInvest = LedgerTimeSlot.slotToEndPOSIXTime Data.Default.def slot
                        deadlineInvest = LedgerTimeSlot.slotToEndPOSIXTime Data.Default.def (slot+50)

                    cUserInvest_User  <- Simulator.activateContract (getWallet walletNro) (PAB.UserInvest UserInvestParams{
                            uipPoolParam =  pParams,
                            uiUserNFTTokenName = user1NFTTokenName,
                            uiUserNFTTxOutRef = user1NFTTxOutRef,
                            uipCreatedAt = createdAtInvest,
                            uipDeadline = deadlineInvest,
                            uipInvest   = 6_000_000
                        })

                    Simulator.waitUntilFinished cUserInvest_User

                    slot <- Simulator.currentSlot >>= liftIO . STM.atomically
                    let posixTime = LedgerTimeSlot.slotToEndPOSIXTime Data.Default.def slot
                    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
                    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
                    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
                    void $ liftIO P.getLine

                    mainLoop  (P.Just walletNro) (P.Just pParams) shutdown

                (_, P.Just pParams) -> do

                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Elija Wallet"
                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
                    void $ liftIO P.getLine

                    mainLoop P.Nothing (P.Just pParams) shutdown

                (P.Just walletNro, _) -> do

                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Elija Pool Params"
                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
                    void $ liftIO P.getLine

                    mainLoop (P.Just walletNro) P.Nothing shutdown

                (_, _) -> do

                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Elija Wallet y Pool Params"
                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
                    void $ liftIO P.getLine

                    mainLoop P.Nothing P.Nothing shutdown

        P.Just 42 -> do
            
            case (walletNro,  pParams) of
                (P.Just walletNro, P.Just pParams) -> do

                    let
                        uTxOutRefAt =  getUtxosListInPABSimulator blockchain (walletPaymentPubKeyHashAddress walletNro)

                        user1NFTTxOutRef = head uTxOutRefAt
                        user1IdTxOut = LedgerApiV1.txOutRefId user1NFTTxOutRef
                        user1IndexTxOut = LedgerApiV1.txOutRefIdx  user1NFTTxOutRef

                        user1NFTTokenName =  LedgerValueV1.TokenName (user1IndexTxOut `consByteString`  LedgerApiV1.getTxId  user1IdTxOut  )
                        user1NFTCurrencySymbol = Helpers.curSymbol OnChainNFT.mintingNFTPolicy

                        userNFT = LedgerValueV1.assetClass user1NFTCurrencySymbol user1NFTTokenName

                    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "user1NFTTxOutRef: " ++ P.show user1NFTTxOutRef
                    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "user1NFTTokenName: " ++ P.show user1NFTTokenName
                    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "user1NFTCurrencySymbol: " ++ P.show user1NFTCurrencySymbol
                    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "userNFT: " ++ P.show userNFT

                    slot <- Simulator.currentSlot >>= liftIO . STM.atomically

                    let

                        createdAtInvest = LedgerTimeSlot.slotToEndPOSIXTime Data.Default.def slot
                        deadlineInvest = LedgerTimeSlot.slotToEndPOSIXTime Data.Default.def (slot+50)

                    cUserInvest_User  <- Simulator.activateContract (getWallet walletNro) (PAB.UserInvest UserInvestParams{
                            uipPoolParam =  pParams,
                            uiUserNFTTokenName = user1NFTTokenName,
                            uiUserNFTTxOutRef = user1NFTTxOutRef,
                            uipCreatedAt = createdAtInvest,
                            uipDeadline = deadlineInvest,
                            uipInvest   = 6_000_000
                        })

                    Simulator.waitUntilFinished cUserInvest_User

                    slot <- Simulator.currentSlot >>= liftIO . STM.atomically
                    let posixTime = LedgerTimeSlot.slotToEndPOSIXTime Data.Default.def slot
                    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
                    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
                    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
                    void $ liftIO P.getLine

                    mainLoop  (P.Just walletNro) (P.Just pParams) shutdown

                (_, P.Just pParams) -> do

                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Elija Wallet"
                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
                    void $ liftIO P.getLine

                    mainLoop P.Nothing (P.Just pParams) shutdown

                (P.Just walletNro, _) -> do

                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Elija Pool Params"
                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
                    void $ liftIO P.getLine

                    mainLoop (P.Just walletNro) P.Nothing shutdown

                (_, _) -> do

                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Elija Wallet y Pool Params"
                    Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
                    void $ liftIO P.getLine

                    mainLoop P.Nothing P.Nothing shutdown
        P.Just 99 -> do

            Simulator.logString @(Builtin PAB.ValidatorContracts) "Balances at the end of the simulation"
            --void $ liftIO P.getLine

            balances <- Simulator.currentBalances
            Simulator.logBalances @(Builtin PAB.ValidatorContracts) balances

            slot <- Simulator.currentSlot >>= liftIO . STM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime Data.Default.def slot
            Simulator.logString @(Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            Simulator.logString @(Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            Simulator.logString @(Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

            shutdown

        _ -> mainLoop walletNro pParams shutdown





test2 :: IO ()
test2  = void $ Simulator.runSimulationWith handlers myTrace2

myTrace2  ::  Control.Monad.Freer.Internal.Eff (Plutus.PAB.Core.PABEffects (Builtin PAB.ValidatorContracts) (Simulator.SimulatorState (Builtin PAB.ValidatorContracts)))  ()
myTrace2 = do
    Simulator.logString @(Builtin PAB.ValidatorContracts) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    Simulator.logString @(Builtin PAB.ValidatorContracts) "********* PAB Server is running *********"
    -- Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
    -- void $ liftIO P.getLine

    let
        master1 = 1
        master2 = 2
        user1 = 3

    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "master1 Pkh = " ++ P.show (walletPaymentPubKeyHash master1)
    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "master1 PkhAddress = " ++ P.show (walletPaymentPubKeyHashAddress master1)

    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "master2 Pkh = " ++ P.show (walletPaymentPubKeyHash master2)
    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "master2 PkhAddress = " ++ P.show (walletPaymentPubKeyHashAddress master2)

    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "user1 Pkh = " ++ P.show (walletPaymentPubKeyHash user1)
    Simulator.logString @(Builtin PAB.ValidatorContracts) $ "user1 PkhAddress = " ++ P.show (walletPaymentPubKeyHashAddress user1)


    mainLoop P.Nothing P.Nothing shutdown



    -- blockchain <- Simulator.blockchain

    -- let 

    --     uTxOutRefAtMaster1 =  getUtxosListInPABSimulator blockchain (walletPaymentPubKeyHashAddress master1)

    --     poolNFTTxOutRef = head uTxOutRefAtMaster1
    --     idTxOut = LedgerApiV1.txOutRefId poolNFTTxOutRef
    --     indexTxOut = LedgerApiV1.txOutRefIdx  poolNFTTxOutRef

    --     poolNFTTokenName =  LedgerValueV1.TokenName (indexTxOut `consByteString`  LedgerApiV1.getTxId  idTxOut  )
    --     poolNFTCurrencySymbol = Helpers.curSymbol OnChainNFT.mintingNFTPolicy 

    --     poolNFT = LedgerValueV1.assetClass poolNFTCurrencySymbol  poolNFTTokenName

    -- Simulator.logString @(Builtin PAB.ValidatorContracts) $ "poolNFTTxOutRef: " ++ P.show poolNFTTxOutRef
    -- Simulator.logString @(Builtin PAB.ValidatorContracts) $ "poolNFTTokenName: " ++ P.show poolNFTTokenName
    -- Simulator.logString @(Builtin PAB.ValidatorContracts) $ "poolNFTCurrencySymbol: " ++ P.show poolNFTCurrencySymbol
    -- Simulator.logString @(Builtin PAB.ValidatorContracts) $ "poolNFT: " ++ P.show poolNFT 

    -- let
    --     uTxOutRefAtUser1 =  getUtxosListInPABSimulator blockchain (walletPaymentPubKeyHashAddress user1)

    --     user1NFTTxOutRef = head uTxOutRefAtUser1
    --     user1IdTxOut = LedgerApiV1.txOutRefId user1NFTTxOutRef
    --     user1IndexTxOut = LedgerApiV1.txOutRefIdx  user1NFTTxOutRef

    --     user1NFTTokenName =  LedgerValueV1.TokenName (user1IndexTxOut `consByteString`  LedgerApiV1.getTxId  user1IdTxOut  )
    --     user1NFTCurrencySymbol = Helpers.curSymbol OnChainNFT.mintingNFTPolicy 

    --     userNFT = LedgerValueV1.assetClass user1NFTCurrencySymbol user1NFTTokenName

    -- Simulator.logString @(Builtin PAB.ValidatorContracts) $ "user1NFTTxOutRef: " ++ P.show user1NFTTxOutRef
    -- Simulator.logString @(Builtin PAB.ValidatorContracts) $ "user1NFTTokenName: " ++ P.show user1NFTTokenName
    -- Simulator.logString @(Builtin PAB.ValidatorContracts) $ "user1NFTCurrencySymbol: " ++ P.show user1NFTCurrencySymbol
    -- Simulator.logString @(Builtin PAB.ValidatorContracts) $ "userNFT: " ++ P.show userNFT 

    -- let
    --     deadlinePool  = LedgerTimeSlot.slotToEndPOSIXTime Data.Default.def 100

    --     pParams = PoolParams
    --         { 
    --             T.ppMasters = [walletPaymentPubKeyHash master1, walletPaymentPubKeyHash master2] , 
    --             T.ppInterest = 10 , 
    --             T.ppMinumunInvest   = 5_000_000 , 
    --             T.ppMinumunCompoundInvest    = 3_000_000 , 
    --             T.ppDeadline  = deadlinePool , 
    --             T.ppPoolNFT = poolNFT , 
    --             T.ppCurSymbolForMintingNFTPolicy = poolNFTCurrencySymbol,
    --             T.ppValidTimeRange = 10_000,
    --             T.ppMinimunClaim = 3_000_000
    --         }



    -- --cidInit <- Simulator.activateContract defaultWallet InitLottoContract
    -- -- void $ Simulator.callEndpointOnInstance cidInit "init" sp
    -- -- Simulator.waitNSlots 2
    -- --activateContractWalletMaster1 <- TraceEmulator.activateContractWallet master1 OffChain.endpoints

    -- -- MasterCreatePool MasterCreatePoolParams |
    -- -- MasterFundPool MasterFundPoolParams |
    -- -- MasterGetBackFund MasterGetBackFundParams |
    -- -- UserInvest UserInvestParams |
    -- -- UserGetBackInvest UserGetBackInvestParams |
    -- -- UserGetRewards UserGetRewardsParams |
    -- -- UserInvestRewards UserInvestRewardsParams

    -- cMasterCreatePool_Master1  <- Simulator.activateContract (getWallet master1) (PAB.MasterCreatePool MasterCreatePoolParams{  
    --             mcpPoolParam = pParams, 
    --             mcpPoolNFTTokenName = poolNFTTokenName,
    --             mcpPoolNFTTxOutRef = poolNFTTxOutRef,
    --             mcpFund   = 100_000_000
    --         })

    -- Simulator.waitUntilFinished  cMasterCreatePool_Master1

    -- slot <- Simulator.currentSlot >>= liftIO . STM.atomically
    -- let posixTime = LedgerTimeSlot.slotToEndPOSIXTime Data.Default.def slot
    -- Simulator.logString @(Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot 
    -- Simulator.logString @(Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
    -- Simulator.logString @(Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

    -- --Simulator.waitNSlots 2
    -- --Simulator.waitUntilSlot 5

    -- Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
    -- void $ liftIO P.getLine

    -- Simulator.logString @(Builtin PAB.ValidatorContracts) "Invest ?"
    -- opcion <- liftIO P.getLine

    -- case P.read opcion of
    --     1 -> do
    --         let 
    --             createdAtInvest = LedgerTimeSlot.slotToEndPOSIXTime Data.Default.def slot
    --             deadlineInvest = LedgerTimeSlot.slotToEndPOSIXTime Data.Default.def (slot+50)

    --         cUserInvest_User1  <- Simulator.activateContract (getWallet user1) (PAB.UserInvest UserInvestParams{  
    --                 uipPoolParam = pParams,
    --                 uiUserNFTTokenName = user1NFTTokenName,
    --                 uiUserNFTTxOutRef = user1NFTTxOutRef,
    --                 uipCreatedAt = createdAtInvest,
    --                 uipDeadline = deadlineInvest,
    --                 uipInvest   = 6_000_000
    --             })

    --         Simulator.waitUntilFinished cUserInvest_User1
    --         --Simulator.waitNSlots 5

    --         slot <- Simulator.currentSlot >>= liftIO . STM.atomically
    --         let posixTime = LedgerTimeSlot.slotToEndPOSIXTime Data.Default.def slot
    --         Simulator.logString @(Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot 
    --         Simulator.logString @(Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
    --         Simulator.logString @(Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

    --         Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
    --         void $ liftIO P.getLine


    -- Simulator.logString @(Builtin PAB.ValidatorContracts) "Invest ?"
    -- opcion <- liftIO P.getLine

    -- case P.read opcion of
    --     1 -> do
    --         let 
    --             createdAtInvest = LedgerTimeSlot.slotToEndPOSIXTime Data.Default.def slot
    --             deadlineInvest = LedgerTimeSlot.slotToEndPOSIXTime Data.Default.def (slot+50)

    --         cUserInvest_User1  <- Simulator.activateContract (getWallet user1) (PAB.UserInvest UserInvestParams{  
    --                 uipPoolParam = pParams,
    --                 uiUserNFTTokenName = user1NFTTokenName,
    --                 uiUserNFTTxOutRef = user1NFTTxOutRef,
    --                 uipCreatedAt = createdAtInvest,
    --                 uipDeadline = deadlineInvest,
    --                 uipInvest   = 6_000_000
    --             })

    --         Simulator.waitUntilFinished cUserInvest_User1
    --         --Simulator.waitNSlots 5

    --         slot <- Simulator.currentSlot >>= liftIO . STM.atomically
    --         let posixTime = LedgerTimeSlot.slotToEndPOSIXTime Data.Default.def slot
    --         Simulator.logString @(Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot 
    --         Simulator.logString @(Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
    --         Simulator.logString @(Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

    --         Simulator.logString @(Builtin PAB.ValidatorContracts) "Press return to continue..."
    --         void $ liftIO P.getLine

    -- -- Pressing enter results in the balances being printed
    -- Simulator.logString @(Builtin PAB.ValidatorContracts) "Balances at the end of the simulation"
    -- --void $ liftIO P.getLine

    -- balances <- Simulator.currentBalances
    -- Simulator.logBalances @(Builtin PAB.ValidatorContracts) balances

    -- slot <- Simulator.currentSlot >>= liftIO . STM.atomically
    -- let posixTime = LedgerTimeSlot.slotToEndPOSIXTime Data.Default.def slot
    -- Simulator.logString @(Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot 
    -- Simulator.logString @(Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
    -- Simulator.logString @(Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

    -- shutdown


