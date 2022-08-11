-- {-# LANGUAGE DataKinds                  #-}
-- {-# LANGUAGE DeriveAnyClass             #-}
-- {-# LANGUAGE DeriveGeneric              #-}
-- {-# LANGUAGE DerivingStrategies         #-}
-- {-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase                 #-}
-- {-# LANGUAGE MultiParamTypeClasses      #-}
-- {-# LANGUAGE NoImplicitPrelude          #-}
-- {-# LANGUAGE OverloadedStrings          #-}
-- {-# LANGUAGE RecordWildCards            #-}
-- {-# LANGUAGE ScopedTypeVariables        #-}
-- {-# LANGUAGE TemplateHaskell            #-}
-- {-# LANGUAGE TypeApplications           #-}
-- {-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
-- {-# LANGUAGE NumericUnderscores    #-}

-- {-# LANGUAGE DataKinds          #-}
-- {-# LANGUAGE RankNTypes         #-}
-- {-# LANGUAGE ImportQualifiedPost #-}

{-# LANGUAGE NoImplicitPrelude          #-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores    #-}

--{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module  Validators.StakeSimple.TestWithPABSimulator
    where

-- import           Control.Monad        hiding (fmap)
-- import           Data.Aeson           (ToJSON, FromJSON)
-- import           Data.List.NonEmpty   (NonEmpty (..))
-- import           Data.Map             as Map
-- import           Data.Text            (pack, Text)
-- import           Data.String  
-- import           GHC.Generics         (Generic)
-- import           Ledger               hiding (singleton)
-- import qualified Ledger.Constraints   as Constraints
-- import qualified Ledger.Typed.Scripts as Scripts
-- import           Ledger.Value         as Value
-- import           Ledger.Ada           as Ada
-- import           Playground.Contract  (IO, ensureKnownCurrencies, printSchemas, stage, printJson)
-- import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
-- import           Playground.Types     (KnownCurrency (..))
-- import           Plutus.Contract
-- import qualified PlutusTx
-- import           PlutusTx.Prelude     hiding (unless)
-- import qualified Prelude              as HASKELL 
-- import           Schema               (ToSchema)
-- import qualified Data.OpenApi.Schema         (ToSchema)
-- import           Text.Printf          (printf)
-- import           Data.Typeable

-- import           Plutus.Trace.Emulator  as Emulator
-- import           Wallet.Emulator.Wallet
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
import           Ledger.Address                      (Address, PaymentPubKeyHash, pubKeyHashAddress)
import           Ledger.CardanoWallet   qualified as CW
import qualified Ledger.TimeSlot                     as TimeSlot
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler(contractHandler))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import qualified Plutus.V1.Ledger.Slot               as Slot
import           Wallet.Emulator.Wallet              (Wallet, knownWallet)


import           Playground.Contract  (IO)
import qualified Prelude                             as HASKELL
import           PlutusTx.Prelude                    hiding (unless)
import           Ledger                              hiding (singleton)
import           Plutus.V1.Ledger.Value
import qualified Plutus.Trace.Emulator  as Trace
import           Data.Map             as Map
import           Control.Lens
import           Plutus.Contract
import           Data.Text            (pack, Text)


import Wallet.Emulator.Chain qualified as Chain
import Wallet.Emulator.Chain (ChainControlEffect, ChainState)

import Control.Concurrent.STM (STM, TMVar, TVar)
import Control.Concurrent.STM qualified as STM
import Ledger (Address, Slot, TxId, TxOutRef)

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
import          Validators.StakeSimple.OffChain
import          Validators.StakeSimple.Typos
import          Validators.StakeSimple.OffChainHelpers
import          Validators.StakeSimple.OnChainNFT     (mintingNFTPolicy)
import          Validators.StakeSimple.Helpers
import          Validators.StakeSimple.PAB

handlers :: SimulatorEffectHandlers (Builtin ValidatorContracts)
handlers = Simulator.mkSimulatorHandlers Data.Default.def  HASKELL.$ interpret (contractHandler Builtin.handleBuiltin)

getWallet :: Integer -> Wallet
getWallet = knownWallet

walletPaymentPubKeyHash :: Integer -> PaymentPubKeyHash
walletPaymentPubKeyHash walletNumber = CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber walletNumber)

walletPaymentPubKeyHashAddress ::  Integer -> Address
walletPaymentPubKeyHashAddress walletNumber = pubKeyHashAddress (walletPaymentPubKeyHash walletNumber) Nothing

getUtxosListInPABSimulator :: Blockchain -> Ledger.Address ->  [TxOutRef]
getUtxosListInPABSimulator blockchain addr =  do
    let

        unspentOutputList = unspentOutputs blockchain

        utxos =  [txOutRef    | (txOutRef, txOut)    <- Map.toList  unspentOutputList , txOutAddress  txOut == addr]
    utxos


getFormatTime :: Ledger.POSIXTime -> HASKELL.String
getFormatTime posixTime = do
    let
        milisegundosFixedPico :: Pico
        milisegundosFixedPico = MkFixed  (Ledger.getPOSIXTime posixTime * 1000000000)
        seconds = Data.Time.Clock.secondsToNominalDiffTime milisegundosFixedPico

    formatTime Data.Time.Format.defaultTimeLocale  "%c" $ posixSecondsToUTCTime seconds
    --iso8601Show $ posixSecondsToUTCTime $ seconds



getJSON :: HASKELL.String ->  IO B.ByteString
getJSON file = B.readFile $ path ++ file

writeJSON :: HASKELL.String -> B.ByteString -> IO  ()
writeJSON file   = B.writeFile $  path ++ file


-- stringEq :: Eq a => HASKELL.String a -> HASKELL.String a
-- stringEq a = a


--path2 = "~/source/cardano-falcon-stakepool-devs/cardano-falcon-stakepol-devs-haskell/"
path = ""

mainLoop :: Maybe Integer -> Maybe PoolParams ->  Control.Monad.Freer.Internal.Eff (Plutus.PAB.Core.PABEffects (Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin ValidatorContracts))) () -> Control.Monad.Freer.Internal.Eff (Plutus.PAB.Core.PABEffects (Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin ValidatorContracts))) ()
mainLoop walletNro pParams shutdown = do

    Simulator.logString @(Builtin ValidatorContracts) "OPERACIONES:"

    case walletNro of
        Nothing ->
            Simulator.logString @(Builtin ValidatorContracts) "1 - Elegir Wallet"
        Just walletNro ->
            Simulator.logString @(Builtin ValidatorContracts) $ "1 - Elegir Wallet (" ++ HASKELL.show walletNro ++ ")"

    Simulator.logString @(Builtin ValidatorContracts) "21 - Crear Pool Params"

    case pParams of
        Nothing ->
            Simulator.logString @(Builtin ValidatorContracts) "22 - Elegir Pool Params"
        Just pParams ->
            Simulator.logString @(Builtin ValidatorContracts) $ "22 - Elegir Pool Params(" ++ HASKELL.show (ppPoolNFT pParams ) ++ ")"

    
    Simulator.logString @(Builtin ValidatorContracts) "31 - Crear Pool"
    Simulator.logString @(Builtin ValidatorContracts) "32 - Fund Pool"

    Simulator.logString @(Builtin ValidatorContracts) "41 - Invertir en Pool"
    Simulator.logString @(Builtin ValidatorContracts) "42 - Claim Rewards de Pool"

    Simulator.logString @(Builtin ValidatorContracts) "99 - Salir"

    Simulator.logString @(Builtin ValidatorContracts) "Ingrese una opción:"
    opcion <- liftIO HASKELL.getLine

    blockchain <- Simulator.blockchain

    case readMaybe opcion of

        Just 1 -> do

            Simulator.logString @(Builtin ValidatorContracts) "Ingrese una wallet:"

            Simulator.logString @(Builtin ValidatorContracts) "1 - Master 1"
            Simulator.logString @(Builtin ValidatorContracts) "2 - Master 2"
            Simulator.logString @(Builtin ValidatorContracts) "3 - Master 3"
            Simulator.logString @(Builtin ValidatorContracts) "4 - User 1"
            Simulator.logString @(Builtin ValidatorContracts) "5 - User 2"
            opcionWallet <- liftIO HASKELL.getLine

            mainLoop (readMaybe opcionWallet)  pParams shutdown

        Just 21 -> do

            let
                master1 = 1
                master2 = 2

                uTxOutRefAt =  getUtxosListInPABSimulator blockchain (walletPaymentPubKeyHashAddress (fromJust walletNro))

                poolNFTTxOutRef = head uTxOutRefAt
                idTxOut = txOutRefId poolNFTTxOutRef
                indexTxOut = txOutRefIdx  poolNFTTxOutRef

                poolNFTTokenName =  TokenName (indexTxOut `consByteString`  getTxId  idTxOut  )
                poolNFTCurrencySymbol = curSymbol mintingNFTPolicy

                poolNFT = assetClass poolNFTCurrencySymbol  poolNFTTokenName

                deadlinePool  = TimeSlot.slotToEndPOSIXTime Data.Default.def 500

                pParams = PoolParams
                    {
                        ppMasters = [walletPaymentPubKeyHash master1, walletPaymentPubKeyHash master2] ,
                        ppInterest = 10 ,
                        ppMinumunInvest   = 5_000_000 ,
                        ppMinumunCompoundInvest    = 3_000_000 ,
                        ppDeadline  = deadlinePool ,
                        ppPoolNFT = poolNFT ,
                        ppPoolNFTTxOutRef  = poolNFTTxOutRef ,
                        ppCurSymbolForMintingNFTPolicy = poolNFTCurrencySymbol,
                        ppValidTimeRange = 10_000,
                        ppMinimunClaim = 3_000_000
                    }

            Simulator.logString @(Builtin ValidatorContracts) $ "poolNFTTxOutRef: " ++ HASKELL.show poolNFTTxOutRef
            Simulator.logString @(Builtin ValidatorContracts) $ "poolNFTTokenName: " ++ HASKELL.show poolNFTTokenName
            Simulator.logString @(Builtin ValidatorContracts) $ "poolNFTCurrencySymbol: " ++ HASKELL.show poolNFTCurrencySymbol
            Simulator.logString @(Builtin ValidatorContracts) $ "poolNFT: " ++ HASKELL.show poolNFT

            Simulator.logString @(Builtin ValidatorContracts) "Ingrese un nombre para guardar el Pool:"
            opcionPool <- liftIO HASKELL.getLine
            liftIO $ writeJSON ("files/stakeSimple/" ++ opcionPool ++ ".json" ) (encode pParams)

            Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
            void $ liftIO HASKELL.getLine

            mainLoop walletNro (Just pParams) shutdown

        Just 22 -> do

            files <- liftIO $ getDirectoryContents "files/stakeSimple"
            
           -- validFIles <- [file | file  <- files ] 

            Simulator.logString @(Builtin ValidatorContracts) "Ingrese un Pool:"

            mapM_ ( Simulator.logString @(Builtin ValidatorContracts)   )  files

            --Simulator.logString @(Builtin ValidatorContracts) "33" >> 

            opcionPool <- liftIO HASKELL.getLine

            jsonFile <- liftIO $ getJSON ("files/stakeSimple/" ++ opcionPool ++ ".json" )

            let
                pParams =  decode jsonFile

            mainLoop walletNro pParams shutdown

        Just 31 ->  do
            
            case (walletNro,  pParams) of
                (Just walletNro, Just pParams) -> do
                    let 

                        poolNFT = ppPoolNFT pParams

                        (_, poolNFTTokenName) = unAssetClass poolNFT   
                        
                        -- idTxOut = txOutRefId poolNFTTxOutRef
                        -- indexTxOut = txOutRefIdx  poolNFTTxOutRef

                        -- poolNFTTokenName =  TokenName (indexTxOut `consByteString`  getTxId  idTxOut  )

                        -- poolNFTTxOutRef = TxOutRef {
                        --         txOutRefId  = unTokenName poolNFTTokenName ,
                        --         txOutRefIdx  = unTokenName poolNFTTokenName ,
                        --     }

                        poolNFTTxOutRef = ppPoolNFTTxOutRef pParams

                    cMasterCreatePool_Master  <- Simulator.activateContract (getWallet  walletNro) (MasterCreatePool MasterCreatePoolParams{
                                mcpPoolParam = pParams,
                                mcpPoolNFTTokenName = poolNFTTokenName,
                                mcpPoolNFTTxOutRef = poolNFTTxOutRef,
                                mcpFund   = 100_000_000
                            })

                    Simulator.waitUntilFinished  cMasterCreatePool_Master

                    slot <- Simulator.currentSlot >>= liftIO . STM.atomically
                    let posixTime = TimeSlot.slotToEndPOSIXTime Data.Default.def slot
                    Simulator.logString @(Builtin ValidatorContracts) $ "slot: " ++  HASKELL.show slot
                    Simulator.logString @(Builtin ValidatorContracts) $ "time: " ++  HASKELL.show posixTime
                    Simulator.logString @(Builtin ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

                    Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
                    void $ liftIO HASKELL.getLine

                    mainLoop (Just walletNro) (Just pParams) shutdown

                (_, Just pParams) -> do

                    Simulator.logString @(Builtin ValidatorContracts) "Elija Wallet"
                    Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
                    void $ liftIO HASKELL.getLine

                    mainLoop Nothing (Just pParams) shutdown

                (Just walletNro, _) -> do

                    Simulator.logString @(Builtin ValidatorContracts) "Elija Pool Params"
                    Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
                    void $ liftIO HASKELL.getLine

                    mainLoop (Just walletNro) Nothing shutdown

                (_, _) -> do

                    Simulator.logString @(Builtin ValidatorContracts) "Elija Wallet y Pool Params"
                    Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
                    void $ liftIO HASKELL.getLine

                    mainLoop Nothing Nothing shutdown

        Just 32 -> do
            case (walletNro,  pParams) of
                (Just walletNro, Just pParams) -> do

                    cMasterFundPool_Master  <- Simulator.activateContract (getWallet walletNro) (MasterFundPool MasterFundPoolParams{
                            mspPoolParam =  pParams,
                            mspFund   = 50_000_000
                        })

                    Simulator.waitUntilFinished  cMasterFundPool_Master

                    slot <- Simulator.currentSlot >>= liftIO . STM.atomically
                    let posixTime = TimeSlot.slotToEndPOSIXTime Data.Default.def slot
                    Simulator.logString @(Builtin ValidatorContracts) $ "slot: " ++  HASKELL.show slot
                    Simulator.logString @(Builtin ValidatorContracts) $ "time: " ++  HASKELL.show posixTime
                    Simulator.logString @(Builtin ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

                    Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
                    void $ liftIO HASKELL.getLine
                    mainLoop (Just walletNro) (Just pParams) shutdown

                (_, Just pParams) -> do

                    Simulator.logString @(Builtin ValidatorContracts) "Elija Wallet"
                    Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
                    void $ liftIO HASKELL.getLine

                    mainLoop Nothing (Just pParams) shutdown

                (Just walletNro, _) -> do

                    Simulator.logString @(Builtin ValidatorContracts) "Elija Pool Params"
                    Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
                    void $ liftIO HASKELL.getLine

                    mainLoop (Just walletNro) Nothing shutdown

                (_, _) -> do

                    Simulator.logString @(Builtin ValidatorContracts) "Elija Wallet y Pool Params"
                    Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
                    void $ liftIO HASKELL.getLine

                    mainLoop Nothing Nothing shutdown

        Just 41 -> do
            
            case (walletNro,  pParams) of
                (Just walletNro, Just pParams) -> do

                    let
                        uTxOutRefAt =  getUtxosListInPABSimulator blockchain (walletPaymentPubKeyHashAddress walletNro)

                        user1NFTTxOutRef = head uTxOutRefAt
                        user1IdTxOut = txOutRefId user1NFTTxOutRef
                        user1IndexTxOut = txOutRefIdx  user1NFTTxOutRef

                        user1NFTTokenName =  TokenName (user1IndexTxOut `consByteString`  getTxId  user1IdTxOut  )
                        user1NFTCurrencySymbol = curSymbol mintingNFTPolicy

                        userNFT = assetClass user1NFTCurrencySymbol user1NFTTokenName

                    Simulator.logString @(Builtin ValidatorContracts) $ "user1NFTTxOutRef: " ++ HASKELL.show user1NFTTxOutRef
                    Simulator.logString @(Builtin ValidatorContracts) $ "user1NFTTokenName: " ++ HASKELL.show user1NFTTokenName
                    Simulator.logString @(Builtin ValidatorContracts) $ "user1NFTCurrencySymbol: " ++ HASKELL.show user1NFTCurrencySymbol
                    Simulator.logString @(Builtin ValidatorContracts) $ "userNFT: " ++ HASKELL.show userNFT

                    slot <- Simulator.currentSlot >>= liftIO . STM.atomically

                    let

                        createdAtInvest = TimeSlot.slotToEndPOSIXTime Data.Default.def slot
                        deadlineInvest = TimeSlot.slotToEndPOSIXTime Data.Default.def (slot+50)

                    cUserInvest_User  <- Simulator.activateContract (getWallet walletNro) (UserInvest UserInvestParams{
                            uipPoolParam =  pParams,
                            uiUserNFTTokenName = user1NFTTokenName,
                            uiUserNFTTxOutRef = user1NFTTxOutRef,
                            uipCreatedAt = createdAtInvest,
                            uipDeadline = deadlineInvest,
                            uipInvest   = 6_000_000
                        })

                    Simulator.waitUntilFinished cUserInvest_User

                    slot <- Simulator.currentSlot >>= liftIO . STM.atomically
                    let posixTime = TimeSlot.slotToEndPOSIXTime Data.Default.def slot
                    Simulator.logString @(Builtin ValidatorContracts) $ "slot: " ++  HASKELL.show slot
                    Simulator.logString @(Builtin ValidatorContracts) $ "time: " ++  HASKELL.show posixTime
                    Simulator.logString @(Builtin ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

                    Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
                    void $ liftIO HASKELL.getLine

                    mainLoop  (Just walletNro) (Just pParams) shutdown

                (_, Just pParams) -> do

                    Simulator.logString @(Builtin ValidatorContracts) "Elija Wallet"
                    Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
                    void $ liftIO HASKELL.getLine

                    mainLoop Nothing (Just pParams) shutdown

                (Just walletNro, _) -> do

                    Simulator.logString @(Builtin ValidatorContracts) "Elija Pool Params"
                    Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
                    void $ liftIO HASKELL.getLine

                    mainLoop (Just walletNro) Nothing shutdown

                (_, _) -> do

                    Simulator.logString @(Builtin ValidatorContracts) "Elija Wallet y Pool Params"
                    Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
                    void $ liftIO HASKELL.getLine

                    mainLoop Nothing Nothing shutdown

        Just 42 -> do
            
            case (walletNro,  pParams) of
                (Just walletNro, Just pParams) -> do

                    let
                        uTxOutRefAt =  getUtxosListInPABSimulator blockchain (walletPaymentPubKeyHashAddress walletNro)

                        user1NFTTxOutRef = head uTxOutRefAt
                        user1IdTxOut = txOutRefId user1NFTTxOutRef
                        user1IndexTxOut = txOutRefIdx  user1NFTTxOutRef

                        user1NFTTokenName =  TokenName (user1IndexTxOut `consByteString`  getTxId  user1IdTxOut  )
                        user1NFTCurrencySymbol = curSymbol mintingNFTPolicy

                        userNFT = assetClass user1NFTCurrencySymbol user1NFTTokenName

                    Simulator.logString @(Builtin ValidatorContracts) $ "user1NFTTxOutRef: " ++ HASKELL.show user1NFTTxOutRef
                    Simulator.logString @(Builtin ValidatorContracts) $ "user1NFTTokenName: " ++ HASKELL.show user1NFTTokenName
                    Simulator.logString @(Builtin ValidatorContracts) $ "user1NFTCurrencySymbol: " ++ HASKELL.show user1NFTCurrencySymbol
                    Simulator.logString @(Builtin ValidatorContracts) $ "userNFT: " ++ HASKELL.show userNFT

                    slot <- Simulator.currentSlot >>= liftIO . STM.atomically

                    let

                        createdAtInvest = TimeSlot.slotToEndPOSIXTime Data.Default.def slot
                        deadlineInvest = TimeSlot.slotToEndPOSIXTime Data.Default.def (slot+50)

                    cUserInvest_User  <- Simulator.activateContract (getWallet walletNro) (UserInvest UserInvestParams{
                            uipPoolParam =  pParams,
                            uiUserNFTTokenName = user1NFTTokenName,
                            uiUserNFTTxOutRef = user1NFTTxOutRef,
                            uipCreatedAt = createdAtInvest,
                            uipDeadline = deadlineInvest,
                            uipInvest   = 6_000_000
                        })

                    Simulator.waitUntilFinished cUserInvest_User

                    slot <- Simulator.currentSlot >>= liftIO . STM.atomically
                    let posixTime = TimeSlot.slotToEndPOSIXTime Data.Default.def slot
                    Simulator.logString @(Builtin ValidatorContracts) $ "slot: " ++  HASKELL.show slot
                    Simulator.logString @(Builtin ValidatorContracts) $ "time: " ++  HASKELL.show posixTime
                    Simulator.logString @(Builtin ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

                    Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
                    void $ liftIO HASKELL.getLine

                    mainLoop  (Just walletNro) (Just pParams) shutdown

                (_, Just pParams) -> do

                    Simulator.logString @(Builtin ValidatorContracts) "Elija Wallet"
                    Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
                    void $ liftIO HASKELL.getLine

                    mainLoop Nothing (Just pParams) shutdown

                (Just walletNro, _) -> do

                    Simulator.logString @(Builtin ValidatorContracts) "Elija Pool Params"
                    Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
                    void $ liftIO HASKELL.getLine

                    mainLoop (Just walletNro) Nothing shutdown

                (_, _) -> do

                    Simulator.logString @(Builtin ValidatorContracts) "Elija Wallet y Pool Params"
                    Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
                    void $ liftIO HASKELL.getLine

                    mainLoop Nothing Nothing shutdown
        Just 99 -> do

            Simulator.logString @(Builtin ValidatorContracts) "Balances at the end of the simulation"
            --void $ liftIO HASKELL.getLine

            balances <- Simulator.currentBalances
            Simulator.logBalances @(Builtin ValidatorContracts) balances

            slot <- Simulator.currentSlot >>= liftIO . STM.atomically
            let posixTime = TimeSlot.slotToEndPOSIXTime Data.Default.def slot
            Simulator.logString @(Builtin ValidatorContracts) $ "slot: " ++  HASKELL.show slot
            Simulator.logString @(Builtin ValidatorContracts) $ "time: " ++  HASKELL.show posixTime
            Simulator.logString @(Builtin ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

            shutdown

        _ -> mainLoop walletNro pParams shutdown





test2 :: IO ()
test2  = void $ Simulator.runSimulationWith handlers myTrace2

myTrace2  ::  Control.Monad.Freer.Internal.Eff (Plutus.PAB.Core.PABEffects (Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin ValidatorContracts)))  ()
myTrace2 = do
    Simulator.logString @(Builtin ValidatorContracts) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    Simulator.logString @(Builtin ValidatorContracts) "********* PAB Server is running *********"
    -- Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
    -- void $ liftIO HASKELL.getLine

    let
        master1 = 1
        master2 = 2
        user1 = 3

    Simulator.logString @(Builtin ValidatorContracts) $ "master1 Pkh = " ++ HASKELL.show (walletPaymentPubKeyHash master1)
    Simulator.logString @(Builtin ValidatorContracts) $ "master1 PkhAddress = " ++ HASKELL.show (walletPaymentPubKeyHashAddress master1)

    Simulator.logString @(Builtin ValidatorContracts) $ "master2 Pkh = " ++ HASKELL.show (walletPaymentPubKeyHash master2)
    Simulator.logString @(Builtin ValidatorContracts) $ "master2 PkhAddress = " ++ HASKELL.show (walletPaymentPubKeyHashAddress master2)

    Simulator.logString @(Builtin ValidatorContracts) $ "user1 Pkh = " ++ HASKELL.show (walletPaymentPubKeyHash user1)
    Simulator.logString @(Builtin ValidatorContracts) $ "user1 PkhAddress = " ++ HASKELL.show (walletPaymentPubKeyHashAddress user1)


    mainLoop Nothing Nothing shutdown



    -- blockchain <- Simulator.blockchain

    -- let 

    --     uTxOutRefAtMaster1 =  getUtxosListInPABSimulator blockchain (walletPaymentPubKeyHashAddress master1)

    --     poolNFTTxOutRef = head uTxOutRefAtMaster1
    --     idTxOut = txOutRefId poolNFTTxOutRef
    --     indexTxOut = txOutRefIdx  poolNFTTxOutRef

    --     poolNFTTokenName =  TokenName (indexTxOut `consByteString`  getTxId  idTxOut  )
    --     poolNFTCurrencySymbol = curSymbol mintingNFTPolicy 

    --     poolNFT = assetClass poolNFTCurrencySymbol  poolNFTTokenName

    -- Simulator.logString @(Builtin ValidatorContracts) $ "poolNFTTxOutRef: " ++ HASKELL.show poolNFTTxOutRef
    -- Simulator.logString @(Builtin ValidatorContracts) $ "poolNFTTokenName: " ++ HASKELL.show poolNFTTokenName
    -- Simulator.logString @(Builtin ValidatorContracts) $ "poolNFTCurrencySymbol: " ++ HASKELL.show poolNFTCurrencySymbol
    -- Simulator.logString @(Builtin ValidatorContracts) $ "poolNFT: " ++ HASKELL.show poolNFT 

    -- let
    --     uTxOutRefAtUser1 =  getUtxosListInPABSimulator blockchain (walletPaymentPubKeyHashAddress user1)

    --     user1NFTTxOutRef = head uTxOutRefAtUser1
    --     user1IdTxOut = txOutRefId user1NFTTxOutRef
    --     user1IndexTxOut = txOutRefIdx  user1NFTTxOutRef

    --     user1NFTTokenName =  TokenName (user1IndexTxOut `consByteString`  getTxId  user1IdTxOut  )
    --     user1NFTCurrencySymbol = curSymbol mintingNFTPolicy 

    --     userNFT = assetClass user1NFTCurrencySymbol user1NFTTokenName

    -- Simulator.logString @(Builtin ValidatorContracts) $ "user1NFTTxOutRef: " ++ HASKELL.show user1NFTTxOutRef
    -- Simulator.logString @(Builtin ValidatorContracts) $ "user1NFTTokenName: " ++ HASKELL.show user1NFTTokenName
    -- Simulator.logString @(Builtin ValidatorContracts) $ "user1NFTCurrencySymbol: " ++ HASKELL.show user1NFTCurrencySymbol
    -- Simulator.logString @(Builtin ValidatorContracts) $ "userNFT: " ++ HASKELL.show userNFT 

    -- let
    --     deadlinePool  = TimeSlot.slotToEndPOSIXTime Data.Default.def 100

    --     pParams = PoolParams
    --         { 
    --             ppMasters = [walletPaymentPubKeyHash master1, walletPaymentPubKeyHash master2] , 
    --             ppInterest = 10 , 
    --             ppMinumunInvest   = 5_000_000 , 
    --             ppMinumunCompoundInvest    = 3_000_000 , 
    --             ppDeadline  = deadlinePool , 
    --             ppPoolNFT = poolNFT , 
    --             ppCurSymbolForMintingNFTPolicy = poolNFTCurrencySymbol,
    --             ppValidTimeRange = 10_000,
    --             ppMinimunClaim = 3_000_000
    --         }



    -- --cidInit <- Simulator.activateContract defaultWallet InitLottoContract
    -- -- void $ Simulator.callEndpointOnInstance cidInit "init" sp
    -- -- Simulator.waitNSlots 2
    -- --activateContractWalletMaster1 <- activateContractWallet master1 endpoints

    -- -- MasterCreatePool MasterCreatePoolParams |
    -- -- MasterFundPool MasterFundPoolParams |
    -- -- MasterGetBackFund MasterGetBackFundParams |
    -- -- UserInvest UserInvestParams |
    -- -- UserGetBackInvest UserGetBackInvestParams |
    -- -- UserGetRewards UserGetRewardsParams |
    -- -- UserInvestRewards UserInvestRewardsParams

    -- cMasterCreatePool_Master1  <- Simulator.activateContract (getWallet master1) (MasterCreatePool MasterCreatePoolParams{  
    --             mcpPoolParam = pParams, 
    --             mcpPoolNFTTokenName = poolNFTTokenName,
    --             mcpPoolNFTTxOutRef = poolNFTTxOutRef,
    --             mcpFund   = 100_000_000
    --         })

    -- Simulator.waitUntilFinished  cMasterCreatePool_Master1

    -- slot <- Simulator.currentSlot >>= liftIO . STM.atomically
    -- let posixTime = TimeSlot.slotToEndPOSIXTime Data.Default.def slot
    -- Simulator.logString @(Builtin ValidatorContracts) $ "slot: " ++  HASKELL.show slot 
    -- Simulator.logString @(Builtin ValidatorContracts) $ "time: " ++  HASKELL.show posixTime
    -- Simulator.logString @(Builtin ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

    -- --Simulator.waitNSlots 2
    -- --Simulator.waitUntilSlot 5

    -- Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
    -- void $ liftIO HASKELL.getLine

    -- Simulator.logString @(Builtin ValidatorContracts) "Invest ?"
    -- opcion <- liftIO HASKELL.getLine

    -- case HASKELL.read opcion of
    --     1 ->  do
    --         let 
    --             createdAtInvest = TimeSlot.slotToEndPOSIXTime Data.Default.def slot
    --             deadlineInvest = TimeSlot.slotToEndPOSIXTime Data.Default.def (slot+50)

    --         cUserInvest_User1  <- Simulator.activateContract (getWallet user1) (UserInvest UserInvestParams{  
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
    --         let posixTime = TimeSlot.slotToEndPOSIXTime Data.Default.def slot
    --         Simulator.logString @(Builtin ValidatorContracts) $ "slot: " ++  HASKELL.show slot 
    --         Simulator.logString @(Builtin ValidatorContracts) $ "time: " ++  HASKELL.show posixTime
    --         Simulator.logString @(Builtin ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

    --         Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
    --         void $ liftIO HASKELL.getLine


    -- Simulator.logString @(Builtin ValidatorContracts) "Invest ?"
    -- opcion <- liftIO HASKELL.getLine

    -- case HASKELL.read opcion of
    --     1 ->  do
    --         let 
    --             createdAtInvest = TimeSlot.slotToEndPOSIXTime Data.Default.def slot
    --             deadlineInvest = TimeSlot.slotToEndPOSIXTime Data.Default.def (slot+50)

    --         cUserInvest_User1  <- Simulator.activateContract (getWallet user1) (UserInvest UserInvestParams{  
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
    --         let posixTime = TimeSlot.slotToEndPOSIXTime Data.Default.def slot
    --         Simulator.logString @(Builtin ValidatorContracts) $ "slot: " ++  HASKELL.show slot 
    --         Simulator.logString @(Builtin ValidatorContracts) $ "time: " ++  HASKELL.show posixTime
    --         Simulator.logString @(Builtin ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

    --         Simulator.logString @(Builtin ValidatorContracts) "Press return to continue..."
    --         void $ liftIO HASKELL.getLine

    -- -- Pressing enter results in the balances being printed
    -- Simulator.logString @(Builtin ValidatorContracts) "Balances at the end of the simulation"
    -- --void $ liftIO HASKELL.getLine

    -- balances <- Simulator.currentBalances
    -- Simulator.logBalances @(Builtin ValidatorContracts) balances

    -- slot <- Simulator.currentSlot >>= liftIO . STM.atomically
    -- let posixTime = TimeSlot.slotToEndPOSIXTime Data.Default.def slot
    -- Simulator.logString @(Builtin ValidatorContracts) $ "slot: " ++  HASKELL.show slot 
    -- Simulator.logString @(Builtin ValidatorContracts) $ "time: " ++  HASKELL.show posixTime
    -- Simulator.logString @(Builtin ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

    -- shutdown


