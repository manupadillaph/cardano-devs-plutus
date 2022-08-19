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

module Validators.StakeSimpleV1.TestWithPABSimulator where

--Import Externos

import qualified Control.Concurrent.STM              as ConcurrentSTM (atomically)
import qualified Control.Monad.IO.Class              as MonadIOClass (MonadIO (..))
import qualified Control.Monad                       as Monad (void)
import qualified Control.Monad.Freer                 as MonadFreer (interpret)
import qualified Control.Monad.Freer.Internal        as MonadFreerInternal (Eff)
import qualified Data.Aeson                          as DataAeson (encode,decode)
import qualified Data.ByteString.Lazy                as DataByteString
import qualified Data.Default                        as DataDefault (def)
import qualified Data.Map                            as DataMap
import qualified Data.List                           as DataList
import qualified Data.Time.Clock                     as DataTimeClock (secondsToNominalDiffTime)
import qualified Data.Time.Clock.POSIX               as DataTimeClockPOSIX (posixSecondsToUTCTime)
import qualified Data.Time.Format                    as DataTimeFormat (defaultTimeLocale,formatTime)
import qualified Data.Fixed                          as DataFixed (Pico, Fixed ( MkFixed ))
import qualified Ledger                              
import qualified Ledger.Blockchain                   as LedgerBlockchain (value)
import qualified Ledger.CardanoWallet                as LedgerCardanoWallet
import qualified Ledger.TimeSlot                     as LedgerTimeSlot
--import qualified Playground.Contract                 as PlaygroundContract (IO)
import qualified Prelude                             as P
import qualified Plutus.PAB.Core                     as PABCore (PABEffects)
import qualified Plutus.PAB.Effects.Contract.Builtin as PABEffectsContractBuiltin (Builtin, BuiltinHandler(contractHandler),handleBuiltin)
import qualified Plutus.PAB.Simulator                as PABSimulator
import qualified Plutus.PAB.Webserver.Server         as PABServer
import qualified Plutus.V1.Ledger.Address            as LedgerAddressV1
import qualified Plutus.V1.Ledger.Api                as LedgerApiV1
import qualified Plutus.V1.Ledger.Value              as LedgerValueV1
import qualified Plutus.V1.Ledger.Tx                 as LedgerTxV1 (txOutDatum)
import qualified PlutusTx.Builtins.Internal          as TxBuiltinsInternal hiding (head,consByteString)
import qualified PlutusTx.Eq                         as PlutusTxEq
import           PlutusTx.Prelude                    hiding (unless)
import qualified System.Directory                    as SystemDirectory 
import qualified Text.Read                           as TextRead (readMaybe)
import qualified Wallet.Emulator.Wallet              as WalletEmulator    

--Import Internos

import qualified Validators.StakeSimpleV1.OnChain        as OnChain  
import qualified Validators.StakeSimpleV1.OnChainHelpers as OnChainHelpers
import qualified Validators.StakeSimpleV1.OnChainNFT     as OnChainNFT(mintingNFTPolicy)
import qualified Validators.StakeSimpleV1.Helpers        as Helpers
import qualified Validators.StakeSimpleV1.PAB            as PAB
import qualified Validators.StakeSimpleV1.Typos          as T

-- Modulo:

handlers :: PABSimulator.SimulatorEffectHandlers (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)
handlers = PABSimulator.mkSimulatorHandlers  DataDefault.def P.$ MonadFreer.interpret (PABEffectsContractBuiltin.contractHandler PABEffectsContractBuiltin.handleBuiltin)

getWallet :: Integer -> WalletEmulator.Wallet
getWallet = WalletEmulator.knownWallet

walletPaymentPubKeyHash :: Integer -> Ledger.PaymentPubKeyHash
walletPaymentPubKeyHash walletNumber = LedgerCardanoWallet.paymentPubKeyHash (LedgerCardanoWallet.fromWalletNumber $ LedgerCardanoWallet.WalletNumber walletNumber)

walletPaymentPubKeyHashAddress ::  Integer -> LedgerAddressV1.Address
walletPaymentPubKeyHashAddress walletNumber = Ledger.pubKeyHashAddress (walletPaymentPubKeyHash walletNumber) Nothing

getUtxosListInPABSimulator :: Ledger.Blockchain -> LedgerAddressV1.Address -> [(Ledger.TxOutRef, Ledger.TxOut)]
getUtxosListInPABSimulator blockchain addr =  do
    let

        unspentOutputList = Ledger.unspentOutputs blockchain

        utxos =  [(txOutRef, txOut)  | (txOutRef, txOut)    <- DataMap.toList  unspentOutputList , Ledger.txOutAddress  txOut == addr]

    utxos


getFormatTime :: LedgerApiV1.POSIXTime -> P.String
getFormatTime posixTime = do
    let
        milisegundosFixedPico :: DataFixed.Pico
        milisegundosFixedPico = DataFixed.MkFixed  (LedgerApiV1.getPOSIXTime posixTime * 1000000000)
        seconds = DataTimeClock.secondsToNominalDiffTime milisegundosFixedPico

    DataTimeFormat.formatTime DataTimeFormat.defaultTimeLocale  "%c" $ DataTimeClockPOSIX.posixSecondsToUTCTime seconds
    --iso8601Show $ posixSecondsToUTCTime $ seconds



getJSON :: P.String -> P.IO DataByteString.ByteString
getJSON file = DataByteString.readFile $ path ++ file

writeJSON :: P.String -> DataByteString.ByteString -> P.IO ()
writeJSON file   = DataByteString.writeFile $  path ++ file


-- stringEq :: Eq a => P.String a -> P.String a
-- stringEq a = a
--path2 = "~/source/Plutus-Devs/cardano-devs-plutus/"
path = ""

mainLoop :: Maybe Integer -> Maybe T.PoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
mainLoop walletNro pParams shutdown = do

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "OPERACIONES:"

    case walletNro of
        Nothing ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "1 - Elegir Wallet"
        Just walletNro ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "1 - Elegir Wallet (" ++ P.show walletNro ++ ")"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "21 - Crear Pool Params"

    case pParams of
        Nothing ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "22 - Elegir Pool Params"
        Just pParams ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "22 - Elegir Pool Params(" ++ P.show (T.ppPoolNFT pParams ) ++ ")"

    
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "31 - Crear Pool"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "32 - Fund Pool"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "41 - Invertir en Pool"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "42 - Claim Rewards de Pool"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "99 - Salir"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Ingrese una opción:"
    opcion <- MonadIOClass.liftIO P.getLine

    blockchain <- PABSimulator.blockchain

    case TextRead.readMaybe opcion of

        Just 1 -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Ingrese una wallet:"

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "1 - Master 1"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "2 - Master 2"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "3 - Master 3"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "4 - User 1"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "5 - User 2"
            opcionWallet <- MonadIOClass.liftIO P.getLine

            mainLoop (TextRead.readMaybe opcionWallet)  pParams shutdown

        Just 21 -> do

            let
                master1 = 1
                master2 = 2

                uTxOutRefAt =  getUtxosListInPABSimulator blockchain (walletPaymentPubKeyHashAddress (Helpers.fromJust walletNro))

                poolNFTTxOutRef = head uTxOutRefAt
                idTxOut = LedgerApiV1.txOutRefId $ fst poolNFTTxOutRef
                indexTxOut = LedgerApiV1.txOutRefIdx $ fst poolNFTTxOutRef

                poolNFTTokenName =  LedgerValueV1.TokenName (indexTxOut `consByteString`  LedgerApiV1.getTxId  idTxOut  )
                poolNFTCurrencySymbol = Helpers.curSymbol OnChainNFT.mintingNFTPolicy

                poolNFT = LedgerValueV1.assetClass poolNFTCurrencySymbol  poolNFTTokenName

                deadlinePool  = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def 500

                pParams = T.PoolParams
                    {
                        T.ppMasters = [walletPaymentPubKeyHash master1, walletPaymentPubKeyHash master2] ,
                        T.ppInterest = 10 ,
                        T.ppMinumunInvest   = 5_000_000 ,
                        T.ppMinumunCompoundInvest    = 3_000_000 ,
                        T.ppDeadline  = deadlinePool ,
                        T.ppPoolNFT = poolNFT ,
                        T.ppPoolNFTTxOutRef  = fst poolNFTTxOutRef ,
                        T.ppCurSymbolForMintingNFTPolicy = poolNFTCurrencySymbol,
                        T.ppValidTimeRange = 10_000,
                        T.ppMinimunClaim = 3_000_000
                    }

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "poolNFTTxOutRef: " ++ P.show poolNFTTxOutRef
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "poolNFTTokenName: " ++ P.show poolNFTTokenName
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "poolNFTCurrencySymbol: " ++ P.show poolNFTCurrencySymbol
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "poolNFT: " ++ P.show poolNFT

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Ingrese un nombre para guardar el Pool:"
            opcionPool <- MonadIOClass.liftIO P.getLine
            MonadIOClass.liftIO $ writeJSON ("files/stakeSimple/" ++ opcionPool ++ ".json" ) (DataAeson.encode pParams)

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop walletNro (Just pParams) shutdown

        Just 22 -> do

            files <- MonadIOClass.liftIO $ SystemDirectory.getDirectoryContents "files/stakeSimple"
            
           -- validFIles <- [file | file  <- files ] 

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Ingrese un Pool:"

            mapM_ ( PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)   )  files

            --PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "33" >> 

            opcionPool <- MonadIOClass.liftIO P.getLine

            jsonFile <- MonadIOClass.liftIO $ getJSON ("files/stakeSimple/" ++ opcionPool ++ ".json" )

            let
                pParams =  DataAeson.decode jsonFile

            mainLoop walletNro pParams shutdown

        Just 31 -> do
            
            case (walletNro,  pParams) of
                (Just walletNro, Just pParams) -> do
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

                    cMasterCreatePool_Master  <- PABSimulator.activateContract (getWallet  walletNro) (PAB.MasterCreatePool T.MasterCreatePoolParams{
                                mcpPoolParam = pParams,
                                mcpPoolNFTTokenName = poolNFTTokenName,
                                mcpPoolNFTTxOutRef = poolNFTTxOutRef,
                                mcpFund   = 100_000_000
                            })

                    PABSimulator.waitUntilFinished  cMasterCreatePool_Master

                    slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
                    let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine

                    mainLoop (Just walletNro) (Just pParams) shutdown

                (_, Just pParams) -> do

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Wallet"
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine

                    mainLoop Nothing (Just pParams) shutdown

                (Just walletNro, _) -> do

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Pool Params"
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine

                    mainLoop (Just walletNro) Nothing shutdown

                (_, _) -> do

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Wallet y Pool Params"
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine

                    mainLoop Nothing Nothing shutdown

        Just 32 -> do
            case (walletNro,  pParams) of
                (Just walletNro, Just pParams) -> do

                    cMasterFundPool_Master  <- PABSimulator.activateContract (getWallet walletNro) (PAB.MasterFundPool T.MasterFundPoolParams{
                            mspPoolParam =  pParams,
                            mspFund   = 50_000_000
                        })

                    PABSimulator.waitUntilFinished  cMasterFundPool_Master

                    slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
                    let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine
                    mainLoop (Just walletNro) (Just pParams) shutdown

                (_, Just pParams) -> do

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Wallet"
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine

                    mainLoop Nothing (Just pParams) shutdown

                (Just walletNro, _) -> do

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Pool Params"
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine

                    mainLoop (Just walletNro) Nothing shutdown

                (_, _) -> do

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Wallet y Pool Params"
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine

                    mainLoop Nothing Nothing shutdown

        Just 41 -> do
            
            case (walletNro,  pParams) of
                (Just walletNro, Just pParams) -> do

                    let
                        uTxOutRefAt =  getUtxosListInPABSimulator blockchain (walletPaymentPubKeyHashAddress walletNro)

                        user1NFTTxOutRef = head uTxOutRefAt
                        user1IdTxOut = LedgerApiV1.txOutRefId $ fst user1NFTTxOutRef
                        user1IndexTxOut = LedgerApiV1.txOutRefIdx $ fst  user1NFTTxOutRef

                        user1NFTTokenName =  LedgerValueV1.TokenName (user1IndexTxOut `consByteString`  LedgerApiV1.getTxId  user1IdTxOut  )
                        user1NFTCurrencySymbol = Helpers.curSymbol OnChainNFT.mintingNFTPolicy

                        userNFT = LedgerValueV1.assetClass user1NFTCurrencySymbol user1NFTTokenName

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "user1NFTTxOutRef: " ++ P.show user1NFTTxOutRef
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "user1NFTTokenName: " ++ P.show user1NFTTokenName
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "user1NFTCurrencySymbol: " ++ P.show user1NFTCurrencySymbol
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "userNFT: " ++ P.show userNFT

                    slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically

                    let

                        createdAtInvest = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot
                        deadlineInvest = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def (slot+50)

                    cUserInvest_User  <- PABSimulator.activateContract (getWallet walletNro) (PAB.UserInvest T.UserInvestParams{
                            uipPoolParam =  pParams,
                            uiUserNFTTokenName = user1NFTTokenName,
                            uiUserNFTTxOutRef = fst user1NFTTxOutRef,
                            uipCreatedAt = createdAtInvest,
                            uipDeadline = deadlineInvest,
                            uipInvest   = 6_000_000
                        })

                    PABSimulator.waitUntilFinished cUserInvest_User

                    slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
                    let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine

                    mainLoop  (Just walletNro) (Just pParams) shutdown

                (_, Just pParams) -> do

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Wallet"
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine

                    mainLoop Nothing (Just pParams) shutdown

                (Just walletNro, _) -> do

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Pool Params"
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine

                    mainLoop (Just walletNro) Nothing shutdown

                (_, _) -> do

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Wallet y Pool Params"
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine

                    mainLoop Nothing Nothing shutdown

        Just 42 -> do
            
            case (walletNro,  pParams) of
                (Just walletNro, Just pParams) -> do

                    let
                        uTxOutRefAt =  getUtxosListInPABSimulator blockchain (walletPaymentPubKeyHashAddress walletNro)

                        user1NFTTxOutRef = head uTxOutRefAt
                        user1IdTxOut = LedgerApiV1.txOutRefId $ fst user1NFTTxOutRef
                        user1IndexTxOut = LedgerApiV1.txOutRefIdx $ fst user1NFTTxOutRef

                        user1NFTTokenName =  LedgerValueV1.TokenName (user1IndexTxOut `consByteString`  LedgerApiV1.getTxId  user1IdTxOut  )
                        user1NFTCurrencySymbol = Helpers.curSymbol OnChainNFT.mintingNFTPolicy

                        userNFT = LedgerValueV1.assetClass user1NFTCurrencySymbol user1NFTTokenName

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "user1NFTTxOutRef: " ++ P.show user1NFTTxOutRef
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "user1NFTTokenName: " ++ P.show user1NFTTokenName
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "user1NFTCurrencySymbol: " ++ P.show user1NFTCurrencySymbol
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "userNFT: " ++ P.show userNFT

                    slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically

                    let

                        createdAtInvest = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot
                        deadlineInvest = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def (slot+50)

                    cUserInvest_User  <- PABSimulator.activateContract (getWallet walletNro) (PAB.UserInvest T.UserInvestParams{
                            uipPoolParam =  pParams,
                            uiUserNFTTokenName = user1NFTTokenName,
                            uiUserNFTTxOutRef = fst user1NFTTxOutRef,
                            uipCreatedAt = createdAtInvest,
                            uipDeadline = deadlineInvest,
                            uipInvest   = 6_000_000
                        })

                    PABSimulator.waitUntilFinished cUserInvest_User

                    slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
                    let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine

                    mainLoop  (Just walletNro) (Just pParams) shutdown

                (_, Just pParams) -> do

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Wallet"
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine

                    mainLoop Nothing (Just pParams) shutdown

                (Just walletNro, _) -> do

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Pool Params"
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine

                    mainLoop (Just walletNro) Nothing shutdown

                (_, _) -> do

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Wallet y Pool Params"
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine

                    mainLoop Nothing Nothing shutdown
        Just 99 -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Balances at the end of the simulation"
            --Monad.void $ MonadIOClass.liftIO P.getLine

            balances <- PABSimulator.currentBalances
            PABSimulator.logBalances @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) balances

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

            shutdown

        _ -> mainLoop walletNro pParams shutdown


testWithPABSimulator :: P.IO ()
testWithPABSimulator  = Monad.void $ PABSimulator.runSimulationWith handlers traceWithPABSimulator

traceWithPABSimulator  ::  MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)))  ()
traceWithPABSimulator = do
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PABServer.startServerDebug

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "********* PAB Server is running *********"
    -- PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
    -- Monad.void $ MonadIOClass.liftIO P.getLine

    let
        master1 = 1
        master2 = 2
        user1 = 3

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "master1 Pkh = " ++ P.show (walletPaymentPubKeyHash master1)
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "master1 PkhAddress = " ++ P.show (walletPaymentPubKeyHashAddress master1)

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "master2 Pkh = " ++ P.show (walletPaymentPubKeyHash master2)
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "master2 PkhAddress = " ++ P.show (walletPaymentPubKeyHashAddress master2)

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "user1 Pkh = " ++ P.show (walletPaymentPubKeyHash user1)
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "user1 PkhAddress = " ++ P.show (walletPaymentPubKeyHashAddress user1)


    mainLoop Nothing Nothing shutdown

