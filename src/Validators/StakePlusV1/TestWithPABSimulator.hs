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

module Validators.StakePlusV1.TestWithPABSimulator where

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
import qualified Playground.Contract                 as PlaygroundContract (IO)
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

import qualified Validators.StakePlusV1.OnChain        as OnChain  
import qualified Validators.StakePlusV1.OnChainHelpers as OnChainHelpers
import qualified Validators.StakePlusV1.OnChainNFT     as OnChainNFT(mintingNFTPolicy)
import qualified Validators.StakePlusV1.Helpers        as Helpers
import qualified Validators.StakePlusV1.PAB            as PAB
import qualified Validators.StakePlusV1.Typos          as T

-- Modulo:

handlers :: PABSimulator.SimulatorEffectHandlers (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)
handlers = PABSimulator.mkSimulatorHandlers  DataDefault.def P.$ MonadFreer.interpret (PABEffectsContractBuiltin.contractHandler PABEffectsContractBuiltin.handleBuiltin)

getWallet :: Integer -> WalletEmulator.Wallet
getWallet = WalletEmulator.knownWallet

walletPaymentPubKeyHash :: Integer -> Ledger.PaymentPubKeyHash
walletPaymentPubKeyHash walletNumber = LedgerCardanoWallet.paymentPubKeyHash (LedgerCardanoWallet.fromWalletNumber $ LedgerCardanoWallet.WalletNumber walletNumber)

walletPaymentPubKeyHashAddress ::  Integer -> LedgerAddressV1.Address
walletPaymentPubKeyHashAddress walletNumber = Ledger.pubKeyHashAddress (walletPaymentPubKeyHash walletNumber) P.Nothing

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



getJSON :: P.String -> PlaygroundContract.IO DataByteString.ByteString
getJSON file = DataByteString.readFile $ path ++ file

writeJSON :: P.String -> DataByteString.ByteString -> PlaygroundContract.IO  ()
writeJSON file   = DataByteString.writeFile $  path ++ file


getAda = do
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Ingrese un monto ADA:"
    numberSrt <- MonadIOClass.liftIO P.getLine
    case TextRead.readMaybe numberSrt of 
        P.Just x -> 
            if x > 0 then return x
            else getAda
        _ -> getAda

getFile :: P.String -> P.String -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) P.String
getFile path ext = do
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Ingrese nombre de archivo:"
    nombre <- MonadIOClass.liftIO P.getLine
    exist <-  MonadIOClass.liftIO $ SystemDirectory.doesFileExist (path ++ nombre ++ ext )  
    if exist then return nombre
    else getFile path ext



isEqWallet :: WalletEmulator.Wallet -> WalletEmulator.Wallet -> Bool
isEqWallet w w' = --w==w'
    --( prettyWalletName  w )== (prettyWalletName  w')
    --( getWalletId $ unWalletId  $ getWalletId w )== (getWalletId  $unWalletId  $ getWalletId w')
    --(WalletEmulator.toBase16 $ WalletEmulator.getWalletId  w) ==(WalletEmulator.toBase16 $  WalletEmulator.getWalletId w')
    --( prettyWalletName  w )== (prettyWalletName  w')
    --( getWalletId $ unWalletId  $ getWalletId w )== (getWalletId  $unWalletId  $ getWalletId w')
    --True
    --BuiltinString  w PlutusTxEq.== BuiltinString  w'
    TxBuiltinsInternal.BuiltinString (WalletEmulator.toBase16 $ WalletEmulator.getWalletId  w) PlutusTxEq.== TxBuiltinsInternal.BuiltinString(WalletEmulator.toBase16 $  WalletEmulator.getWalletId w')
    

fromWallet :: Integer -> WalletEmulator.Entity -> Bool
fromWallet numWallet entity = 
    case entity of 
        WalletEmulator.WalletEntity wallet -> 
            isEqWallet wallet (getWallet numWallet)
            --   (isEqWallet wallet (getWallet 1))    
            -- || (isEqWallet wallet (getWallet 2 ))|| (isEqWallet wallet (getWallet 3)) || (isEqWallet wallet (getWallet 4)) || (isEqWallet wallet (getWallet 5))
            -- unsafeDataAsB (dataToBuiltinData (toConstr (getWalletId (Helpers.fromJust (walletFromEntity entity)))))  == unsafeDataAsB (dataToBuiltinData  (toConstr ((getWalletId (getWallet 1)))))
        _ -> False

fromScript :: T.PoolParams -> WalletEmulator.Entity -> Bool
fromScript pParams entity = 
    case entity of 
        WalletEmulator.ScriptEntity scriptHast -> 
            OnChain.hashValidator pParams == scriptHast    
        _ -> False


walletFromEntity :: WalletEmulator.Entity -> P.Maybe WalletEmulator.Wallet
walletFromEntity entity = 
    case entity of 
        WalletEmulator.WalletEntity wallet -> P.Just wallet
        _ -> P.Nothing

--path2 = "~/source/cardano-falcon-stakepool-devs/cardano-falcon-stakepol-devs-haskell/"
path = ""


elegirWallet ::  P.Maybe Integer -> P.Maybe T.PoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () 
elegirWallet walletNro pParams shutdown = do
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Ingrese una wallet:"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "1 - Master 1"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "2 - Master 2"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "3 - Master 3"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "4 - User 1"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "5 - User 2"
    opcionWallet <- MonadIOClass.liftIO P.getLine

    mainLoop (TextRead.readMaybe opcionWallet)  pParams shutdown

crearPoolParams ::  P.Maybe Integer -> P.Maybe T.PoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () 
crearPoolParams walletNro pParams shutdown = do

    case walletNro of
        P.Just walletNro -> do

            blockchain <- PABSimulator.blockchain

            let
                master1 = 1
                master2 = 2
                master3 = 3
                user1 = 4
                user2 = 5

                uTxOutRefAt = fst <$> getUtxosListInPABSimulator blockchain (walletPaymentPubKeyHashAddress  walletNro)

                poolNFTTxOutRef = head uTxOutRefAt
                idTxOut = LedgerApiV1.txOutRefId poolNFTTxOutRef
                indexTxOut = LedgerApiV1.txOutRefIdx  poolNFTTxOutRef

                poolNFTTokenName =  
                    LedgerValueV1.TokenName (indexTxOut `consByteString`  LedgerApiV1.getTxId  idTxOut  )
                poolNFTCurrencySymbol = Helpers.curSymbol OnChainNFT.mintingNFTPolicy

                poolNFT = LedgerValueV1.assetClass poolNFTCurrencySymbol  poolNFTTokenName

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically

            let

                deadlinePool  = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def (slot+500)

                pParams = T.PoolParams
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
                        T.ppMinimunClaim = 1_000_000
                    }

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "poolNFTTxOutRef: " ++ P.show poolNFTTxOutRef
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "poolNFTTokenName: " ++ P.show poolNFTTokenName
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "poolNFTCurrencySymbol: " ++ P.show poolNFTCurrencySymbol
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "poolNFT: " ++ P.show poolNFT

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Ingrese un nombre para guardar el Pool:"
            opcionPool <- MonadIOClass.liftIO P.getLine
            MonadIOClass.liftIO $ writeJSON ("files/stakePlus/" ++ opcionPool ++ ".json" ) (DataAeson.encode pParams)

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (P.Just walletNro) (P.Just pParams) shutdown

        _  -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop walletNro pParams shutdown

    

elegirPoolParams ::  P.Maybe Integer -> P.Maybe T.PoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () 
elegirPoolParams walletNro pParams shutdown = do

    files <- MonadIOClass.liftIO $ SystemDirectory.getDirectoryContents "files/stakePlus"
            
    -- validFIles <- [file | file  <- files ] 

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Pool Params:"

    mapM_ ( PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)   )  files

    --PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "33" >> 
    opcionPool <- getFile "files/stakePlus/" ".json"
    --opcionPool <- MonadIOClass.liftIO P.getLine

    jsonFile <- MonadIOClass.liftIO $ getJSON ("files/stakePlus/" ++ opcionPool ++ ".json" )

    let
        pParams =  DataAeson.decode jsonFile

    mainLoop walletNro pParams shutdown



crearPool ::  P.Maybe Integer -> P.Maybe T.PoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () 
crearPool walletNro pParams shutdown = do

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

            fund <- getAda

            cMasterCreatePool_Master  <- PABSimulator.activateContract (getWallet  walletNro) (PAB.MasterCreatePool T.MasterCreatePoolParams{
                        T.pmcpPoolParam = pParams,
                        T.pmcpPoolNFTTokenName = poolNFTTokenName,
                        T.pmcpPoolNFTTxOutRef = poolNFTTxOutRef,
                        T.pmcpFund   = fund * 1_000_000
                    })

            PABSimulator.waitUntilFinished  cMasterCreatePool_Master

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (P.Just walletNro) (P.Just pParams) shutdown

        (_, P.Just pParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop P.Nothing (P.Just pParams) shutdown

        (P.Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (P.Just walletNro) P.Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Wallet y Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop P.Nothing P.Nothing shutdown

fundPool ::  P.Maybe Integer -> P.Maybe T.PoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () 
fundPool walletNro pParams shutdown = do
    case (walletNro,  pParams) of
        (P.Just walletNro, P.Just pParams) -> do

            fund <- getAda

            cMasterFundPool_Master  <- PABSimulator.activateContract (getWallet walletNro) (PAB.MasterFundPool T.MasterFundPoolParams{
                    T.pmfpPoolParam =  pParams,
                    T.pmfpFund   = fund * 1_000_000 
                })

            PABSimulator.waitUntilFinished  cMasterFundPool_Master

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine
            mainLoop (P.Just walletNro) (P.Just pParams) shutdown

        (_, P.Just pParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop P.Nothing (P.Just pParams) shutdown

        (P.Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (P.Just walletNro) P.Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Wallet y Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop P.Nothing P.Nothing shutdown

fundAndMergePool ::  P.Maybe Integer -> P.Maybe T.PoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () 
fundAndMergePool walletNro pParams shutdown = do

    case (walletNro,  pParams) of
        (P.Just walletNro, P.Just pParams) -> do
            
            blockchain <- PABSimulator.blockchain    

            let 
                
                uTxOuts =  getUtxosListInPABSimulator blockchain (OnChain.addressValidator pParams)   

                datumFrom utxout = do 
                    P.show $ Helpers.fromJust $ LedgerTxV1.txOutDatum utxout

                formatValues utxoRef =  [P.show val   |  val <- LedgerValueV1.flattenValue $ Helpers.fromJust $ LedgerBlockchain.value blockchain utxoRef ]

                formatUtxoValues = concat [(P.show ( 1 P.+  Helpers.fromJust(DataList.elemIndex (utxoRef, utxout) uTxOuts))): (    "At: " ++ P.show utxoRef):("Datum: " ++  datumFrom utxout):formatValues utxoRef | (utxoRef, utxout) <-  uTxOuts ]

                formatSelected :: [(Integer,Ledger.TxOutRef)]  -> [P.String]  
                formatSelected opciones = concat [ (P.show numOpcion):(P.show utxoRef) :[] | (numOpcion, utxoRef) <-  opciones ]


                selectUtxo :: [(Integer,Ledger.TxOutRef)] -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)))  [(Integer,Ledger.TxOutRef)]
                selectUtxo opciones  = do

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Choose Utxo at Pool To Merge:"

                    --PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "" ++ P.show uTxOutRefAt
                    mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) formatUtxoValues
                    
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Selected:"
                    --mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) (P.show <$> (fst <$> opciones))
                    mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) (formatSelected opciones)
        
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Option (0 to finish):"

                    opcionUtxo <- MonadIOClass.liftIO P.getLine

                    case TextRead.readMaybe opcionUtxo of 
                        P.Just 0 -> 
                            return opciones
                        P.Just x -> do
                            let 
                                new = (x,fst $ uTxOuts!!(x-1)):opciones
                            selectUtxo new
                        _ -> 
                            selectUtxo opciones

            selectedUtxos <- selectUtxo []

            let
                selectedUtxosRef = snd <$> selectedUtxos


            fund <- getAda

            cMasterFundAndMergePool_Master  <- PABSimulator.activateContract (getWallet walletNro) (PAB.MasterFundAndMergePool T.MasterFundAndMergePoolParams{
                    T.pmfampPoolParam =  pParams,
                    T.pmfampUtxoToMerge = selectedUtxosRef,
                    T.pmfampFund   = fund * 1_000_000
                })

            PABSimulator.waitUntilFinished  cMasterFundAndMergePool_Master

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine
            mainLoop (P.Just walletNro) (P.Just pParams) shutdown

        (_, P.Just pParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop P.Nothing (P.Just pParams) shutdown

        (P.Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (P.Just walletNro) P.Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Wallet y Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop P.Nothing P.Nothing shutdown

investInPool ::  P.Maybe Integer -> P.Maybe T.PoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () 
investInPool walletNro pParams shutdown = do
    case (walletNro,  pParams) of
        (P.Just walletNro, P.Just pParams) -> do
            
            blockchain <- PABSimulator.blockchain

            let
                uTxOutRefAt = fst <$> getUtxosListInPABSimulator blockchain (walletPaymentPubKeyHashAddress walletNro)

                user1NFTTxOutRef = head uTxOutRefAt
                user1IdTxOut = LedgerApiV1.txOutRefId user1NFTTxOutRef
                user1IndexTxOut = LedgerApiV1.txOutRefIdx  user1NFTTxOutRef

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

            invest <- getAda

            let 

                userInvestParams = T.UserInvestParams{
                        T.puiPoolParam =  pParams,
                        T.puiUserNFTTokenName = user1NFTTokenName,
                        T.puiUserNFTTxOutRef = user1NFTTxOutRef,
                        T.puiCreatedAt = createdAtInvest,
                        T.puiDeadline = deadlineInvest,
                        T.puiInvest   = invest * 1_000_000
                    }



            cUserInvest_User  <- PABSimulator.activateContract (getWallet walletNro) (PAB.UserInvest userInvestParams )

            PABSimulator.waitUntilFinished cUserInvest_User

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Ingrese un nombre para guardar el Invest Params:"
            nombreInvest <- MonadIOClass.liftIO P.getLine
            MonadIOClass.liftIO $ writeJSON ("files/stakePlus/invest-" ++ nombreInvest ++ ".json" ) (DataAeson.encode userInvestParams)

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop  (P.Just walletNro) (P.Just pParams) shutdown

        (_, P.Just pParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop P.Nothing (P.Just pParams) shutdown

        (P.Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (P.Just walletNro) P.Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Wallet y Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop P.Nothing P.Nothing shutdown

claimRewardsFromPool ::  P.Maybe Integer -> P.Maybe T.PoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () 
claimRewardsFromPool walletNro pParams shutdown = do
    case (walletNro,  pParams) of
        (P.Just walletNro, P.Just pParams) -> do
            
            files <- MonadIOClass.liftIO $ SystemDirectory.getDirectoryContents "files/stakePlus"
            
            -- validFIles <- [file | file  <- files ] 

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Invest Params:"

            mapM_ ( PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)   )  files

            --PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "33" >> 

            nombreInvest <- getFile "files/stakePlus/invest-" ".json"
            --nombreInvest <- MonadIOClass.liftIO P.getLine

            jsonFile <- MonadIOClass.liftIO $ getJSON ("files/stakePlus/invest-" ++ nombreInvest ++ ".json" )

            let
                userInvestParams =  Helpers.fromJust(  DataAeson.decode jsonFile) :: T.UserInvestParams

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically

            invest <- getAda

            let
                
                userGetRewardsParams = T.UserGetRewardsParams{ 
                        T.pugrPoolParam = pParams,
                        T.pugrUserNFTTokenName = T.puiUserNFTTokenName userInvestParams,
                        T.pugrUserNFTTxOutRef = T.puiUserNFTTxOutRef userInvestParams,
                        T.pugrClaim    = invest* 1_000_000 

                    } 

            cUserGetRewards_User  <- PABSimulator.activateContract (getWallet walletNro) (PAB.UserGetRewards userGetRewardsParams )

            PABSimulator.waitUntilFinished cUserGetRewards_User

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop  (P.Just walletNro) (P.Just pParams) shutdown

        (_, P.Just pParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop P.Nothing (P.Just pParams) shutdown

        (P.Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (P.Just walletNro) P.Nothing shutdown

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Wallet y Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop P.Nothing P.Nothing shutdown





balances ::  P.Maybe Integer -> P.Maybe T.PoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () 
balances walletNro pParams shutdown = do
    let
        master1 = 1
        master2 = 2
        master3 = 3
        user1 = 4
        user2 = 5

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Balances:"
    --Monad.void $ MonadIOClass.liftIO P.getLine

    balances <- PABSimulator.currentBalances

    -- mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) ["Master 1: " ++ P.show (WalletEmulator.getWalletId (Helpers.fromJust (walletFromEntity entity)) ) ++ " " ++  P.show value | (entity, value) <-  DataMap.toList balances, fromWallet master1 entity ]
    -- mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) ["Master 2: " ++ P.show (WalletEmulator.getWalletId (Helpers.fromJust (walletFromEntity entity)) ) ++ " " ++  P.show value | (entity, value) <-  DataMap.toList balances, fromWallet master2 entity ]
    -- mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) ["Master 3: " ++ P.show (WalletEmulator.getWalletId (Helpers.fromJust (walletFromEntity entity)) ) ++ " " ++  P.show value | (entity, value) <-  DataMap.toList balances, fromWallet master3 entity ]
    -- mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) ["User 1: " ++ P.show (WalletEmulator.getWalletId (Helpers.fromJust (walletFromEntity entity)) ) ++ " " ++  P.show value | (entity, value) <-  DataMap.toList balances, fromWallet user1 entity ]
    -- mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) ["User 2: " ++ P.show (WalletEmulator.getWalletId (Helpers.fromJust (walletFromEntity entity)) ) ++ " " ++  P.show value | (entity, value) <-  DataMap.toList balances, fromWallet user2 entity ]
    
    mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) ["Master 1: " ++ P.show (walletPaymentPubKeyHash master1 ) ++ " " ++  P.show value | (entity, value) <-  DataMap.toList balances, fromWallet master1 entity ]
    mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) ["Master 2: " ++ P.show (walletPaymentPubKeyHash master2 ) ++ " " ++  P.show value | (entity, value) <-  DataMap.toList balances, fromWallet master2 entity ]
    mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) ["Master 3: " ++ P.show (walletPaymentPubKeyHash master3  ) ++ " " ++  P.show value | (entity, value) <-  DataMap.toList balances, fromWallet master3 entity ]
    mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) ["User 1: " ++ P.show   (walletPaymentPubKeyHash user1 ) ++ " " ++  P.show value | (entity, value) <-  DataMap.toList balances, fromWallet user1 entity ]
    mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) ["User 2: " ++ P.show   (walletPaymentPubKeyHash user2  ) ++ " " ++  P.show value | (entity, value) <-  DataMap.toList balances, fromWallet user2 entity ]
    
    case pParams of
        P.Just pParams -> do
            mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) ["Script: " ++ P.show (OnChain.hashValidator pParams) ++ " " ++  P.show value | (entity, value) <-  DataMap.toList balances, fromScript pParams  entity ]
    
        _  -> 
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "" 

    slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
    let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "slot: " ++  P.show slot
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "time: " ++  P.show posixTime
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

    


utxoAtWallet ::  P.Maybe Integer -> P.Maybe T.PoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () 
utxoAtWallet walletNro pParams shutdown = do
    case walletNro of
        P.Just walletNro -> do

            blockchain <- PABSimulator.blockchain

            let 

                uTxOutRefAt = fst <$>  getUtxosListInPABSimulator blockchain (walletPaymentPubKeyHashAddress walletNro)    

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Utxo at Wallet"

            --PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "" ++ P.show uTxOutRefAt

            mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) ["Values at: " ++ P.show utxo ++ " " ++  P.show (LedgerBlockchain.value blockchain utxo) | utxo <-  uTxOutRefAt ]
            
            

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop (P.Just walletNro) pParams shutdown 

        _  -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop walletNro pParams shutdown


utxoAtScript ::  P.Maybe Integer -> P.Maybe T.PoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () 
utxoAtScript walletNro pParams shutdown = do

    case pParams of
        P.Just pParams -> do
            blockchain <- PABSimulator.blockchain

            let 

                uTxOuts =  getUtxosListInPABSimulator blockchain (OnChain.addressValidator pParams)   

                datumFrom utxout = do

                    -- let
                    --     dat = getDatumFromTxOut utxout 

                    -- if isJust dat then
                    --     if Helpers.datumIsPoolState dat then
                    --         Helpers.getPoolStateFromMaybeDatum dat   
                    --     else
                    --         if Helpers.datumIsUserState dat then
                    --             Helpers.getUserStateFromMaybeDatum dat   
                    --         else 
                    --             P.Nothing
                    -- else 
                    --     P.Nothing

                    P.show $ Helpers.fromJust $ LedgerTxV1.txOutDatum utxout

                formatValues utxoRef =  [P.show val   |  val <- LedgerValueV1.flattenValue $ Helpers.fromJust $ LedgerBlockchain.value blockchain utxoRef ]

                formatUtxoValues = concat [(P.show ( 1 P.+  Helpers.fromJust(DataList.elemIndex (utxoRef, utxout) uTxOuts))): (    "At: " ++ P.show utxoRef):("Datum: " ++  datumFrom utxout):formatValues utxoRef | (utxoRef, utxout) <-  uTxOuts ]


            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Utxo at Script"
            
            --PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "" ++ P.show uTxOutRefAt
            mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)) formatUtxoValues
            

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop walletNro (P.Just pParams) shutdown 

        _  -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Elija Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop walletNro pParams shutdown

mainLoop :: P.Maybe Integer -> P.Maybe T.PoolParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts))) ()
mainLoop walletNro pParams shutdown = do

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "OPERACIONES:"

    case walletNro of
        P.Nothing ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "1 - Elegir Wallet"
        P.Just walletNro ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "1 - Elegir Wallet (" ++ P.show walletNro ++ ")"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "21 - Crear Pool Params"

    case pParams of
        P.Nothing ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "22 - Elegir Pool Params"
        P.Just pParams ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "22 - Elegir Pool Params(" ++ P.show (T.ppPoolNFT pParams ) ++ ")"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "31 - Crear Pool"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "32 - Fund Pool"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "33 - Fund And Merge Pool"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "34 - Get Back Fund"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "41 - Invertir en Pool"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "42 - Claim Rewards de Pool"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "43 - Get Back Invest"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "81 - Balances"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "82 - Utxo at Wallet"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "83 - Utxo at Script"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) ""
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "99 - Salir"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Ingrese una opción:"
    opcion <- MonadIOClass.liftIO P.getLine

    case TextRead.readMaybe opcion of

        P.Just 1 -> do

            elegirWallet walletNro pParams shutdown

        P.Just 21 -> do
            
            crearPoolParams walletNro pParams shutdown

        P.Just 22 -> do

            elegirPoolParams walletNro pParams shutdown

        P.Just 31 -> do
            
            crearPool walletNro pParams shutdown

        P.Just 32 -> do
            
            fundPool walletNro pParams shutdown

        P.Just 33 -> do
            
            fundAndMergePool walletNro pParams shutdown

        P.Just 41 -> do
            
            investInPool walletNro pParams shutdown

        P.Just 42 -> do
            
            claimRewardsFromPool walletNro pParams shutdown


        P.Just 81 -> do
            
            balances walletNro pParams shutdown

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoop walletNro pParams shutdown

        P.Just 82 -> do
            
            utxoAtWallet walletNro pParams shutdown

        P.Just 83 -> do
            
            utxoAtScript walletNro pParams shutdown


        P.Just 99 -> do
            
            balances walletNro pParams shutdown

            shutdown

        _ -> mainLoop walletNro pParams shutdown





test2 :: PlaygroundContract.IO ()
test2  = Monad.void $ PABSimulator.runSimulationWith handlers myTrace2

myTrace2  ::  MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts)))  ()
myTrace2 = do
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PABServer.startServerDebug

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "********* PAB Server is running *********"
    -- PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) "Press return to continue..."
    -- Monad.void $ MonadIOClass.liftIO P.getLine

    -- let
    --     master1 = 1
    --     master2 = 2
    --     master3 = 3
    --     user1 = 4
    --     user2 = 5

    -- PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "master1 Pkh = " ++ P.show (walletPaymentPubKeyHash master1)
    -- PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "master1 PkhAddress = " ++ P.show (walletPaymentPubKeyHashAddress master1)

    -- PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "master2 Pkh = " ++ P.show (walletPaymentPubKeyHash master2)
    -- PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "master2 PkhAddress = " ++ P.show (walletPaymentPubKeyHashAddress master2)

    -- PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "user1 Pkh = " ++ P.show (walletPaymentPubKeyHash user1)
    -- PABSimulator.logString @(PABEffectsContractBuiltin.Builtin PAB.ValidatorContracts) $ "user1 PkhAddress = " ++ P.show (walletPaymentPubKeyHashAddress user1)


    mainLoop P.Nothing P.Nothing shutdown





