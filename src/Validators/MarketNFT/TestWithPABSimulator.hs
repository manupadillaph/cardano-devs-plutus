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

--{-# LANGUAGE DataKinds          #-}
--{-# LANGUAGE DeriveAnyClass     #-}
--{-# LANGUAGE DerivingStrategies #-}
--{-# LANGUAGE FlexibleContexts   #-}
--{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
--{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
--{-# LANGUAGE TypeFamilies       #-}
--{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores    #-}

--{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module  Validators.MarketNFT.TestWithPABSimulator
    where

     
import qualified Control.Concurrent.STM              as STM (atomically)


import qualified Control.Monad.IO.Class              as CMIOC (MonadIO (..))
import qualified Control.Monad                       as CMON (void)
import qualified Control.Monad.Freer                 as CMON (interpret)

import qualified Data.Aeson                          as DA (encode,decode)
import qualified Data.ByteString.Lazy                as B
import qualified Data.Default                        (def)
import qualified Data.Map                            as Map
import qualified Data.List                           as DL
import qualified Data.Time.Clock                     as DTC (secondsToNominalDiffTime)
import qualified Data.Time.Clock.POSIX               as DTCP (posixSecondsToUTCTime)
import qualified Data.Time.Format                    as DTF (defaultTimeLocale,formatTime)
import qualified Data.Fixed                          (Pico, Fixed ( MkFixed ))
 
import qualified Control.Monad.Freer.Internal        as CMFI (Eff)
 
import qualified Ledger                              hiding (singleton)
import qualified Ledger.Address                      as LA (Address, PaymentPubKeyHash, pubKeyHashAddress)
import qualified Ledger.Blockchain                   as LB (value)
import qualified Ledger.CardanoWallet                as CW
import qualified Ledger.TimeSlot                     as TimeSlot

import qualified Playground.Contract                 as PC (IO)
import qualified Prelude                             as HASKELL
 
import qualified Plutus.PAB.Core                     as PPC (PABEffects)
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin (Builtin, BuiltinHandler(contractHandler),handleBuiltin)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
 
import qualified Plutus.V1.Ledger.Value              as VALUE
 
import qualified PlutusTx.Builtins.Internal          as PBI hiding (head,consByteString)
import qualified PlutusTx.Eq                         as Eq
import           PlutusTx.Prelude                    as PLUTUS hiding (unless)
 
import qualified System.Directory                    as SD 
 
import qualified Text.Read                           as TR (readMaybe)
 
import qualified Wallet.Emulator.Wallet              as WEW    


--import Ledger (Address, Slot, TxId, TxOutRef)

--import qualified PlutusTx
--import PlutusTx.Builtins (mkB,unsafeDataAsB,dataToBuiltinData)
--import qualified Cardano.Wallet
--import Wallet.Emulator.Chain qualified as Chain
--import Wallet.Emulator.Chain (ChainControlEffect, ChainState)
--import Wallet.Emulator.Wallet
--import Data.Time.Format.ISO8601
--import         Data.String.Utils
--import System.Locale
--import qualified Plutus.Trace.Emulator  as Trace
--import           Control.Lens
--import           Plutus.Contract
--import           Data.Text            (pack, Text)

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
--import           Data.CMON.void
--import           Control.Monad.IO.Class              (MonadIO (..))
--import           Data.Aeson                          (Result (..), fromJSON)

--import qualified Data.Monoid                         as Monoid
--import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)

--import qualified Plutus.V1.Ledger.Slot               as Slot

--Import Internos
import           Validators.MarketNFT.OffChain
import           Validators.MarketNFT.Typos
import           Validators.MarketNFT.OffChainHelpers
import           Validators.MarketNFT.OnChain 
import           Validators.MarketNFT.OnChainHelpers 
import           Validators.MarketNFT.OnChainNFT     (mintingNFTPolicy)
import           Validators.MarketNFT.Helpers
import           Validators.MarketNFT.PAB

handlers :: Simulator.SimulatorEffectHandlers (Builtin.Builtin ValidatorContracts)
handlers = Simulator.mkSimulatorHandlers  Data.Default.def HASKELL.$ CMON.interpret (Builtin.contractHandler Builtin.handleBuiltin)

getWallet :: Integer -> WEW.Wallet
getWallet = WEW.knownWallet

walletPaymentPubKeyHash :: Integer -> LA.PaymentPubKeyHash
walletPaymentPubKeyHash walletNumber = CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber walletNumber)

walletPaymentPubKeyHashAddress ::  Integer -> Ledger.Address
walletPaymentPubKeyHashAddress walletNumber = LA.pubKeyHashAddress (walletPaymentPubKeyHash walletNumber) Nothing

getUtxosListInPABSimulator :: Ledger.Blockchain -> Ledger.Address ->  [(Ledger.TxOutRef, Ledger.TxOut)]
getUtxosListInPABSimulator blockchain addr =  do
    let

        unspentOutputList = Ledger.unspentOutputs blockchain

        utxos =  [(txOutRef, txOut)  | (txOutRef, txOut)    <- Map.toList  unspentOutputList , Ledger.txOutAddress  txOut == addr]

    utxos


getFormatTime :: Ledger.POSIXTime -> HASKELL.String
getFormatTime posixTime = do
    let
        milisegundosFixedPico :: Data.Fixed.Pico
        milisegundosFixedPico = Data.Fixed.MkFixed  (Ledger.getPOSIXTime posixTime * 1000000000)
        seconds = DTC.secondsToNominalDiffTime milisegundosFixedPico

    DTF.formatTime DTF.defaultTimeLocale  "%c" $ DTCP.posixSecondsToUTCTime seconds
    --iso8601Show $ posixSecondsToUTCTime $ seconds



getJSON :: HASKELL.String ->  PC.IO B.ByteString
getJSON file = B.readFile $ path ++ file

writeJSON :: HASKELL.String -> B.ByteString -> PC.IO  ()
writeJSON file   = B.writeFile $  path ++ file


getAda = do
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "Ingrese un monto ADA:"
    numberSrt <- CMIOC.liftIO HASKELL.getLine
    case TR.readMaybe numberSrt of 
        Just x -> 
            if x > 0 then return x
            else getAda
        _ -> getAda

getFile :: HASKELL.String -> HASKELL.String ->  CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) HASKELL.String
getFile path ext = do
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "Ingrese nombre de archivo:"
    nombre <- CMIOC.liftIO HASKELL.getLine
    exist <-  CMIOC.liftIO $ SD.doesFileExist (path ++ nombre ++ ext )  
    if exist then return nombre
    else getFile path ext



isEqWallet :: WEW.Wallet -> WEW.Wallet -> Bool
isEqWallet w w' = --w==w'
    --( prettyWalletName  w )== (prettyWalletName  w')
    --( getWalletId $ unWalletId  $ getWalletId w )== (getWalletId  $unWalletId  $ getWalletId w')
    --(WEW.toBase16 $ WEW.getWalletId  w) ==(WEW.toBase16 $  WEW.getWalletId w')
    --( prettyWalletName  w )== (prettyWalletName  w')
    --( getWalletId $ unWalletId  $ getWalletId w )== (getWalletId  $unWalletId  $ getWalletId w')
    --True
    --BuiltinString  w Eq.== BuiltinString  w'
    PBI.BuiltinString (WEW.toBase16 $ WEW.getWalletId  w) Eq.== PBI.BuiltinString(WEW.toBase16 $  WEW.getWalletId w')
    

fromWallet :: Integer -> WEW.Entity -> Bool
fromWallet numWallet entity = 
    case entity of 
        WEW.WalletEntity wallet -> 
            isEqWallet wallet (getWallet numWallet)
            --   (isEqWallet wallet (getWallet 1))    
            -- || (isEqWallet wallet (getWallet 2 ))|| (isEqWallet wallet (getWallet 3)) || (isEqWallet wallet (getWallet 4)) || (isEqWallet wallet (getWallet 5))
            -- unsafeDataAsB (dataToBuiltinData (toConstr (getWalletId (fromJust (walletFromEntity entity)))))  == unsafeDataAsB (dataToBuiltinData  (toConstr ((getWalletId (getWallet 1)))))
        _ -> False

fromScript :: PoolParams -> WEW.Entity -> Bool
fromScript pParams entity = 
    case entity of 
        WEW.ScriptEntity scriptHast -> 
            hashValidator pParams == scriptHast    
        _ -> False


walletFromEntity :: WEW.Entity -> Maybe WEW.Wallet
walletFromEntity entity = 
    case entity of 
        WEW.WalletEntity wallet -> Just wallet
        _ -> Nothing

--path2 = "~/source/cardano-falcon-stakepool-devs/cardano-falcon-stakepol-devs-haskell/"
path = ""


elegirWallet ::  Maybe Integer -> Maybe PoolParams ->  CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () -> CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () 
elegirWallet walletNro pParams shutdown = do
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "Ingrese una wallet:"

    Simulator.logString @(Builtin.Builtin ValidatorContracts) "1 - Master 1"
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "2 - Master 2"
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "3 - Master 3"
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "4 - User 1"
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "5 - User 2"
    opcionWallet <- CMIOC.liftIO HASKELL.getLine

    mainLoop (TR.readMaybe opcionWallet)  pParams shutdown

crearPoolParams ::  Maybe Integer -> Maybe PoolParams ->  CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () -> CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () 
crearPoolParams walletNro pParams shutdown = do

    case walletNro of
        Just walletNro -> do

            blockchain <- Simulator.blockchain

            let
                master1 = 1
                master2 = 2
                master3 = 3
                user1 = 4
                user2 = 5

                uTxOutRefAt = fst <$> getUtxosListInPABSimulator blockchain (walletPaymentPubKeyHashAddress  walletNro)

                poolNFTTxOutRef = head uTxOutRefAt
                idTxOut = Ledger.txOutRefId poolNFTTxOutRef
                indexTxOut = Ledger.txOutRefIdx  poolNFTTxOutRef

                poolNFTTokenName =  VALUE.TokenName (indexTxOut `consByteString`  Ledger.getTxId  idTxOut  )
                poolNFTCurrencySymbol = curSymbol mintingNFTPolicy

                poolNFT = VALUE.assetClass poolNFTCurrencySymbol  poolNFTTokenName

            slot <- Simulator.currentSlot >>= CMIOC.liftIO . STM.atomically

            let

                deadlinePool  = TimeSlot.slotToEndPOSIXTime Data.Default.def (slot+500)

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
                        ppMinimunClaim = 1_000_000
                    }

            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "poolNFTTxOutRef: " ++ HASKELL.show poolNFTTxOutRef
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "poolNFTTokenName: " ++ HASKELL.show poolNFTTokenName
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "poolNFTCurrencySymbol: " ++ HASKELL.show poolNFTCurrencySymbol
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "poolNFT: " ++ HASKELL.show poolNFT

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Ingrese un nombre para guardar el Pool:"
            opcionPool <- CMIOC.liftIO HASKELL.getLine
            CMIOC.liftIO $ writeJSON ("files/stakePlus/" ++ opcionPool ++ ".json" ) (DA.encode pParams)

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop (Just walletNro) (Just pParams) shutdown

        _  -> do

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Elija Wallet"
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop walletNro pParams shutdown

    

elegirPoolParams ::  Maybe Integer -> Maybe PoolParams ->  CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () -> CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () 
elegirPoolParams walletNro pParams shutdown = do

    files <- CMIOC.liftIO $ SD.getDirectoryContents "files/stakePlus"
            
    -- validFIles <- [file | file  <- files ] 

    Simulator.logString @(Builtin.Builtin ValidatorContracts) "Pool Params:"

    mapM_ ( Simulator.logString @(Builtin.Builtin ValidatorContracts)   )  files

    --Simulator.logString @(Builtin.Builtin ValidatorContracts) "33" >> 
    opcionPool <- getFile "files/stakePlus/" ".json"
    --opcionPool <- CMIOC.liftIO HASKELL.getLine

    jsonFile <- CMIOC.liftIO $ getJSON ("files/stakePlus/" ++ opcionPool ++ ".json" )

    let
        pParams =  DA.decode jsonFile

    mainLoop walletNro pParams shutdown



crearPool ::  Maybe Integer -> Maybe PoolParams ->  CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () -> CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () 
crearPool walletNro pParams shutdown = do

    case (walletNro,  pParams) of
        (Just walletNro, Just pParams) -> do
            let 

                poolNFT = ppPoolNFT pParams

                (_, poolNFTTokenName) = VALUE.unAssetClass poolNFT   
                
                -- idTxOut = txOutRefId poolNFTTxOutRef
                -- indexTxOut = txOutRefIdx  poolNFTTxOutRef

                -- poolNFTTokenName =  TokenName (indexTxOut `consByteString`  getTxId  idTxOut  )

                -- poolNFTTxOutRef = TxOutRef {
                --         txOutRefId  = unTokenName poolNFTTokenName ,
                --         txOutRefIdx  = unTokenName poolNFTTokenName ,
                --     }

                poolNFTTxOutRef = ppPoolNFTTxOutRef pParams

            fund <- getAda

            cMasterCreatePool_Master  <- Simulator.activateContract (getWallet  walletNro) (MasterCreatePool MasterCreatePoolParams{
                        pmcpPoolParam = pParams,
                        pmcpPoolNFTTokenName = poolNFTTokenName,
                        pmcpPoolNFTTxOutRef = poolNFTTxOutRef,
                        pmcpFund   = fund * 1_000_000
                    })

            Simulator.waitUntilFinished  cMasterCreatePool_Master

            slot <- Simulator.currentSlot >>= CMIOC.liftIO . STM.atomically
            let posixTime = TimeSlot.slotToEndPOSIXTime Data.Default.def slot

            Simulator.logString @(Builtin.Builtin ValidatorContracts) ""
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "slot: " ++  HASKELL.show slot
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "time: " ++  HASKELL.show posixTime
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop (Just walletNro) (Just pParams) shutdown

        (_, Just pParams) -> do

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Elija Wallet"
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop Nothing (Just pParams) shutdown

        (Just walletNro, _) -> do

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Elija Pool Params"
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop (Just walletNro) Nothing shutdown

        (_, _) -> do

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Elija Wallet y Pool Params"
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop Nothing Nothing shutdown

fundPool ::  Maybe Integer -> Maybe PoolParams ->  CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () -> CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () 
fundPool walletNro pParams shutdown = do
    case (walletNro,  pParams) of
        (Just walletNro, Just pParams) -> do

            fund <- getAda

            cMasterFundPool_Master  <- Simulator.activateContract (getWallet walletNro) (MasterFundPool MasterFundPoolParams{
                    pmfpPoolParam =  pParams,
                    pmfpFund   = fund * 1_000_000 
                })

            Simulator.waitUntilFinished  cMasterFundPool_Master

            slot <- Simulator.currentSlot >>= CMIOC.liftIO . STM.atomically
            let posixTime = TimeSlot.slotToEndPOSIXTime Data.Default.def slot

            Simulator.logString @(Builtin.Builtin ValidatorContracts) ""
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "slot: " ++  HASKELL.show slot
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "time: " ++  HASKELL.show posixTime
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine
            mainLoop (Just walletNro) (Just pParams) shutdown

        (_, Just pParams) -> do

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Elija Wallet"
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop Nothing (Just pParams) shutdown

        (Just walletNro, _) -> do

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Elija Pool Params"
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop (Just walletNro) Nothing shutdown

        (_, _) -> do

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Elija Wallet y Pool Params"
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop Nothing Nothing shutdown

fundAndMergePool ::  Maybe Integer -> Maybe PoolParams ->  CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () -> CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () 
fundAndMergePool walletNro pParams shutdown = do

    case (walletNro,  pParams) of
        (Just walletNro, Just pParams) -> do
            
            blockchain <- Simulator.blockchain    

            let 
                
                uTxOuts =  getUtxosListInPABSimulator blockchain (addressValidator pParams)   

                datumFrom utxout = do 
                    HASKELL.show $ fromJust $ Ledger.txOutDatum utxout

                formatValues utxoRef =  [HASKELL.show val   |  val <- VALUE.flattenValue $ fromJust $ LB.value blockchain utxoRef ]

                formatUtxoValues = concat [(HASKELL.show ( 1 HASKELL.+  fromJust(DL.elemIndex (utxoRef, utxout) uTxOuts))): (    "At: " ++ HASKELL.show utxoRef):("Datum: " ++  datumFrom utxout):formatValues utxoRef | (utxoRef, utxout) <-  uTxOuts ]

                formatSelected :: [(Integer,Ledger.TxOutRef)]  -> [HASKELL.String]  
                formatSelected opciones = concat [ (HASKELL.show numOpcion):(HASKELL.show utxoRef) :[] | (numOpcion, utxoRef) <-  opciones ]


                selectUtxo :: [(Integer,Ledger.TxOutRef)] -> CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts)))  [(Integer,Ledger.TxOutRef)]
                selectUtxo opciones  = do

                    Simulator.logString @(Builtin.Builtin ValidatorContracts) ""
                    Simulator.logString @(Builtin.Builtin ValidatorContracts) "Choose Utxo at Pool To Merge:"

                    --Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "" ++ HASKELL.show uTxOutRefAt
                    mapM_ (Simulator.logString @(Builtin.Builtin ValidatorContracts)) formatUtxoValues
                    
                    Simulator.logString @(Builtin.Builtin ValidatorContracts) "Selected:"
                    --mapM_ (Simulator.logString @(Builtin.Builtin ValidatorContracts)) (HASKELL.show <$> (fst <$> opciones))
                    mapM_ (Simulator.logString @(Builtin.Builtin ValidatorContracts)) (formatSelected opciones)
        
                    Simulator.logString @(Builtin.Builtin ValidatorContracts) "Option (0 to finish):"

                    opcionUtxo <- CMIOC.liftIO HASKELL.getLine

                    case TR.readMaybe opcionUtxo of 
                        Just 0 -> 
                            return opciones
                        Just x -> do
                            let 
                                new = (x,fst $ uTxOuts!!(x-1)):opciones
                            selectUtxo new
                        _ -> 
                            selectUtxo opciones

            selectedUtxos <- selectUtxo []

            let
                selectedUtxosRef = snd <$> selectedUtxos


            fund <- getAda

            cMasterFundAndMergePool_Master  <- Simulator.activateContract (getWallet walletNro) (MasterFundAndMergePool MasterFundAndMergePoolParams{
                    pmfampPoolParam =  pParams,
                    pmfampUtxoToMerge = selectedUtxosRef,
                    pmfampFund   = fund * 1_000_000
                })

            Simulator.waitUntilFinished  cMasterFundAndMergePool_Master

            slot <- Simulator.currentSlot >>= CMIOC.liftIO . STM.atomically
            let posixTime = TimeSlot.slotToEndPOSIXTime Data.Default.def slot

            Simulator.logString @(Builtin.Builtin ValidatorContracts) ""

            Simulator.logString @(Builtin.Builtin ValidatorContracts) ""
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "slot: " ++  HASKELL.show slot
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "time: " ++  HASKELL.show posixTime
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine
            mainLoop (Just walletNro) (Just pParams) shutdown

        (_, Just pParams) -> do

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Elija Wallet"
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop Nothing (Just pParams) shutdown

        (Just walletNro, _) -> do

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Elija Pool Params"
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop (Just walletNro) Nothing shutdown

        (_, _) -> do

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Elija Wallet y Pool Params"
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop Nothing Nothing shutdown

investInPool ::  Maybe Integer -> Maybe PoolParams ->  CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () -> CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () 
investInPool walletNro pParams shutdown = do
    case (walletNro,  pParams) of
        (Just walletNro, Just pParams) -> do
            
            blockchain <- Simulator.blockchain

            let
                uTxOutRefAt = fst <$> getUtxosListInPABSimulator blockchain (walletPaymentPubKeyHashAddress walletNro)

                user1NFTTxOutRef = head uTxOutRefAt
                user1IdTxOut = Ledger.txOutRefId user1NFTTxOutRef
                user1IndexTxOut = Ledger.txOutRefIdx  user1NFTTxOutRef

                user1NFTTokenName =  VALUE.TokenName (user1IndexTxOut `consByteString`  Ledger.getTxId  user1IdTxOut  )
                user1NFTCurrencySymbol = curSymbol mintingNFTPolicy

                userNFT = VALUE.assetClass user1NFTCurrencySymbol user1NFTTokenName

            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "user1NFTTxOutRef: " ++ HASKELL.show user1NFTTxOutRef
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "user1NFTTokenName: " ++ HASKELL.show user1NFTTokenName
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "user1NFTCurrencySymbol: " ++ HASKELL.show user1NFTCurrencySymbol
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "userNFT: " ++ HASKELL.show userNFT

            slot <- Simulator.currentSlot >>= CMIOC.liftIO . STM.atomically

            let

                createdAtInvest = TimeSlot.slotToEndPOSIXTime Data.Default.def slot
                deadlineInvest = TimeSlot.slotToEndPOSIXTime Data.Default.def (slot+50)

            invest <- getAda

            let 

                userInvestParams = UserInvestParams{
                        puiPoolParam =  pParams,
                        puiUserNFTTokenName = user1NFTTokenName,
                        puiUserNFTTxOutRef = user1NFTTxOutRef,
                        puiCreatedAt = createdAtInvest,
                        puiDeadline = deadlineInvest,
                        puiInvest   = invest * 1_000_000
                    }



            cUserInvest_User  <- Simulator.activateContract (getWallet walletNro) (UserInvest userInvestParams )

            Simulator.waitUntilFinished cUserInvest_User

            slot <- Simulator.currentSlot >>= CMIOC.liftIO . STM.atomically
            let posixTime = TimeSlot.slotToEndPOSIXTime Data.Default.def slot

            Simulator.logString @(Builtin.Builtin ValidatorContracts) ""
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "slot: " ++  HASKELL.show slot
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "time: " ++  HASKELL.show posixTime
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Ingrese un nombre para guardar el Invest Params:"
            nombreInvest <- CMIOC.liftIO HASKELL.getLine
            CMIOC.liftIO $ writeJSON ("files/stakePlus/invest-" ++ nombreInvest ++ ".json" ) (DA.encode userInvestParams)

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop  (Just walletNro) (Just pParams) shutdown

        (_, Just pParams) -> do

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Elija Wallet"
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop Nothing (Just pParams) shutdown

        (Just walletNro, _) -> do

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Elija Pool Params"
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop (Just walletNro) Nothing shutdown

        (_, _) -> do

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Elija Wallet y Pool Params"
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop Nothing Nothing shutdown

claimRewardsFromPool ::  Maybe Integer -> Maybe PoolParams ->  CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () -> CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () 
claimRewardsFromPool walletNro pParams shutdown = do
    case (walletNro,  pParams) of
        (Just walletNro, Just pParams) -> do
            
            files <- CMIOC.liftIO $ SD.getDirectoryContents "files/stakePlus"
            
            -- validFIles <- [file | file  <- files ] 

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Invest Params:"

            mapM_ ( Simulator.logString @(Builtin.Builtin ValidatorContracts)   )  files

            --Simulator.logString @(Builtin.Builtin ValidatorContracts) "33" >> 

            nombreInvest <- getFile "files/stakePlus/invest-" ".json"
            --nombreInvest <- CMIOC.liftIO HASKELL.getLine

            jsonFile <- CMIOC.liftIO $ getJSON ("files/stakePlus/invest-" ++ nombreInvest ++ ".json" )

            let
                userInvestParams =  fromJust(  DA.decode jsonFile) :: UserInvestParams

            slot <- Simulator.currentSlot >>= CMIOC.liftIO . STM.atomically

            invest <- getAda

            let
                
                userGetRewardsParams = UserGetRewardsParams{ 
                        pugrPoolParam = pParams,
                        pugrUserNFTTokenName = puiUserNFTTokenName userInvestParams,
                        pugrUserNFTTxOutRef = puiUserNFTTxOutRef userInvestParams,
                        pugrClaim    = invest* 1_000_000 

                    } 

            cUserGetRewards_User  <- Simulator.activateContract (getWallet walletNro) (UserGetRewards userGetRewardsParams )

            Simulator.waitUntilFinished cUserGetRewards_User

            slot <- Simulator.currentSlot >>= CMIOC.liftIO . STM.atomically
            let posixTime = TimeSlot.slotToEndPOSIXTime Data.Default.def slot

            Simulator.logString @(Builtin.Builtin ValidatorContracts) ""
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "slot: " ++  HASKELL.show slot
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "time: " ++  HASKELL.show posixTime
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop  (Just walletNro) (Just pParams) shutdown

        (_, Just pParams) -> do

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Elija Wallet"
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop Nothing (Just pParams) shutdown

        (Just walletNro, _) -> do

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Elija Pool Params"
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop (Just walletNro) Nothing shutdown

        (_, _) -> do

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Elija Wallet y Pool Params"
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop Nothing Nothing shutdown





balances ::  Maybe Integer -> Maybe PoolParams ->  CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () -> CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () 
balances walletNro pParams shutdown = do
    let
        master1 = 1
        master2 = 2
        master3 = 3
        user1 = 4
        user2 = 5

    Simulator.logString @(Builtin.Builtin ValidatorContracts) "Balances:"
    --CMON.void $ CMIOC.liftIO HASKELL.getLine

    balances <- Simulator.currentBalances

    -- mapM_ (Simulator.logString @(Builtin.Builtin ValidatorContracts)) ["Master 1: " ++ HASKELL.show (WEW.getWalletId (fromJust (walletFromEntity entity)) ) ++ " " ++  HASKELL.show value | (entity, value) <-  Map.toList balances, fromWallet master1 entity ]
    -- mapM_ (Simulator.logString @(Builtin.Builtin ValidatorContracts)) ["Master 2: " ++ HASKELL.show (WEW.getWalletId (fromJust (walletFromEntity entity)) ) ++ " " ++  HASKELL.show value | (entity, value) <-  Map.toList balances, fromWallet master2 entity ]
    -- mapM_ (Simulator.logString @(Builtin.Builtin ValidatorContracts)) ["Master 3: " ++ HASKELL.show (WEW.getWalletId (fromJust (walletFromEntity entity)) ) ++ " " ++  HASKELL.show value | (entity, value) <-  Map.toList balances, fromWallet master3 entity ]
    -- mapM_ (Simulator.logString @(Builtin.Builtin ValidatorContracts)) ["User 1: " ++ HASKELL.show (WEW.getWalletId (fromJust (walletFromEntity entity)) ) ++ " " ++  HASKELL.show value | (entity, value) <-  Map.toList balances, fromWallet user1 entity ]
    -- mapM_ (Simulator.logString @(Builtin.Builtin ValidatorContracts)) ["User 2: " ++ HASKELL.show (WEW.getWalletId (fromJust (walletFromEntity entity)) ) ++ " " ++  HASKELL.show value | (entity, value) <-  Map.toList balances, fromWallet user2 entity ]
    
    mapM_ (Simulator.logString @(Builtin.Builtin ValidatorContracts)) ["Master 1: " ++ HASKELL.show (walletPaymentPubKeyHash master1 ) ++ " " ++  HASKELL.show value | (entity, value) <-  Map.toList balances, fromWallet master1 entity ]
    mapM_ (Simulator.logString @(Builtin.Builtin ValidatorContracts)) ["Master 2: " ++ HASKELL.show (walletPaymentPubKeyHash master2 ) ++ " " ++  HASKELL.show value | (entity, value) <-  Map.toList balances, fromWallet master2 entity ]
    mapM_ (Simulator.logString @(Builtin.Builtin ValidatorContracts)) ["Master 3: " ++ HASKELL.show (walletPaymentPubKeyHash master3  ) ++ " " ++  HASKELL.show value | (entity, value) <-  Map.toList balances, fromWallet master3 entity ]
    mapM_ (Simulator.logString @(Builtin.Builtin ValidatorContracts)) ["User 1: " ++ HASKELL.show   (walletPaymentPubKeyHash user1 ) ++ " " ++  HASKELL.show value | (entity, value) <-  Map.toList balances, fromWallet user1 entity ]
    mapM_ (Simulator.logString @(Builtin.Builtin ValidatorContracts)) ["User 2: " ++ HASKELL.show   (walletPaymentPubKeyHash user2  ) ++ " " ++  HASKELL.show value | (entity, value) <-  Map.toList balances, fromWallet user2 entity ]
    
    case pParams of
        Just pParams -> do
            mapM_ (Simulator.logString @(Builtin.Builtin ValidatorContracts)) ["Script: " ++ HASKELL.show (hashValidator pParams) ++ " " ++  HASKELL.show value | (entity, value) <-  Map.toList balances, fromScript pParams  entity ]
    
        _  -> 
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "" 

    slot <- Simulator.currentSlot >>= CMIOC.liftIO . STM.atomically
    let posixTime = TimeSlot.slotToEndPOSIXTime Data.Default.def slot

    Simulator.logString @(Builtin.Builtin ValidatorContracts) ""
    Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "slot: " ++  HASKELL.show slot
    Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "time: " ++  HASKELL.show posixTime
    Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "format time: " ++  getFormatTime posixTime

    


utxoAtWallet ::  Maybe Integer -> Maybe PoolParams ->  CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () -> CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () 
utxoAtWallet walletNro pParams shutdown = do
    case walletNro of
        Just walletNro -> do

            blockchain <- Simulator.blockchain

            let 

                uTxOutRefAt = fst <$>  getUtxosListInPABSimulator blockchain (walletPaymentPubKeyHashAddress walletNro)    

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Utxo at Wallet"

            --Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "" ++ HASKELL.show uTxOutRefAt

            mapM_ (Simulator.logString @(Builtin.Builtin ValidatorContracts)) ["Values at: " ++ HASKELL.show utxo ++ " " ++  HASKELL.show (LB.value blockchain utxo) | utxo <-  uTxOutRefAt ]
            
            

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop (Just walletNro) pParams shutdown 

        _  -> do

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Elija Wallet"
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop walletNro pParams shutdown


utxoAtScript ::  Maybe Integer -> Maybe PoolParams ->  CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () -> CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () 
utxoAtScript walletNro pParams shutdown = do

    case pParams of
        Just pParams -> do
            blockchain <- Simulator.blockchain

            let 

                uTxOuts =  getUtxosListInPABSimulator blockchain (addressValidator pParams)   

                datumFrom utxout = do

                    -- let
                    --     dat = getDatumFromTxOut utxout 

                    -- if isJust dat then
                    --     if datumIsPoolState dat then
                    --         getPoolStateFromMaybeDatum dat   
                    --     else
                    --         if datumIsUserState dat then
                    --             getUserStateFromMaybeDatum dat   
                    --         else 
                    --             Nothing
                    -- else 
                    --     Nothing

                    HASKELL.show $ fromJust $ Ledger.txOutDatum utxout

                formatValues utxoRef =  [HASKELL.show val   |  val <- VALUE.flattenValue $ fromJust $ LB.value blockchain utxoRef ]

                formatUtxoValues = concat [(HASKELL.show ( 1 HASKELL.+  fromJust(DL.elemIndex (utxoRef, utxout) uTxOuts))): (    "At: " ++ HASKELL.show utxoRef):("Datum: " ++  datumFrom utxout):formatValues utxoRef | (utxoRef, utxout) <-  uTxOuts ]


            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Utxo at Script"
            
            --Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "" ++ HASKELL.show uTxOutRefAt
            mapM_ (Simulator.logString @(Builtin.Builtin ValidatorContracts)) formatUtxoValues
            

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop walletNro (Just pParams) shutdown 

        _  -> do

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Elija Pool Params"
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop walletNro pParams shutdown

mainLoop :: Maybe Integer -> Maybe PoolParams ->  CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) () -> CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts))) ()
mainLoop walletNro pParams shutdown = do

    Simulator.logString @(Builtin.Builtin ValidatorContracts) ""
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "OPERACIONES:"

    case walletNro of
        Nothing ->
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "1 - Elegir Wallet"
        Just walletNro ->
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "1 - Elegir Wallet (" ++ HASKELL.show walletNro ++ ")"

    Simulator.logString @(Builtin.Builtin ValidatorContracts) ""
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "21 - Crear Pool Params"

    case pParams of
        Nothing ->
            Simulator.logString @(Builtin.Builtin ValidatorContracts) "22 - Elegir Pool Params"
        Just pParams ->
            Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "22 - Elegir Pool Params(" ++ HASKELL.show (ppPoolNFT pParams ) ++ ")"

    Simulator.logString @(Builtin.Builtin ValidatorContracts) ""
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "31 - Crear Pool"
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "32 - Fund Pool"
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "33 - Fund And Merge Pool"
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "34 - Get Back Fund"

    Simulator.logString @(Builtin.Builtin ValidatorContracts) ""
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "41 - Invertir en Pool"
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "42 - Claim Rewards de Pool"
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "43 - Get Back Invest"

    Simulator.logString @(Builtin.Builtin ValidatorContracts) ""
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "81 - Balances"
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "82 - Utxo at Wallet"
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "83 - Utxo at Script"

    Simulator.logString @(Builtin.Builtin ValidatorContracts) ""
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "99 - Salir"

    Simulator.logString @(Builtin.Builtin ValidatorContracts) "Ingrese una opción:"
    opcion <- CMIOC.liftIO HASKELL.getLine

    case TR.readMaybe opcion of

        Just 1 -> do

            elegirWallet walletNro pParams shutdown

        Just 21 -> do
            
            crearPoolParams walletNro pParams shutdown

        Just 22 -> do

            elegirPoolParams walletNro pParams shutdown

        Just 31 ->  do
            
            crearPool walletNro pParams shutdown

        Just 32 -> do
            
            fundPool walletNro pParams shutdown

        Just 33 -> do
            
            fundAndMergePool walletNro pParams shutdown

        Just 41 -> do
            
            investInPool walletNro pParams shutdown

        Just 42 -> do
            
            claimRewardsFromPool walletNro pParams shutdown


        Just 81 -> do
            
            balances walletNro pParams shutdown

            Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
            CMON.void $ CMIOC.liftIO HASKELL.getLine

            mainLoop walletNro pParams shutdown

        Just 82 -> do
            
            utxoAtWallet walletNro pParams shutdown

        Just 83 -> do
            
            utxoAtScript walletNro pParams shutdown


        Just 99 -> do
            
            balances walletNro pParams shutdown

            shutdown

        _ -> mainLoop walletNro pParams shutdown





test2 :: PC.IO ()
test2  = CMON.void $ Simulator.runSimulationWith handlers myTrace2

myTrace2  ::  CMFI.Eff (PPC.PABEffects (Builtin.Builtin ValidatorContracts) (Simulator.SimulatorState (Builtin.Builtin ValidatorContracts)))  ()
myTrace2 = do
    Simulator.logString @(Builtin.Builtin ValidatorContracts) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    Simulator.logString @(Builtin.Builtin ValidatorContracts) "********* PAB Server is running *********"
    -- Simulator.logString @(Builtin.Builtin ValidatorContracts) "Press return to continue..."
    -- CMON.void $ CMIOC.liftIO HASKELL.getLine

    -- let
    --     master1 = 1
    --     master2 = 2
    --     master3 = 3
    --     user1 = 4
    --     user2 = 5

    -- Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "master1 Pkh = " ++ HASKELL.show (walletPaymentPubKeyHash master1)
    -- Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "master1 PkhAddress = " ++ HASKELL.show (walletPaymentPubKeyHashAddress master1)

    -- Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "master2 Pkh = " ++ HASKELL.show (walletPaymentPubKeyHash master2)
    -- Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "master2 PkhAddress = " ++ HASKELL.show (walletPaymentPubKeyHashAddress master2)

    -- Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "user1 Pkh = " ++ HASKELL.show (walletPaymentPubKeyHash user1)
    -- Simulator.logString @(Builtin.Builtin ValidatorContracts) $ "user1 PkhAddress = " ++ HASKELL.show (walletPaymentPubKeyHashAddress user1)


    mainLoop Nothing Nothing shutdown





