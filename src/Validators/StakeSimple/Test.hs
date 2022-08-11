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
{-# LANGUAGE NumericUnderscores    #-}

--{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module  Validators.StakeSimple.Test
    where

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

import           Control.Monad.Freer.Extras as Extras
import           Data.Void

--Import Internos
import Validators.StakeSimple.OffChain               
import Validators.StakeSimple.Typos           
import Validators.StakeSimple.OffChainHelpers           
import Validators.StakeSimple.OnChainNFT     (mintingNFTPolicy)
import Validators.StakeSimple.Helpers    

test :: IO ()
test = runEmulatorTraceIO' def emCfg myTrace

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(knownWallet w, v) | w <- [1 .. 3]]) def def
  where
    v :: Value
    v = Ada.lovelaceValueOf 2000_000_000 


myTrace :: EmulatorTrace ()
myTrace = do

    let 
        master1 = knownWallet 1
        master2 = knownWallet 2
        user1 = knownWallet 3

    Extras.logInfo $ "Wallet 1 MASTER: " ++ P.show master1 ++ " - PubKeyHash: " ++ P.show (mockWalletPaymentPubKeyHash master1)
    Extras.logInfo $ "Wallet 2 MASTER: " ++ P.show master2 ++ " - PubKeyHash: " ++ P.show (mockWalletPaymentPubKeyHash master2)
    Extras.logInfo $ "Wallet 3 USER: " ++ P.show user1 ++ " - PubKeyHash: " ++ P.show (mockWalletPaymentPubKeyHash user1)

    utxosMaster1 <- getUtxoListInEmulator (mockWalletAddress master1)
    utxosUser1 <- getUtxoListInEmulator (mockWalletAddress user1)

    let 
        deadlinePool  = slotToEndPOSIXTime def 15
        deadlineInvest = slotToEndPOSIXTime def 10
        deadlineReInvest = slotToEndPOSIXTime def 15

        poolNFTUtxo = head utxosMaster1
        poolNFTTxOutRef = fst poolNFTUtxo
        idTxOut = txOutRefId poolNFTTxOutRef
        indexTxOut = txOutRefIdx  poolNFTTxOutRef

        poolNFTTokenName =  TokenName (indexTxOut `consByteString`  getTxId  idTxOut  )
        poolNFTCurrencySymbol = curSymbol mintingNFTPolicy 

        poolNFT = assetClass poolNFTCurrencySymbol  poolNFTTokenName
        
    Extras.logInfo $ "poolNFTTxOutRef: " ++ P.show poolNFTTxOutRef
    Extras.logInfo $ "poolNFTTokenName: " ++ P.show poolNFTTokenName
    Extras.logInfo $ "poolNFTCurrencySymbol: " ++ P.show poolNFTCurrencySymbol
    Extras.logInfo $ "poolNFT: " ++ P.show poolNFT 

    -- Extras.logInfo $ "poolNFTCurrencySymbol: " ++ P.show ( fst (unAssetClass poolNFT ))
    -- Extras.logInfo $ "currencyMPSHash: " ++ P.show (currencyMPSHash  poolNFTCurrencySymbol) 
    -- Extras.logInfo $ "currencyMPSHash: " ++ P.show ( currencyMPSHash (fst (unAssetClass poolNFT ))) 

    
    void $ Emulator.waitNSlots 1

    let


        user1NFTUtxo = head utxosUser1
        user1NFTTxOutRef = fst user1NFTUtxo
        user1IdTxOut = txOutRefId user1NFTTxOutRef
        user1IndexTxOut = txOutRefIdx  user1NFTTxOutRef

        user1NFTTokenName =  TokenName (user1IndexTxOut `consByteString`  getTxId  user1IdTxOut  )
        user1NFTCurrencySymbol = curSymbol mintingNFTPolicy 

        userNFT = assetClass user1NFTCurrencySymbol user1NFTTokenName

    Extras.logInfo $ "user1NFTUtxo: " ++ P.show user1NFTUtxo
    Extras.logInfo $ "user1NFTTokenName: " ++ P.show user1NFTTokenName
    Extras.logInfo $ "user1NFTCurrencySymbol: " ++ P.show user1NFTCurrencySymbol
    Extras.logInfo $ "userNFT: " ++ P.show userNFT 

    let
        pParams = PoolParams
            { 
                spMasters = [mockWalletPaymentPubKeyHash master1, mockWalletPaymentPubKeyHash master2] , 
                spInterest = 10 , 
                spMinumunInvest   = 5_000_000 , 
                spMinumunCompoundInvest    = 3_000_000 , 
                spDeadline  = deadlinePool , 
                spPoolNFT = poolNFT , 
                spCurSymbolForMintingNFTPolicy = poolNFTCurrencySymbol
            }
     

    activateContractWalletMaster1 <- activateContractWallet master1 endpoints
    activateContractWalletMaster2 <- activateContractWallet master2 endpoints
    activateContractWalletUser1 <- activateContractWallet user1 endpoints

    void $ Emulator.waitNSlots 1

    callEndpoint @"masterCreatePool" activateContractWalletMaster1 $ MasterCreatePoolParams{  
            mcpPoolParam = pParams, 
            mcpPoolNFTTokenName = poolNFTTokenName,
            mcpPoolNFTTxOutRef = poolNFTTxOutRef,
            mcpFund   = 100_000_000
        }

    void $ Emulator.waitNSlots 2

    -- callEndpoint @"masterFundPool" activateContractWalletMaster2 $ MasterFundPoolParams{  
    --         mspPoolParam = pParams, 
    --         mspPoolNFTTokenName = poolNFTTokenName,
    --         mspPoolNFTTxOutRef = poolNFTTxOutRef,
    --         mspFund   = 50_000_000
    --     }

    -- void $ Emulator.waitNSlots 2


    -- -- callEndpoint @"masterCreatePool" activateContractWalletMaster1 $ MasterCreatePoolParams{  
    -- --         mcpPoolParam = pParams, 
    -- --         --mcpPoolNFTTokenName = poolNFTTokenNameString,
    -- --         mcpPoolNFTTokenName = poolNFTTokenName,
    -- --         mcpPoolNFTTxOutRef = poolNFTTxOutRef,
    -- --         mcpFund   = 200_000_000
    -- --     }

    -- -- void $ Emulator.waitNSlots 1


    -- -- callEndpoint @"masterFundPool" activateContractWalletUser1 $ MasterFundPoolParams{  
    -- --         mspPoolParam = pParams, 
    -- --         mspFund   = 70_000_000
    -- --     }

    -- -- void $ Emulator.waitNSlots 1


    -- -- callEndpoint @"masterFundPool" activateContractWalletUser $ MasterFundPoolParams{  
    -- --         mspPoolParam = pParams
    -- --     , mspFund   = 25_000_000
    -- --     }

    -- -- void $ Emulator.waitNSlots 3


    callEndpoint @"userInvest" activateContractWalletUser1 $ UserInvestParams{  
        uipPoolParam = pParams,
        uiUserNFTTokenName = user1NFTTokenName,
        uiUserNFTTxOutRef = user1NFTTxOutRef,
        uipDeadline = deadlineInvest,
        uipInvest   = 6_000_000
    }

    void $ Emulator.waitNSlots 2

    -- -- callEndpoint @"userInvest" activateContractWalletUser1 $ UserInvestParams{  
    -- --     uipPoolParam = pParams,
    -- --     uiUserNFTTokenName = user1NFTTokenNameString,
    -- --     uiUserNFTTxOutRef = user1NFTTxOutRef,
    -- --     uipDeadline = deadlineInvest,
    -- --     uipInvest   = 4_000_000
    -- -- }

    -- -- void $ Emulator.waitNSlots 1


    -- -- Extras.logInfo $ "Wallet 1 MASTER: " ++ P.show master1 ++ " - PubKeyHash: " ++ P.show (mockWalletPaymentPubKeyHash master1)
    -- -- Extras.logInfo $ "Wallet 2 MASTER: " ++ P.show master2 ++ " - PubKeyHash: " ++ P.show (mockWalletPaymentPubKeyHash master2)
    -- -- Extras.logInfo $ "Wallet 3 USER: " ++ P.show user1 ++ " - PubKeyHash: " ++ P.show (mockWalletPaymentPubKeyHash user1)

    
    

    -- callEndpoint @"masterGetBackFund" activateContractWalletMaster $ MasterGetBackFundParams{  
    --         mgpPoolParam = pParams
    --    }

    -- void $ Emulator.waitNSlots 15

    -- callEndpoint @"masterGetBackFund" activateContractWalletUser $ MasterGetBackFundParams{  
    --         mgpPoolParam = pParams
    --     }
    -- void $ Emulator.waitNSlots 1

    -- callEndpoint @"userInvest" activateContractWalletUser $ UserInvestParams{  
    --         uipPoolParam = pParams,
    --         uipDeadline = deadlineInvest,
    --         uipInvest   = 6_000_000
    --     }

    -- void $ Emulator.waitNSlots 10

    -- callEndpoint @"userGetBackInvest" activateContractWalletUser $ UserGetBackInvestParams{  
    --         ugipPoolParam = pParams,
    --         ugipDeadline = deadlineInvest
    --     }

    -- void $ Emulator.waitNSlots 1

    -- callEndpoint @"userGetRewards" activateContractWalletUser $ UserGetRewardsParams{  
    --         ugrpPoolParam = pParams
    --     }

    -- void $ Emulator.waitNSlots 1

    -- callEndpoint @"userInvestRewards" activateContractWalletUser $ UserInvestRewardsParams{  
    --         uirpPoolParam = pParams,
    --         uirpDeadline = deadlineReInvest
    --     }
    
    -- void $ Emulator.waitNSlots 1
