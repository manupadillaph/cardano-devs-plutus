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

module Validators.StakeSimpleV1.TestWithEmulator
    where

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

import           Control.Monad.Freer.Extras as Extras
import           Data.Void

--Import Internos
import Validators.StakeSimpleV1.OffChain               
import Validators.StakeSimpleV1.Typos           
import Validators.StakeSimpleV1.OffChainHelpers           
import Validators.StakeSimpleV1.OnChainNFT     (mintingNFTPolicy)
import Validators.StakeSimpleV1.Helpers    

test :: Integer -> P.IO ()
test opcion = TraceEmulator.runEmulatorTraceIO' DataDefault.def emCfg (myTrace opcion)

emCfg :: TraceEmulator.EmulatorConfig
emCfg = TraceEmulator.EmulatorConfig (Left $ DataMap.fromList [(WalletEmulator.knownWallet w, v) | w <- [1 .. 3]]) DataDefault.def 
  where
    v :: LedgerValueV1.Value
    v = LedgerAda.lovelaceValueOf 2000_000_000 


myTrace :: Integer -> TraceEmulator.EmulatorTrace ()
myTrace opcion = do

    let 
        master1 = WalletEmulator.knownWallet 1
        master2 = WalletEmulator.knownWallet 2
        user1 = WalletEmulator.knownWallet 3

    MonadExtras.logInfo $ "Wallet 1 MASTER: " ++ P.show master1 ++ " - PubKeyHash: " ++ P.show (WalletEmulator.mockWalletPaymentPubKeyHash master1)
    MonadExtras.logInfo $ "Wallet 2 MASTER: " ++ P.show master2 ++ " - PubKeyHash: " ++ P.show (WalletEmulator.mockWalletPaymentPubKeyHash master2)
    MonadExtras.logInfo $ "Wallet 3 USER: " ++ P.show user1 ++ " - PubKeyHash: " ++ P.show (WalletEmulator.mockWalletPaymentPubKeyHash user1)

    utxosAtMaster1 <- OffChainHelpers.getUtxoListInEmulator (WalletEmulator.mockWalletAddress master1)
    utxosAtUser1 <- OffChainHelpers.getUtxoListInEmulator (WalletEmulator.mockWalletAddress user1)

    let 
        deadlinePool  = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def 1500
        deadlineInvest = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def 10
        deadlineReInvest = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def 15

        poolNFTUtxo = head utxosAtMaster1
        poolNFTTxOutRef = fst poolNFTUtxo
        idTxOut = LedgerApiV1.txOutRefId poolNFTTxOutRef
        indexTxOut = LedgerApiV1.txOutRefIdx  poolNFTTxOutRef

        poolNFTTokenName =  LedgerValueV1.TokenName (indexTxOut `consByteString`  LedgerApiV1.getTxId  idTxOut  )
        poolNFTCurrencySymbol = Helpers.curSymbol OnChainNFT.mintingNFTPolicy 

        poolNFT = LedgerValueV1.assetClass poolNFTCurrencySymbol  poolNFTTokenName
        
    MonadExtras.logInfo $ "poolNFTTxOutRef: " ++ P.show poolNFTTxOutRef
    MonadExtras.logInfo $ "poolNFTTokenName: " ++ P.show poolNFTTokenName
    MonadExtras.logInfo $ "poolNFTCurrencySymbol: " ++ P.show poolNFTCurrencySymbol
    MonadExtras.logInfo $ "poolNFT: " ++ P.show poolNFT 

    let


        user1NFTUtxo = head utxosAtUser1
        user1NFTTxOutRef = fst user1NFTUtxo
        user1IdTxOut = LedgerApiV1.txOutRefId user1NFTTxOutRef
        user1IndexTxOut = LedgerApiV1.txOutRefIdx  user1NFTTxOutRef

        user1NFTTokenName =  LedgerValueV1.TokenName (user1IndexTxOut `consByteString`  LedgerApiV1.getTxId  user1IdTxOut  )
        user1NFTCurrencySymbol = Helpers.curSymbol OnChainNFT.mintingNFTPolicy 

        userNFT = LedgerValueV1.assetClass user1NFTCurrencySymbol user1NFTTokenName

    MonadExtras.logInfo $ "user1NFTUtxo: " ++ P.show user1NFTUtxo
    MonadExtras.logInfo $ "user1NFTTokenName: " ++ P.show user1NFTTokenName
    MonadExtras.logInfo $ "user1NFTCurrencySymbol: " ++ P.show user1NFTCurrencySymbol
    MonadExtras.logInfo $ "userNFT: " ++ P.show userNFT 

    let
        pParams = PoolParams
            { 
                T.ppMasters = [WalletEmulator.mockWalletPaymentPubKeyHash master1, WalletEmulator.mockWalletPaymentPubKeyHash master2] , 
                T.ppInterest = 10 , 
                T.ppMinumunInvest   = 5_000_000 , 
                T.ppMinumunCompoundInvest    = 3_000_000 , 
                T.ppDeadline  = deadlinePool , 
                T.ppPoolNFT = poolNFT , 
                T.ppPoolNFTTxOutRef = poolNFTTxOutRef,
                T.ppCurSymbolForMintingNFTPolicy = poolNFTCurrencySymbol,
                T.ppValidTimeRange = 10_000,
                T.ppMinimunClaim = 3_000_000
            }
     

    activateContractWalletMaster1 <- TraceEmulator.activateContractWallet master1 OffChain.endpoints
    activateContractWalletMaster2 <- TraceEmulator.activateContractWallet master2 OffChain.endpoints
    activateContractWalletUser1 <- TraceEmulator.activateContractWallet user1 OffChain.endpoints

    case opcion of
        1 -> do
        
            Monad.void $ TraceEmulator.waitNSlots 1

            TraceEmulator.callEndpoint @"masterCreatePool" activateContractWalletMaster1 $ MasterCreatePoolParams{  
                mcpPoolParam = pParams, 
                mcpPoolNFTTokenName = poolNFTTokenName,
                mcpPoolNFTTxOutRef = poolNFTTxOutRef,
                mcpFund   = 100_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 2

            TraceEmulator.callEndpoint @"masterFundPool" activateContractWalletMaster2 $ MasterFundPoolParams{  
                mspPoolParam = pParams, 
                -- mspPoolNFTTokenName = poolNFTTokenName,
                -- mspPoolNFTTxOutRef = poolNFTTxOutRef,
                mspFund   = 50_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 2
 
        2 -> do 
            
            Monad.void $ TraceEmulator.waitNSlots 1

            TraceEmulator.callEndpoint @"masterCreatePool" activateContractWalletMaster1 $ MasterCreatePoolParams{  
                mcpPoolParam = pParams, 
                mcpPoolNFTTokenName = poolNFTTokenName,
                mcpPoolNFTTxOutRef = poolNFTTxOutRef,
                mcpFund   = 100_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 2

            TraceEmulator.callEndpoint @"userInvest" activateContractWalletUser1 $ UserInvestParams{  
                uipPoolParam = pParams,
                uiUserNFTTokenName = user1NFTTokenName,
                uiUserNFTTxOutRef = user1NFTTxOutRef,
                uipDeadline = deadlineInvest,
                uipInvest   = 6_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 2

        3 -> do 
            
            Monad.void $ TraceEmulator.waitNSlots 1

            TraceEmulator.callEndpoint @"masterCreatePool" activateContractWalletMaster1 $ MasterCreatePoolParams{  
                mcpPoolParam = pParams, 
                mcpPoolNFTTokenName = poolNFTTokenName,
                mcpPoolNFTTxOutRef = poolNFTTxOutRef,
                mcpFund   = 100_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 2

            TraceEmulator.callEndpoint @"userInvest" activateContractWalletUser1 $ UserInvestParams{  
                uipPoolParam = pParams,
                uiUserNFTTokenName = user1NFTTokenName,
                uiUserNFTTxOutRef = user1NFTTxOutRef,
                uipDeadline = deadlineInvest,
                uipInvest   = 10_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 100

            TraceEmulator.callEndpoint @"userGetRewards" activateContractWalletUser1 $ UserGetRewardsParams{  
                ugrpPoolParam = pParams,
                ugrpUserNFTTokenName = user1NFTTokenName,
                ugrpUserNFTTxOutRef = user1NFTTxOutRef,
                ugrpClaim  = 3_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 2

        4 -> do 
            
            Monad.void $ TraceEmulator.waitNSlots 1

            TraceEmulator.callEndpoint @"masterCreatePool" activateContractWalletMaster1 $ MasterCreatePoolParams{  
                mcpPoolParam = pParams, 
                mcpPoolNFTTokenName = poolNFTTokenName,
                mcpPoolNFTTxOutRef = poolNFTTxOutRef,
                mcpFund   = 100_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 2

            TraceEmulator.callEndpoint @"userInvest" activateContractWalletUser1 $ UserInvestParams{  
                uipPoolParam = pParams,
                uiUserNFTTokenName = user1NFTTokenName,
                uiUserNFTTxOutRef = user1NFTTxOutRef,
                uipDeadline = deadlineInvest,
                uipInvest   = 10_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 75

            TraceEmulator.callEndpoint @"userGetRewards" activateContractWalletUser1 $ UserGetRewardsParams{  
                ugrpPoolParam = pParams,
                ugrpUserNFTTokenName = user1NFTTokenName,
                ugrpUserNFTTxOutRef = user1NFTTxOutRef,
                ugrpClaim  = 4_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 20

            TraceEmulator.callEndpoint @"userGetRewards" activateContractWalletUser1 $ UserGetRewardsParams{  
                ugrpPoolParam = pParams,
                ugrpUserNFTTokenName = user1NFTTokenName,
                ugrpUserNFTTxOutRef = user1NFTTxOutRef,
                ugrpClaim  = 5_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 2

    -- TraceEmulator.callEndpoint @"masterFundPool" activateContractWalletMaster2 $ MasterFundPoolParams{  
    --         mspPoolParam = pParams, 
    --         mspPoolNFTTokenName = poolNFTTokenName,
    --         mspPoolNFTTxOutRef = poolNFTTxOutRef,
    --         mspFund   = 50_000_000
    --     }

    -- Monad.void $ TraceEmulator.waitNSlots 2


    -- -- TraceEmulator.callEndpoint @"masterCreatePool" activateContractWalletMaster1 $ MasterCreatePoolParams{  
    -- --         mcpPoolParam = pParams, 
    -- --         --mcpPoolNFTTokenName = poolNFTTokenNameString,
    -- --         mcpPoolNFTTokenName = poolNFTTokenName,
    -- --         mcpPoolNFTTxOutRef = poolNFTTxOutRef,
    -- --         mcpFund   = 200_000_000
    -- --     }

    -- -- Monad.void $ TraceEmulator.waitNSlots 1


    -- -- TraceEmulator.callEndpoint @"masterFundPool" activateContractWalletUser1 $ MasterFundPoolParams{  
    -- --         mspPoolParam = pParams, 
    -- --         mspFund   = 70_000_000
    -- --     }

    -- -- Monad.void $ TraceEmulator.waitNSlots 1


    -- TraceEmulator.callEndpoint @"userInvest" activateContractWalletUser1 $ UserInvestParams{  
    --     uipPoolParam = pParams,
    --     uiUserNFTTokenName = user1NFTTokenName,
    --     uiUserNFTTxOutRef = user1NFTTxOutRef,
    --     uipDeadline = deadlineInvest,
    --     uipInvest   = 6_000_000
    -- }

    -- Monad.void $ TraceEmulator.waitNSlots 2

    -- -- TraceEmulator.callEndpoint @"userInvest" activateContractWalletUser1 $ UserInvestParams{  
    -- --     uipPoolParam = pParams,
    -- --     uiUserNFTTokenName = user1NFTTokenNameString,
    -- --     uiUserNFTTxOutRef = user1NFTTxOutRef,
    -- --     uipDeadline = deadlineInvest,
    -- --     uipInvest   = 4_000_000
    -- -- }

    -- -- Monad.void $ TraceEmulator.waitNSlots 1


    -- -- MonadExtras.logInfo $ "Wallet 1 MASTER: " ++ P.show master1 ++ " - PubKeyHash: " ++ P.show (WalletEmulator.mockWalletPaymentPubKeyHash master1)
    -- -- MonadExtras.logInfo $ "Wallet 2 MASTER: " ++ P.show master2 ++ " - PubKeyHash: " ++ P.show (WalletEmulator.mockWalletPaymentPubKeyHash master2)
    -- -- MonadExtras.logInfo $ "Wallet 3 USER: " ++ P.show user1 ++ " - PubKeyHash: " ++ P.show (WalletEmulator.mockWalletPaymentPubKeyHash user1)

    
    

    -- TraceEmulator.callEndpoint @"masterGetBackFund" activateContractWalletMaster $ MasterGetBackFundParams{  
    --         mgpPoolParam = pParams
    --    }

    -- Monad.void $ TraceEmulator.waitNSlots 15

    -- TraceEmulator.callEndpoint @"masterGetBackFund" activateContractWalletUser $ MasterGetBackFundParams{  
    --         mgpPoolParam = pParams
    --     }
    -- Monad.void $ TraceEmulator.waitNSlots 1

    -- TraceEmulator.callEndpoint @"userInvest" activateContractWalletUser $ UserInvestParams{  
    --         uipPoolParam = pParams,
    --         uipDeadline = deadlineInvest,
    --         uipInvest   = 6_000_000
    --     }

    -- Monad.void $ TraceEmulator.waitNSlots 10

    -- TraceEmulator.callEndpoint @"userGetBackInvest" activateContractWalletUser $ UserGetBackInvestParams{  
    --         ugipPoolParam = pParams,
    --         ugipDeadline = deadlineInvest
    --     }

    -- Monad.void $ TraceEmulator.waitNSlots 1

    -- TraceEmulator.callEndpoint @"userGetRewards" activateContractWalletUser $ UserGetRewardsParams{  
    --         ugrpPoolParam = pParams
    --     }

    -- Monad.void $ TraceEmulator.waitNSlots 1

    -- TraceEmulator.callEndpoint @"userInvestRewards" activateContractWalletUser $ UserInvestRewardsParams{  
    --         uirpPoolParam = pParams,
    --         uirpDeadline = deadlineReInvest
    --     }
    
    -- Monad.void $ TraceEmulator.waitNSlots 1
