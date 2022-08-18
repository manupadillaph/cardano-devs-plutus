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

module Validators.StakePlusV1.TestWithEmulator
    where

--Import Externos

import qualified Control.Monad                       as Monad (void)
import qualified Control.Monad.Freer.Extras          as MonadExtras
import qualified Data.Default                        as DataDefault (def) 
import qualified Data.Map                            as DataMap
import qualified Ledger.Ada                          as LedgerAda 
import qualified Ledger.TimeSlot                     as LedgerTimeSlot (slotToEndPOSIXTime)    
import qualified Plutus.Trace.Emulator               as TraceEmulator
import qualified Plutus.V1.Ledger.Api                as LedgerApiV1
import qualified Plutus.V1.Ledger.Value              as LedgerValueV1
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Wallet.Emulator.Wallet              as WalletEmulator

--Import Internos

import qualified Validators.StakePlusV1.Helpers         as Helpers
import qualified Validators.StakePlusV1.OffChain        as OffChain       
import qualified Validators.StakePlusV1.OffChainHelpers as OffChainHelpers         
import qualified Validators.StakePlusV1.OnChainNFT      as OnChainNFT (mintingNFTPolicy)
import qualified Validators.StakePlusV1.Typos           as T 

-- Modulo:

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
        pParams = T.PoolParams
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

            TraceEmulator.callEndpoint @"masterCreatePool" activateContractWalletMaster1 $ T.MasterCreatePoolParams{  
                T.pmcpPoolParam = pParams, 
                T.pmcpPoolNFTTokenName = poolNFTTokenName,
                T.pmcpPoolNFTTxOutRef = poolNFTTxOutRef,
                T.pmcpFund   = 100_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 2

            TraceEmulator.callEndpoint @"masterFundPool" activateContractWalletMaster2 $ T.MasterFundPoolParams{  
                T.pmfpPoolParam = pParams, 
                -- mspPoolNFTTokenName = poolNFTTokenName,
                -- mspPoolNFTTxOutRef = poolNFTTxOutRef,
                T.pmfpFund   = 50_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 2
 
        2 -> do 
            
            Monad.void $ TraceEmulator.waitNSlots 1

            TraceEmulator.callEndpoint @"masterCreatePool" activateContractWalletMaster1 $ T.MasterCreatePoolParams{  
                T.pmcpPoolParam = pParams, 
                T.pmcpPoolNFTTokenName = poolNFTTokenName,
                T.pmcpPoolNFTTxOutRef = poolNFTTxOutRef,
                T.pmcpFund   = 100_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 2

            TraceEmulator.callEndpoint @"userInvest" activateContractWalletUser1 $ T.UserInvestParams{  
                T.puiPoolParam = pParams,
                T.puiUserNFTTokenName = user1NFTTokenName,
                T.puiUserNFTTxOutRef = user1NFTTxOutRef,
                T.puiDeadline = deadlineInvest,
                T.puiInvest   = 6_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 2

        3 -> do 
            
            Monad.void $ TraceEmulator.waitNSlots 1

            TraceEmulator.callEndpoint @"masterCreatePool" activateContractWalletMaster1 $ T.MasterCreatePoolParams{  
                T.pmcpPoolParam = pParams, 
                T.pmcpPoolNFTTokenName = poolNFTTokenName,
                T.pmcpPoolNFTTxOutRef = poolNFTTxOutRef,
                T.pmcpFund   = 100_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 2

            TraceEmulator.callEndpoint @"userInvest" activateContractWalletUser1 $ T.UserInvestParams{  
                T.puiPoolParam = pParams,
                T.puiUserNFTTokenName = user1NFTTokenName,
                T.puiUserNFTTxOutRef = user1NFTTxOutRef,
                T.puiDeadline = deadlineInvest,
                T.puiInvest   = 10_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 100

            TraceEmulator.callEndpoint @"userGetRewards" activateContractWalletUser1 $ T.UserGetRewardsParams{  
                T.pugrPoolParam = pParams,
                T.pugrUserNFTTokenName = user1NFTTokenName,
                T.pugrUserNFTTxOutRef = user1NFTTxOutRef,
                T.pugrClaim  = 3_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 2

        4 -> do 
            
            Monad.void $ TraceEmulator.waitNSlots 1

            TraceEmulator.callEndpoint @"masterCreatePool" activateContractWalletMaster1 $ T.MasterCreatePoolParams{  
                T.pmcpPoolParam = pParams, 
                T.pmcpPoolNFTTokenName = poolNFTTokenName,
                T.pmcpPoolNFTTxOutRef = poolNFTTxOutRef,
                T.pmcpFund   = 100_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 2

            TraceEmulator.callEndpoint @"userInvest" activateContractWalletUser1 $ T.UserInvestParams{  
                T.puiPoolParam = pParams,
                T.puiUserNFTTokenName = user1NFTTokenName,
                T.puiUserNFTTxOutRef = user1NFTTxOutRef,
                T.puiDeadline = deadlineInvest,
                T.puiInvest   = 10_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 75

            TraceEmulator.callEndpoint @"userGetRewards" activateContractWalletUser1 $ T.UserGetRewardsParams{  
                T.pugrPoolParam = pParams,
                T.pugrUserNFTTokenName = user1NFTTokenName,
                T.pugrUserNFTTxOutRef = user1NFTTxOutRef,
                T.pugrClaim  = 4_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 20

            TraceEmulator.callEndpoint @"userGetRewards" activateContractWalletUser1 $ T.UserGetRewardsParams{  
                T.pugrPoolParam = pParams,
                T.pugrUserNFTTokenName = user1NFTTokenName,
                T.pugrUserNFTTxOutRef = user1NFTTxOutRef,
                T.pugrClaim  = 5_000_000
            }

            Monad.void $ TraceEmulator.waitNSlots 2

    -- TraceEmulator.callEndpoint @"masterFundPool" activateContractWalletMaster2 $ T.MasterFundPoolParams{  
    --         T.pmfpPoolParam = pParams, 
    --         mspPoolNFTTokenName = poolNFTTokenName,
    --         mspPoolNFTTxOutRef = poolNFTTxOutRef,
    --         T.pmfpFund   = 50_000_000
    --     }

    -- Monad.void $ TraceEmulator.waitNSlots 2


    -- -- TraceEmulator.callEndpoint @"masterCreatePool" activateContractWalletMaster1 $ T.MasterCreatePoolParams{  
    -- --         T.pmcpPoolParam = pParams, 
    -- --         --T.pmcpPoolNFTTokenName = poolNFTTokenNameString,
    -- --         T.pmcpPoolNFTTokenName = poolNFTTokenName,
    -- --         T.pmcpPoolNFTTxOutRef = poolNFTTxOutRef,
    -- --         T.pmcpFund   = 200_000_000
    -- --     }

    -- -- Monad.void $ TraceEmulator.waitNSlots 1


    -- -- TraceEmulator.callEndpoint @"masterFundPool" activateContractWalletUser1 $ T.MasterFundPoolParams{  
    -- --         T.pmfpPoolParam = pParams, 
    -- --         T.pmfpFund   = 70_000_000
    -- --     }

    -- -- Monad.void $ TraceEmulator.waitNSlots 1


    -- TraceEmulator.callEndpoint @"userInvest" activateContractWalletUser1 $ T.UserInvestParams{  
    --     T.puiPoolParam = pParams,
    --     T.puiUserNFTTokenName = user1NFTTokenName,
    --     T.puiUserNFTTxOutRef = user1NFTTxOutRef,
    --     T.puiDeadline = deadlineInvest,
    --     T.puiInvest   = 6_000_000
    -- }

    -- Monad.void $ TraceEmulator.waitNSlots 2

    -- -- TraceEmulator.callEndpoint @"userInvest" activateContractWalletUser1 $ T.UserInvestParams{  
    -- --     T.puiPoolParam = pParams,
    -- --     T.puiUserNFTTokenName = user1NFTTokenNameString,
    -- --     T.puiUserNFTTxOutRef = user1NFTTxOutRef,
    -- --     T.puiDeadline = deadlineInvest,
    -- --     T.puiInvest   = 4_000_000
    -- -- }

    -- -- Monad.void $ TraceEmulator.waitNSlots 1


    -- -- MonadExtras.logInfo $ "Wallet 1 MASTER: " ++ P.show master1 ++ " - PubKeyHash: " ++ P.show (WalletEmulator.mockWalletPaymentPubKeyHash master1)
    -- -- MonadExtras.logInfo $ "Wallet 2 MASTER: " ++ P.show master2 ++ " - PubKeyHash: " ++ P.show (WalletEmulator.mockWalletPaymentPubKeyHash master2)
    -- -- MonadExtras.logInfo $ "Wallet 3 USER: " ++ P.show user1 ++ " - PubKeyHash: " ++ P.show (WalletEmulator.mockWalletPaymentPubKeyHash user1)

    
    

    -- TraceEmulator.callEndpoint @"masterGetBackFund" activateContractWalletMaster $ T.MasterGetBackFundParams{  
    --         T.pmgbfPoolParam = pParams
    --    }

    -- Monad.void $ TraceEmulator.waitNSlots 15

    -- TraceEmulator.callEndpoint @"masterGetBackFund" activateContractWalletUser $ T.MasterGetBackFundParams{  
    --         T.pmgbfPoolParam = pParams
    --     }
    -- Monad.void $ TraceEmulator.waitNSlots 1

    -- TraceEmulator.callEndpoint @"userInvest" activateContractWalletUser $ T.UserInvestParams{  
    --         T.puiPoolParam = pParams,
    --         T.puiDeadline = deadlineInvest,
    --         T.puiInvest   = 6_000_000
    --     }

    -- Monad.void $ TraceEmulator.waitNSlots 10

    -- TraceEmulator.callEndpoint @"userGetBackInvest" activateContractWalletUser $ T.UserGetBackInvestParams{  
    --         T.pugbiPoolParam = pParams,
    --         T.pugbiDeadline = deadlineInvest
    --     }

    -- Monad.void $ TraceEmulator.waitNSlots 1

    -- TraceEmulator.callEndpoint @"userGetRewards" activateContractWalletUser $ T.UserGetRewardsParams{  
    --         T.pugrPoolParam = pParams
    --     }

    -- Monad.void $ TraceEmulator.waitNSlots 1

    -- TraceEmulator.callEndpoint @"userInvestRewards" activateContractWalletUser $ T.UserInvestRewardsParams{  
    --         T.puirPoolParam = pParams,
    --         T.puirDeadline = deadlineReInvest
    --     }
    
    -- Monad.void $ TraceEmulator.waitNSlots 1
