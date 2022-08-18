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

module Validators.LockerV1.TestWithEmulator where

--Import Externos

import qualified Control.Monad                       as Monad (void)
import qualified Data.Default                        as DataDefault (def) 
import qualified Data.Map                            as DataMap
import qualified Ledger.Ada                          as LedgerAda 
import qualified Ledger.TimeSlot                     as LedgerTimeSlot (slotToEndPOSIXTime)    
import qualified Playground.Contract                 as PlaygroundContract (IO) --, ensureKnownCurrencies, printSchemas, stage, printJson
import qualified Plutus.Trace.Emulator               as TraceEmulator
import qualified Plutus.V1.Ledger.Value              as LedgerValueV1
import qualified Prelude                             as P
import qualified Wallet.Emulator.Wallet              as WalletEmulator

--Import Internos

import qualified Validators.LockerV1.OffChain        as OffChain

-- Modulo:

test :: PlaygroundContract.IO ()
test = TraceEmulator.runEmulatorTraceIO' DataDefault.def emCfg myTrace

emCfg :: TraceEmulator.EmulatorConfig
emCfg = TraceEmulator.EmulatorConfig (P.Left P.$ DataMap.fromList [(WalletEmulator.knownWallet w, v) | w <- [1 .. 1]]) DataDefault.def 
  where
    v :: LedgerValueV1.Value
    v = LedgerAda.lovelaceValueOf 2000_000_000 

myTrace :: TraceEmulator.EmulatorTrace ()
myTrace = do
    let deadline = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def 6

    h1 <- TraceEmulator.activateContractWallet (WalletEmulator.knownWallet 1) OffChain.endpoints
    --h2 <- TraceEmulator.activateContractWallet (WalletEmulator.knownWallet 2) endpoints

    TraceEmulator.callEndpoint @"start" h1 P.$ OffChain.StartParams{  
            ppDeadline = deadline,
            spName = 55,
            spAdaQty   = 3000000
        }
    Monad.void P.$ TraceEmulator.waitNSlots 7
    TraceEmulator.callEndpoint @"get" h1 P.$ OffChain.GetParams { 
            gpName = 55,
            gpAdaQty    = 3000000
        }
    Monad.void P.$ TraceEmulator.waitNSlots 6
    TraceEmulator.callEndpoint @"get" h1 P.$ OffChain.GetParams{ 
            gpName = 56,
            gpAdaQty    = 3000000
        }
    Monad.void P.$ TraceEmulator.waitNSlots 3
    TraceEmulator.callEndpoint @"get" h1 P.$ OffChain.GetParams { 
            gpName = 55,
            gpAdaQty    = 3000000
        }
    Monad.void P.$ TraceEmulator.waitNSlots 1
    
test2 :: PlaygroundContract.IO ()
test2 = TraceEmulator.runEmulatorTraceIO P.$ do

    let deadline = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def 6

    h1 <- TraceEmulator.activateContractWallet (WalletEmulator.knownWallet 1) OffChain.endpoints
    --h2 <- TraceEmulator.activateContractWallet (WalletEmulator.knownWallet 2) endpoints

    TraceEmulator.callEndpoint @"start" h1 P.$ OffChain.StartParams{  
            ppDeadline  = deadline,
            spName      = 55,
            spAdaQty    = 3000000
        }
    Monad.void P.$ TraceEmulator.waitNSlots 7 
    TraceEmulator.callEndpoint @"get" h1 P.$ OffChain.GetParams { 
            gpName      = 55,
            gpAdaQty    = 3000000
        }
    Monad.void P.$ TraceEmulator.waitNSlots 6
    TraceEmulator.callEndpoint @"get" h1 P.$ OffChain.GetParams{ 
            gpName      = 56,
            gpAdaQty    = 3000000
        }
    Monad.void P.$ TraceEmulator.waitNSlots 3
    TraceEmulator.callEndpoint @"get" h1 P.$ OffChain.GetParams { 
            gpName      = 55,
            gpAdaQty    = 3000000
        }
    Monad.void P.$ TraceEmulator.waitNSlots 1