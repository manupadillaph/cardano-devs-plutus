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

module Validators.MarketNFTV1.TestWithPABSimulator where

--Import Externos:

import qualified Control.Monad                       as Monad (void)
--import qualified Data.Default                        as DataDefault (def) 
--import qualified Data.Map                            as DataMap
--import qualified Ledger.Ada                          as LedgerAda 
--import qualified Ledger.TimeSlot                     as LedgerTimeSlot (slotToEndPOSIXTime)    
--import qualified Playground.Contract                 as PlaygroundContract (IO) --, ensureKnownCurrencies, printSchemas, stage, printJson
import qualified Plutus.Trace.Emulator               as TraceEmulator
--import qualified Plutus.V1.Ledger.Value              as LedgerValueV1
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
--import qualified Wallet.Emulator.Wallet              as WalletEmulator

--Import Internos:

--import qualified Validators.MarketNFTV1.OffChain    as OffChain

-- Modulo:

testWithPABSimulator :: P.IO ()
testWithPABSimulator = TraceEmulator.runEmulatorTraceIO $ do

--     let deadline = slotToEndPOSIXTime def 6

--     h1 <- TraceEmulator.activateContractWallet (WalletEmulator.knownWallet 1) OffChain.endpoints
--     --h2 <- TraceEmulator.activateContractWallet (WalletEmulator.knownWallet 2) endpoints

--     TraceEmulator.callEndpoint @"start" h1 $ OffChain.StartParams{  
--             spDeadline = deadline,
--             spName = 55,
--             spAdaQty   = 3000000
--         }
--     Monad.void $ TraceEmulator.waitNSlots 7
--     TraceEmulator.callEndpoint @"get" h1 $ OffChain.GetParams { 
--             gpName = 55,
--             gpAdaQty    = 3000000
--         }
--     Monad.void $ TraceEmulator.waitNSlots 6
--     TraceEmulator.callEndpoint @"get" h1 $ OffChain.GetParams{ 
--             gpName = 56,
--             gpAdaQty    = 3000000
--         }
--     Monad.void $ TraceEmulator.waitNSlots 3
--     TraceEmulator.callEndpoint @"get" h1 $ OffChain.GetParams { 
--             gpName = 55,
--             gpAdaQty    = 3000000
--         }
    Monad.void $ TraceEmulator.waitNSlots 1