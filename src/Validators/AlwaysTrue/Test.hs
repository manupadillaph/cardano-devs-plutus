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

module  Validators.AlwaysTrue.Test
    ( 
     test
    ) where

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

--Import Internos
import qualified Validators.AlwaysTrue.OffChain                     as ValidatorOffChain

test2 :: IO ()
test2 = runEmulatorTraceIO $ do

    let deadline = slotToEndPOSIXTime def 6

    h1 <- activateContractWallet (knownWallet 1) ValidatorOffChain.endpoints
    --h2 <- activateContractWallet (knownWallet 2) endpoints

    callEndpoint @"start" h1 $ ValidatorOffChain.StartParams{  
            spDeadline = deadline,
            spName = 55,
            spAdaQty   = 3000000
        }
    void $ Emulator.waitNSlots 7
    callEndpoint @"get" h1 $ ValidatorOffChain.GetParams { 
            gpName = 55,
            gpAdaQty    = 3000000
        }
    void $ Emulator.waitNSlots 6
    callEndpoint @"get" h1 $ ValidatorOffChain.GetParams{ 
            gpName = 56,
            gpAdaQty    = 3000000
        }
    void $ Emulator.waitNSlots 3
    callEndpoint @"get" h1 $ ValidatorOffChain.GetParams { 
            gpName = 55,
            gpAdaQty    = 3000000
        }
    void $ Emulator.waitNSlots 1



-- test :: IO ()
-- test = runEmulatorTraceIO' def emCfg myTrace
--   where
--     emCfg :: EmulatorConfig
--     emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet i, v) | i <- [1 .. 2]]

--     v :: Value
--     v = Ada.lovelaceValueOf                    100_000_000


test :: IO ()
test = runEmulatorTraceIO' def emCfg myTrace

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(knownWallet w, v) | w <- [1 .. 1]]) def def
  where
    v :: Value
    v = Ada.lovelaceValueOf 2000_000_000 





myTrace :: EmulatorTrace ()
myTrace = do
    let deadline = slotToEndPOSIXTime def 6

    h1 <- activateContractWallet (knownWallet 1) ValidatorOffChain.endpoints
    --h2 <- activateContractWallet (knownWallet 2) endpoints

    callEndpoint @"start" h1 $ ValidatorOffChain.StartParams{  
            spDeadline = deadline,
            spName = 55,
            spAdaQty   = 3000000
        }
    void $ Emulator.waitNSlots 7
    callEndpoint @"get" h1 $ ValidatorOffChain.GetParams { 
            gpName = 55,
            gpAdaQty    = 3000000
        }
    void $ Emulator.waitNSlots 6
    callEndpoint @"get" h1 $ ValidatorOffChain.GetParams{ 
            gpName = 56,
            gpAdaQty    = 3000000
        }
    void $ Emulator.waitNSlots 3
    callEndpoint @"get" h1 $ ValidatorOffChain.GetParams { 
            gpName = 55,
            gpAdaQty    = 3000000
        }
    void $ Emulator.waitNSlots 1