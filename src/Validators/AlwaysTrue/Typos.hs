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

--{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module  Validators.AlwaysTrue.Typos (
    ValidatorDatum (..), ValidatorData (..), ValidatorRedeemer (..)  
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


data ValidatorData = ValidatorData
    { 
        aCreator   :: !PaymentPubKeyHash
        , aDeadline :: !POSIXTime
        , aName :: !Integer
        , aAdaQty    :: !Integer 
    } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

instance  Eq ValidatorData where
    {-# INLINABLE (==) #-}
    a == b = (aCreator   a == aCreator   b) &&
             (aDeadline a == aDeadline b) &&
             (aName a == aName b) &&  
             (aAdaQty   a == aAdaQty   b)

PlutusTx.unstableMakeIsData ''ValidatorData
PlutusTx.makeLift ''ValidatorData

data ValidatorRedeemer = ValidatorRedeemer
    {
        rTipo :: !Integer
    }
    deriving P.Show

PlutusTx.unstableMakeIsData ''ValidatorRedeemer
PlutusTx.makeLift ''ValidatorRedeemer

data ValidatorDatum = ValidatorDatum
    {
        dData    :: !ValidatorData
    } deriving P.Show

PlutusTx.unstableMakeIsData ''ValidatorDatum
PlutusTx.makeLift ''ValidatorDatum




