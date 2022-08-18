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

module Validators.AlwaysFalseV1.Typos where

--Import Externos

import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Ledger                              (PaymentPubKeyHash)
import qualified Plutus.V1.Ledger.Api                as LedgerApiV1
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Schema                              (ToSchema)

-- Modulo:

minLovelace :: Integer
minLovelace = 2000000

data ValidatorData = ValidatorData
    { 
        aCreator  :: Ledger.PaymentPubKeyHash,
        aDeadline :: LedgerApiV1.POSIXTime, 
        aName     :: Integer,
        aAdaQty   :: Integer 
    } deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema)

instance P.Eq ValidatorData where
    {-# INLINABLE (==) #-}
    a == b = (aCreator   a P.== aCreator   b) P.&&
             (aDeadline a P.== aDeadline b) P.&&
             (aName a P.== aName b) P.&&  
             (aAdaQty a P.== aAdaQty   b)

PlutusTx.unstableMakeIsData ''ValidatorData
PlutusTx.makeLift ''ValidatorData

newtype ValidatorDatum = ValidatorDatum
    {
        dData :: ValidatorData
    } deriving P.Show

PlutusTx.unstableMakeIsData ''ValidatorDatum
PlutusTx.makeLift ''ValidatorDatum

newtype ValidatorRedeemer = ValidatorRedeemer
    {
        rTipo :: Integer
    } deriving P.Show

PlutusTx.unstableMakeIsData ''ValidatorRedeemer
PlutusTx.makeLift ''ValidatorRedeemer





