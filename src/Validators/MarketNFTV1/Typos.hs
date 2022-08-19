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

module Validators.MarketNFTV1.Typos where

--Import Externos:

import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Ledger                              (PaymentPubKeyHash, AssetClass)
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Schema                              (ToSchema)

-- Modulo:

minLovelace :: Integer
minLovelace = 2000000

type NFT = Ledger.AssetClass

data ValidatorDatum = ValidatorDatum
    { 
        dSeller :: Ledger.PaymentPubKeyHash, 
        dNFT :: NFT,
        dPrice  :: Integer
    } deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema)

instance P.Eq ValidatorDatum where
    {-# INLINABLE (==) #-}
    a == b = (dSeller a == dSeller b) &&
             (dPrice a == dPrice b) &&
             (dNFT a == dNFT b)

PlutusTx.unstableMakeIsData ''ValidatorDatum
PlutusTx.makeLift ''ValidatorDatum

data ValidatorRedeemer = 
    RedeemSellerGetBackNFT |
    RedeemBuyerBuyNFT
    
    deriving P.Show

PlutusTx.unstableMakeIsData ''ValidatorRedeemer
PlutusTx.makeLift ''ValidatorRedeemer
