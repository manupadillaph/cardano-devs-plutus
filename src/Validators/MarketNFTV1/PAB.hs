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

module Validators.MarketNFTV1.PAB
    ( 
        ValidatorContracts (..)
    ) where

--Import Externos

import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Plutus.PAB.Effects.Contract.Builtin as PABEffectsContractBuiltin (Empty, HasDefinitions (..), SomeBuiltin (..), endpointsToSchemas)
import qualified Plutus.V1.Ledger.Value              as LedgerValueV1
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Prettyprinter                       (Pretty (..), viaShow)

--Import Internos

import qualified Validators.MarketNFTV1.OffChain        as OffChain

--Import Modulo:
 
data ValidatorContracts = 
    SellNFT OffChain.SellNFTParams | 
    BuyNFT OffChain.BuyNFTParams | 
    GetBackNFT OffChain.GetBackNFTParams
    deriving (P.Eq, P.Ord, P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)

instance Prettyprinter.Pretty ValidatorContracts where
    pretty = Prettyprinter.viaShow

instance PABEffectsContractBuiltin.HasDefinitions ValidatorContracts where

    getDefinitions        = [SellNFT exampleSellNFTParams, BuyNFT exampleBuyNFTParams, GetBackNFT exampleGetBackNFTParams]

    getContract (SellNFT sp) = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.sell         @() @PABEffectsContractBuiltin.Empty sp
    getContract (BuyNFT bp)   = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.buy         @() @PABEffectsContractBuiltin.Empty bp
    getContract (GetBackNFT gbp)  = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.getback @() @PABEffectsContractBuiltin.Empty gbp

    getSchema = const $ PABEffectsContractBuiltin.endpointsToSchemas @PABEffectsContractBuiltin.Empty

exampleSellNFTParams :: OffChain.SellNFTParams
exampleSellNFTParams = OffChain.SellNFTParams
    { 
        OffChain.spNFT   = LedgerValueV1.assetClass (LedgerValueV1.currencySymbol "TEST") (LedgerValueV1.tokenName "TEST"),
        OffChain.spPrice = 0
    }

exampleBuyNFTParams :: OffChain.BuyNFTParams
exampleBuyNFTParams = OffChain.BuyNFTParams
    { 
        OffChain.bpNFT   = LedgerValueV1.assetClass (LedgerValueV1.currencySymbol "TEST") (LedgerValueV1.tokenName "TEST"),
        OffChain.bpPrice = 0

    }

exampleGetBackNFTParams :: OffChain.GetBackNFTParams
exampleGetBackNFTParams = OffChain.GetBackNFTParams
    { 
        OffChain.gbpNFT   = LedgerValueV1.assetClass (LedgerValueV1.currencySymbol "TEST") (LedgerValueV1.tokenName "TEST")
    }
