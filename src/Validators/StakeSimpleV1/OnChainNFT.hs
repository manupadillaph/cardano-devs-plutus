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

module Validators.StakeSimpleV1.OnChainNFT where

-- import qualified Control.Monad                       as Monad (void)
-- import           Data.Aeson             (DataAeson.ToJSON, DataAeson.FromJSON)
-- import qualified Data.Map               as Map
-- import           Data.Text              (Text)
-- import           Data.Void              (Void)
-- import           GHC.Generics           (Generic)
-- import  Plutus.Contract                     as PlutusContract 
-- import           Plutus.Trace.Emulator  as Emulator
-- import qualified PlutusTx
-- import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
-- import           Ledger                 hiding (mint, singleton)
-- import           Ledger.Constraints     as Constraints
-- import qualified Ledger.Typed.Scripts   as Scripts
-- import           LedgerValueV1.Value           as Value
-- import           Prelude                (IO, Semigroup (..), Show (..), String)
-- import           Text.Printf            (printf)
-- import qualified Wallet.Emulator.Wallet              as WalletEmulator

-- --Import Internos
-- import qualified Validators.StakeSimpleV1.Typos 
-- import qualified Validators.StakeSimpleV1.Helpers 

--Import Internos

import qualified Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies as UtilsTypedScriptsMintingV1
import qualified Plutus.V1.Ledger.Api                as LedgerApiV1
import qualified Plutus.V1.Ledger.Contexts           as LedgerContextsV1 (ScriptContext, TxInfo, scriptContextTxInfo, ownCurrencySymbol) --, txSignedBy
import qualified Plutus.V1.Ledger.Scripts            as LedgerScriptsV1
import qualified Plutus.V1.Ledger.Value              as LedgerValueV1
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)

--Import Internos

import qualified Validators.StakeSimpleV1.Typos        as T

-- Modulo:

-- {-# INLINABLE mkPolicy #-}
-- mkPolicy :: LedgerApiV1.TxOutRef -> LedgerValueV1.TokenName  ->() -> LedgerContextsV1.ScriptContext -> Bool
-- mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasInputUTxO           &&
--                           traceIfFalse "wrong amount minted" checkMintedAmount
--   where
--     info :: LedgerContextsV1.TxInfo
--     info = LedgerContextsV1.scriptContextTxInfo ctx

--     hasInputUTxO :: Bool
--     hasInputUTxO = any (\i -> LedgerApiV1.txInInfoOutRef i == oref) $ LedgerApiV1.txInfoInputs info

--     checkMintedAmount :: Bool
--     checkMintedAmount = case LedgerValueV1.flattenValue (LedgerApiV1.txInfoMint info) of
--         [(_, tn', amt)] -> tn' == tn && amt == 1
--         -- [(cs, tn', amt)] -> cs  == LedgerContextsV1.ownCurrencySymbol ctx && tn' == tn && amt == 1
--         _              -> False


-- {-# INLINABLE mintingNFTPolicy #-}
-- mintingNFTPolicy :: LedgerApiV1.TxOutRef -> LedgerValueV1.TokenName -> LedgerScriptsV1.MintingPolicy
-- mintingNFTPolicy oref tn = LedgerScriptsV1.mkMintingPolicyScript $
--     $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
--     `PlutusTx.applyCode`
--     PlutusTx.liftCode oref
--     `PlutusTx.applyCode`
--     PlutusTx.liftCode tn


-- {-# INLINABLE mkPolicy #-}
-- mkPolicy :: LedgerApiV1.TxOutRef -> TxOutRef -> () -> LedgerContextsV1.ScriptContext -> Bool
-- mkPolicy oref oref2 _ ctx = traceIfFalse "UTxO not consumed"   hasInputUTxO           &&
--                           traceIfFalse "wrong amount minted" checkMintedAmount
--   where
--     info :: LedgerContextsV1.TxInfo
--     info = LedgerContextsV1.scriptContextTxInfo ctx

--     tn :: LedgerValueV1.TokenName
--     tn = "PPEPE"

--     hasInputUTxO :: Bool
--     hasInputUTxO = any (\i -> LedgerApiV1.txInInfoOutRef i == oref) $ LedgerApiV1.txInfoInputs info

--     checkMintedAmount :: Bool
--     checkMintedAmount = case LedgerValueV1.flattenValue (LedgerApiV1.txInfoMint info) of
-- --         [(_, tn', amt)] -> tn' == tn && amt == 1
-- --         _              -> False
            -- [(cs, tn', amt)] -> cs  == LedgerContextsV1.ownCurrencySymbol ctx && tn' == tn && amt == 1
            --   _               -> False


-- {-# INLINABLE mintingNFTPolicy #-}
-- mintingNFTPolicy :: LedgerApiV1.TxOutRef -> LedgerApiV1.TxOutRef -> LedgerScriptsV1.MintingPolicy
-- mintingNFTPolicy oref oref2 = LedgerScriptsV1.mkMintingPolicyScript $
--     $$(PlutusTx.compile [|| \oref' oref2' -> Scripts.wrapMintingPolicy $ mkPolicy oref' oref2'  ||])
--     `PlutusTx.applyCode`
--     PlutusTx.liftCode oref
--      `PlutusTx.applyCode`
--     PlutusTx.liftCode oref2


{-# INLINABLE mkPolicy #-}
mkPolicy ::   T.MintingRedeemer -> LedgerContextsV1.ScriptContext -> Bool
mkPolicy (T.MintingRedeemer redeemerNFTTokenName redeemerNFTTxOutRef) ctx = 
    traceIfFalse "Minting NFT Policy: UTxO not consumed"   hasInputUTxO           &&
    traceIfFalse "Minting NFT Policy: Wrong amount minted" checkMintedAmount &&
    traceIfFalse "Minting NFT Policy: Wrong NFT TokeName" checkTokenName


  where
    info :: LedgerContextsV1.TxInfo
    info = LedgerContextsV1.scriptContextTxInfo ctx

    hasInputUTxO :: Bool
    hasInputUTxO = any (\i -> LedgerApiV1.txInInfoOutRef i == redeemerNFTTxOutRef) $ LedgerApiV1.txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case LedgerValueV1.flattenValue (LedgerApiV1.txInfoMint info) of
        [(cs, tn', amt)] -> cs  == LedgerContextsV1.ownCurrencySymbol ctx && tn' == redeemerNFTTokenName && amt == 1
        _               -> False

    checkTokenName :: Bool
    checkTokenName = do
      let 
        idTxOut = LedgerApiV1.txOutRefId redeemerNFTTxOutRef
        indexTxOut = LedgerApiV1.txOutRefIdx  redeemerNFTTxOutRef
      
      redeemerNFTTokenName ==  LedgerValueV1.TokenName (indexTxOut `consByteString`  LedgerApiV1.getTxId  idTxOut  )
  

mkPolicy _ ctx = False

{-# INLINABLE mintingNFTPolicy #-}
mintingNFTPolicy ::  LedgerScriptsV1.MintingPolicy
mintingNFTPolicy  = LedgerScriptsV1.mkMintingPolicyScript 
    $$(PlutusTx.compile [|| UtilsTypedScriptsMintingV1.mkUntypedMintingPolicy  mkPolicy   ||])
  
