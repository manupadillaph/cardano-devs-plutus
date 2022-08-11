{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Validators.MarketNFT.OnChainNFT where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

--Import Internos
import  Validators.MarketNFT.Typos 
import  Validators.MarketNFT.Helpers 

-- {-# INLINABLE mkPolicy #-}
-- mkPolicy :: TxOutRef -> TokenName  ->() -> ScriptContext -> Bool
-- mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasInputUTxO           &&
--                           traceIfFalse "wrong amount minted" checkMintedAmount
--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx

--     hasInputUTxO :: Bool
--     hasInputUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

--     checkMintedAmount :: Bool
--     checkMintedAmount = case flattenValue (txInfoMint info) of
--         [(_, tn', amt)] -> tn' == tn && amt == 1
--         -- [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1
--         _               -> False


-- {-# INLINABLE mintingNFTPolicy #-}
-- mintingNFTPolicy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
-- mintingNFTPolicy oref tn = mkMintingPolicyScript $
--     $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
--     `PlutusTx.applyCode`
--     PlutusTx.liftCode oref
--     `PlutusTx.applyCode`
--     PlutusTx.liftCode tn


-- {-# INLINABLE mkPolicy #-}
-- mkPolicy :: TxOutRef ->  TxOutRef -> () -> ScriptContext -> Bool
-- mkPolicy oref oref2 _ ctx = traceIfFalse "UTxO not consumed"   hasInputUTxO           &&
--                           traceIfFalse "wrong amount minted" checkMintedAmount
--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx

--     tn :: TokenName
--     tn = "PPEPE"

--     hasInputUTxO :: Bool
--     hasInputUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

--     checkMintedAmount :: Bool
--     checkMintedAmount = case flattenValue (txInfoMint info) of
-- --         [(_, tn', amt)] -> tn' == tn && amt == 1
-- --         _               -> False
            -- [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1
            --   _                -> False


-- {-# INLINABLE mintingNFTPolicy #-}
-- mintingNFTPolicy :: TxOutRef -> TxOutRef ->  Scripts.MintingPolicy
-- mintingNFTPolicy oref oref2 = mkMintingPolicyScript $
--     $$(PlutusTx.compile [|| \oref' oref2' -> Scripts.wrapMintingPolicy $ mkPolicy oref' oref2'  ||])
--     `PlutusTx.applyCode`
--     PlutusTx.liftCode oref
--      `PlutusTx.applyCode`
--     PlutusTx.liftCode oref2


{-# INLINABLE mkPolicy #-}
--kPolicy ::   MintingRedeemer  -> ScriptContext -> Bool
mkPolicy ::   MintingRedeemer  -> ScriptContext -> Bool

mkPolicy (MintingRedeemer redeemerNFTTokenName redeemerNFTTxOutRef) ctx = 
    traceIfFalse "Minting NFT Policy: UTxO not consumed"   hasInputUTxO           &&
    traceIfFalse "Minting NFT Policy: Wrong amount minted" checkMintedAmount &&
    traceIfFalse "Minting NFT Policy: Wrong NFT TokeName" checkTokenName


  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasInputUTxO :: Bool
    hasInputUTxO = any (\i -> txInInfoOutRef i == redeemerNFTTxOutRef) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == redeemerNFTTokenName && amt == 1
        _                -> False

    checkTokenName :: Bool
    checkTokenName = do
      let 
        idTxOut = txOutRefId redeemerNFTTxOutRef
        indexTxOut = txOutRefIdx  redeemerNFTTxOutRef
      
      redeemerNFTTokenName ==  TokenName (indexTxOut `consByteString`  getTxId  idTxOut  )
  

mkPolicy _ ctx = False

{-# INLINABLE mintingNFTPolicy #-}
mintingNFTPolicy ::  Scripts.MintingPolicy
mintingNFTPolicy  = mkMintingPolicyScript 
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy  mkPolicy   ||])
  
