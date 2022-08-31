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

module Validators.MarketNFTV1.OnChain
  ( 
    codeValidator, 
    typedValidator, 
    hashValidator, 
    addressValidator
  ) where

--Import Externos

import qualified Ledger                              (unPaymentPubKeyHash, valuePaidTo)
import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators as UtilsTypedScriptsValidatorsV1
import qualified Ledger.Ada                          as LedgerAda   
import qualified Plutus.V1.Ledger.Address            as LedgerAddressV1
import qualified Plutus.V1.Ledger.Contexts           as LedgerContextsV1 (ScriptContext, TxInfo, scriptContextTxInfo, txSignedBy)
import qualified Plutus.V1.Ledger.Scripts            as LedgerScriptsV1
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)


    --solo debe haber dos entradas y dos salidas
    --la entrada con el NFT desde el contrato
    --la entrada con el pago desde la wallet del comprador
    --la salida con el NFT para el comprador
    --la salida con el pago para el vendedor
    -- TODO:
    -- deberia permitir cuatas entradas quiera
    -- solo que por cada datum que vaya a consumir, ver datum de este parametro,
    -- que haya una salida que vaya a la payment address que dice el datum con el monto que dice el datum


--Import Internos:

import qualified Validators.MarketNFTV1.Typos           as T (ValidatorDatum (..), ValidatorRedeemer (..))

--Modulo:

data ValidatorScriptV1
instance UtilsTypedScriptsValidatorsV1.ValidatorTypes ValidatorScriptV1 where
    type instance RedeemerType ValidatorScriptV1 = T.ValidatorRedeemer
    type instance DatumType ValidatorScriptV1 = T.ValidatorDatum

{-# INLINABLE mkValidator #-}
mkValidator :: T.ValidatorDatum -> T.ValidatorRedeemer -> LedgerContextsV1.ScriptContext -> Bool

mkValidator datum T.RedeemBuyerBuyNFT ctx =
    validateBuyerBuyNFT datum ctx

mkValidator datum T.RedeemSellerGetBackNFT ctx =
    validateSellerGetBack datum ctx

mkValidator _ _ _ =
     traceIfFalse "ALDEA Market NFT: Operación Inválida" False 


{-# INLINABLE validateBuyerBuyNFT #-}
validateBuyerBuyNFT :: T.ValidatorDatum -> LedgerContextsV1.ScriptContext -> Bool
validateBuyerBuyNFT datum ctx  =

    traceIfFalse "ALDEA Market NFT: Monto de pago incorrecto" checkPaymentInOutputToSeller

  where
    info :: LedgerContextsV1.TxInfo
    info = LedgerContextsV1.scriptContextTxInfo ctx

    checkPaymentInOutputToSeller :: Bool
    checkPaymentInOutputToSeller = do
        let 
          outPutValue = Ledger.valuePaidTo info (Ledger.unPaymentPubKeyHash $ T.dSeller datum )

        outPutValue ==  (LedgerAda.lovelaceValueOf $ T.dPrice datum)


{-# INLINABLE validateSellerGetBack #-}
validateSellerGetBack :: T.ValidatorDatum -> LedgerContextsV1.ScriptContext -> Bool
validateSellerGetBack datum  ctx  =

    traceIfFalse "ALDEA Market NFT: Solo el vendedor puede recuperar su NFT" signedBySeller

  where
    info :: LedgerContextsV1.TxInfo
    info = LedgerContextsV1.scriptContextTxInfo ctx

    signedBySeller :: Bool
    signedBySeller = LedgerContextsV1.txSignedBy info $ Ledger.unPaymentPubKeyHash $ T.dSeller datum


typedValidator :: UtilsTypedScriptsValidatorsV1.TypedValidator ValidatorScriptV1
typedValidator = UtilsTypedScriptsValidatorsV1.mkTypedValidator @ValidatorScriptV1
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = UtilsTypedScriptsValidatorsV1.mkUntypedValidator 

codeValidator :: LedgerScriptsV1.Validator
codeValidator = UtilsTypedScriptsValidatorsV1.validatorScript typedValidator

hashValidator :: LedgerScriptsV1.ValidatorHash
hashValidator = UtilsTypedScriptsValidatorsV1.validatorHash typedValidator

addressValidator :: LedgerAddressV1.Address
addressValidator = UtilsTypedScriptsValidatorsV1.validatorAddress typedValidator












