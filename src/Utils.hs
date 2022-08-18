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

module Utils
  ( 
    tryReadAddress, unsafeReadAddress,
    tryReadWalletId, unsafeReadWalletId,
    unsafeReadTxOutRef,
    -- contractActivationArgs,
    getCredentials, unsafePaymentPubKeyHash, unsafeStakePubKeyHash, pkhFromStr,
    cidToString,
    unsafeTokenNameToHex
  ) where

--Import Externos

import qualified Cardano.Api                         as CardanoApi
import qualified Cardano.Api.Shelley                 as ApiShelley (Address (..), AddressAny(..)) --, PlutusScript (..)
import qualified Cardano.Crypto.Hash.Class           as CryptoHashClass (hashToBytes)
import qualified Cardano.Ledger.BaseTypes            as LedgerBaseTypes (certIxToInt, txIxToInt)
import qualified Cardano.Ledger.Credential           as LedgerCredential
import qualified Cardano.Ledger.Crypto               as LedgerCrypto (StandardCrypto)
import qualified Cardano.Ledger.Hashes               as LedgerHashes (ScriptHash (..))
import qualified Cardano.Ledger.Keys                 as LedgerKeys (KeyHash (..))
import qualified Data.Aeson                          as DataAeson (decode, encode)
import qualified Data.ByteString.Char8               as DataByteStringChar8
import qualified Data.Maybe                          as DataMaybe (fromJust, fromMaybe)
import qualified Data.String                         as DataString (IsString (fromString))
import qualified Data.Text                           as DataText (pack) --Text, 
import qualified Ledger  
import qualified Ledger.Bytes                        as LedgerBytes (LedgerBytes(LedgerBytes), fromHex)
import qualified Plutus.V1.Ledger.Api                as LedgerApiV1
import qualified Plutus.V1.Ledger.Credential         as LedgerCredentialV1
--import qualified Plutus.V1.Ledger.Crypto     as LedgerCryptoV1 
import qualified Plutus.V1.Ledger.Value              as LedgerValueV1 (TokenName (..))
import qualified PlutusTx.Builtins                   as TxBuiltins (toBuiltin)
import qualified PlutusTx.Builtins.Internal          as TxBuiltinsInternal (BuiltinByteString (..))
import qualified Prelude                             as P
import qualified Wallet.Emulator.Wallet              as WalletEmulator      (WalletId (..)) --, Wallet (..)
import qualified Wallet.Types                        as WalletTypes (ContractInstanceId (..))

--Modulo:

credentialLedgerToPlutus :: LedgerCredential.Credential a LedgerCrypto.StandardCrypto -> LedgerCredentialV1.Credential
credentialLedgerToPlutus (LedgerCredential.ScriptHashObj (LedgerHashes.ScriptHash h)) = LedgerApiV1.ScriptCredential P.$ LedgerApiV1.ValidatorHash P.$ TxBuiltins.toBuiltin P.$ CryptoHashClass.hashToBytes h
credentialLedgerToPlutus (LedgerCredential.KeyHashObj (LedgerKeys.KeyHash h))       = LedgerApiV1.PubKeyCredential P.$ LedgerApiV1.PubKeyHash P.$ TxBuiltins.toBuiltin P.$ CryptoHashClass.hashToBytes h

stakeReferenceLedgerToPlutus :: LedgerCredential.StakeReference LedgerCrypto.StandardCrypto -> P.Maybe LedgerCredentialV1.StakingCredential
stakeReferenceLedgerToPlutus (LedgerCredential.StakeRefBase x)                   = P.Just P.$ LedgerApiV1.StakingHash P.$ credentialLedgerToPlutus x
stakeReferenceLedgerToPlutus (LedgerCredential.StakeRefPtr (LedgerCredential.Ptr (CardanoApi.SlotNo x) txIx certIx)) = 
    let 
      txIxInteger = P.toInteger (LedgerBaseTypes.txIxToInt txIx)
      certIxInteger = P.toInteger (LedgerBaseTypes.certIxToInt certIx)
    in P.Just P.$ LedgerApiV1.StakingPtr (P.fromIntegral x) txIxInteger  certIxInteger
stakeReferenceLedgerToPlutus LedgerCredential.StakeRefNull                       = P.Nothing

tryReadAddress :: P.String -> P.Maybe LedgerApiV1.Address
tryReadAddress x = case CardanoApi.deserialiseAddress CardanoApi.AsAddressAny P.$ DataText.pack x of
    P.Nothing                                      -> P.Nothing
    P.Just (ApiShelley.AddressByron _)                        -> P.Nothing
    P.Just (ApiShelley.AddressShelley (ApiShelley.ShelleyAddress _ p s)) -> P.Just LedgerApiV1.Address
        { 
          LedgerApiV1.addressCredential        = credentialLedgerToPlutus p, 
          LedgerApiV1.addressStakingCredential = stakeReferenceLedgerToPlutus s
        }

tryReadWalletId :: P.String -> P.Maybe WalletEmulator.WalletId
tryReadWalletId = DataAeson.decode P.. DataAeson.encode

unsafeReadWalletId :: P.String -> WalletEmulator.WalletId
unsafeReadWalletId s = DataMaybe.fromMaybe (P.error P.$ "can't parse " P.++ s P.++ " as a WalletId") P.$ tryReadWalletId s

unsafeReadAddress :: P.String -> LedgerApiV1.Address
unsafeReadAddress s = DataMaybe.fromMaybe (P.error P.$ "can't parse " P.++ s P.++ " as an address") P.$ tryReadAddress s

unsafeReadTxOutRef :: P.String -> LedgerApiV1.TxOutRef
unsafeReadTxOutRef s =
  let
    (x, _ : y) = P.span (P./= '#') s
  in
    LedgerApiV1.TxOutRef
        { 
          LedgerApiV1.txOutRefId  = DataString.fromString x,
          LedgerApiV1.txOutRefIdx = P.read y
        }

getCredentials :: LedgerApiV1.Address -> P.Maybe (Ledger.PaymentPubKeyHash, P.Maybe Ledger.StakePubKeyHash)
getCredentials (LedgerApiV1.Address x y) = case x of
    LedgerApiV1.ScriptCredential _   -> P.Nothing
    LedgerApiV1.PubKeyCredential pkh ->
      let
        ppkh = Ledger.PaymentPubKeyHash pkh
      in
        case y of
            P.Nothing                        -> P.Just (ppkh, P.Nothing)
            P.Just (LedgerApiV1.StakingPtr _ _ _) -> P.Nothing
            P.Just (LedgerApiV1.StakingHash h)           -> case h of
                LedgerApiV1.ScriptCredential _    -> P.Nothing
                LedgerApiV1.PubKeyCredential pkh' -> P.Just (ppkh, P.Just P.$ Ledger.StakePubKeyHash pkh')

unsafePaymentPubKeyHash :: LedgerApiV1.Address -> Ledger.PaymentPubKeyHash
unsafePaymentPubKeyHash addr = P.maybe (P.error P.$ "script address " P.++ P.show addr P.++ " does not contain a payment key") P.fst P.$ getCredentials addr

unsafeStakePubKeyHash :: LedgerApiV1.Address -> Ledger.StakePubKeyHash
unsafeStakePubKeyHash addr = case getCredentials addr of
    P.Nothing           -> P.error P.$ "unexpected script address " P.++ P.show addr
    P.Just (_, P.Nothing) -> P.error P.$ "addres " P.++ P.show addr P.++ " contains no stake component"
    P.Just (_, P.Just x)  -> x

cidToString :: WalletTypes.ContractInstanceId -> P.String
cidToString = P.show P.. WalletTypes.unContractInstanceId

unsafeTokenNameToHex :: LedgerValueV1.TokenName -> P.String
unsafeTokenNameToHex = DataByteStringChar8.unpack P.. CardanoApi.serialiseToRawBytesHex P.. DataMaybe.fromJust P.. CardanoApi.deserialiseFromRawBytes CardanoApi.AsAssetName P.. getByteString P.. LedgerValueV1.unTokenName
  where
    getByteString (TxBuiltinsInternal.BuiltinByteString bs) = bs

pkhFromStr :: P.String -> LedgerApiV1.PubKeyHash   
pkhFromStr s =         
    case LedgerBytes.fromHex (DataString.fromString s) of 
        P.Right (LedgerBytes.LedgerBytes bytes) ->  LedgerApiV1.PubKeyHash bytes 
        P.Left msg -> P.error P.$ "Could not convert from hex to bytes: " P.++ P.show msg



-- pkhFromStr :: P.String ->  P.Maybe PubKeyHash   
-- pkhFromStr s =  do
--   let 
--     hex = LedgerBytes.fromHex (DataString.fromString s)
--   case hex of
--     P.Right (LedgerBytes.LedgerBytes bytes) -> P.Just (PubKeyHash bytes )
--     P.Left msg -> P.Nothing
  

  --  case LedgerBytes.fromHex (DataString.fromString s) of 
  --   (LedgerBytes.LedgerBytes bytes) -> PubKeyHash bytes 
  --   P.Nothing -> error P.$ "Could not convert from hex to bytes: " P.++ show s
    


    -- case LedgerBytes.fromHex (DataString.fromString s) of 
    --     P.Right (LedgerBytes.LedgerBytes bytes) -> P.Right P.$ PubKeyHash bytes 
    --     P.Left msg -> P.Left P.$ DataText.pack ("Could not convert from hex to bytes: " <> msg)




-- contractActivationArgs :: WalletId -> a -> ContractActivationArgs a
-- contractActivationArgs wid a = ContractActivationArgs
--     { caID = a
--     , caWallet = P.Just P.$ Wallet {getWalletId = wid}
--     }



-- dataToScriptData :: Data -> ScriptData
-- dataToScriptData (Constr n xs) = ScriptDataConstructor n P.$ dataToScriptData P.<$> xs
-- dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
-- dataToScriptData (List xs)     = ScriptDataList P.$ dataToScriptData P.<$> xs
-- dataToScriptData (I n)         = ScriptDataNumber n
-- dataToScriptData (B bs)        = ScriptDataBytes bs
