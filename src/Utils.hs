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
    unsafeTokenNameToHex--,
    --myConsByteString
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
import qualified Data.Text                           as DataText (pack, Text) --, 
import qualified Ledger  
import qualified Ledger.Bytes                        as LedgerBytes (LedgerBytes(LedgerBytes), fromHex)
import qualified Plutus.V1.Ledger.Api                as LedgerApiV1
import qualified Plutus.V1.Ledger.Credential         as LedgerCredentialV1
--import qualified Plutus.V1.Ledger.Crypto     as LedgerCryptoV1 
import qualified Plutus.V1.Ledger.Value              as LedgerValueV1 (TokenName (..))
import qualified PlutusTx.Builtins                   as TxBuiltins (toBuiltin)
import qualified PlutusTx.Builtins.Internal          as TxBuiltinsInternal (BuiltinByteString (..))
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Wallet.Emulator.Wallet              as WalletEmulator      (WalletId (..)) --, Wallet (..)
import qualified Wallet.Types                        as WalletTypes (ContractInstanceId (..))

--Modulo:

credentialLedgerToPlutus :: LedgerCredential.Credential a LedgerCrypto.StandardCrypto -> LedgerCredentialV1.Credential
credentialLedgerToPlutus (LedgerCredential.ScriptHashObj (LedgerHashes.ScriptHash h)) = LedgerApiV1.ScriptCredential $ LedgerApiV1.ValidatorHash $ TxBuiltins.toBuiltin $ CryptoHashClass.hashToBytes h
credentialLedgerToPlutus (LedgerCredential.KeyHashObj (LedgerKeys.KeyHash h))       = LedgerApiV1.PubKeyCredential $ LedgerApiV1.PubKeyHash $ TxBuiltins.toBuiltin $ CryptoHashClass.hashToBytes h

stakeReferenceLedgerToPlutus :: LedgerCredential.StakeReference LedgerCrypto.StandardCrypto -> Maybe LedgerCredentialV1.StakingCredential
stakeReferenceLedgerToPlutus (LedgerCredential.StakeRefBase x)                   = Just $ LedgerApiV1.StakingHash $ credentialLedgerToPlutus x
stakeReferenceLedgerToPlutus (LedgerCredential.StakeRefPtr (LedgerCredential.Ptr (CardanoApi.SlotNo x) txIx certIx)) = 
    let 
      txIxInteger = P.toInteger (LedgerBaseTypes.txIxToInt txIx)
      certIxInteger = P.toInteger (LedgerBaseTypes.certIxToInt certIx)
    in Just $ LedgerApiV1.StakingPtr (P.fromIntegral x) txIxInteger  certIxInteger
stakeReferenceLedgerToPlutus LedgerCredential.StakeRefNull                       = Nothing

tryReadAddress :: P.String -> Maybe LedgerApiV1.Address
tryReadAddress x = case CardanoApi.deserialiseAddress CardanoApi.AsAddressAny $ DataText.pack x of
    Nothing                                      -> Nothing
    Just (ApiShelley.AddressByron _)                        -> Nothing
    Just (ApiShelley.AddressShelley (ApiShelley.ShelleyAddress _ p s)) -> Just LedgerApiV1.Address
        { 
          LedgerApiV1.addressCredential        = credentialLedgerToPlutus p, 
          LedgerApiV1.addressStakingCredential = stakeReferenceLedgerToPlutus s
        }

tryReadWalletId :: P.String -> Maybe WalletEmulator.WalletId
tryReadWalletId = DataAeson.decode . DataAeson.encode

unsafeReadWalletId :: P.String -> WalletEmulator.WalletId
unsafeReadWalletId s = DataMaybe.fromMaybe (P.error $ "can't parse " ++ s ++ " as a WalletId") $ tryReadWalletId s

unsafeReadAddress :: P.String -> LedgerApiV1.Address
unsafeReadAddress s = DataMaybe.fromMaybe (P.error $ "can't parse " ++ s ++ " as an address") $ tryReadAddress s

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

getCredentials :: LedgerApiV1.Address -> Maybe (Ledger.PaymentPubKeyHash, Maybe Ledger.StakePubKeyHash)
getCredentials (LedgerApiV1.Address x y) = case x of
    LedgerApiV1.ScriptCredential _   -> Nothing
    LedgerApiV1.PubKeyCredential pkh ->
      let
        ppkh = Ledger.PaymentPubKeyHash pkh
      in
        case y of
            Nothing                        -> Just (ppkh, Nothing)
            Just (LedgerApiV1.StakingPtr _ _ _) -> Nothing
            Just (LedgerApiV1.StakingHash h)           -> case h of
                LedgerApiV1.ScriptCredential _    -> Nothing
                LedgerApiV1.PubKeyCredential pkh' -> Just (ppkh, Just $ Ledger.StakePubKeyHash pkh')

unsafePaymentPubKeyHash :: LedgerApiV1.Address -> Ledger.PaymentPubKeyHash
unsafePaymentPubKeyHash addr = maybe (P.error $ "script address " ++ P.show addr ++ " does not contain a payment key") fst $ getCredentials addr

unsafeStakePubKeyHash :: LedgerApiV1.Address -> Ledger.StakePubKeyHash
unsafeStakePubKeyHash addr = case getCredentials addr of
    Nothing           -> P.error $ "unexpected script address " ++ P.show addr
    Just (_, Nothing) -> P.error $ "addres " ++ P.show addr ++ " contains no stake component"
    Just (_, Just x)  -> x

cidToString :: WalletTypes.ContractInstanceId -> P.String
cidToString = P.show . WalletTypes.unContractInstanceId

unsafeTokenNameToHex :: LedgerValueV1.TokenName -> P.String
unsafeTokenNameToHex = DataByteStringChar8.unpack . CardanoApi.serialiseToRawBytesHex . DataMaybe.fromJust . CardanoApi.deserialiseFromRawBytes CardanoApi.AsAssetName . getByteString . LedgerValueV1.unTokenName
  where
    getByteString (TxBuiltinsInternal.BuiltinByteString bs) = bs

pkhFromStr :: P.String -> LedgerApiV1.PubKeyHash   
pkhFromStr s =         
    case LedgerBytes.fromHex (DataString.fromString s) of 
        Right (LedgerBytes.LedgerBytes bytes) ->  LedgerApiV1.PubKeyHash bytes 
        Left msg -> P.error $ "Could not convert from hex to bytes: " ++ P.show msg

-- myConsByteString :: DataText.Text -> Integer -> DataText.Text
-- myConsByteString text int = do
--     let
--       builtByteString = PlutusTx.Prelude.encodeUtf8  $ PlutusTx.Prelude.toBuiltin text
--       res = int `PlutusTx.Prelude.consByteString` builtByteString
--       --res1 = PlutusTx.Prelude.decodeUtf8 res
--     PlutusTx.Prelude.fromBuiltin res

-- pkhFromStr :: P.String ->  Maybe PubKeyHash   
-- pkhFromStr s =  do
--   let 
--     hex = LedgerBytes.fromHex (DataString.fromString s)
--   case hex of
--     Right (LedgerBytes.LedgerBytes bytes) -> Just (PubKeyHash bytes )
--     Left msg -> Nothing
  

  --  case LedgerBytes.fromHex (DataString.fromString s) of 
  --   (LedgerBytes.LedgerBytes bytes) -> PubKeyHash bytes 
  --   Nothing -> error $ "Could not convert from hex to bytes: " ++ show s
    


    -- case LedgerBytes.fromHex (DataString.fromString s) of 
    --     Right (LedgerBytes.LedgerBytes bytes) -> Right $ PubKeyHash bytes 
    --     Left msg -> Left $ DataText.pack ("Could not convert from hex to bytes: " <> msg)




-- contractActivationArgs :: WalletId -> a -> ContractActivationArgs a
-- contractActivationArgs wid a = ContractActivationArgs
--     { caID = a
--     , caWallet = Just $ Wallet {getWalletId = wid}
--     }



-- dataToScriptData :: Data -> ScriptData
-- dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
-- dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
-- dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
-- dataToScriptData (I n)         = ScriptDataNumber n
-- dataToScriptData (B bs)        = ScriptDataBytes bs
