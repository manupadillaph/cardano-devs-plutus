{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Deploy
    ( writeUnit
    , writeRedeemer
    , writeDatum
    , writeValidatorLockerHash
    , writeValidatorLocker
    , writeValidatorAlwaysTrueHash
    , writeValidatorAlwaysTrue
    , writeValidatorAlwaysFalseHash
    , writeValidatorAlwaysFalse
    , writeValidatorBeneficiaryHash
    , writeValidatorBeneficiary
    , writeValidatorDeadlineHash
    , writeValidatorDeadline
    , writeValidatorRedeemerHash
    , writeValidatorRedeemer
    , writeMintingPolicyFree
    , writeMintingPolicyNFT
    , writeMintingPolicyPlus
    , writeMintingPolicySignedAddr
    , writeMintingPolicySignedPkh
    ) where

import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode,ToJSON, FromJSON)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           PlutusTx              (Data (..))
import qualified PlutusTx
import           Plutus.V1.Ledger.Credential as Plutus
import           Plutus.V1.Ledger.Crypto     as Plutus
import qualified Ledger                      as Plutus

import  Prelude              as P

import           Data.String (IsString(fromString))
import           Data.Either (fromRight)
import           Data.Text (Text, pack)
import           Ledger 
import           Ledger.Bytes (LedgerBytes(LedgerBytes), fromHex)

import qualified Data.ByteString.Char8 as BS 
import Data.Char                   (chr)

import  qualified        Plutus.V1.Ledger.Scripts as SC

import   qualified        ValidatorLocker
import   qualified        ValidatorAlwaysTrue
import   qualified        ValidatorAlwaysFalse
import   qualified        ValidatorBeneficiary
import   qualified        ValidatorDeadline
import   qualified        ValidatorRedeemer

import qualified MintPolicyFree (policy)
import qualified MintPolicyNFT (policy)
import qualified MintPolicyPlus (policy)
import qualified MintPolicySigned (policy)

import Utils         (unsafeReadTxOutRef,unsafeReadAddress, unsafePaymentPubKeyHash,pkhFromStr)

stringToBS :: String -> BS.ByteString
stringToBS str = BS.pack str

stringToLBS :: String -> LBS.ByteString
stringToLBS str = LBS.fromStrict (stringToBS str)

bsToString :: LBS.ByteString -> String
bsToString bs = map (chr . fromEnum) . LBS.unpack $ bs

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData


writeUnit :: String -> IO ()
writeUnit path = writeJSON (path ++ "/unit.json") ()

writeRedeemer :: String -> String -> Integer -> IO ()
writeRedeemer path file opcion = do
    let 
        redeemer = ValidatorLocker.ValidatorRedeemer { 
            ValidatorLocker.rTipo = opcion
        }
        opcionStr = P.show opcion
    writeJSON (path ++ "/" ++ file ++ ".json") redeemer



writeDatum:: String -> String -> String -> Integer -> Integer -> Integer-> IO ()
writeDatum path file creator deadline name qty = do
    let 
        pkh = pkhFromStr creator
    
        datum = ValidatorLocker.ValidatorDatum {
            ValidatorLocker.dData = ValidatorLocker.ValidatorData{
                    ValidatorLocker.aCreator =  Ledger.PaymentPubKeyHash pkh
                    , ValidatorLocker.aDeadline    = fromInteger  deadline
                    , ValidatorLocker.aName = name
                    , ValidatorLocker.aAdaQty  = qty 
                }
            }

    writeJSON (path ++ "/" ++ file ++ ".json") datum


writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

writeValidatorLocker :: String -> String -> IO (Either (FileError ()) ())
writeValidatorLocker path file = do
    writeValidator (path ++ "/" ++ file ++ ".plutus") ValidatorLocker.codeValidator  

writeValidatorLockerHash :: String ->String -> IO ()
writeValidatorLockerHash path file = do
    writeJSON (path ++ "/" ++ file ++ ".hash") ValidatorLocker.hashValidator

-- writeValidatorLockerAddress :: String -> String -> IO ()
-- writeValidatorLockerAddress path file = do
--     writeJSON  (path ++ "/" ++ file ++ ".addr") (ValidatorLocker.addressValidator)


writeValidatorAlwaysTrue :: String -> String -> IO (Either (FileError ()) ())
writeValidatorAlwaysTrue path file = do
    writeValidator (path ++ "/" ++ file ++ ".plutus") ValidatorAlwaysTrue.codeValidator  

writeValidatorAlwaysTrueHash :: String ->String -> IO ()
writeValidatorAlwaysTrueHash path file = do
    writeJSON (path ++ "/" ++ file ++ ".hash") ValidatorAlwaysTrue.hashValidator

    
writeValidatorAlwaysFalse :: String -> String -> IO (Either (FileError ()) ())
writeValidatorAlwaysFalse path file = do
    writeValidator (path ++ "/" ++ file ++ ".plutus") ValidatorAlwaysFalse.codeValidator  

writeValidatorAlwaysFalseHash :: String ->String -> IO ()
writeValidatorAlwaysFalseHash path file = do
    writeJSON (path ++ "/" ++ file ++ ".hash") ValidatorAlwaysFalse.hashValidator


writeValidatorBeneficiary :: String -> String -> IO (Either (FileError ()) ())
writeValidatorBeneficiary path file = do
    writeValidator (path ++ "/" ++ file ++ ".plutus") ValidatorBeneficiary.codeValidator  

writeValidatorBeneficiaryHash :: String ->String -> IO ()
writeValidatorBeneficiaryHash path file = do
    writeJSON (path ++ "/" ++ file ++ ".hash") ValidatorBeneficiary.hashValidator

    
writeValidatorDeadline :: String -> String -> IO (Either (FileError ()) ())
writeValidatorDeadline path file = do
    writeValidator (path ++ "/" ++ file ++ ".plutus") ValidatorDeadline.codeValidator  

writeValidatorDeadlineHash :: String ->String -> IO ()
writeValidatorDeadlineHash path file = do
    writeJSON (path ++ "/" ++ file ++ ".hash") ValidatorDeadline.hashValidator


    
writeValidatorRedeemer :: String -> String -> IO (Either (FileError ()) ())
writeValidatorRedeemer path file = do
    writeValidator (path ++ "/" ++ file ++ ".plutus") ValidatorRedeemer.codeValidator  

writeValidatorRedeemerHash :: String ->String -> IO ()
writeValidatorRedeemerHash path file = do
    writeJSON (path ++ "/" ++ file ++ ".hash") ValidatorRedeemer.hashValidator




writeMintingPolicy :: FilePath -> Plutus.MintingPolicy -> IO (Either (FileError ()) ())
writeMintingPolicy file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.getMintingPolicy


writeMintingPolicyFree :: String -> String -> IO (Either (FileError ()) ())
writeMintingPolicyFree path file  = do
    let   p    = MintPolicyFree.policy
    writeMintingPolicy (path ++ "/" ++ file ++ ".plutus") p 

writeMintingPolicyNFT :: String -> String ->  String ->  String -> IO (Either (FileError ()) ())
writeMintingPolicyNFT path file oref' tn' = do
    let oref = unsafeReadTxOutRef oref'
        tn   = fromString tn'
        p    = MintPolicyNFT.policy oref tn
    writeMintingPolicy (path ++ "/" ++ file ++ ".plutus") p 


writeMintingPolicyPlus :: String -> String -> String -> String -> String -> IO (Either (FileError ()) ())
writeMintingPolicyPlus path file oref' tn' amt' = do
    let oref = unsafeReadTxOutRef oref'
        tn   = fromString tn'
        amt  = read amt'
        p    = MintPolicyPlus.policy oref tn amt
    writeMintingPolicy (path ++ "/" ++ file ++ ".plutus") p 


writeMintingPolicySignedAddr ::  String -> String -> String -> IO (Either (FileError ()) ())
writeMintingPolicySignedAddr path file addr' = do
    let pkh = unsafePaymentPubKeyHash $ unsafeReadAddress addr'
        p    = MintPolicySigned.policy 112 pkh
    writeMintingPolicy (path ++ "/" ++ file ++ ".plutus") p 

writeMintingPolicySignedPkh ::  String -> String -> String -> IO (Either (FileError ()) ())
writeMintingPolicySignedPkh path file pkh' = do
    let pkh =  Ledger.PaymentPubKeyHash $ pkhFromStr pkh'
        p    = MintPolicySigned.policy 111 pkh
    writeMintingPolicy (path ++ "/" ++ file ++ ".plutus") p 
