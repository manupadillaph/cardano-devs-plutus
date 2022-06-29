{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Deploy
    ( writeUnit
    , writeRedeemer
    , writeDatum
    , writeValidatorHash
    , writeValidatorAddress
    , writeValidatorLocker
    ) where

import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode,ToJSON, FromJSON)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           PlutusTx              (Data (..))
import qualified PlutusTx

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

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

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

pkhFromStr :: String -> Either Text PubKeyHash   
pkhFromStr s =         
    case fromHex (fromString s) of 
        Right (LedgerBytes bytes) -> Right $ PubKeyHash bytes 
        Left msg -> Left $ pack ("Could not convert from hex to bytes: " <> msg)

writeDatum:: String -> String -> String -> Integer -> Integer -> Integer-> IO ()
writeDatum path file creator deadline name qty = do
    let 
        eiPkh = pkhFromStr creator
    
    case eiPkh of 
        Left msg  -> 
            writeJSON (path ++ "/error.json") ()
        Right pkh -> do
            let 
                datum = ValidatorLocker.ValidatorDatum {
                    ValidatorLocker.dData = ValidatorLocker.ValidatorData{
                            ValidatorLocker.aCreator =  Ledger.PaymentPubKeyHash pkh
                            , ValidatorLocker.aDeadline    = fromInteger  deadline
                            , ValidatorLocker.aName = name
                            , ValidatorLocker.aAdaQty  = qty 
                        }
                    }
            writeJSON (path ++ "/" ++ file ++ ".json") datum

writeValidatorLocker :: String -> String -> IO (Either (FileError ()) ())
writeValidatorLocker path file = do
    writeValidator (path ++ "/" ++ file ++ ".plutus") ValidatorLocker.codeValidator  

writeValidatorHash :: String ->String -> IO ()
writeValidatorHash path file = do
    writeJSON (path ++ "/" ++ file ++ ".hash") ValidatorLocker.hashValidator

writeValidatorAddress :: String -> String -> IO ()
writeValidatorAddress path file = do
    writeJSON  (path ++ "/" ++ file ++ ".addr") ValidatorLocker.addressValidator
