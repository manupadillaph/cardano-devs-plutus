module Main where

import           Control.Exception (throwIO)
import           Data.List  
import           System.Environment  

import qualified Deploy

--Modulo: 

main :: IO ()
main = do

  
  -- putStrLn "Deploy Smart Contracts:"
  -- putStrLn "1: write datum"
  -- putStrLn "2: write redeemer"
  -- putStrLn "3: write ValidatorLocker plutus cbor"
  -- putStrLn "4: write ValidatorLocker hash"
  -- putStrLn "5: write ValidatorAlwaysTrue plutus cbor"
  -- putStrLn "6: write ValidatorAlwaysTrue hash"
  -- putStrLn "7: write ValidatorAlwaysFalseV1 plutus cbor"
  -- putStrLn "8: write ValidatorAlwaysFalseV1 hash"
  -- putStrLn "9: write ValidatorBeneficiary plutus cbor"
  -- putStrLn "10: write ValidatorBeneficiary hash"
  -- putStrLn "11: write ValidatorDeadline plutus cbor"
  -- putStrLn "12: write ValidatorDeadline hash"
  -- putStrLn "13: write ValidatorRedeemer plutus cbor"
  -- putStrLn "14: write ValidatorRedeemer hash"

  -- putStrLn "15: write Minting PolicyFree"
  -- putStrLn "16: write Minting PolicyNFT"
  -- putStrLn "17: write Minting PolicyTokens"
  -- putStrLn "18: write Minting PolicySigned"


  opcion <- getLine

  case read opcion of
    -- 1 -> do
    --   --putStrLn "Ingrese path:"
    --   path <- getLine
    --   --putStrLn "Ingrese nombre para el archivo Datum:"
    --   filename <- getLine
    --   --putStrLn "Ingrese creator:"
    --   creator <- getLine
    --   --putStrLn "Ingrese deadline:"
    --   deadline <- getLine
    --   --putStrLn "Ingrese name:"
    --   name <- getLine
    --   --putStrLn "Ingrese qty:"
    --   qty <- getLine
    --   Deploy.writeDatum path filename creator (read deadline) (read name) (read qty)
    --   putStrLn "Datum File Hecho en:"
    --   putStrLn filename
    -- 2 -> do
    --   --putStrLn "Ingrese path:"
    --   path <- getLine
    --   --putStrLn "Ingrese nombre para el archivo Redeemer:"
    --   filename <- getLine
    --   --putStrLn "Ingrese redeemer (1 o 2):"
    --   opcion <- getLine
    --   Deploy.writeRedeemer path filename (read opcion)
    --   putStrLn "Redeemer File Hecho en:"
    --   putStrLn filename  
    3 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorLockerV1 path file
      putStrLn "Script File Hecho"
    4 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorLockerV1Hash path file
      putStrLn "Hash File Hecho"
    5 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorAlwaysTrueV1 path file
      putStrLn "Script File Hecho"
    6 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorAlwaysTrueV1Hash path file
      putStrLn "Hash File Hecho"
    7 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorAlwaysFalseV1 path file
      putStrLn "Script File Hecho"
    8 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorAlwaysFalseV1Hash path file
      putStrLn "Hash File Hecho"
    9 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorBeneficiaryV1 path file
      putStrLn "Script File Hecho"
    10 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorBeneficiaryV1Hash path file
      putStrLn "Hash File Hecho"
    11 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorDeadlineV1 path file
      putStrLn "Script File Hecho"
    12 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorDeadlineV1Hash path file
      putStrLn "Hash File Hecho"
    13 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorRedeemerV1 path file
      putStrLn "Script File Hecho"
    14 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorRedeemerV1Hash path file
      putStrLn "Hash File Hecho"

    15 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      e <- Deploy.writeMintingPolicyFreeV1 path file
      case e of
        Left err -> throwIO $ userError $ show err
        Right () -> putStrLn "Minting Policy File Hecho"
    16 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      --putStrLn "Ingrese TxOutRef:"
      oref' <- getLine
      --putStrLn "Ingrese Token Name:"
      tn' <- getLine
      e <- Deploy.writeMintingPolicyNFTV1 path file oref' tn' 
      case e of
        Left err -> throwIO $ userError $ show err
        Right () -> putStrLn "Minting Policy File Hecho"
    17 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      --putStrLn "Ingrese TxOutRef:"
      oref' <- getLine
      --putStrLn "Ingrese Token Name:"
      tn' <- getLine
      --putStrLn "Ingrese Amount:"
      amt' <- getLine
      e <- Deploy.writeMintingPolicyTokensV1 path file  oref' tn' amt'
      case e of
        Left err -> throwIO $ userError $ show err
        Right () -> putStrLn "Minting Policy File Hecho"
    18 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      --putStrLn "Ingrese Wallet Addr para calcular PKH:"
      addr' <- getLine
      e <- Deploy.writeMintingPolicySignedV1Addr path file addr'
      case e of
        Left err -> throwIO $ userError $ show err
        Right () -> putStrLn "Minting Policy File Hecho"
    19 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      --putStrLn "Ingrese Wallet PKH:"
      pkh' <- getLine
      e <- Deploy.writeMintingPolicySignedV1Pkh path file pkh'
      case e of
        Left err -> throwIO $ userError $ show err
        Right () -> putStrLn "Minting Policy File Hecho"
      






