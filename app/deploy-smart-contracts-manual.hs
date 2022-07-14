module Main where

import System.Environment  
import Data.List  
import  Prelude              as P

import Control.Exception    (throwIO)

import Deploy


main :: IO ()
main = do
  -- putStrLn "Hello, Haskell!"
  -- MyLib.someFunc

  -- args <- getArgs                  -- IO [String]
  -- progName <- getProgName          -- IO String

  -- putStrLn "The arguments are:"  
  -- mapM putStrLn args  
  -- putStrLn "The program name is:"  
  -- putStrLn progName


  putStrLn "Deploy Smart Contracts:"

  putStrLn "1: write datum"
  putStrLn "2: write redeemer"

  putStrLn "3: write Validator Locker plutus cbor"
  putStrLn "4: write Validator Locker hash"
  putStrLn "5: write Validator AlwaysTrue plutus cbor"
  putStrLn "6: write Validator AlwaysTrue hash"
  putStrLn "7: write Validator AlwaysFalse plutus cbor"
  putStrLn "8: write Validator AlwaysFalse hash"
  putStrLn "9: write Validator Beneficiary plutus cbor"
  putStrLn "10: write Validator Beneficiary hash"
  putStrLn "11: write Validator Deadline plutus cbor"
  putStrLn "12: write Validator Deadline hash"
  putStrLn "13: write Validator Redeemer plutus cbor"
  putStrLn "14: write Validator Redeemer hash"

  putStrLn "15: write Minting PolicyFree"
  putStrLn "16: write Minting PolicyNFT"
  putStrLn "17: write Minting PolicyPlus"
  putStrLn "18: write Minting PolicySigned"

  opcion <- getLine

  case read opcion of
    1 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre para el archivo Datum:"
      filename <- getLine
      putStrLn "Ingrese creator:"
      creator <- getLine
      putStrLn "Ingrese deadline:"
      deadline <- getLine
      putStrLn "Ingrese name:"
      name <- getLine
      putStrLn "Ingrese qty:"
      qty <- getLine
      Deploy.writeDatum path filename creator (read deadline) (read name) (read qty)
      putStrLn "Datum File Hecho en:"
      putStrLn filename
    2 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre para el archivo Redeemer:"
      filename <- getLine
      putStrLn "Ingrese redeemer (1 o 2):"
      opcion <- getLine
      Deploy.writeRedeemer path filename (read opcion)
      putStrLn "Redeemer File Hecho en:"
      putStrLn filename  
    3 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorLocker path file
      putStrLn "Script File Hecho"
    4 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorLockerHash path file
      putStrLn "Hash File Hecho"
    5 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorAlwaysTrue path file
      putStrLn "Script File Hecho"
    6 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorAlwaysTrueHash path file
      putStrLn "Hash File Hecho"
    7 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorAlwaysFalse path file
      putStrLn "Script File Hecho"
    8 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorAlwaysFalseHash path file
      putStrLn "Hash File Hecho"
    9 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorBeneficiary path file
      putStrLn "Script File Hecho"
    10 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorBeneficiaryHash path file
      putStrLn "Hash File Hecho"
    11 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorDeadline path file
      putStrLn "Script File Hecho"
    12 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorDeadlineHash path file
      putStrLn "Hash File Hecho"
    13 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorRedeemer path file
      putStrLn "Script File Hecho"
    14 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorRedeemerHash path file
      putStrLn "Hash File Hecho"


    15 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre del script:"
      file <- getLine
      e <- Deploy.writeMintingPolicyFree path file
      case e of
        Left err -> throwIO $ userError $ show err
        Right () -> putStrLn "Minting Policy File Hecho"
    16 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre del script:"
      file <- getLine
      putStrLn "Ingrese TxOutRef:"
      oref' <- getLine
      putStrLn "Ingrese Token Name:"
      tn' <- getLine
      e <- Deploy.writeMintingPolicyNFT path file oref' tn' 
      case e of
        Left err -> throwIO $ userError $ show err
        Right () -> putStrLn "Minting Policy File Hecho"
    17 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre del script:"
      file <- getLine
      putStrLn "Ingrese TxOutRef:"
      oref' <- getLine
      putStrLn "Ingrese Token Name:"
      tn' <- getLine
      putStrLn "Ingrese Amount:"
      amt' <- getLine
      e <- Deploy.writeMintingPolicyPlus path file  oref' tn' amt'
      case e of
        Left err -> throwIO $ userError $ show err
        Right () -> putStrLn "Minting Policy File Hecho"
    18 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre del script:"
      file <- getLine
      putStrLn "Ingrese Wallet Addr para calcular PKH:"
      addr' <- getLine
      e <- Deploy.writeMintingPolicySignedAddr path file addr'
      case e of
        Left err -> throwIO $ userError $ show err
        Right () -> putStrLn "Minting Policy File Hecho"


    19 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre del script:"
      file <- getLine
      putStrLn "Ingrese Wallet PKH:"
      pkh' <- getLine
      e <- Deploy.writeMintingPolicySignedPkh path file pkh'
      case e of
        Left err -> throwIO $ userError $ show err
        Right () -> putStrLn "Minting Policy File Hecho"



