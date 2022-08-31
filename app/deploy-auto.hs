module Main where

import           Control.Exception (throwIO)
import           Data.List  
import           System.Environment  

import qualified Deploy

--Modulo: 

main :: IO ()
main = do

  
  -- putStrLn "Deploy Smart Contracts:"

  -- putStrLn "1: write Validator LockerV1 datum"
  -- putStrLn "2: write Validator LockerV1 redeemer"
  -- putStrLn "3: write Validator LockerV1 plutus cbor"
  -- putStrLn "4: write Validator LockerV1 hash"

  -- putStrLn "5: write Validator AlwaysTrueV1 plutus cbor"
  -- putStrLn "6: write Validator AlwaysTrueV1 hash"
  -- putStrLn "7: write Validator AlwaysFalseV1 plutus cbor"
  -- putStrLn "8: write Validator AlwaysFalseV1 hash"
  -- putStrLn "9: write Validator BeneficiaryV1 plutus cbor"
  -- putStrLn "10: write Validator BeneficiaryV1 hash"
  -- putStrLn "11: write Validator DeadlineV1 plutus cbor"
  -- putStrLn "12: write Validator DeadlineV1 hash"
  -- putStrLn "13: write Validator RedeemerV1 plutus cbor"
  -- putStrLn "14: write Validator RedeemerV1 hash"

  -- putStrLn "121: write Validator Market NFT V1 datum"
  -- putStrLn "122: write Validator Market NFT V1 redeemer"
  -- putStrLn "123: write Validator Market NFT V1 cbor"
  -- putStrLn "124: write Validator Market NFT V1 hash"

  -- putStrLn "131: write Validator Stake Simple V1 datum"
  -- putStrLn "132: write Validator Stake Simple V1 redeemer"
  -- putStrLn "133: write Validator Stake Simple V1 cbor"
  -- putStrLn "134: write Validator Stake Simple V1 hash"

  -- putStrLn "141: write Validator Stake Plus V1 datum"
  -- putStrLn "142: write Validator Stake Plus V1 redeemer"
  -- putStrLn "143: write Validator Stake Plus V1 cbor"
  -- putStrLn "144: write Validator Stake Plus V1 hash"

  -- putStrLn "15:  write Minting Policy Free V1"
  -- putStrLn "16:  write Minting Policy NFT V1"
  -- putStrLn "161: write Minting Policy NFT Signed V1"
  
  -- putStrLn "17:  write Minting Policy Tokens V1"
  -- putStrLn "171: write Minting Policy Tokens Signed V1"

  -- putStrLn "18:  write Minting Policy Signed V1"

  opcion <- getLine

  case read opcion of
    1 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre para el archivo Datum:"
      filename <- getLine
      --putStrLn "Ingrese creator:"
      creator <- getLine
      --putStrLn "Ingrese deadline:"
      deadline <- getLine
      --putStrLn "Ingrese name:"
      name <- getLine
      --putStrLn "Ingrese qty:"
      qty <- getLine
      Deploy.writeValidatorLockerV1Datum path filename creator (read deadline) (read name) (read qty)
      putStrLn "Datum File Hecho en:"
      putStrLn filename
    2 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre para el archivo Redeemer:"
      filename <- getLine
      --putStrLn "Ingrese redeemer (1 o 2):"
      opcion <- getLine
      Deploy.writeValidatorLockerV1Redeemer path filename (read opcion)
      putStrLn "Redeemer File Hecho en:"
      putStrLn filename  
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

    121 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre para el archivo Datum:"
      filename <- getLine
      --putStrLn "Ingrese creator:"
      creator <- getLine
      --putStrLn "Ingrese deadline:"
      deadline <- getLine
      --putStrLn "Ingrese name:"
      name <- getLine
      --putStrLn "Ingrese qty:"
      qty <- getLine
      Deploy.writeValidatorMarketNFTV1Datum path filename creator (read deadline) (read name) (read qty)
      putStrLn "Datum File Hecho en:"
      putStrLn filename
    122 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre para el archivo Redeemer:"
      filename <- getLine
      --putStrLn "Ingrese redeemer (1 o 2):"
      opcion <- getLine
      Deploy.writeValidatorMarketNFTV1Redeemer path filename (read opcion)
      putStrLn "Redeemer File Hecho en:"
      putStrLn filename  
    123 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorMarketNFTV1 path file
      putStrLn "Script File Hecho"
    124 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorMarketNFTV1Hash path file
      putStrLn "Hash File Hecho"


    131 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre para el archivo Datum:"
      filename <- getLine
      --putStrLn "Ingrese creator:"
      creator <- getLine
      --putStrLn "Ingrese deadline:"
      deadline <- getLine
      --putStrLn "Ingrese name:"
      name <- getLine
      --putStrLn "Ingrese qty:"
      qty <- getLine
      Deploy.writeValidatorStakeSimpleV1Datum path filename creator (read deadline) (read name) (read qty)
      putStrLn "Datum File Hecho en:"
      putStrLn filename
    132 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre para el archivo Redeemer:"
      filename <- getLine
      --putStrLn "Ingrese redeemer (1 o 2):"
      opcion <- getLine
      Deploy.writeValidatorStakeSimpleV1Redeemer path filename (read opcion)
      putStrLn "Redeemer File Hecho en:"
      putStrLn filename  
    133 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorStakeSimpleV1 path file
      putStrLn "Script File Hecho"
    134 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorStakeSimpleV1Hash path file
      putStrLn "Hash File Hecho"


    141 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre para el archivo Datum:"
      filename <- getLine
      --putStrLn "Ingrese creator:"
      creator <- getLine
      --putStrLn "Ingrese deadline:"
      deadline <- getLine
      --putStrLn "Ingrese name:"
      name <- getLine
      --putStrLn "Ingrese qty:"
      qty <- getLine
      Deploy.writeValidatorStakePlusV1Datum path filename creator (read deadline) (read name) (read qty)
      putStrLn "Datum File Hecho en:"
      putStrLn filename
    142 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre para el archivo Redeemer:"
      filename <- getLine
      --putStrLn "Ingrese redeemer (1 o 2):"
      opcion <- getLine
      Deploy.writeValidatorStakePlusV1Redeemer path filename (read opcion)
      putStrLn "Redeemer File Hecho en:"
      putStrLn filename  
    143 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorStakePlusV1 path file
      putStrLn "Script File Hecho"
    144 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorStakePlusV1Hash path file
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

    161 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      --putStrLn "Ingrese Wallet Addr para calcular PKH:"
      addr' <- getLine
      --putStrLn "Ingrese TxOutRef:"
      oref' <- getLine
      --putStrLn "Ingrese Token Name:"
      tn' <- getLine
      e <- Deploy.writeMintingPolicyNFTSignedV1 path file addr' oref' tn' 
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

    171 -> do
      --putStrLn "Ingrese path:"
      path <- getLine
      --putStrLn "Ingrese nombre del script:"
      file <- getLine
      -- putStrLn "Ingrese Wallet Addr para calcular PKH:"
      addr' <- getLine
      --putStrLn "Ingrese TxOutRef:"
      oref' <- getLine
      --putStrLn "Ingrese Token Name:"
      tn' <- getLine
      --putStrLn "Ingrese Amount:"
      amt' <- getLine
      e <- Deploy.writeMintingPolicyTokensSignedV1 path file addr' oref' tn' amt'
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
      






