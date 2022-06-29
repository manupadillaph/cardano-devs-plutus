module Main where

import System.Environment  
import Data.List  
import  Prelude              as P



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


  putStrLn "Deploy Plazo Fijo:"
  putStrLn "1: write script plutus cbor"
  putStrLn "2: write script hash"
  putStrLn "3: write script address"
  putStrLn "4: write datum"
  putStrLn "5: write redeemer"

  putStrLn "6: abrir plazo fijo"

  opcion <- getLine

  case read opcion of
    1 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorLocker path file
      putStrLn "Script File Hecho"
    2 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorHash path file
      putStrLn "Hash File Hecho"
    3 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre del script:"
      file <- getLine
      Deploy.writeValidatorAddress path file
      putStrLn "Address File Hecho"
    4 -> do
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
    5 -> do
      putStrLn "Ingrese path:"
      path <- getLine
      putStrLn "Ingrese nombre para el archivo Redeemer:"
      filename <- getLine
      putStrLn "Ingrese redeemer (1 o 2):"
      opcion <- getLine
      Deploy.writeRedeemer path filename (read opcion)
      putStrLn "Redeemer File Hecho en:"
      putStrLn filename
    6 -> do
      putStrLn "Ingrese billetera:"
      wallet <- getLine
      
      putStrLn "Plazo Fijo Creado"





