module Main where

import           Control.Exception (throwIO)
import           Data.List  
import           System.Environment  

import qualified TestWithPABSimulator

--Modulo: 

main :: IO ()
main = do

  putStrLn "PAB SIMULATOR:"

  putStrLn "125: test Validator Market NFT V1 with PAB Simulator"

  putStrLn "135: test Validator Stake Simple V1 with PAB Simulator"

  putStrLn "145: test Validator Stake Plus V1 with PAB Simulator"

  opcion <- getLine

  case read opcion of
   
    125 -> do
      TestWithPABSimulator.testValidatorMarketNFTV1WithPABSimulator 
    135 -> do
      TestWithPABSimulator.testValidatorStakeSimpleV1WithPABSimulator 
    145 -> do
      TestWithPABSimulator.testValidatorStakePlusV1WithPABSimulator 

      






