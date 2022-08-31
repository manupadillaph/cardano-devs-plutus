module Main where

import           Control.Exception (throwIO)
import           Data.List  
import           System.Environment  

import qualified TestWithTraceEmulator

--Modulo: 

main :: IO ()
main = do

  putStrLn "TRACE EMULATOR:"

  putStrLn "116: test Validator Locker V1 with Trace Emulator"
  putStrLn "126: test Validator AlwaysTrue V1 with Trace Emulator"
  putStrLn "136: test Validator AlwaysFalse V1 with Trace Emulator"
  putStrLn "146: test Validator Beneficiary V1 with Trace Emulator"
  putStrLn "156: test Validator Deadline V1 with Trace Emulator"
  putStrLn "166: test Validator Redeemer V1 with Trace Emulator"

  putStrLn "176: test Validator Market NFT V1 with Trace Emulator"

  putStrLn "186: test Validator Stake Simple V1 with Trace Emulator"
  putStrLn "196: test Validator Stake Plus V1 with Trace Emulator"
  
  putStrLn "206:  test Validator Minting Policy Free V1"
  putStrLn "216:  test Validator Minting Policy NFT V1"
  putStrLn "2161: test Validator Minting Policy NFT Signed V1"
  putStrLn "226:  test Validator Minting Policy Tokens V1"
  putStrLn "2261: test Validator Minting Policy Tokens Signed V1"
  putStrLn "236:  test Validator Minting Policy Signed V1"

  opcion <- getLine

  case read opcion of

    116 -> do
      TestWithTraceEmulator.testValidatorLockerV1WithTraceEmulator 
    126 -> do
      TestWithTraceEmulator.testValidatorAlwaysTrueV1WithTraceEmulator 
    136 -> do
      TestWithTraceEmulator.testValidatorAlwaysFalseV1WithTraceEmulator 
    146 -> do
      TestWithTraceEmulator.testValidatorBeneficiaryV1WithTraceEmulator 
    156 -> do
      TestWithTraceEmulator.testValidatorDeadlineV1WithTraceEmulator 
    166 -> do
      TestWithTraceEmulator.testValidatorRedeemerV1WithTraceEmulator 
    -- 176 -> do
    --   TestWithTraceEmulator.testValidatorMarketNFTV1WithTraceEmulator 
    -- 186 -> do
    --   TestWithTraceEmulator.testValidatorStakeSimpleV1WithTraceEmulator 
    -- 196 -> do
    --   TestWithTraceEmulator.testValidatorStakePlusV1WithTraceEmulator 

    206 -> do
      TestWithTraceEmulator.testMintingPolicyFreeV1WithTraceEmulator 
    216 -> do
      TestWithTraceEmulator.testMintingPolicyNFTV1WithTraceEmulator 
    2161 -> do
      TestWithTraceEmulator.testMintingPolicyNFTSignedV1WithTraceEmulator   
    226 -> do
      TestWithTraceEmulator.testMintingPolicyTokensV1WithTraceEmulator
    2261 -> do
      TestWithTraceEmulator.testMintingPolicyTokensSignedV1WithTraceEmulator
    236 -> do
      TestWithTraceEmulator.testMintingPolicySignedV1WithTraceEmulator  

 



