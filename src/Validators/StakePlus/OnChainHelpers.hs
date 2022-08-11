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

{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE AllowAmbiguousTypes              #-}
{-# LANGUAGE NumericUnderscores    #-}

--{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Validators.StakePlus.OnChainHelpers where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           Data.String  
import           GHC.Generics         (Generic)
--import           Ledger               hiding (singleton)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada
import           Playground.Contract  (IO, ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (unless)
import qualified Prelude              as HASKELL 
import           Schema               (ToSchema)
import     qualified      Data.OpenApi.Schema         (ToSchema)
import           Text.Printf          (printf)
import Data.Typeable

import          Plutus.Trace.Emulator  as Emulator
import          Wallet.Emulator.Wallet
import          Data.Default
import          Ledger.TimeSlot 

--Import Nuevos

import          Control.Lens

import PlutusTx.Builtins

import qualified Data.Map as Map
import Ledger 
import Ledger.Index
import qualified Plutus.Trace.Emulator  as Trace
import qualified Data.List

--Import Internos
import  Validators.StakePlus.Typos 
import  Validators.StakePlus.Helpers 
import  Validators.StakePlus.OnChainNFT     (mintingNFTPolicy)




--Checking Signatures

{-# INLINABLE signedByMaster #-}
signedByMaster :: Master -> TxInfo -> Bool
signedByMaster master info  = 
    txSignedBy info (unPaymentPubKeyHash master)

{-# INLINABLE signedByUser #-}
signedByUser :: User -> TxInfo -> Bool
signedByUser user info  = 
    txSignedBy info (unPaymentPubKeyHash user)

{-# INLINABLE signedByPoolParamMaster #-}
signedByPoolParamMaster :: PoolParams -> TxInfo -> Bool
signedByPoolParamMaster pParams info  = 
    PlutusTx.Prelude.any (txSignedBy info ) [unPaymentPubKeyHash $ master | master <- ppMasters pParams]


{-# INLINABLE signedByPoolStateMasterFunder #-}
signedByPoolStateMasterFunder :: PoolStateTypo -> TxInfo -> Bool
signedByPoolStateMasterFunder dPoolState info  = 
    PlutusTx.Prelude.any (txSignedBy info ) [unPaymentPubKeyHash $ mfMaster masterFunder | masterFunder <- psMasterFunders dPoolState]

-- {-# INLINABLE signedBySingleOwnOutputPoolStateMasterFunder #-}
-- signedBySingleOwnOutputPoolStateMasterFunder :: TxInfo -> ScriptContext-> Bool
-- signedBySingleOwnOutputPoolStateMasterFunder info ctx  = do
--     let 
--         (_, outputWithPoolState_Datum) = getSingleOwnOutputPoolStateDatum ctx

--     signedByPoolStateMasterFunder outputWithPoolState_Datum info


{-# INLINABLE signedByUserStateUser #-}
signedByUserStateUser :: UserStateTypo ->TxInfo -> Bool
signedByUserStateUser dUserState info = txSignedBy info $ unPaymentPubKeyHash $ usUser dUserState

-- {-# INLINABLE signedByDoubleOwnOutputsUserStateUser #-}
-- signedByDoubleOwnOutputsUserStateUser :: TxInfo -> ScriptContext-> Bool
-- signedByDoubleOwnOutputsUserStateUser info ctx  = do
--     let 
--         ((_, _),(_, outputUserState_Datum)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

--     signedByUserStateUser outputUserState_Datum info


--Checking Deadline

{-# INLINABLE deadlinePoolParamReached #-}
deadlinePoolParamReached :: PoolParams -> TxInfo ->Bool
deadlinePoolParamReached pParams info  = Ledger.contains (Ledger.from $ ppDeadline pParams) $ txInfoValidRange info

{-# INLINABLE deadlinePoolParamNotReached #-}
deadlinePoolParamNotReached :: PoolParams -> TxInfo ->Bool
deadlinePoolParamNotReached  pParams info = not (deadlinePoolParamReached  pParams info)

{-# INLINABLE deadlineUserStateReached #-}
deadlineUserStateReached :: UserStateTypo -> TxInfo -> Bool
deadlineUserStateReached dUserState info = Ledger.contains (Ledger.from $ usDeadline dUserState) $ txInfoValidRange info

{-# INLINABLE deadlineUserStateNotReached #-}
deadlineUserStateNotReached :: UserStateTypo -> TxInfo -> Bool
deadlineUserStateNotReached dUserState info = not (deadlineUserStateReached dUserState info )


--Checking NFT

{- | Check if txoutref is pressent at the inputs to be consumed. -}
{-# INLINABLE hasInputUTxO #-}
hasInputUTxO :: TxInfo -> TxOutRef -> Bool
hasInputUTxO info txOutRef = any (\i -> txInInfoOutRef i == txOutRef) $ txInfoInputs info

{- | Check if NFT was produced by specific policy. -}
{-# INLINABLE correctMintigPolicyUsedForNFT #-}
correctMintigPolicyUsedForNFT :: PoolParams-> NFT   -> Bool
correctMintigPolicyUsedForNFT  pParams redeemerNFT   = do
    let 
        currencySymbolNFT = fst (unAssetClass redeemerNFT )
        currencySymbolMintingPolicy = ppCurSymbolForMintingNFTPolicy pParams

    currencySymbolNFT == currencySymbolMintingPolicy  

{- | Check if the minting quantity is equal to 1. -}
{-# INLINABLE isMintedNFTValid #-}
isMintedNFTValid :: TxInfo -> NFT  -> Bool
isMintedNFTValid info redeemerNFT  = do
    let 
        mintedValue = txInfoMint info
        mintedValueOf = assetClassValueOf mintedValue redeemerNFT
    mintedValueOf == 1

{- | Check if NFT is the same that the PoolNFT. -}
{-# INLINABLE isPoolNFTParam #-}
isPoolNFTParam ::  PoolParams -> PoolNFT ->  Bool
isPoolNFTParam  pParams redeemerPoolNFT   = do
    let 
        poolNFT  = ppPoolNFT pParams

    redeemerPoolNFT == poolNFT

{- | Check if the redeemerPoolNFT is the same in all the inputs PoolState Datums. -}
{-# INLINABLE isPoolNFTDatum #-}
isPoolNFTDatum ::  PoolNFT -> ScriptContext ->  Bool
isPoolNFTDatum   poolNFT ctx   = do
    let 
        inputsWithPoolState = getInputsWithPoolState ctx
        inputsPoolState_Datums = getInput_Datum <$> inputsWithPoolState

    PlutusTx.Prelude.all (poolNFT==) [psPoolNFT inputPoolState_Datum |  inputPoolState_Datum <- inputsPoolState_Datums]

{- | Check if the redeemerUserNFT is the same in all the inputs UserState Datums. -}
{-# INLINABLE isUserNFTDatum #-}
isUserNFTDatum ::  UserNFT -> ScriptContext ->  Bool
isUserNFTDatum   userNFT ctx   = do
    let 
        inputsWithUserState = getInputsWithUserState ctx
        inputsWithUserState_Datums = getInput_Datum <$> inputsWithUserState

    PlutusTx.Prelude.all (userNFT==)  [usUserNFT inputWithUserState_Datum |  inputWithUserState_Datum <- inputsWithUserState_Datums]


{- | Check if User NFT is in any output going to the user wallet. -}
{-# INLINABLE isUserNFTInUserOutputValue #-}
isUserNFTInUserOutputValue :: UserNFT ->  User -> TxInfo -> Bool
isUserNFTInUserOutputValue  redeemerUserNFT user info  = do
    let 
        outPutValue = valuePaidTo info (unPaymentPubKeyHash user )
        outPutNFTQuantity = assetClassValueOf outPutValue redeemerUserNFT

    outPutNFTQuantity == 1 

isPoolNFTInSomeOutputWithPoolState :: [(TxOut,PoolStateTypo)] -> PoolNFT ->  ScriptContext -> Bool
isPoolNFTInSomeOutputWithPoolState outputsWithPoolState redeemerPoolNFT ctx  = do
    let 
        outputsWithPoolState_TxOut = fst <$> outputsWithPoolState

        outputsWithPoolState_Values = txOutValue <$> outputsWithPoolState_TxOut

        outPutNFTQuantities = [assetClassValueOf  outPutValue redeemerPoolNFT | outPutValue <- outputsWithPoolState_Values ]

    1 `elem` outPutNFTQuantities --HASKELL.any (==1) outPutNFTQuantities

{-  Check if this the invest created at date is correct.  -}
{-# INLINABLE correctInvestCreatedAt #-}
correctInvestCreatedAt :: POSIXTime -> TxInfo -> Bool
correctInvestCreatedAt ruiCreatedAt info = do
    -- XXX: +1 because timeRange lower closure may be False
   (ruiCreatedAt + 1) `Ledger.member` txInfoValidRange info

{-  Check if this the invest deadline date is correct. 
    Need to be after createdAt
    Before deadline of the Pool
-}
{-# INLINABLE correctInvestDeadlineAt #-}
correctInvestDeadlineAt :: PoolParams ->  POSIXTime -> POSIXTime -> TxInfo -> Bool
correctInvestDeadlineAt pParams ruiCreatedAt ruiDeadline info = do
    -- XXX: +1 because timeRange lower closure may be False
   (ruiDeadline + 1) `Ledger.member` Ledger.interval ruiCreatedAt (ppDeadline pParams)



{- Check if this the claim date at is correct. -}
{-# INLINABLE correctClaimDateAt #-}
correctClaimDateAt :: POSIXTime -> TxInfo -> Bool
correctClaimDateAt rugrClaimAt info = do
    -- XXX: +1 because timeRange lower closure may be False
   (rugrClaimAt + 1) `Ledger.member` txInfoValidRange info


{- Check that the tx range interval of validity. Can't be infinitum and need to be lees than ppValidTimeRange Pool Param . . -}
{-# INLINABLE isValidRange #-}
isValidRange :: PoolParams -> TxInfo -> Bool
isValidRange pParams info = checkIntervalSize (txInfoValidRange info) $ ppValidTimeRange pParams


{- Check if this the claim is more than the minumun claim value. -}
{-# INLINABLE isMoreThanMinimunClaim #-}
isMoreThanMinimunClaim :: PoolParams -> Proffit -> Bool
isMoreThanMinimunClaim pParams rugrClaim = do
    rugrClaim >= ppMinimunClaim pParams 


--Getting Inputs and Output with PoolState or UserState Datums

{- | Get the Outputs going again to the script address. -}
{-# INLINABLE getOnwOutputs #-}
getOnwOutputs :: ScriptContext -> [TxOut]
getOnwOutputs = getContinuingOutputs

{- | Gets the datum attached to a utxo. -}
{-# INLINABLE getDatumFromTxOut #-}
getDatumFromTxOut :: PlutusTx.FromData ValidatorDatum => TxOut -> ScriptContext -> Maybe ValidatorDatum
getDatumFromTxOut o ctx = txOutDatum o >>= (`findDatum` scriptContextTxInfo ctx)
                   >>= PlutusTx.fromBuiltinData . getDatum :: Maybe ValidatorDatum


{-# INLINABLE getInput_TxOutRef #-}
getInput_TxOutRef :: (TxOutRef,TxOut,a) -> TxOutRef
getInput_TxOutRef (x,_,_) = x

{-# INLINABLE getInput_TxOut #-}
getInput_TxOut :: (TxOutRef,TxOut,a) -> TxOut
getInput_TxOut (_,x,_) = x

{-# INLINABLE getInput_Datum #-}
getInput_Datum :: (TxOutRef,TxOut, a ) -> a 
getInput_Datum (_,_,x) = x

{-# INLINABLE removeInputByTxOutRef #-}
removeInputByTxOutRef  ::  TxOutRef -> [(TxOutRef, TxOut, PoolStateTypo)] -> [(TxOutRef, TxOut, PoolStateTypo)]
removeInputByTxOutRef _ []                 = []
removeInputByTxOutRef x (y:ys) 
            | x == (getInput_TxOutRef y)      = removeInputByTxOutRef x ys
            | otherwise                     =  y : removeInputByTxOutRef x ys


{- | Gets the input PoolState Datums of the tx ScriptContext. -}
{-# INLINABLE getInputsWithPoolState #-}
getInputsWithPoolState :: ScriptContext -> [(TxOutRef,TxOut,PoolStateTypo)]
getInputsWithPoolState ctx = do
    let
        txInInfoResolveds = [  ( txInInfoOutRef txInfoInput,  txInInfoResolved  txInfoInput) | txInfoInput <- txInfoInputs (scriptContextTxInfo ctx)]

        txOutReftxOutsAndDatums = [  (txInInfoOutRef, txInInfoResolved, getDatumFromTxOut txInInfoResolved ctx) | (txInInfoOutRef, txInInfoResolved) <- txInInfoResolveds]

        txOutReftxOutsAndJustDatums = [  (txoutref, txtout, dat) | (txoutref, txtout, dat) <- txOutReftxOutsAndDatums, isJust dat ]

        inputsWithPoolState =  [  (txoutref, txtout, fromJust $ getPoolStateFromMaybeDatum dat) | (txoutref,txtout, dat) <- txOutReftxOutsAndJustDatums,  datumIsPoolState dat ] 

    inputsWithPoolState

{- | Gets the input PoolState Datum with the Pool NFT. -}
{-# INLINABLE getInputWithPoolStateAndPoolNFT #-}
getInputWithPoolStateAndPoolNFT ::   [(TxOutRef,TxOut,PoolStateTypo)] -> PoolNFT -> Maybe (TxOutRef,TxOut,PoolStateTypo)
getInputWithPoolStateAndPoolNFT  inputsWithPoolState redeemerPoolNFT = do
    let 
        inputsWithPoolState_TxOut = getInput_TxOut <$> inputsWithPoolState

        inputsWithPoolState_Values = txOutValue <$> inputsWithPoolState_TxOut

        inputsWithPoolStateAndPoolNFT = [(txOutRef,txOut,poolStateTypo) | (txOutRef,txOut,poolStateTypo) <- inputsWithPoolState , assetClassValueOf (txOutValue txOut) redeemerPoolNFT > 0]

    case inputsWithPoolStateAndPoolNFT of
        [inputWithPoolStateAndPoolNFT] -> Just inputWithPoolStateAndPoolNFT
        _ -> Nothing

{- | Gets the output PoolState Datum with the Pool NFT. -}
{-# INLINABLE getOutputWithPoolStateAndPoolNFT #-}
getOutputWithPoolStateAndPoolNFT ::   [(TxOut,PoolStateTypo)] -> PoolNFT -> Maybe (TxOut,PoolStateTypo)
getOutputWithPoolStateAndPoolNFT  outputsWithPoolState redeemerPoolNFT = do
    let 
        ouputsWithPoolState_TxOut = fst <$> outputsWithPoolState

        outputsWithPoolState_Values = txOutValue <$> ouputsWithPoolState_TxOut

        outputsWithPoolStateAndPoolNFT = [(txOut,poolStateTypo) | (txOut,poolStateTypo) <- outputsWithPoolState , assetClassValueOf (txOutValue txOut) redeemerPoolNFT > 0]

    case outputsWithPoolStateAndPoolNFT of
        [outputWithPoolStateAndPoolNFT] -> Just outputWithPoolStateAndPoolNFT
        _ -> Nothing


{- | Gets the input UserState Datums of the tx ScriptContext. -}
{-# INLINABLE getInputsWithUserState #-}
getInputsWithUserState :: ScriptContext -> [(TxOutRef, TxOut,UserStateTypo)]
getInputsWithUserState ctx = do
    let
        txInInfoResolveds = [ ( txInInfoOutRef txInfoInput,  txInInfoResolved  txInfoInput)  | txInfoInput <- txInfoInputs (scriptContextTxInfo ctx)]

        txOutReftxOutsAndDatums = [  (txInInfoOutRef, txInInfoResolved, getDatumFromTxOut txInInfoResolved ctx) |  (txInInfoOutRef, txInInfoResolved) <- txInInfoResolveds]

        txOutReftxOutsAndJustDatums = [  (txoutref, txtout, dat) | (txoutref, txtout, dat) <- txOutReftxOutsAndDatums, isJust dat ]

        inputsWithUserState =  [  (txoutref, txtout, fromJust $ getUserStateFromMaybeDatum dat) | (txoutref, txtout, dat) <- txOutReftxOutsAndJustDatums,  datumIsUserState dat ] 

    inputsWithUserState

{- | Gets all the outputs with PoolState Datums. -}
{-# INLINABLE getOutputsWithPoolState #-}
getOutputsWithPoolState :: ScriptContext -> [(TxOut,PoolStateTypo)]
getOutputsWithPoolState ctx = 
    case getOnwOutputs ctx of
        []   -> traceError "Expected at least one output."
        txOuts -> do
            let 
                txOutsAndDatums = [  (txOut, getDatumFromTxOut txOut ctx) | txOut <- txOuts ]
                txOutsAndJustDatums = [  (txtout, dat) | (txtout, dat) <- txOutsAndDatums, isJust dat ]
                outputsWithPoolState =  [  (txtout, fromJust $ getPoolStateFromMaybeDatum dat) | (txtout, dat) <- txOutsAndJustDatums,  datumIsPoolState dat ] 
            
            outputsWithPoolState

{- | Gets all the outputs with UserState Datums. -}
{-# INLINABLE getOutputsWithUserState #-}
getOutputsWithUserState :: ScriptContext -> [(TxOut,UserStateTypo)]
getOutputsWithUserState ctx = 
    case getOnwOutputs ctx of
        []   -> traceError "Expected at least one output."
        txOuts -> do
            let 
                txOutsAndDatums = [  (txOut, getDatumFromTxOut txOut ctx) | txOut <- txOuts ]
                txOutsAndJustDatums = [  (txtout, dat) | (txtout, dat) <- txOutsAndDatums, isJust dat ]
                txOutsAndUserState =  [  (txtout, fromJust $ getUserStateFromMaybeDatum dat) | (txtout, dat) <- txOutsAndJustDatums,  datumIsUserState dat ] 
            
            txOutsAndUserState
