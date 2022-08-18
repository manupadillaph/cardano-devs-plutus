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

--{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Validators.StakePlusV1.OnChainHelpers where

--Import Externos

import qualified Ledger                              (unPaymentPubKeyHash, valuePaidTo) --PaymentPubKeyHash, getCardanoTxId, pubKeyHashAddress, 
import qualified Plutus.V1.Ledger.Api                as LedgerApiV1
import qualified Plutus.V1.Ledger.Interval           as LedgerIntervalV1 (contains, interval, from, member) --, 
import qualified Plutus.V1.Ledger.Value              as LedgerValueV1
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Plutus.V1.Ledger.Contexts           as LedgerContextsV1 (ScriptContext, TxInfo, scriptContextTxInfo, txSignedBy, getContinuingOutputs, findDatum)
import qualified Plutus.V1.Ledger.Scripts            as LedgerScriptsV1
import qualified Plutus.V1.Ledger.Tx                 as LedgerTxV1 (txOutDatum)

--Import Internos

import qualified Validators.StakePlusV1.Helpers      as Helpers
import qualified Validators.StakePlusV1.Typos        as T

-- Modulo:

--Checking Signatures

{-# INLINABLE signedByMaster #-}
signedByMaster :: T.Master -> LedgerContextsV1.TxInfo -> Bool
signedByMaster master info  = 
    LedgerContextsV1.txSignedBy info (Ledger.unPaymentPubKeyHash master)

{-# INLINABLE signedByUser #-}
signedByUser :: T.User -> LedgerContextsV1.TxInfo -> Bool
signedByUser user info  = 
    LedgerContextsV1.txSignedBy info (Ledger.unPaymentPubKeyHash user)

{-# INLINABLE signedByPoolParamMaster #-}
signedByPoolParamMaster :: T.PoolParams -> LedgerContextsV1.TxInfo -> Bool
signedByPoolParamMaster pParams info  = 
    any (LedgerContextsV1.txSignedBy info ) [Ledger.unPaymentPubKeyHash $ master | master <- T.ppMasters pParams]


{-# INLINABLE signedByPoolStateMasterFunder #-}
signedByPoolStateMasterFunder :: T.PoolStateTypo -> LedgerContextsV1.TxInfo -> Bool
signedByPoolStateMasterFunder dPoolState info  = 
    any (LedgerContextsV1.txSignedBy info ) [Ledger.unPaymentPubKeyHash $ T.mfMaster masterFunder | masterFunder <- T.psMasterFunders dPoolState]

-- {-# INLINABLE signedBySingleOwnOutputPoolStateMasterFunder #-}
-- signedBySingleOwnOutputPoolStateMasterFunder :: LedgerContextsV1.TxInfo -> LedgerContextsV1.ScriptContext-> Bool
-- signedBySingleOwnOutputPoolStateMasterFunder info ctx  = do
--     let 
--         (_, outputWithPoolState_Datum) = getSingleOwnOutputPoolStateDatum ctx

--     signedByPoolStateMasterFunder outputWithPoolState_Datum info


{-# INLINABLE signedByUserStateUser #-}
signedByUserStateUser :: T.UserStateTypo ->LedgerContextsV1.TxInfo -> Bool
signedByUserStateUser dUserState info = LedgerContextsV1.txSignedBy info $ Ledger.unPaymentPubKeyHash $ T.usUser dUserState

-- {-# INLINABLE signedByDoubleOwnOutputsUserStateUser #-}
-- signedByDoubleOwnOutputsUserStateUser :: LedgerContextsV1.TxInfo -> LedgerContextsV1.ScriptContext-> Bool
-- signedByDoubleOwnOutputsUserStateUser info ctx  = do
--     let 
--         ((_, _),(_, outputUserState_Datum)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

--     signedByUserStateUser outputUserState_Datum info


--Checking Deadline

{-# INLINABLE deadlinePoolParamReached #-}
deadlinePoolParamReached :: T.PoolParams -> LedgerContextsV1.TxInfo ->Bool
deadlinePoolParamReached pParams info  = LedgerIntervalV1.contains (LedgerIntervalV1.from $ T.ppDeadline pParams) $ LedgerApiV1.txInfoValidRange info

{-# INLINABLE deadlinePoolParamNotReached #-}
deadlinePoolParamNotReached :: T.PoolParams -> LedgerContextsV1.TxInfo ->Bool
deadlinePoolParamNotReached  pParams info = not (deadlinePoolParamReached  pParams info)

{-# INLINABLE deadlineUserStateReached #-}
deadlineUserStateReached :: T.UserStateTypo -> LedgerContextsV1.TxInfo -> Bool
deadlineUserStateReached dUserState info = LedgerIntervalV1.contains (LedgerIntervalV1.from $ T.usDeadline dUserState) $ LedgerApiV1.txInfoValidRange info

{-# INLINABLE deadlineUserStateNotReached #-}
deadlineUserStateNotReached :: T.UserStateTypo -> LedgerContextsV1.TxInfo -> Bool
deadlineUserStateNotReached dUserState info = not (deadlineUserStateReached dUserState info )


--Checking NFT

{- | Check if txoutref is pressent at the inputs to be consumed. -}
{-# INLINABLE hasInputUTxO #-}
hasInputUTxO :: LedgerContextsV1.TxInfo -> LedgerApiV1.TxOutRef -> Bool
hasInputUTxO info txOutRef = any (\i -> LedgerApiV1.txInInfoOutRef i == txOutRef) $ LedgerApiV1.txInfoInputs info

{- | Check if NFT was produced by specific policy. -}
{-# INLINABLE correctMintigPolicyUsedForNFT #-}
correctMintigPolicyUsedForNFT :: T.PoolParams -> T.NFT -> Bool
correctMintigPolicyUsedForNFT  pParams redeemerNFT   = do
    let 
        currencySymbolNFT = fst (LedgerValueV1.unAssetClass redeemerNFT )
        currencySymbolMintingPolicy = T.ppCurSymbolForMintingNFTPolicy pParams

    currencySymbolNFT == currencySymbolMintingPolicy  

{- | Check if the minting quantity is equal to 1. -}
{-# INLINABLE isMintedNFTValid #-}
isMintedNFTValid :: LedgerContextsV1.TxInfo -> T.NFT -> Bool
isMintedNFTValid info redeemerNFT  = do
    let 
        mintedValue = LedgerApiV1.txInfoMint info
        mintedValueOf = LedgerValueV1.assetClassValueOf mintedValue redeemerNFT
    mintedValueOf == 1

{- | Check if NFT is the same that the T.PoolNFT. -}
{-# INLINABLE isPoolNFTParam #-}
isPoolNFTParam ::  T.PoolParams -> T.PoolNFT -> Bool
isPoolNFTParam  pParams redeemerPoolNFT   = do
    let 
        poolNFT  = T.ppPoolNFT pParams

    redeemerPoolNFT == poolNFT

{- | Check if the redeemerPoolNFT is the same in all the inputs T.PoolState Datums. -}
{-# INLINABLE isPoolNFTDatum #-}
isPoolNFTDatum ::  T.PoolNFT -> LedgerContextsV1.ScriptContext -> Bool
isPoolNFTDatum   poolNFT ctx   = do
    let 
        inputsWithPoolState = getInputsWithPoolState ctx
        inputsPoolState_Datums = getInput_Datum <$> inputsWithPoolState

    all (poolNFT==) [T.psPoolNFT inputPoolState_Datum |  inputPoolState_Datum <- inputsPoolState_Datums]

{- | Check if the redeemerUserNFT is the same in all the inputs UserState Datums. -}
{-# INLINABLE isUserNFTDatum #-}
isUserNFTDatum ::  T.UserNFT -> LedgerContextsV1.ScriptContext -> Bool
isUserNFTDatum   userNFT ctx   = do
    let 
        inputsWithUserState = getInputsWithUserState ctx
        inputsWithUserState_Datums = getInput_Datum <$> inputsWithUserState

    all (userNFT==)  [T.usUserNFT inputWithUserState_Datum |  inputWithUserState_Datum <- inputsWithUserState_Datums]


{- | Check if User NFT is in any output going to the user wallet. -}
{-# INLINABLE isUserNFTInUserOutputValue #-}
isUserNFTInUserOutputValue :: T.UserNFT -> T.User -> LedgerContextsV1.TxInfo -> Bool
isUserNFTInUserOutputValue  redeemerUserNFT user info  = do
    let 
        outPutValue = Ledger.valuePaidTo info (Ledger.unPaymentPubKeyHash user )
        outPutNFTQuantity = LedgerValueV1.assetClassValueOf outPutValue redeemerUserNFT

    outPutNFTQuantity == 1 

isPoolNFTInSomeOutputWithPoolState :: [(LedgerApiV1.TxOut, T.PoolStateTypo)] -> T.PoolNFT -> LedgerContextsV1.ScriptContext -> Bool
isPoolNFTInSomeOutputWithPoolState outputsWithPoolState redeemerPoolNFT ctx  = do
    let 
        outputsWithPoolState_TxOut = fst <$> outputsWithPoolState

        outputsWithPoolState_Values = LedgerApiV1.txOutValue <$> outputsWithPoolState_TxOut

        outPutNFTQuantities = [LedgerValueV1.assetClassValueOf  outPutValue redeemerPoolNFT | outPutValue <- outputsWithPoolState_Values ]

    1 `elem` outPutNFTQuantities --any (==1) outPutNFTQuantities

{-  Check if this the invest created at date is correct.  -}
{-# INLINABLE correctInvestCreatedAt #-}
correctInvestCreatedAt :: LedgerApiV1.POSIXTime -> LedgerContextsV1.TxInfo -> Bool
correctInvestCreatedAt ruiCreatedAt info = do
    -- XXX: +1 because timeRange lower closure may be False
   (ruiCreatedAt + 1) `LedgerIntervalV1.member` LedgerApiV1.txInfoValidRange info

{-  Check if this the invest deadline date is correct. 
    Need to be after createdAt
    Before deadline of the Pool
-}
{-# INLINABLE correctInvestDeadlineAt #-}
correctInvestDeadlineAt :: T.PoolParams -> LedgerApiV1.POSIXTime -> LedgerApiV1.POSIXTime -> LedgerContextsV1.TxInfo -> Bool
correctInvestDeadlineAt pParams ruiCreatedAt ruiDeadline info = do
    -- XXX: +1 because timeRange lower closure may be False
   (ruiDeadline + 1) `LedgerIntervalV1.member` LedgerIntervalV1.interval ruiCreatedAt (T.ppDeadline pParams)



{- Check if this the claim date at is correct. -}
{-# INLINABLE correctClaimDateAt #-}
correctClaimDateAt :: LedgerApiV1.POSIXTime -> LedgerContextsV1.TxInfo -> Bool
correctClaimDateAt rugrClaimAt info = do
    -- XXX: +1 because timeRange lower closure may be False
   (rugrClaimAt + 1) `LedgerIntervalV1.member` LedgerApiV1.txInfoValidRange info


{- Check that the tx range interval of validity. Can't be infinitum and need to be lees than T.ppValidTimeRange Pool Param . . -}
{-# INLINABLE isValidRange #-}
isValidRange :: T.PoolParams -> LedgerContextsV1.TxInfo -> Bool
isValidRange pParams info = Helpers.checkIntervalSize (LedgerApiV1.txInfoValidRange info) $ T.ppValidTimeRange pParams


{- Check if this the claim is more than the minumun claim value. -}
{-# INLINABLE isMoreThanMinimunClaim #-}
isMoreThanMinimunClaim :: T.PoolParams -> T.Proffit -> Bool
isMoreThanMinimunClaim pParams rugrClaim = do
    rugrClaim >= T.ppMinimunClaim pParams 


--Getting Inputs and Output with PoolState or UserState Datums

{- | Get the Outputs going again to the script address. -}
{-# INLINABLE getOnwOutputs #-}
getOnwOutputs :: LedgerContextsV1.ScriptContext -> [LedgerApiV1.TxOut]
getOnwOutputs = LedgerContextsV1.getContinuingOutputs

{- | Gets the datum attached to a utxo. -}
{-# INLINABLE getDatumFromTxOut #-}
getDatumFromTxOut :: PlutusTx.FromData T.ValidatorDatum => LedgerApiV1.TxOut -> LedgerContextsV1.ScriptContext -> Maybe T.ValidatorDatum
getDatumFromTxOut o ctx = LedgerTxV1.txOutDatum o >>= (`LedgerContextsV1.findDatum` LedgerContextsV1.scriptContextTxInfo ctx)
                   >>= PlutusTx.fromBuiltinData . LedgerScriptsV1.getDatum :: Maybe T.ValidatorDatum


{-# INLINABLE getInput_TxOutRef #-}
getInput_TxOutRef :: (LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, a) -> LedgerApiV1.TxOutRef
getInput_TxOutRef (x,_,_) = x

{-# INLINABLE getInput_TxOut #-}
getInput_TxOut :: (LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, a) -> LedgerApiV1.TxOut
getInput_TxOut (_,x,_) = x

{-# INLINABLE getInput_Datum #-}
getInput_Datum :: (LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, a) -> a 
getInput_Datum (_,_,x) = x

{-# INLINABLE removeInputByTxOutRef #-}
removeInputByTxOutRef  :: LedgerApiV1.TxOutRef -> [(LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, T.PoolStateTypo)] -> [(LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, T.PoolStateTypo)]
removeInputByTxOutRef _ []                 = []
removeInputByTxOutRef x (y:ys) 
            | x == (getInput_TxOutRef y)      = removeInputByTxOutRef x ys
            | otherwise                     =  y : removeInputByTxOutRef x ys


{- | Gets the input T.PoolState Datums of the tx ScriptContext. -}
{-# INLINABLE getInputsWithPoolState #-}
getInputsWithPoolState :: LedgerContextsV1.ScriptContext -> [(LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, T.PoolStateTypo)]
getInputsWithPoolState ctx = do
    let
        txInInfoResolveds = [  ( LedgerApiV1.txInInfoOutRef txInfoInput,  LedgerApiV1.txInInfoResolved  txInfoInput) | txInfoInput <- LedgerApiV1.txInfoInputs (LedgerContextsV1.scriptContextTxInfo ctx)]

        txOutReftxOutsAndDatums = [  (txInInfoOutRef, txInInfoResolved, getDatumFromTxOut txInInfoResolved ctx) | (txInInfoOutRef, txInInfoResolved) <- txInInfoResolveds]

        txOutReftxOutsAndJustDatums = [  (txoutref, txtout, dat) | (txoutref, txtout, dat) <- txOutReftxOutsAndDatums, isJust dat ]

        inputsWithPoolState =  [  (txoutref, txtout, Helpers.fromJust $ Helpers.getPoolStateFromMaybeDatum dat) | (txoutref,txtout, dat) <- txOutReftxOutsAndJustDatums,  Helpers.datumIsPoolState dat ] 

    inputsWithPoolState

{- | Gets the input T.PoolState Datum with the Pool NFT. -}
{-# INLINABLE getInputWithPoolStateAndPoolNFT #-}
getInputWithPoolStateAndPoolNFT ::   [(LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, T.PoolStateTypo)] -> T.PoolNFT -> Maybe (LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, T.PoolStateTypo)
getInputWithPoolStateAndPoolNFT  inputsWithPoolState redeemerPoolNFT = do
    let 
        inputsWithPoolState_TxOut = getInput_TxOut <$> inputsWithPoolState

        inputsWithPoolState_Values = LedgerApiV1.txOutValue <$> inputsWithPoolState_TxOut

        inputsWithPoolStateAndPoolNFT = [(txOutRef,txOut,poolStateTypo) | (txOutRef,txOut,poolStateTypo) <- inputsWithPoolState , LedgerValueV1.assetClassValueOf (LedgerApiV1.txOutValue txOut) redeemerPoolNFT > 0]

    case inputsWithPoolStateAndPoolNFT of
        [inputWithPoolStateAndPoolNFT] -> Just inputWithPoolStateAndPoolNFT
        _ -> Nothing

{- | Gets the output T.PoolState Datum with the Pool NFT. -}
{-# INLINABLE getOutputWithPoolStateAndPoolNFT #-}
getOutputWithPoolStateAndPoolNFT ::   [(LedgerApiV1.TxOut, T.PoolStateTypo)] -> T.PoolNFT -> Maybe (LedgerApiV1.TxOut, T.PoolStateTypo)
getOutputWithPoolStateAndPoolNFT  outputsWithPoolState redeemerPoolNFT = do
    let 
        ouputsWithPoolState_TxOut = fst <$> outputsWithPoolState

        outputsWithPoolState_Values = LedgerApiV1.txOutValue <$> ouputsWithPoolState_TxOut

        outputsWithPoolStateAndPoolNFT = [(txOut,poolStateTypo) | (txOut,poolStateTypo) <- outputsWithPoolState , LedgerValueV1.assetClassValueOf (LedgerApiV1.txOutValue txOut) redeemerPoolNFT > 0]

    case outputsWithPoolStateAndPoolNFT of
        [outputWithPoolStateAndPoolNFT] -> Just outputWithPoolStateAndPoolNFT
        _ -> Nothing


{- | Gets the input UserState Datums of the tx ScriptContext. -}
{-# INLINABLE getInputsWithUserState #-}
getInputsWithUserState :: LedgerContextsV1.ScriptContext -> [(LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, T.UserStateTypo)]
getInputsWithUserState ctx = do
    let
        txInInfoResolveds = [ ( LedgerApiV1.txInInfoOutRef txInfoInput,  LedgerApiV1.txInInfoResolved  txInfoInput)  | txInfoInput <- LedgerApiV1.txInfoInputs (LedgerContextsV1.scriptContextTxInfo ctx)]

        txOutReftxOutsAndDatums = [  (txInInfoOutRef, txInInfoResolved, getDatumFromTxOut txInInfoResolved ctx) |  (txInInfoOutRef, txInInfoResolved) <- txInInfoResolveds]

        txOutReftxOutsAndJustDatums = [  (txoutref, txtout, dat) | (txoutref, txtout, dat) <- txOutReftxOutsAndDatums, isJust dat ]

        inputsWithUserState =  [  (txoutref, txtout, Helpers.fromJust $ Helpers.getUserStateFromMaybeDatum dat) | (txoutref, txtout, dat) <- txOutReftxOutsAndJustDatums,  Helpers.datumIsUserState dat ] 

    inputsWithUserState

{- | Gets all the outputs with PoolState Datums. -}
{-# INLINABLE getOutputsWithPoolState #-}
getOutputsWithPoolState :: LedgerContextsV1.ScriptContext -> [(LedgerApiV1.TxOut, T.PoolStateTypo)]
getOutputsWithPoolState ctx = 
    case getOnwOutputs ctx of
        []   -> traceError "Expected at least one output."
        txOuts -> do
            let 
                txOutsAndDatums = [  (txOut, getDatumFromTxOut txOut ctx) | txOut <- txOuts ]
                txOutsAndJustDatums = [  (txtout, dat) | (txtout, dat) <- txOutsAndDatums, isJust dat ]
                outputsWithPoolState =  [  (txtout, Helpers.fromJust $ Helpers.getPoolStateFromMaybeDatum dat) | (txtout, dat) <- txOutsAndJustDatums,  Helpers.datumIsPoolState dat ] 
            
            outputsWithPoolState

{- | Gets all the outputs with UserState Datums. -}
{-# INLINABLE getOutputsWithUserState #-}
getOutputsWithUserState :: LedgerContextsV1.ScriptContext -> [(LedgerApiV1.TxOut, T.UserStateTypo)]
getOutputsWithUserState ctx = 
    case getOnwOutputs ctx of
        []   -> traceError "Expected at least one output."
        txOuts -> do
            let 
                txOutsAndDatums = [  (txOut, getDatumFromTxOut txOut ctx) | txOut <- txOuts ]
                txOutsAndJustDatums = [  (txtout, dat) | (txtout, dat) <- txOutsAndDatums, isJust dat ]
                txOutsAndUserState =  [  (txtout, Helpers.fromJust $ Helpers.getUserStateFromMaybeDatum dat) | (txtout, dat) <- txOutsAndJustDatums,  Helpers.datumIsUserState dat ] 
            
            txOutsAndUserState
