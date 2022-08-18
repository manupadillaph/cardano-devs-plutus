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

module Validators.StakeSimpleV1.OnChainHelpers where

import           Control.Monad        hiding (fmap)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           Data.String  
import qualified GHC.Generics                        as GHCGenerics (Generic)
--import           Ledger               hiding (singleton)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           LedgerValueV1.Value         as Value
import           Ledger.Ada           as Ada
import           Playground.Contract  (IO, ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (unless)
import qualified Prelude              as P 
import qualified Schema                              (ToSchema)
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
import qualified Validators.StakeSimpleV1.Typos 
import qualified Validators.StakeSimpleV1.Helpers 
import qualified Validators.StakeSimpleV1.OnChainNFT     (mintingNFTPolicy)




--Checking Signatures

{-# INLINABLE signedByMaster #-}
signedByMaster :: Master -> LedgerContextsV1.TxInfo -> Bool
signedByMaster master info  = 
    LedgerContextsV1.txSignedBy info (Ledger.unPaymentPubKeyHash master)

{-# INLINABLE signedByUser #-}
signedByUser :: User -> LedgerContextsV1.TxInfo -> Bool
signedByUser user info  = 
    LedgerContextsV1.txSignedBy info (Ledger.unPaymentPubKeyHash user)

{-# INLINABLE signedByPoolParamMaster #-}
signedByPoolParamMaster :: PoolParams -> LedgerContextsV1.TxInfo -> Bool
signedByPoolParamMaster pParams info  = 
    any (LedgerContextsV1.txSignedBy info ) [Ledger.unPaymentPubKeyHash $ master | master <- T.ppMasters pParams]


{-# INLINABLE signedByPoolStateMasterFunder #-}
signedByPoolStateMasterFunder :: PoolStateTypo -> LedgerContextsV1.TxInfo -> Bool
signedByPoolStateMasterFunder dPoolState info  = 
    any (LedgerContextsV1.txSignedBy info ) [Ledger.unPaymentPubKeyHash $ T.mfMaster masterFunder | masterFunder <- T.psMasterFunders dPoolState]

{-# INLINABLE signedBySingleOwnOutputPoolStateMasterFunder #-}
signedBySingleOwnOutputPoolStateMasterFunder :: LedgerContextsV1.TxInfo -> LedgerContextsV1.ScriptContext-> Bool
signedBySingleOwnOutputPoolStateMasterFunder info ctx  = do
    let 
        (_, outputPoolStateDatum) = getSingleOwnOutputPoolStateDatum ctx

    signedByPoolStateMasterFunder outputPoolStateDatum info


{-# INLINABLE signedByUserStateUser #-}
signedByUserStateUser :: UserStateTypo ->LedgerContextsV1.TxInfo -> Bool
signedByUserStateUser dUserState info = LedgerContextsV1.txSignedBy info $ Ledger.unPaymentPubKeyHash $ T.usUser dUserState

{-# INLINABLE signedByDoubleOwnOutputsUserStateUser #-}
signedByDoubleOwnOutputsUserStateUser :: LedgerContextsV1.TxInfo -> LedgerContextsV1.ScriptContext-> Bool
signedByDoubleOwnOutputsUserStateUser info ctx  = do
    let 
        ((_, _),(_, outputUserStateDatum)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

    signedByUserStateUser outputUserStateDatum info


--Checking Deadline

{-# INLINABLE deadlinePoolParamReached #-}
deadlinePoolParamReached :: PoolParams -> LedgerContextsV1.TxInfo ->Bool
deadlinePoolParamReached pParams info  = LedgerIntervalV1.contains (LedgerIntervalV1.from $ T.ppDeadline pParams) $ LedgerApiV1.txInfoValidRange info

{-# INLINABLE deadlinePoolParamNotReached #-}
deadlinePoolParamNotReached :: PoolParams -> LedgerContextsV1.TxInfo ->Bool
deadlinePoolParamNotReached  pParams info = not (deadlinePoolParamReached  pParams info)

{-# INLINABLE deadlineUserStateReached #-}
deadlineUserStateReached :: UserStateTypo -> LedgerContextsV1.TxInfo -> Bool
deadlineUserStateReached dUserState info = LedgerIntervalV1.contains (LedgerIntervalV1.from $ T.usDeadline dUserState) $ LedgerApiV1.txInfoValidRange info

{-# INLINABLE deadlineUserStateNotReached #-}
deadlineUserStateNotReached :: UserStateTypo -> LedgerContextsV1.TxInfo -> Bool
deadlineUserStateNotReached dUserState info = not (deadlineUserStateReached dUserState info )


--Checking NFT

{- | Check if txoutref is pressent at the inputs to be consumed. -}
{-# INLINABLE hasInputUTxO #-}
hasInputUTxO :: LedgerContextsV1.TxInfo -> LedgerApiV1.TxOutRef -> Bool
hasInputUTxO info txOutRef = any (\i -> LedgerApiV1.txInInfoOutRef i == txOutRef) $ LedgerApiV1.txInfoInputs info

{- | Check if NFT was produced by specific policy. -}
{-# INLINABLE correctMintigPolicyUsedForNFT #-}
correctMintigPolicyUsedForNFT :: PoolParams -> NFT -> Bool
correctMintigPolicyUsedForNFT  pParams redeemerNFT   = do
    let 
        currencySymbolNFT = fst (LedgerValueV1.unAssetClass redeemerNFT )
        currencySymbolMintingPolicy = T.ppCurSymbolForMintingNFTPolicy pParams

    currencySymbolNFT == currencySymbolMintingPolicy  

{- | Check if the minting quantity is equal to 1. -}
{-# INLINABLE isMintedNFTValid #-}
isMintedNFTValid :: LedgerContextsV1.TxInfo -> NFT -> Bool
isMintedNFTValid info redeemerNFT  = do
    let 
        mintedValue = LedgerApiV1.txInfoMint info
        mintedValueOf = LedgerValueV1.assetClassValueOf mintedValue redeemerNFT
    mintedValueOf == 1

{- | Check if NFT is the same that the PoolNFT. -}
{-# INLINABLE isPoolNFTParam #-}
isPoolNFTParam ::  PoolParams -> PoolNFT -> Bool
isPoolNFTParam  pParams redeemerPoolNFT   = do
    let 
        poolNFT  = T.ppPoolNFT pParams

    redeemerPoolNFT == poolNFT

{- | Check if the redeemerPoolNFT is the same in all the inputs PoolState Datums. -}
{-# INLINABLE isPoolNFTDatum #-}
isPoolNFTDatum ::  PoolNFT -> LedgerContextsV1.ScriptContext -> Bool
isPoolNFTDatum   poolNFT ctx   = do
    let 
        inputsTxOutWithPoolStateDatums = getInputPoolStateDatums ctx
        inputPoolStateDatums = snd <$> inputsTxOutWithPoolStateDatums

    all (poolNFT==) [T.psPoolNFT inputPoolStateDatum |  inputPoolStateDatum <- inputPoolStateDatums]

{- | Check if the redeemerUserNFT is the same in all the inputs UserState Datums. -}
{-# INLINABLE isUserNFTDatum #-}
isUserNFTDatum ::  UserNFT -> LedgerContextsV1.ScriptContext -> Bool
isUserNFTDatum   userNFT ctx   = do
    let 
        inputTxOutWithUserStateDatums = getInputUserStateDatums ctx
        inputUserStateDatums = snd <$> inputTxOutWithUserStateDatums

    all (userNFT==)  [T.usUserNFT inputUserStateDatum |  inputUserStateDatum <- inputUserStateDatums]

{- | Check if Pool NFT is the single own output with Pool State Datum. -}
{-# INLINABLE isPoolNFTInSingleOwnOutputValue #-}
isPoolNFTInSingleOwnOutputValue :: PoolNFT -> LedgerContextsV1.ScriptContext -> Bool
isPoolNFTInSingleOwnOutputValue  redeemerPoolNFT ctx  = do
    let 
        (txtout, _) = getSingleOwnOutputPoolStateDatum ctx

        outPutValue = LedgerApiV1.txOutValue txtout
        outPutNFTQuantity = LedgerValueV1.assetClassValueOf outPutValue redeemerPoolNFT

    outPutNFTQuantity == 1

{- | Check if User NFT is in any output going to the user wallet. -}
{-# INLINABLE isUserNFTInUserOutputValue #-}
isUserNFTInUserOutputValue :: UserNFT -> User -> LedgerContextsV1.TxInfo -> Bool
isUserNFTInUserOutputValue  redeemerUserNFT user info  = do
    let 
        outPutValue = Ledger.valuePaidTo info (Ledger.unPaymentPubKeyHash user )
        outPutNFTQuantity = LedgerValueV1.assetClassValueOf outPutValue redeemerUserNFT

    outPutNFTQuantity == 1 


{- | Check if there is one single input UserState Datum to redeem  -}
{-# INLINABLE isUnicSingleUserStateInput #-}
isUnicSingleUserStateInput :: LedgerContextsV1.ScriptContext -> Bool
isUnicSingleUserStateInput ctx = do
    let
        inputTxOutWithUserStateDatums = getInputUserStateDatums ctx
        inputUserStateDatums = snd <$> inputTxOutWithUserStateDatums

    case inputUserStateDatums of
        [d] -> True
        _ -> False
 
{- | 
    Check if the PoolState datum produced is correct. 
-- It must include the PoolState datums in all the inputs, calculate the sum of all the funds in them.
-- It must include in the sum of the redeemerMaster that is calling this tx the new fund of his (redeemerFund).
-- It must include the correct redeemerPoolNFT.
 -}
{-# INLINABLE correctSingleOwnOutputPoolState_Datum_WithNewFund #-}
correctSingleOwnOutputPoolState_Datum_WithNewFund :: PoolNFT -> Master -> Fund -> LedgerContextsV1.ScriptContext -> Bool
correctSingleOwnOutputPoolState_Datum_WithNewFund  redeemerPoolNFT redeemerMaster redeemerFund ctx = do
    let
        (_, outputPoolStateDatum) = getSingleOwnOutputPoolStateDatum ctx

        inputsTxOutWithPoolStateDatums = getInputPoolStateDatums ctx
        inputPoolStateDatums = snd <$> inputsTxOutWithPoolStateDatums

        newOutputDatum = T.Helpers.mkPoolStateWithNewFundFromPoolStateList inputPoolStateDatums  redeemerPoolNFT redeemerMaster redeemerFund

    newOutputDatum == PoolState outputPoolStateDatum


{- correctOutputValue: checks if output value is the same than the sum of inputs value plus the new fund -}
{-# INLINABLE correctSingleOwnOutputPoolState_Value_WithSumValuesAndNewFund #-}
correctSingleOwnOutputPoolState_Value_WithSumValuesAndNewFund :: Fund -> LedgerContextsV1.ScriptContext -> Bool
correctSingleOwnOutputPoolState_Value_WithSumValuesAndNewFund redeemerFund ctx = do
    let
        (txtout, _) = getSingleOwnOutputPoolStateDatum ctx

        inputsTxOutWithPoolStateDatums = getInputPoolStateDatums ctx
        inputsTxOut = fst <$> inputsTxOutWithPoolStateDatums
        listValuesEnInputsUtxos = [ LedgerApiV1.txOutValue inputTxOut | inputTxOut <- inputsTxOut] 

        valueRedeemerFund  = LedgerAda.lovelaceValueOf redeemerFund 

        valueTotal = P.foldl (<>) valueRedeemerFund listValuesEnInputsUtxos

    valueTotal == LedgerApiV1.txOutValue txtout



{- 
    Check if the Single Own Output Value is the correct sum of all the funds in the PoolState Datums in the inputs, plus the new fund (redeemerFund)
    The output Value can be also be cheked against the new PoolState datum produced, because was already checked that the datum had the correct sum if the inputs datums. 
-}
-- TODO: compara el value de ada solamente, por que las inversiones por ahora son en ada, pero esto cambiaria con el tipo de moneda que se use poara la inversion
--       ademas con esto evito sumar el valor del token nft que esta en valueTotal que viene de la salida que lleva esta tx
--       la salida lleva el token de nuevo al sctrip y este no quiero tenerlo en cuenta opara esta suma AQUI.
{-# INLINABLE correctSingleOwnOutputPoolState_Datum_Value_WithSumDatumsAndNewFund #-}
correctSingleOwnOutputPoolState_Datum_Value_WithSumDatumsAndNewFund :: Fund -> LedgerContextsV1.ScriptContext -> Bool
correctSingleOwnOutputPoolState_Datum_Value_WithSumDatumsAndNewFund  redeemerFund ctx  = do
    let
        (txtout, _) = getSingleOwnOutputPoolStateDatum ctx
        
        inputsTxOutWithPoolStateDatums = getInputPoolStateDatums ctx
        inputPoolStateDatums = snd <$> inputsTxOutWithPoolStateDatums

        listMasterFunders = concat [ T.psMasterFunders poolStateDatum | poolStateDatum <- inputPoolStateDatums] 
        listValuesEnPoolStateMasterFunders = [ LedgerAda.lovelaceValueOf (T.mfFund masterFunder) | masterFunder <- listMasterFunders] 

        valueRedeemerFund  = LedgerAda.lovelaceValueOf redeemerFund 

        valueTotal = P.foldl (<>) valueRedeemerFund listValuesEnPoolStateMasterFunders

    LedgerAda.fromValue valueTotal == LedgerAda.fromValue (LedgerApiV1.txOutValue txtout)

{- 
Check if the PoolState Datum produced is correct, with the new User added and all the rest the same than before.
Inside is going to fail if the outputs at the script aren't just two, one with the PoolState Datum and the other with the UserStateDatum.
-}
{-# INLINABLE correctDoubleOwnOutputsPoolState_Datum_WithNewUser #-}
correctDoubleOwnOutputsPoolState_Datum_WithNewUser :: PoolNFT -> UserNFT -> LedgerContextsV1.ScriptContext -> Bool
correctDoubleOwnOutputsPoolState_Datum_WithNewUser  redeemerPoolNFT redeemerUserNFT ctx = do
    let
        ((_, outputPoolStateDatum),(_, _)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx
 
        inputsTxOutWithPoolStateDatums = getInputPoolStateDatums ctx
        inputPoolStateDatums = snd <$> inputsTxOutWithPoolStateDatums

        newOutputDatum = T.mkPoolStateWithNewUserInvestFromPoolStateList inputPoolStateDatums  redeemerPoolNFT redeemerUserNFT

    newOutputDatum == PoolState outputPoolStateDatum

{- Check if the Output Value with PoolState Datum is the same that in the Inputs with PoolSate Datum -}
{-# INLINABLE correctDoubleOwnOutputPoolState_Value_WithSumValues #-}
correctDoubleOwnOutputPoolState_Value_WithSumValues :: LedgerContextsV1.ScriptContext -> Bool
correctDoubleOwnOutputPoolState_Value_WithSumValues ctx  = do
    let
        ((txtout, _),(_, _)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

        inputsTxOutWithPoolStateDatums = getInputPoolStateDatums ctx
        inputsTxOut = fst <$> inputsTxOutWithPoolStateDatums
        listValuesEnInputsUtxos = [ LedgerApiV1.txOutValue inputTxOut | inputTxOut <- inputsTxOut] 

        valueTotal = P.foldl (<>) (LedgerAda.lovelaceValueOf 0) listValuesEnInputsUtxos

    valueTotal == LedgerApiV1.txOutValue txtout

{- 
    -- TODO: check all of this
    -- T.usUser, the same that redeemerUser. After is checked that the redeemerUser is signing this tx.
    -- T.usUserNFT, the same that redeemerUserNFT. 
    -- T.usInvest, the same that redeemerInvest,
    -- T.usCreatedAt, time can't be in the past. 
    -- T.usDeadline, time can't be before T.usCreatedAt and with a minimut distance 
    -- usTotal, the total proffit to win, calculated with the interest of the Pool.
    -- T.usChashedOut, need to be 0.
    -- usLastClain, need to be nothing. 
-}
{-# INLINABLE correctDoubleOwnOutputsUserState_Datum_OfNewUser #-}
correctDoubleOwnOutputsUserState_Datum_OfNewUser :: User -> UserNFT -> Invest -> LedgerApiV1.POSIXTime -> T.Deadline -> LedgerContextsV1.ScriptContext -> Bool
correctDoubleOwnOutputsUserState_Datum_OfNewUser  redeemerUser redeemerUserNFT redeemerInvest redeemerCreatedAt redeemerDeadline ctx = do
    let
        ((_, _),(_, outputUserStateDatum)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

        --LedgerApiV1.txInfoValidRange info

        newOutputDatum = T.mkUserState  redeemerUser redeemerUserNFT redeemerInvest  redeemerCreatedAt redeemerDeadline 0 0 P.Nothing

    newOutputDatum == T.UserState outputUserStateDatum


{- Check if the Output Value with UserState Datum is the same that in redeemerInvest, witch is at the same time the T.usInvest in the UserState Datum, cheked before. -}
{-# INLINABLE correctDoubleOwnOutputUserState_Value_OfNewUser #-}
correctDoubleOwnOutputUserState_Value_OfNewUser ::  Invest -> LedgerContextsV1.ScriptContext -> Bool
correctDoubleOwnOutputUserState_Value_OfNewUser redeemerInvest ctx = do
    let
        ((_, _),(txtout, _)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

        valueTotal = LedgerAda.lovelaceValueOf redeemerInvest

    valueTotal == LedgerApiV1.txOutValue txtout   


{- 
Check if the PoolState Datum produced is correct, need to be the same that the sum of inputs PoolState
Inside is going to fail if the outputs at the script aren't just two, one with the PoolState Datum and the other with the UserStateDatum.
-}
{-# INLINABLE correctDoubleOwnOutputsPoolState_Datum_WithNoChanges #-}
correctDoubleOwnOutputsPoolState_Datum_WithNoChanges ::  PoolNFT -> LedgerContextsV1.ScriptContext -> Bool
correctDoubleOwnOutputsPoolState_Datum_WithNoChanges T.rugrPoolNFT  ctx = do
    let
        ((_, outputPoolStateDatum),(_, _)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx
 
        inputsTxOutWithPoolStateDatums = getInputPoolStateDatums ctx
        inputPoolStateDatumss = snd <$> inputsTxOutWithPoolStateDatums

        newOutputDatum = T.mkPoolStateFromPoolStateList inputPoolStateDatumss T.rugrPoolNFT 

    newOutputDatum == PoolState outputPoolStateDatum

{-
    Check if the UserState Datum produced is correct, with the new claim
    TODO: 
    Inside is going to fail if the outputs at the script aren't just two, one with the PoolState Datum and the other with the UserStateDatum.
-}



{-# INLINABLE correctDoubleOwnOutputsUserState_Datum_WithNewClaimRewards #-}
correctDoubleOwnOutputsUserState_Datum_WithNewClaimRewards :: User -> UserNFT -> Proffit -> LedgerApiV1.POSIXTime   -> LedgerContextsV1.ScriptContext -> Bool
correctDoubleOwnOutputsUserState_Datum_WithNewClaimRewards redeemerUser redeemerUserNFT rugrClaim T.rugrClaimAt ctx = do
    let
        ((_, _),(_, outputUserStateDatum)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

        inputTxOutWithUserStateDatums = getInputUserStateDatums ctx
        inputUserStateDatums = snd <$> inputTxOutWithUserStateDatums

    case inputUserStateDatums of
        [inputUserStateDatum] -> do
                let 

                    rewards = Helpers.getRewardsPerInvest (T.usLastClaimAt inputUserStateDatum) T.rugrClaimAt  (T.usCreatedAt  inputUserStateDatum )  (T.usInvest inputUserStateDatum ) 
                    totalNewRewards = rewards  + T.usRewardsNotClaimed inputUserStateDatum
                    rewardsNotClaimed = totalNewRewards - T.rugrClaim
                    totalRewardsCashedOut = T.usChashedOut inputUserStateDatum + T.rugrClaim 

                    newOutputDatum = T.mkUserState redeemerUser redeemerUserNFT 
                                    (T.usInvest inputUserStateDatum ) 
                                    (T.usCreatedAt  inputUserStateDatum ) 
                                    (T.usDeadline  inputUserStateDatum )  
                                    totalRewardsCashedOut
                                    rewardsNotClaimed
                                    (P.Just T.rugrClaimAt)

                newOutputDatum == T.UserState outputUserStateDatum
        _ -> traceError "ERROR in correctDoubleOwnOutputsUserState_Datum_WithNewClaimRewards: Not one single UserState Input"

   

{-Check if the Output Value with PoolState Datum is the same that in the Inputs with PoolSate Datum less the rewards claimed -}
{-# INLINABLE correctDoubleOwnOutputPoolState_Value_LessClaimRewards #-}
correctDoubleOwnOutputPoolState_Value_LessClaimRewards ::  Proffit -> LedgerContextsV1.ScriptContext -> Bool
correctDoubleOwnOutputPoolState_Value_LessClaimRewards T.rugrClaim ctx = do
    let
        ((txtout, _),(_, _)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

        inputsTxOutWithPoolStateDatums = getInputPoolStateDatums ctx
        inputsTxOut = fst <$> inputsTxOutWithPoolStateDatums

        listValuesEnInputsUtxos = [ LedgerApiV1.txOutValue inputTxOut | inputTxOut <- inputsTxOut] 

        valueTotal = P.foldl (<>)  (LedgerAda.lovelaceValueOf 0) listValuesEnInputsUtxos

         

    valueTotal == LedgerApiV1.txOutValue txtout <> LedgerAda.lovelaceValueOf T.rugrClaim


    

{-Check if the Output Value with UserState Datum is the same that the input UserState. -}
{-# INLINABLE correctDoubleOwnOutputUserState_Value_WithNoChanges #-}
correctDoubleOwnOutputUserState_Value_WithNoChanges :: LedgerContextsV1.ScriptContext -> Bool
correctDoubleOwnOutputUserState_Value_WithNoChanges ctx = do
    let
        ((_, _),(txtout, _)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

        inputTxOutWithUserStateDatums = getInputUserStateDatums ctx
        inputsTxOut = fst <$> inputTxOutWithUserStateDatums

        listValuesEnInputsUtxos = [ LedgerApiV1.txOutValue inputTxOut | inputTxOut <- inputsTxOut] 

        valueTotal = foldl (<>)  ( LedgerAda.lovelaceValueOf 0) listValuesEnInputsUtxos

    valueTotal == LedgerApiV1.txOutValue txtout


{-  Check if this the invest created at date is correct.  -}
{-# INLINABLE correctInvestCreatedAt #-}
correctInvestCreatedAt :: LedgerApiV1.POSIXTime -> LedgerContextsV1.TxInfo -> Bool
correctInvestCreatedAt T.ruiCreatedAt info = do
    -- XXX: +1 because timeRange lower closure may be False
   (T.ruiCreatedAt + 1) `LedgerIntervalV1.member` LedgerApiV1.txInfoValidRange info

{-  Check if this the invest deadline date is correct. 
    Need to be after createdAt
    Before deadline of the Pool
-}
{-# INLINABLE correctInvestDeadlineAt #-}
correctInvestDeadlineAt :: PoolParams -> LedgerApiV1.POSIXTime -> LedgerApiV1.POSIXTime -> LedgerContextsV1.TxInfo -> Bool
correctInvestDeadlineAt pParams T.ruiCreatedAt T.ruiDeadline info = do
    -- XXX: +1 because timeRange lower closure may be False
   (T.ruiDeadline + 1) `LedgerIntervalV1.member` LedgerIntervalV1.interval T.ruiCreatedAt (T.ppDeadline pParams)



{- Check if this the claim date at is correct. -}
{-# INLINABLE correctClaimDateAt #-}
correctClaimDateAt :: LedgerApiV1.POSIXTime -> LedgerContextsV1.TxInfo -> Bool
correctClaimDateAt T.rugrClaimAt info = do
    -- XXX: +1 because timeRange lower closure may be False
   (T.rugrClaimAt + 1) `LedgerIntervalV1.member` LedgerApiV1.txInfoValidRange info


{- Check that the tx range interval of validity. Can't be infinitum and need to be lees than T.ppValidTimeRange Pool Param . . -}
{-# INLINABLE isValidRange #-}
isValidRange :: PoolParams -> LedgerContextsV1.TxInfo -> Bool
isValidRange pParams info = Helpers.checkIntervalSize (LedgerApiV1.txInfoValidRange info) $ T.ppValidTimeRange pParams


{- Check if this the claim is more than the minumun claim value. -}
{-# INLINABLE isMoreThanMinimunClaim #-}
isMoreThanMinimunClaim :: PoolParams -> Proffit -> Bool
isMoreThanMinimunClaim pParams T.rugrClaim = do
    T.rugrClaim >= T.ppMinimunClaim pParams 


{- Check if this the claim is correct. -}
{-# INLINABLE correctClaimValue #-}
correctClaimValue :: PoolParams -> Proffit -> LedgerApiV1.POSIXTime   -> LedgerContextsV1.ScriptContext -> Bool
correctClaimValue pParams T.rugrClaim T.rugrClaimAt ctx = do
    let
        ((_, _),(_, outputUserStateDatum)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

        inputTxOutWithUserStateDatums = getInputUserStateDatums ctx
        inputUserStateDatums = snd <$> inputTxOutWithUserStateDatums

    case inputUserStateDatums of
        [inputUserStateDatum] -> do
                let 
                    rewards = Helpers.getRewardsPerInvest (T.usLastClaimAt inputUserStateDatum) T.rugrClaimAt (T.usCreatedAt  inputUserStateDatum )  (T.usInvest inputUserStateDatum ) 
                    totalNewRewards = rewards  + T.usRewardsNotClaimed inputUserStateDatum

                T.rugrClaim >= T.ppMinimunClaim pParams && T.rugrClaim <= totalNewRewards


-- {-| If the amount to claim is greater or equal than the minimum amount setted,
--     then, this function computes the new UserState by updating his last claim
--     and, the total amount of rewards to be claimed.

--     Other way it returns P.Nothing.
-- -}
-- {-# INLINABLE claim #-}
-- claim :: UserState -> LedgerApiV1.POSIXTime -> OperationSettings -> P.Maybe (UserState, Integer)
-- claim userState cTime fconf =
--     if rewards < minClaim fconf
--     then P.Nothing
--     else P.Just (newUserState, rewards)
--   where
--     newUserState :: UserState
--     newUserState = userState { lastClaim = P.Just cTime}

--     rewards :: Integer
--     rewards = computeRewards (deposits userState) (lastClaim userState) cTime



-- claimRes :: P.Maybe (UserState, Integer)
-- claimRes = Business.claim inputUserState time (opSettings $ settings staking)

-- {-# INLINABLE computeRewards #-}
-- computeRewards
--     :: [Deposit]
--     -> P.Maybe LedgerApiV1.POSIXTime
--     -> LedgerApiV1.POSIXTime
--     -> Integer
-- computeRewards deposits lastClaim now =
--     foldr getRewards 0 deposits
--   where
--     getRewards :: Deposit -> Integer -> Integer
--     getRewards dep rews = rews + rewardsPerDeposit lastClaim now dep



