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

module Validators.StakeSimple.OnChainHelpers where

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
import  Validators.StakeSimple.Typos 
import  Validators.StakeSimple.Helpers 
import  Validators.StakeSimple.OnChainNFT     (mintingNFTPolicy)




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

{-# INLINABLE signedBySingleOwnOutputPoolStateMasterFunder #-}
signedBySingleOwnOutputPoolStateMasterFunder :: TxInfo -> ScriptContext-> Bool
signedBySingleOwnOutputPoolStateMasterFunder info ctx  = do
    let 
        (_, outputPoolStateDatum) = getSingleOwnOutputPoolStateDatum ctx

    signedByPoolStateMasterFunder outputPoolStateDatum info


{-# INLINABLE signedByUserStateUser #-}
signedByUserStateUser :: UserStateTypo ->TxInfo -> Bool
signedByUserStateUser dUserState info = txSignedBy info $ unPaymentPubKeyHash $ usUser dUserState

{-# INLINABLE signedByDoubleOwnOutputsUserStateUser #-}
signedByDoubleOwnOutputsUserStateUser :: TxInfo -> ScriptContext-> Bool
signedByDoubleOwnOutputsUserStateUser info ctx  = do
    let 
        ((_, _),(_, outputUserStateDatum)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

    signedByUserStateUser outputUserStateDatum info


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
        inputsTxOutWithPoolStateDatums = getInputPoolStateDatums ctx
        inputPoolStateDatums = snd <$> inputsTxOutWithPoolStateDatums

    PlutusTx.Prelude.all (poolNFT==) [psPoolNFT inputPoolStateDatum |  inputPoolStateDatum <- inputPoolStateDatums]

{- | Check if the redeemerUserNFT is the same in all the inputs UserState Datums. -}
{-# INLINABLE isUserNFTDatum #-}
isUserNFTDatum ::  UserNFT -> ScriptContext ->  Bool
isUserNFTDatum   userNFT ctx   = do
    let 
        inputTxOutWithUserStateDatums = getInputUserStateDatums ctx
        inputUserStateDatums = snd <$> inputTxOutWithUserStateDatums

    PlutusTx.Prelude.all (userNFT==)  [usUserNFT inputUserStateDatum |  inputUserStateDatum <- inputUserStateDatums]

{- | Check if Pool NFT is the single own output with Pool State Datum. -}
{-# INLINABLE isPoolNFTInSingleOwnOutputValue #-}
isPoolNFTInSingleOwnOutputValue :: PoolNFT ->  ScriptContext -> Bool
isPoolNFTInSingleOwnOutputValue  redeemerPoolNFT ctx  = do
    let 
        (txtout, _) = getSingleOwnOutputPoolStateDatum ctx

        outPutValue = txOutValue txtout
        outPutNFTQuantity = assetClassValueOf outPutValue redeemerPoolNFT

    outPutNFTQuantity == 1

{- | Check if User NFT is in any output going to the user wallet. -}
{-# INLINABLE isUserNFTInUserOutputValue #-}
isUserNFTInUserOutputValue :: UserNFT ->  User -> TxInfo -> Bool
isUserNFTInUserOutputValue  redeemerUserNFT user info  = do
    let 
        outPutValue = valuePaidTo info (unPaymentPubKeyHash user )
        outPutNFTQuantity = assetClassValueOf outPutValue redeemerUserNFT

    outPutNFTQuantity == 1 


{- | Check if there is one single input UserState Datum to redeem  -}
{-# INLINABLE isUnicSingleUserStateInput #-}
isUnicSingleUserStateInput :: ScriptContext -> Bool
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
correctSingleOwnOutputPoolState_Datum_WithNewFund :: PoolNFT -> Master -> Fund -> ScriptContext -> Bool
correctSingleOwnOutputPoolState_Datum_WithNewFund  redeemerPoolNFT redeemerMaster redeemerFund ctx = do
    let
        (_, outputPoolStateDatum) = getSingleOwnOutputPoolStateDatum ctx

        inputsTxOutWithPoolStateDatums = getInputPoolStateDatums ctx
        inputPoolStateDatums = snd <$> inputsTxOutWithPoolStateDatums

        newOutputDatum = mkPoolStateWithNewFundFromPoolStateList inputPoolStateDatums  redeemerPoolNFT redeemerMaster redeemerFund

    newOutputDatum == PoolState outputPoolStateDatum


{- correctOutputValue: checks if output value is the same than the sum of inputs value plus the new fund -}
{-# INLINABLE correctSingleOwnOutputPoolState_Value_WithSumValuesAndNewFund #-}
correctSingleOwnOutputPoolState_Value_WithSumValuesAndNewFund :: Fund -> ScriptContext ->  Bool
correctSingleOwnOutputPoolState_Value_WithSumValuesAndNewFund redeemerFund ctx = do
    let
        (txtout, _) = getSingleOwnOutputPoolStateDatum ctx

        inputsTxOutWithPoolStateDatums = getInputPoolStateDatums ctx
        inputsTxOut = fst <$> inputsTxOutWithPoolStateDatums
        listValuesEnInputsUtxos = [ txOutValue inputTxOut | inputTxOut <- inputsTxOut] 

        valueRedeemerFund  = Ada.lovelaceValueOf redeemerFund 

        valueTotal = HASKELL.foldl (<>) valueRedeemerFund listValuesEnInputsUtxos

    valueTotal == txOutValue txtout



{- 
    Check if the Single Own Output Value is the correct sum of all the funds in the PoolState Datums in the inputs, plus the new fund (redeemerFund)
    The output Value can be also be cheked against the new PoolState datum produced, because was already checked that the datum had the correct sum if the inputs datums. 
-}
-- TODO: compara el value de ada solamente, por que las inversiones por ahora son en ada, pero esto cambiaria con el tipo de moneda que se use poara la inversion
--       ademas con esto evito sumar el valor del token nft que esta en valueTotal que viene de la salida que lleva esta tx
--       la salida lleva el token de nuevo al sctrip y este no quiero tenerlo en cuenta opara esta suma AQUI.
{-# INLINABLE correctSingleOwnOutputPoolState_Datum_Value_WithSumDatumsAndNewFund #-}
correctSingleOwnOutputPoolState_Datum_Value_WithSumDatumsAndNewFund :: Fund ->  ScriptContext -> Bool
correctSingleOwnOutputPoolState_Datum_Value_WithSumDatumsAndNewFund  redeemerFund ctx  = do
    let
        (txtout, _) = getSingleOwnOutputPoolStateDatum ctx
        
        inputsTxOutWithPoolStateDatums = getInputPoolStateDatums ctx
        inputPoolStateDatums = snd <$> inputsTxOutWithPoolStateDatums

        listMasterFunders = concat [ psMasterFunders poolStateDatum | poolStateDatum <- inputPoolStateDatums] 
        listValuesEnPoolStateMasterFunders = [ Ada.lovelaceValueOf (mfFund masterFunder) | masterFunder <- listMasterFunders] 

        valueRedeemerFund  = Ada.lovelaceValueOf redeemerFund 

        valueTotal = HASKELL.foldl (<>) valueRedeemerFund listValuesEnPoolStateMasterFunders

    Ada.fromValue valueTotal == Ada.fromValue (txOutValue txtout)

{- 
Check if the PoolState Datum produced is correct, with the new User added and all the rest the same than before.
Inside is going to fail if the outputs at the script aren't just two, one with the PoolState Datum and the other with the UserStateDatum.
-}
{-# INLINABLE correctDoubleOwnOutputsPoolState_Datum_WithNewUser #-}
correctDoubleOwnOutputsPoolState_Datum_WithNewUser :: PoolNFT -> UserNFT ->  ScriptContext -> Bool
correctDoubleOwnOutputsPoolState_Datum_WithNewUser  redeemerPoolNFT redeemerUserNFT ctx = do
    let
        ((_, outputPoolStateDatum),(_, _)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx
 
        inputsTxOutWithPoolStateDatums = getInputPoolStateDatums ctx
        inputPoolStateDatums = snd <$> inputsTxOutWithPoolStateDatums

        newOutputDatum = mkPoolStateWithNewUserInvestFromPoolStateList inputPoolStateDatums  redeemerPoolNFT redeemerUserNFT

    newOutputDatum == PoolState outputPoolStateDatum

{- Check if the Output Value with PoolState Datum is the same that in the Inputs with PoolSate Datum -}
{-# INLINABLE correctDoubleOwnOutputPoolState_Value_WithSumValues #-}
correctDoubleOwnOutputPoolState_Value_WithSumValues ::  ScriptContext -> Bool
correctDoubleOwnOutputPoolState_Value_WithSumValues ctx  = do
    let
        ((txtout, _),(_, _)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

        inputsTxOutWithPoolStateDatums = getInputPoolStateDatums ctx
        inputsTxOut = fst <$> inputsTxOutWithPoolStateDatums
        listValuesEnInputsUtxos = [ txOutValue inputTxOut | inputTxOut <- inputsTxOut] 

        valueTotal = HASKELL.foldl (<>) (Ada.lovelaceValueOf 0) listValuesEnInputsUtxos

    valueTotal == txOutValue txtout

{- 
    -- TODO: check all of this
    -- usUser, the same that redeemerUser. After is checked that the redeemerUser is signing this tx.
    -- usUserNFT, the same that redeemerUserNFT. 
    -- usInvest, the same that redeemerInvest,
    -- usCreatedAt, time can't be in the past. 
    -- usDeadline, time can't be before usCreatedAt and with a minimut distance 
    -- usTotal, the total proffit to win, calculated with the interest of the Pool.
    -- usChashedOut, need to be 0.
    -- usLastClain, need to be nothing. 
-}
{-# INLINABLE correctDoubleOwnOutputsUserState_Datum_OfNewUser #-}
correctDoubleOwnOutputsUserState_Datum_OfNewUser :: User -> UserNFT ->  Invest -> POSIXTime ->  Deadline ->  ScriptContext -> Bool
correctDoubleOwnOutputsUserState_Datum_OfNewUser  redeemerUser redeemerUserNFT redeemerInvest redeemerCreatedAt redeemerDeadline ctx = do
    let
        ((_, _),(_, outputUserStateDatum)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

        --txInfoValidRange info

        newOutputDatum = mkUserState  redeemerUser redeemerUserNFT redeemerInvest  redeemerCreatedAt redeemerDeadline 0 0 Nothing

    newOutputDatum == UserState outputUserStateDatum


{- Check if the Output Value with UserState Datum is the same that in redeemerInvest, witch is at the same time the usInvest in the UserState Datum, cheked before. -}
{-# INLINABLE correctDoubleOwnOutputUserState_Value_OfNewUser #-}
correctDoubleOwnOutputUserState_Value_OfNewUser ::  Invest -> ScriptContext -> Bool
correctDoubleOwnOutputUserState_Value_OfNewUser redeemerInvest ctx = do
    let
        ((_, _),(txtout, _)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

        valueTotal = Ada.lovelaceValueOf redeemerInvest

    valueTotal == txOutValue txtout   


{- 
Check if the PoolState Datum produced is correct, need to be the same that the sum of inputs PoolState
Inside is going to fail if the outputs at the script aren't just two, one with the PoolState Datum and the other with the UserStateDatum.
-}
{-# INLINABLE correctDoubleOwnOutputsPoolState_Datum_WithNoChanges #-}
correctDoubleOwnOutputsPoolState_Datum_WithNoChanges ::  PoolNFT -> ScriptContext -> Bool
correctDoubleOwnOutputsPoolState_Datum_WithNoChanges rugrPoolNFT  ctx = do
    let
        ((_, outputPoolStateDatum),(_, _)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx
 
        inputsTxOutWithPoolStateDatums = getInputPoolStateDatums ctx
        inputPoolStateDatumss = snd <$> inputsTxOutWithPoolStateDatums

        newOutputDatum = mkPoolStateFromPoolStateList inputPoolStateDatumss rugrPoolNFT 

    newOutputDatum == PoolState outputPoolStateDatum

{-
    Check if the UserState Datum produced is correct, with the new claim
    TODO: 
    Inside is going to fail if the outputs at the script aren't just two, one with the PoolState Datum and the other with the UserStateDatum.
-}



{-# INLINABLE correctDoubleOwnOutputsUserState_Datum_WithNewClaimRewards #-}
correctDoubleOwnOutputsUserState_Datum_WithNewClaimRewards :: User -> UserNFT -> Proffit ->  POSIXTime   -> ScriptContext -> Bool
correctDoubleOwnOutputsUserState_Datum_WithNewClaimRewards redeemerUser redeemerUserNFT rugrClaim rugrClaimAt ctx = do
    let
        ((_, _),(_, outputUserStateDatum)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

        inputTxOutWithUserStateDatums = getInputUserStateDatums ctx
        inputUserStateDatums = snd <$> inputTxOutWithUserStateDatums

    case inputUserStateDatums of
        [inputUserStateDatum] -> do
                let 

                    rewards = getRewardsPerInvest (usLastClaimAt inputUserStateDatum) rugrClaimAt  (usCreatedAt  inputUserStateDatum )  (usInvest inputUserStateDatum ) 
                    totalNewRewards = rewards  + usRewardsNotClaimed inputUserStateDatum
                    rewardsNotClaimed = totalNewRewards - rugrClaim
                    totalRewardsCashedOut = usChashedOut inputUserStateDatum + rugrClaim 

                    newOutputDatum = mkUserState redeemerUser redeemerUserNFT 
                                    (usInvest inputUserStateDatum ) 
                                    (usCreatedAt  inputUserStateDatum ) 
                                    (usDeadline  inputUserStateDatum )  
                                    totalRewardsCashedOut
                                    rewardsNotClaimed
                                    (Just rugrClaimAt)

                newOutputDatum == UserState outputUserStateDatum
        _ -> traceError "ERROR in correctDoubleOwnOutputsUserState_Datum_WithNewClaimRewards: Not one single UserState Input"

   

{-Check if the Output Value with PoolState Datum is the same that in the Inputs with PoolSate Datum less the rewards claimed -}
{-# INLINABLE correctDoubleOwnOutputPoolState_Value_LessClaimRewards #-}
correctDoubleOwnOutputPoolState_Value_LessClaimRewards ::  Proffit -> ScriptContext -> Bool
correctDoubleOwnOutputPoolState_Value_LessClaimRewards rugrClaim ctx = do
    let
        ((txtout, _),(_, _)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

        inputsTxOutWithPoolStateDatums = getInputPoolStateDatums ctx
        inputsTxOut = fst <$> inputsTxOutWithPoolStateDatums

        listValuesEnInputsUtxos = [ txOutValue inputTxOut | inputTxOut <- inputsTxOut] 

        valueTotal = HASKELL.foldl (<>)  (Ada.lovelaceValueOf 0) listValuesEnInputsUtxos

         

    valueTotal == txOutValue txtout <> Ada.lovelaceValueOf rugrClaim


    

{-Check if the Output Value with UserState Datum is the same that the input UserState. -}
{-# INLINABLE correctDoubleOwnOutputUserState_Value_WithNoChanges #-}
correctDoubleOwnOutputUserState_Value_WithNoChanges ::  ScriptContext -> Bool
correctDoubleOwnOutputUserState_Value_WithNoChanges ctx = do
    let
        ((_, _),(txtout, _)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

        inputTxOutWithUserStateDatums = getInputUserStateDatums ctx
        inputsTxOut = fst <$> inputTxOutWithUserStateDatums

        listValuesEnInputsUtxos = [ txOutValue inputTxOut | inputTxOut <- inputsTxOut] 

        valueTotal = PlutusTx.Prelude.foldl (<>)  ( Ada.lovelaceValueOf 0) listValuesEnInputsUtxos

    valueTotal == txOutValue txtout


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


{- Check if this the claim is correct. -}
{-# INLINABLE correctClaimValue #-}
correctClaimValue :: PoolParams ->  Proffit ->  POSIXTime   -> ScriptContext -> Bool
correctClaimValue pParams rugrClaim rugrClaimAt ctx = do
    let
        ((_, _),(_, outputUserStateDatum)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

        inputTxOutWithUserStateDatums = getInputUserStateDatums ctx
        inputUserStateDatums = snd <$> inputTxOutWithUserStateDatums

    case inputUserStateDatums of
        [inputUserStateDatum] -> do
                let 
                    rewards = getRewardsPerInvest (usLastClaimAt inputUserStateDatum) rugrClaimAt (usCreatedAt  inputUserStateDatum )  (usInvest inputUserStateDatum ) 
                    totalNewRewards = rewards  + usRewardsNotClaimed inputUserStateDatum

                rugrClaim >= ppMinimunClaim pParams && rugrClaim <= totalNewRewards


-- {-| If the amount to claim is greater or equal than the minimum amount setted,
--     then, this function computes the new UserState by updating his last claim
--     and, the total amount of rewards to be claimed.

--     Other way it returns Nothing.
-- -}
-- {-# INLINABLE claim #-}
-- claim :: UserState -> POSIXTime -> OperationSettings -> Maybe (UserState, Integer)
-- claim userState cTime fconf =
--     if rewards < minClaim fconf
--     then Nothing
--     else Just (newUserState, rewards)
--   where
--     newUserState :: UserState
--     newUserState = userState { lastClaim = Just cTime}

--     rewards :: Integer
--     rewards = computeRewards (deposits userState) (lastClaim userState) cTime



-- claimRes :: Maybe (UserState, Integer)
-- claimRes = Business.claim inputUserState time (opSettings $ settings staking)

-- {-# INLINABLE computeRewards #-}
-- computeRewards
--     :: [Deposit]
--     -> Maybe POSIXTime
--     -> POSIXTime
--     -> Integer
-- computeRewards deposits lastClaim now =
--     foldr getRewards 0 deposits
--   where
--     getRewards :: Deposit -> Integer -> Integer
--     getRewards dep rews = rews + rewardsPerDeposit lastClaim now dep



