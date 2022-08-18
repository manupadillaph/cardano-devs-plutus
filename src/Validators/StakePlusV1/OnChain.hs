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

module Validators.StakePlusV1.OnChain
    ( 
        codeValidator,
        typedValidator,
        hashValidator,
        addressValidator
    ) where

--Import Externos

import qualified Ledger.Ada                          as LedgerAda
import qualified Plutus.Script.Utils.V1.Typed.Scripts          as UtilsTypedScriptsV1
import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators as UtilsTypedScriptsValidatorsV1
import qualified Plutus.V1.Ledger.Address            as LedgerAddressV1
import qualified Plutus.V1.Ledger.Api                as LedgerApiV1
import qualified Plutus.V1.Ledger.Contexts           as LedgerContextsV1 (ScriptContext, TxInfo, scriptContextTxInfo) --txSignedBy, getContinuingOutputs, findDatum
import qualified Plutus.V1.Ledger.Scripts            as LedgerScriptsV1
import qualified Plutus.V1.Ledger.Value              as LedgerValueV1
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P

--Import Internos

import qualified Validators.StakePlusV1.Helpers        as Helpers
import qualified Validators.StakePlusV1.OnChainHelpers as OnChainHelpers
import qualified Validators.StakePlusV1.Typos          as T

-- Modulo:

data ValidatorScriptV1
instance UtilsTypedScriptsValidatorsV1.ValidatorTypes ValidatorScriptV1 where
    type instance RedeemerType ValidatorScriptV1 = T.ValidatorRedeemer
    type instance DatumType ValidatorScriptV1 = T.ValidatorDatum


{-# INLINABLE mkValidator #-}
mkValidator :: T.PoolParams -> T.ValidatorDatum -> T.ValidatorRedeemer -> LedgerContextsV1.ScriptContext -> Bool

mkValidator pParams (T.PoolState dPoolState) (T.RedeemMasterFundPool redeemMasterFundPool) ctx =
    validateMasterFundPool pParams dPoolState redeemMasterFundPool ctx

mkValidator pParams (T.PoolState dPoolState) (T.RedeemMasterFundAndMergePool redeemMasterFundAndMergePool) ctx =
    validateMasterFundAndMergePool pParams dPoolState redeemMasterFundAndMergePool ctx

mkValidator pParams (T.PoolState dPoolState) (T.RedeemMasterGetPool redeemMasterGetPool) ctx =
    validateMasterGetPool pParams dPoolState redeemMasterGetPool ctx

mkValidator pParams (T.PoolState dPoolState)  (T.RedeemUserInvest redeemUserInvest) ctx =
    validateUserInvest pParams dPoolState redeemUserInvest ctx

mkValidator pParams (T.UserState dUserState) (T.RedeemUserGetInvest redeemUserGetInvest) ctx =
    validateUserGetInvest pParams dUserState redeemUserGetInvest ctx

mkValidator pParams (T.UserState dUserState)  (T.RedeemUserGetRewards redeemUserGetRewards) ctx =
    validateUserGetRewards pParams (T.UserState dUserState) redeemUserGetRewards ctx

mkValidator pParams (T.PoolState dPoolState) (T.RedeemUserGetRewards redeemUserGetRewards) ctx =
    validateUserGetRewards pParams (T.PoolState dPoolState) redeemUserGetRewards ctx

mkValidator pParams (T.UserState dUserState) (T.RedeemUserInvestRewards redeemUserInvestRewards) ctx =
    validateUserInvestRewards pParams dUserState redeemUserInvestRewards ctx

mkValidator _ (T.PoolState _) _ _ =
    traceIfFalse "Stake Pool Message: Wrong Redeemer For PoolState utxo" False 

mkValidator _ (T.UserState _) _ _ =
    traceIfFalse "Stake Pool Message: Wrong Redeemer For UserState utxo" False 

mkValidator _ _ _ _ =
     traceIfFalse "Stake Pool Message: Invalid Operationddd" False 


{-# INLINABLE validateMasterFundPool #-}
validateMasterFundPool :: T.PoolParams -> T.PoolStateTypo -> T.RedeemMasterFundPoolTypo -> LedgerContextsV1.ScriptContext -> Bool
validateMasterFundPool pParams dPoolStateFromInputBeingValidated T.RedeemMasterFundPoolTypo{..}  ctx  =
    -- TODO: 

    -- En esta nueva version, voy a tener de entrada una utxo con PoolState datum, representado con dPoolStateFromInputBeingValidated y en el valor del redeemer
    -- Eso es para obligar a que la accion de fund pase por el validador
    -- En la salida tengo que tener esa misma entrada, con el mismo datum y value, recreados, y la nueva salida con el Fund
    -- o sea, tengo dos salidas con PoolState, la igual y la nueva

    traceIfFalse "Validate Master Fund Pool Message: Wrong Input, not matching redeemer rmfpUsingUtxo" correctInputs && 

    traceIfFalse "Validate Master Fund Pool Message: Wrong Outpus" correctOutputs && 

    -- Verificar que solo modifique el valor del inversor que esta firmando esta transacion, o sea que no pueda moficiar el valor de otro inversor
    -- traceIfFalse "Validate Master Fund Pool Message: Can't change other Master's Fund" (changingOnlyMyselfFund redeemerMaster ctx) &&

    -- Check if the redeemerPoolNFT was minted with the right minting policy.
    traceIfFalse "Validate Master Fund Pool Message: Wrong NFT Minting Policy " (OnChainHelpers.correctMintigPolicyUsedForNFT pParams rmfpPoolNFT  ) &&
    
    -- Check if the redeemerPoolNFT is included in the value of the Own's Single Output comming to the script. 
    -- The redeemerPoolNFT is going to be holded in the script until the Pool is alive.
    -- Esta verificacion es importante por que me asegura que haya al menos un PoolState que contiene todas los Fund, es un conteo.
    -- Esto se neceseita para recuperar la inversion. Alli se verificará que se usen todas las utxo con PoolState, segun el contador de utxoFunds

    traceIfFalse "Validate Master Fund Pool Message: Redeemer Pool NFT not found in Own's Output Value" (isPoolNFTInOldOutputWithPoolState rmfpPoolNFT ctx) &&

    -- Check if the redeemerPoolNFT is the same that identifies the Pool in the PoolParams.
    traceIfFalse "Validate Master Fund Pool Message: Pool Param's NFT dosen't matches Redeemer Pool NFT" (OnChainHelpers.isPoolNFTParam pParams rmfpPoolNFT ) &&
    -- Check if the redeemerPoolNFT is the same in all the inputs PoolState Datums
    traceIfFalse "Validate Master Fund Pool Message: Input PoolState Datum's NFTs dosen't matches Redeemer Pool NFT" (OnChainHelpers.isPoolNFTDatum rmfpPoolNFT ctx ) &&

    -- Check if this tx was signed by any of the Masters included in the PoolParams. 
    -- This are all the Masters that can interact with the script at any time. Nobody else will be able to change or redeem the funds.
    traceIfFalse "Validate Master Fund Pool Message: Pool Params Master's signature missing" (OnChainHelpers.signedByPoolParamMaster pParams info) &&
    -- Check if this tx was signed by any of the Masters included in the PoolState Datum produced by this tx. 
    -- This datum includes all the Masters already interacted with the script. Are all the ones who funded the Pool.
    traceIfFalse "Validate Master Fund Pool Message: Output PoolState Datum Master's signature missing" (signedByNewOutputPoolStateMasterFunder info ctx) &&
    -- Check if this tx was signed by the Master specified in the redeemer.
    traceIfFalse "Validate Master Fund Pool Message: Redeemer Master's signature missing" (OnChainHelpers.signedByMaster rmfpMaster info) &&


    traceIfFalse "Validate Master Fund Pool Message: Output NEW PoolState Datum wrong, not matching provided redeemer" (correctNewOutputPoolState_Datum rmfpPoolNFT rmfpMaster rmfpFund  ctx)   &&

    traceIfFalse "Validate Master Fund Pool Message: Output Value Error, not matching PoolState Datums Funds" (correctOutputsPoolState_Datum_Values ctx) && 

    -- Check that the tx range interval of validity. Can't be infinitum and need to be lees than ppValidTimeRange Pool Param . 
    traceIfFalse "Validate Master Fund Pool Message: Can't Fund Pool, tx validity time range is not valid" (OnChainHelpers.isValidRange pParams info) &&

    -- Check that the Pool Deadline is not reached. That means that the Pool is still alive. 
    traceIfFalse "Validate Master Fund Pool Message: Can't Fund Pool, Deadline already passed" (OnChainHelpers.deadlinePoolParamNotReached pParams info)
    
  where 

    info :: LedgerContextsV1.TxInfo
    info = LedgerContextsV1.scriptContextTxInfo ctx

    -- En esta nueva version, voy a tener de entrada una utxo con PoolState datum, representado con dPoolStateFromInputBeingValidated y en el valor del redeemer rmfpUsingUtxo
    -- Eso es para obligar a que la accion de fund pase por el validador
    -- En la salida tengo que tener esa misma entrada, con el mismo datum y value, recreados, y la nueva salida con el Fund

    inputBeingConsumed :: LedgerApiV1.TxOutRef
    inputBeingConsumed = rmfpUsingUtxo

    inputWithPoolState :: (LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, T.PoolStateTypo)
    inputWithPoolState =  do
        let 
            inputsWithPoolState = OnChainHelpers.getInputsWithPoolState ctx

        case inputsWithPoolState of 
            [inputWithPoolState] -> 
                if inputBeingConsumed == OnChainHelpers.getInput_TxOutRef inputWithPoolState && dPoolStateFromInputBeingValidated == OnChainHelpers.getInput_Datum inputWithPoolState then
                    inputWithPoolState
                else
                    traceError "Validate Master Fund Pool Message: Error. Found one Input with PoolState Datum but it is different to the redeemer"   

            _ -> traceError "Validate Master Fund Pool Message: Error. Found more than one Input with PoolState Datum" 

    inputWithPoolStateAndPoolNFT :: (LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, T.PoolStateTypo)
    inputWithPoolStateAndPoolNFT =  case OnChainHelpers.getInputWithPoolStateAndPoolNFT [inputWithPoolState] rmfpPoolNFT of
        Just inputWithPoolStateAndPoolNFT -> inputWithPoolStateAndPoolNFT
        Nothing -> traceError "Validate Master Fund And Merge Pool Message: Can't find Pool NFT on Inputs."  

    {- | Gets, if there is only one, the output with same PoolState Datum and Value than the input PoolState Datum plus 1 in the count of utxo with Pool State. -} 
    getOutputPoolStateDatumSameDatumAndValueThanInputPlus1CountUxto :: T.PoolNFT -> T.PoolStateTypo -> LedgerContextsV1.ScriptContext -> (LedgerApiV1.TxOut, T.PoolStateTypo)
    getOutputPoolStateDatumSameDatumAndValueThanInputPlus1CountUxto redeemerPoolNFT datumInputPoolState ctx = do
        let
            oldPoolState = Helpers.mkPoolStateWithNewCountFundsFromPoolState datumInputPoolState redeemerPoolNFT

            outputsWithPoolState = OnChainHelpers.getOutputsWithPoolState ctx

            oldTxOuts =  [(outputWithPoolState_TxOut, outputWithPoolState_Datum) | (outputWithPoolState_TxOut, outputWithPoolState_Datum) <- outputsWithPoolState , T.PoolState outputWithPoolState_Datum == oldPoolState]

            oldTxOutsWithSameValue =  [(outputWithPoolState_TxOut, outputWithPoolState_Datum) | (outputWithPoolState_TxOut, outputWithPoolState_Datum) <- outputsWithPoolState , LedgerApiV1.txOutValue outputWithPoolState_TxOut ==  (LedgerApiV1.txOutValue $ OnChainHelpers.getInput_TxOut inputWithPoolState) ]

        case oldTxOutsWithSameValue of
            [oldTxOutWithSameValue] -> oldTxOutWithSameValue
            _ -> traceError "Validate Master Fund Pool Message: Expected exactly one output with the same PoolState Datum and value than the input."
        
    getOutputPoolStateDatumDiffThan :: (LedgerApiV1.TxOut, T.PoolStateTypo) -> LedgerContextsV1.ScriptContext -> (LedgerApiV1.TxOut, T.PoolStateTypo)
    getOutputPoolStateDatumDiffThan outputSameThanInput ctx = do
        let
            outputsWithPoolState = OnChainHelpers.getOutputsWithPoolState ctx

            newtxOuts =  [txOutAndPoolState | txOutAndPoolState <- outputsWithPoolState ,txOutAndPoolState /= outputSameThanInput]
        
        case newtxOuts of
            [newtxOut] -> newtxOut
            _ -> traceError "Validate Master Fund Pool Message: Expected exactly one new output with PoolState Datum."
    

    oldOutputWithPoolState ::  (LedgerApiV1.TxOut, T.PoolStateTypo)
    oldOutputWithPoolState =  getOutputPoolStateDatumSameDatumAndValueThanInputPlus1CountUxto rmfpPoolNFT dPoolStateFromInputBeingValidated ctx

    newOutputWithPoolState ::  (LedgerApiV1.TxOut, T.PoolStateTypo)
    newOutputWithPoolState  =  getOutputPoolStateDatumDiffThan oldOutputWithPoolState  ctx

    outputsWithPoolState ::  [(LedgerApiV1.TxOut, T.PoolStateTypo)]
    outputsWithPoolState  =  [oldOutputWithPoolState ,newOutputWithPoolState ]

    correctInputs :: Bool
    correctInputs = do
        case inputWithPoolState of 
            (_, _, _) -> True
            _ -> False    

    correctOutputs :: Bool
    correctOutputs = do
        case outputsWithPoolState of 
            (_:_) -> True
            _ -> False   

    isPoolNFTInOldOutputWithPoolState :: T.PoolNFT -> LedgerContextsV1.ScriptContext -> Bool
    isPoolNFTInOldOutputWithPoolState  redeemerPoolNFT ctx  = do
        OnChainHelpers.isPoolNFTInSomeOutputWithPoolState [oldOutputWithPoolState] redeemerPoolNFT ctx

    signedByNewOutputPoolStateMasterFunder :: LedgerContextsV1.TxInfo -> LedgerContextsV1.ScriptContext-> Bool
    signedByNewOutputPoolStateMasterFunder info ctx  = do
        
        let 
            (_, outputWithPoolState_Datum) = newOutputWithPoolState

        OnChainHelpers.signedByPoolStateMasterFunder outputWithPoolState_Datum info


    correctNewOutputPoolState_Datum :: T.PoolNFT -> T.Master -> T.Fund -> LedgerContextsV1.ScriptContext -> Bool
    correctNewOutputPoolState_Datum  redeemerPoolNFT redeemerMaster redeemerFund ctx = do
        let

            (_, outputWithPoolState_Datum) = newOutputWithPoolState

            countTotalUtxoWithPoolState = 0

            newOutputDatum = Helpers.mkPoolStateWithNewFundFromPoolStateList [] redeemerPoolNFT redeemerMaster redeemerFund countTotalUtxoWithPoolState

        newOutputDatum == T.PoolState outputWithPoolState_Datum
        

    correctOldOutputPoolState_Datum :: T.PoolNFT -> Bool
    correctOldOutputPoolState_Datum  rmfpPoolNFT  = do
        let

            (_, outputWithPoolState_Datum) = oldOutputWithPoolState

            inputWithPoolStateAndPoolNFT_Datum = OnChainHelpers.getInput_Datum inputWithPoolStateAndPoolNFT

            newOutputDatum = Helpers.mkPoolStateWithNewCountFundsFromPoolState inputWithPoolStateAndPoolNFT_Datum rmfpPoolNFT     

        newOutputDatum == T.PoolState outputWithPoolState_Datum
        

    correctOutputPoolState_Datum_Value ::  (LedgerApiV1.TxOut, T.PoolStateTypo) -> Bool
    correctOutputPoolState_Datum_Value   (outputWithPoolState_TxOut, outputWithPoolState_Datum)  = do    
        let  
            
            masterFunders = T.psMasterFunders outputWithPoolState_Datum 

            sumFunds = sum $ T.mfFund <$> masterFunders

            valueFund  = LedgerAda.lovelaceValueOf sumFunds <> negate (LedgerAda.lovelaceValueOf $ T.psChashedOut outputWithPoolState_Datum)

        LedgerAda.fromValue valueFund == LedgerAda.fromValue (LedgerApiV1.txOutValue outputWithPoolState_TxOut)

    correctOutputsPoolState_Datum_Values :: LedgerContextsV1.ScriptContext -> Bool
    correctOutputsPoolState_Datum_Values   ctx  = do
        all  correctOutputPoolState_Datum_Value outputsWithPoolState
            

{-# INLINABLE validateMasterFundAndMergePool #-}
validateMasterFundAndMergePool :: T.PoolParams -> T.PoolStateTypo -> T.RedeemMasterFundAndMergePoolTypo -> LedgerContextsV1.ScriptContext -> Bool
validateMasterFundAndMergePool pParams dPoolStateFromInputBeingValidated T.RedeemMasterFundAndMergePoolTypo{..}  ctx  =

    -- TODO: 
    -- -- Verificar que solo modifique el valor del inversor que esta firmando esta transacion, o sea que no pueda moficiar el valor de otro inversor
    -- -- traceIfFalse "Validate Master Fund Pool Message: Can't change other Master's Fund" (changingOnlyMyselfFund redeemerMaster ctx) &&
    -- En esta nueva version, puedo querer unir varios PoolState Datums en uno solo.
    -- Tendrá entocnes muchas entradas de OLD Pool State Datums y como salida tendrá una Sola con el Datum que une a todos esos y suma el nuevo Fund

    traceIfFalse "Validate Master Fund And Merge Pool Message: Wrong Input" correctInputs &&

    traceIfFalse "Validate Master Fund And Merge Pool Message: Wrong Outpus" correctOutputs && 

    -- Verificar que solo modifique el valor del inversor que esta firmando esta transacion, o sea que no pueda moficiar el valor de otro inversor
    -- traceIfFalse "Validate Master Fund Pool Message: Can't change other Master's Fund" (changingOnlyMyselfFund redeemerMaster ctx) &&

    -- Check if the redeemerPoolNFT was minted with the right minting policy.
    traceIfFalse "Validate Master Fund And Merge Pool Message: Wrong NFT Minting Policy " (OnChainHelpers.correctMintigPolicyUsedForNFT pParams rmfampPoolNFT  ) &&

    -- Check if the redeemerPoolNFT is included in the value of the Own's Single Output comming to the script. 
    -- The redeemerPoolNFT is going to be holded in the script until the Pool is alive.
    -- Es necesario que este la PoolState que tenía el NFT por que ese es el unico que estará siempre actualizado con la cantidad de utxo con PoolState Datum

    traceIfFalse "Validate Master Fund Pool Message: Redeemer Pool NFT not found in Own's Output Value" (isPoolNFTInSomeOutputWithPoolState rmfampPoolNFT ctx) &&

    -- Check if the redeemerPoolNFT is the same that identifies the Pool in the PoolParams.
    traceIfFalse "Validate Master Fund And Merge Pool Message: Pool Param's NFT dosen't matches Redeemer Pool NFT" (OnChainHelpers.isPoolNFTParam pParams rmfampPoolNFT ) &&
    -- Check if the redeemerPoolNFT is the same in all the inputs PoolState Datums
    traceIfFalse "Validate Master Fund And Merge Pool Message: Input PoolState Datum's NFTs dosen't matches Redeemer Pool NFT" (OnChainHelpers.isPoolNFTDatum rmfampPoolNFT ctx ) &&

    -- Check if this tx was signed by any of the Masters included in the PoolParams. 
    -- This are all the Masters that can interact with the script at any time. Nobody else will be able to change or redeem the funds.
    traceIfFalse "Validate Master Fund And Merge Pool Message: Pool Params Master's signature missing" (OnChainHelpers.signedByPoolParamMaster pParams info) &&
    -- Check if this tx was signed by any of the Masters included in the PoolState Datum produced by this tx. 
    -- This datum includes all the Masters already interacted with the script. Are all the ones who funded the Pool.
    traceIfFalse "Validate Master Fund And Merge Pool Message: Output PoolState Datum Master's signature missing" (signedByOutputPoolStateMasterFunder info ctx) &&
    -- Check if this tx was signed by the Master specified in the redeemer.
    traceIfFalse "Validate Master Fund And Merge Pool Message: Redeemer Master's signature missing" (OnChainHelpers.signedByMaster rmfampMaster info) &&

    traceIfFalse "Validate Master Fund And Merge Pool Message: Output NEW PoolState Datum wrong, not matching provided redeemer" (correctOutputPoolState_Datum rmfampPoolNFT rmfampMaster rmfampFund  ctx)   &&

    traceIfFalse "Validate Master Fund And Merge Pool Message: Output Value Error, not matching PoolState Datums Funds" (correctOutputsPoolState_Datum_Values ctx) && 

    -- Check that the tx range interval of validity. Can't be infinitum and need to be lees than ppValidTimeRange Pool Param . 
    traceIfFalse "Validate Master Fund And Merge Pool Message: Can't Fund Pool, tx validity time range is not valid" (OnChainHelpers.isValidRange pParams info) &&

    -- Check that the Pool Deadline is not reached. That means that the Pool is still alive. 
    traceIfFalse "Validate Master Fund And Merge Pool Message: Can't Fund Pool, Deadline already passed" (OnChainHelpers.deadlinePoolParamNotReached pParams info)
    
  where 

    info :: LedgerContextsV1.TxInfo
    info = LedgerContextsV1.scriptContextTxInfo ctx

    inputsBeingConsumed = rmfampUtxoToMerge 

    inputsWithPoolState :: [(LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, T.PoolStateTypo)]
    inputsWithPoolState =  do
        let 
            inputsWithPoolState = OnChainHelpers.getInputsWithPoolState ctx
            inputsWithPoolState_UtxoRef = OnChainHelpers.getInput_TxOutRef <$> inputsWithPoolState

        if all (==True) [ input `elem` inputsWithPoolState_UtxoRef  | input <- inputsBeingConsumed] &&
         all (==True) [ input `elem` inputsBeingConsumed  | input <- inputsWithPoolState_UtxoRef] then
            inputsWithPoolState
        else
            traceError "Validate Master Fund And Merge Pool Message: Not Matching redeemer UtxoRef to merge with inputs."  

    inputWithPoolStateAndPoolNFT :: (LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, T.PoolStateTypo)
    inputWithPoolStateAndPoolNFT =  case OnChainHelpers.getInputWithPoolStateAndPoolNFT inputsWithPoolState rmfampPoolNFT of
        Just inputWithPoolStateAndPoolNFT -> inputWithPoolStateAndPoolNFT
        Nothing -> traceError "Validate Master Fund: Can't find Pool NFT on Inputs."  

    outputWithPoolState :: (LedgerApiV1.TxOut, T.PoolStateTypo)
    outputWithPoolState =  do
        case OnChainHelpers.getOutputsWithPoolState  ctx of
            [(x,y)] -> (x,y)
            _ -> traceError "Validate Master Fund: Expected exactly one output with the PoolState Datum."  

    correctInputs :: Bool
    correctInputs = do
        case inputsWithPoolState of 
            x -> True
            _ -> False
 
    correctOutputs :: Bool
    correctOutputs = do
        case outputWithPoolState of 
            x -> True
            _ -> False
    
    isPoolNFTInSomeOutputWithPoolState :: T.PoolNFT -> LedgerContextsV1.ScriptContext -> Bool
    isPoolNFTInSomeOutputWithPoolState  redeemerPoolNFT ctx  = do
        OnChainHelpers.isPoolNFTInSomeOutputWithPoolState [outputWithPoolState] redeemerPoolNFT ctx

    signedByOutputPoolStateMasterFunder :: LedgerContextsV1.TxInfo -> LedgerContextsV1.ScriptContext-> Bool
    signedByOutputPoolStateMasterFunder info ctx  = do
        
        let 
            (_, outputWithPoolState_Datum) = outputWithPoolState

        OnChainHelpers.signedByPoolStateMasterFunder outputWithPoolState_Datum info

    correctOutputPoolState_Datum :: T.PoolNFT -> T.Master -> T.Fund -> LedgerContextsV1.ScriptContext -> Bool
    correctOutputPoolState_Datum  redeemerPoolNFT redeemerMaster redeemerFund ctx = do
        let
            (_, outputWithPoolState_Datum) = outputWithPoolState

            inputWithPoolStateAndPoolNFT_Datum = OnChainHelpers.getInput_Datum inputWithPoolStateAndPoolNFT

            inputsWithPoolState_Datums = OnChainHelpers.getInput_Datum <$> inputsWithPoolState

            countTotalUtxoWithPoolState = T.psCountTotalUtxoWithPoolState inputWithPoolStateAndPoolNFT_Datum - length inputsWithPoolState + 1 

            newOutputDatum = Helpers.mkPoolStateWithNewFundFromPoolStateList inputsWithPoolState_Datums redeemerPoolNFT redeemerMaster redeemerFund countTotalUtxoWithPoolState

        newOutputDatum == T.PoolState outputWithPoolState_Datum

    correctOutputPoolState_Datum_Value ::  (LedgerApiV1.TxOut, T.PoolStateTypo) -> Bool
    correctOutputPoolState_Datum_Value   (outputWithPoolState_TxOut, outputWithPoolState_Datum)  = do    
        let  
            
            masterFunders = T.psMasterFunders outputWithPoolState_Datum 

            sumFunds = sum $ T.mfFund <$> masterFunders

            valueFund  = LedgerAda.lovelaceValueOf sumFunds <> negate (LedgerAda.lovelaceValueOf $ T.psChashedOut outputWithPoolState_Datum)

        LedgerAda.fromValue valueFund == LedgerAda.fromValue (LedgerApiV1.txOutValue outputWithPoolState_TxOut)

    correctOutputsPoolState_Datum_Values :: LedgerContextsV1.ScriptContext -> Bool
    correctOutputsPoolState_Datum_Values   ctx  = do
        --correctOutputPoolState_Datum_Value outputWithPoolState
        True


{-# INLINABLE validateMasterGetPool #-}
validateMasterGetPool :: T.PoolParams -> T.PoolStateTypo -> T.RedeemMasterGetPoolTypo -> LedgerContextsV1.ScriptContext -> Bool
validateMasterGetPool pParams dPoolStateFromInputBeingValidated T.RedeemMasterGetPoolTypo{..}  ctx  =
    True
--     -- TODO: 

--     -- tiene como entrada varias PoolState y de salida una sola Pool State con la suma de todos los funds y el VALUE tambien la suma menos lo recuperado

--     traceIfFalse "Validate Master Get Fund Pool Message: Wrong Input" correctInputs &&

--     traceIfFalse "Validate Master Get Fund Pool Message: Wrong Outpus" correctOutputs && 

--     -- Verificar que solo modifique el valor del inversor que esta firmando esta transacion, o sea que no pueda moficiar el valor de otro inversor
--     -- traceIfFalse "Validate Master Fund Pool Message: Can't change other Master's Fund" (changingOnlyMyselfFund redeemerMaster ctx) &&

--     -- Check if the redeemerPoolNFT was minted with the right minting policy.
--     traceIfFalse "Validate Master Get Fund Pool Message: Wrong NFT Minting Policy " (OnChainHelpers.correctMintigPolicyUsedForNFT pParams rmgpPoolNFT  ) &&

--     -- Check if the redeemerPoolNFT is included in the value of the Own's Single Output comming to the script. 
--     -- The redeemerPoolNFT is going to be holded in the script until the Pool is alive.
--     -- es importante que este el NFT para asegurarme que tengo la version mas actualizada de la cantidad de funds
--     -- ademas luego tendre que verificar que esten todos los utxo aqui antes de hacer el get y unirlos
--     traceIfFalse "Validate Master Fund Pool Message: Redeemer Pool NFT not found in Own's Output Value" (isPoolNFTInSomeOutputWithPoolState rmgpPoolNFT ctx) &&

--     -- Check if the redeemerPoolNFT is the same that identifies the Pool in the PoolParams.
--     traceIfFalse "Validate Master Get Fund Pool Message: Pool Param's NFT dosen't matches Redeemer Pool NFT" (OnChainHelpers.isPoolNFTParam pParams rmgpPoolNFT ) &&
--     -- Check if the redeemerPoolNFT is the same in all the inputs PoolState Datums
--     traceIfFalse "Validate Master Get Fund Pool Message: Input PoolState Datum's NFTs dosen't matches Redeemer Pool NFT" (OnChainHelpers.isPoolNFTDatum rmgpPoolNFT ctx ) &&

--     -- Check if this tx was signed by any of the Masters included in the PoolParams. 
--     -- This are all the Masters that can interact with the script at any time. Nobody else will be able to change or redeem the funds.
--     traceIfFalse "Validate Master Get Fund Pool Message: Pool Params Master's signature missing" (OnChainHelpers.signedByPoolParamMaster pParams info) &&
--     -- Check if this tx was signed by any of the Masters included in the PoolState Datum produced by this tx. 
--     -- This datum includes all the Masters already interacted with the script. Are all the ones who funded the Pool.
--     traceIfFalse "Validate Master Get Fund Pool Message: Output PoolState Datum Master's signature missing" (signedByOutputPoolStateMasterFunder info ctx) &&
--     -- Check if this tx was signed by the Master specified in the redeemer.
--     traceIfFalse "Validate Master Get Fund Pool Message: Redeemer Master's signature missing" (OnChainHelpers.signedByMaster rmgpMaster info) &&

--     traceIfFalse "Validate Master Get Fund Pool Message: Output PoolState Datum wrong, not matching provided redeemer" (correctOutputPoolState_Datum rmgpPoolNFT rmgpMaster rmgpGetFund  ctx)   &&

--     traceIfFalse "Validate Master Get Fund Pool Message: Output Value Error, not matching PoolState Datums Funds" (correctOutputsPoolState_Datum_Values ctx) && 

--     -- Check that the tx range interval of validity. Can't be infinitum and need to be lees than ppValidTimeRange Pool Param . 
--     traceIfFalse "Validate Master Get Fund Pool Message: Can't Fund Pool, tx validity time range is not valid" (OnChainHelpers.isValidRange pParams info) &&

--     -- Check that the Pool Deadline is not reached. That means that the Pool is still alive. 
--     traceIfFalse "Validate Master Get Fund Pool Message: Can't Fund Pool, Deadline already passed" (OnChainHelpers.deadlinePoolParamNotReached pParams info)
    
--   where 

--     info :: LedgerContextsV1.TxInfo
--     info = LedgerContextsV1.scriptContextTxInfo ctx

--     -- En esta nueva version, puedo querer unir varios PoolState Datums en uno solo.
--     -- Tendrá entocnes muchas entradas de OLD Pool State Datums y como salida tendrá una Sola con el Datum que une a todos esos y suma el nuevo Fund

--     inputsBeingConsumed :: [LedgerApiV1.TxOutRef]
--     inputsBeingConsumed = rmgpUtxoToMerge

--     inputsWithPoolState :: [(LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, T.PoolStateTypo)]
--     inputsWithPoolState =  do
--         let 
--             inputsWithPoolState = OnChainHelpers.getInputsWithPoolState ctx
--             inputsWithPoolState_UtxoRef = OnChainHelpers.getInput_TxOutRef <$> inputsWithPoolState

--         if all (==True) [ input `elem` inputsWithPoolState_UtxoRef  | input <- inputsBeingConsumed] &&
--          all (==True) [ input `elem` inputsBeingConsumed  | input <- inputsWithPoolState_UtxoRef] then
--             inputsWithPoolState
--         else
--             traceError "Validate Master Get Fund Pool Message: Not Matching redeemer UtxoRef to merge with inputs."  

--     outputWithPoolState :: (LedgerApiV1.TxOut, T.PoolStateTypo)
--     outputWithPoolState =  do
--         case OnChainHelpers.getOutputsWithPoolState  ctx of
--             [(x,y)] -> (x,y)
--             _ -> traceError "Validate Master Get Fund Pool Message: Expected exactly one output with the PoolState Datum."  

--     correctInputs :: Bool
--     correctInputs = do
--         case inputsWithPoolState of 
--             x -> True
--             _ -> False
 
--     correctOutputs :: Bool
--     correctOutputs = do
--         case outputWithPoolState of 
--             x -> True
--             _ -> False
    
--     isPoolNFTInSomeOutputWithPoolState :: T.PoolNFT -> LedgerContextsV1.ScriptContext -> Bool
--     isPoolNFTInSomeOutputWithPoolState  redeemerPoolNFT ctx  = do
--         OnChainHelpers.isPoolNFTInSomeOutputWithPoolState [outputWithPoolState] redeemerPoolNFT ctx
        
--     signedByOutputPoolStateMasterFunder :: LedgerContextsV1.TxInfo -> LedgerContextsV1.ScriptContext-> Bool
--     signedByOutputPoolStateMasterFunder info ctx  = do
        
--         let 
--             (_, outputWithPoolState_Datum) = outputWithPoolState

--         OnChainHelpers.signedByPoolStateMasterFunder outputWithPoolState_Datum info

--     correctOutputPoolState_Datum :: T.PoolNFT -> T.Master -> T.Fund -> LedgerContextsV1.ScriptContext -> Bool
--     correctOutputPoolState_Datum  redeemerPoolNFT redeemerMaster redeemerFund ctx = do
--         let
--             (_, outputWithPoolState_Datum) = outputWithPoolState

--             inputsWithPoolState_Datums = OnChainHelpers.getInput_Datum <$> inputsWithPoolState

--             newOutputDatum = T.Helpers.mkPoolStateWithNewFundFromPoolStateList inputsWithPoolState_Datums redeemerPoolNFT redeemerMaster redeemerFund

--         newOutputDatum == T.PoolState outputWithPoolState_Datum

--     correctOutputPoolState_Datum_Value ::  (LedgerApiV1.TxOut, T.PoolStateTypo) -> Bool
--     correctOutputPoolState_Datum_Value   (outputWithPoolState_TxOut, outputWithPoolState_Datum)  = do    
--         let  
            
--             masterFunders = T.psMasterFunders outputWithPoolState_Datum 

--             sumFunds = sum $ T.mfFund <$> masterFunders

--             valueFund  = LedgerAda.lovelaceValueOf sumFunds <> negate (LedgerAda.lovelaceValueOf $ T.psChashedOut outputWithPoolState_Datum)

--         LedgerAda.fromValue valueFund == LedgerAda.fromValue (LedgerApiV1.txOutValue outputWithPoolState_TxOut)

--     correctOutputsPoolState_Datum_Values :: LedgerContextsV1.ScriptContext -> Bool
--     correctOutputsPoolState_Datum_Values   ctx  = do

--         correctOutputPoolState_Datum_Value outputWithPoolState
    

{-# INLINABLE validateUserInvest #-}
validateUserInvest :: T.PoolParams -> T.PoolStateTypo -> T.RedeemUserInvestTypo -> LedgerContextsV1.ScriptContext -> Bool
validateUserInvest pParams dPoolStateFromInputBeingValidated T.RedeemUserInvestTypo{..} ctx  =
       
    -- TODO: 

    -- necesito controlar que de entrada tenga un T.PoolState Datum y de salida el mismo T.PoolState Datum mas el usuario agregado y un nuevo UserState Datum

    traceIfFalse "Validate User Invest Message: Wrong Input" correctInputs && 

    traceIfFalse "Validate User Invest Message: Wrong Outpus" correctOutputs && 

    -- Chekear que se agrega el redeemerUserNFT a la lista de userNFT en T.PoolState Datum de salida.
    -- Que la salida con T.PoolState Datum sea unica y tenga el valor sumado de todas las T.PoolState de entrada

    -- Que redeemerInvest sea mayor que la minima inversion en pParams
    -- Que la fecha sea dentro de la deadline del Pool en pParams

    -- que el redeemerUserNFT creado este en la salida que vuelve al usuario y valueOf == 1

    -- Check if the redeemerPoolNFT was minted with the right minting policy.
    traceIfFalse "Validate User Invest Message: Wrong NFT Minting Policy " (OnChainHelpers.correctMintigPolicyUsedForNFT pParams ruiPoolNFT  ) &&
    -- Check que la txtoutRef usada en el redeemerUserNFT este como entrada en el script, para asegurarme que es gastada  
    traceIfFalse "Validate User Invest Message: Redeemer TxOutRef used for NFT not found in Inputs" (OnChainHelpers.hasInputUTxO info ruiUserNFTTxOutRef ) &&
    -- Check if the redeemerUserNFT was minted with the right minting policy
    traceIfFalse "Validate User Invest Message: Wrong NFT Minting Policy " (OnChainHelpers.correctMintigPolicyUsedForNFT pParams ruiUserNFT  ) &&
    -- Check if the redeemerUserNFT was minted with the right quantity, just 1.
    -- TODO: this is not really needed, because the minting policy checks that too and it checked before if this NFT was minted with that specific minting policy.
    traceIfFalse "Validate User Invest Message: User NFT is not valid. Wrong Minted quantity, must be equal 1" (OnChainHelpers.isMintedNFTValid info ruiUserNFT ) &&
    -- Check if the redeemerUserNFT is going to the user wallet. He must keep it in order to claim rewards from the invest. 
    -- He can give it away to other user to seel the benefitis.
    -- TODO: this is not really needed, because all minted value are going to somewhere after all. If the tx was built in incorrects ways is not my problem.
    traceIfFalse "Validate User Invest Message: Redeemer User NFT not found in User's Output Value" (OnChainHelpers.isUserNFTInUserOutputValue ruiUserNFT ruiUser info)  &&
    -- Check if the redeemerPoolNFT is the same that identifies the Pool in the PoolParams.
    traceIfFalse "Validate User Invest Message: Pool Param's NFT dosen't matches Redeemer Pool NFT" (OnChainHelpers.isPoolNFTParam pParams ruiPoolNFT ) &&
    -- Check if the redeemerPoolNFT is the same in all the inputs PoolState Datums
    traceIfFalse "Validate User Invest Message: Input PoolState Datum's NFTs dosen't matches Redeemer Pool NFT" (OnChainHelpers.isPoolNFTDatum ruiPoolNFT ctx ) &&

    -- Check if this tx was signed by the User specified in the redeemerUser.
    traceIfFalse "Validate User Invest Message: Redeemer User's signature missing" (OnChainHelpers.signedByUser ruiUser info) &&
    
    -- Check if this tx was signed by User in the UserState Datum produced by this tx. 
    -- Inside is going to fail if the outputs at the script aren't just two, one with the PoolState Datum and the other with the UserStateDatum.
    traceIfFalse "Validate User Invest Message: Output UserState Datum User's signature missing" (signedByOutputsUserStateUser info ctx) &&

    -- Check if the PoolState Datum produced is correct, with the new User added and all the rest the same than before.
    -- Inside is going to fail if the outputs at the script aren't just two, one with the PoolState Datum and the other with the UserStateDatum.
    traceIfFalse "Validate User Invest Message: Output PoolState Datum wrong" (correctOutputsPoolState_Datum_WithNewUser ruiPoolNFT ruiUserNFT ctx)   &&

    -- Check if the UserState Datum produced is correct, with the right User
    -- Inside is going to fail if the outputs at the script aren't just two, one with the PoolState Datum and the other with the UserStateDatum.
    traceIfFalse "Validate User Invest Message: Output UserState Datum wrong" (correctOutputsUserState_Datum_OfNewUser ruiUser ruiUserNFT ruiInvest ruiCreatedAt ruiDeadline ctx)   &&
    
    -- Check if the Output Value with PoolState Datum is the same that in the Inputs with PoolSate Datum
    traceIfFalse "Validate User Invest Message: PoolState's Output Value Error. Must be the same that in PoolState's Inputs." (correctOutputPoolState_Value_WithSameValue ctx) &&
    
    -- Check if the Output Value with UserState Datum is the same that in redeemerInvest, witch is at the same time the usInvest in the UserState Datum, cheked before.
    traceIfFalse "Validate User Invest Message: UserState's Output Value Error. Must be the same that redeemerInvest." (correctOutputUserState_Value_OfNewUser ruiInvest ctx) &&
    

   
    -- Check in all the outputs if the sum of cashedout plus the value sum equal to the sum of funds inside
    traceIfFalse "Validate User Invest Message: PoolState's Output Datums And Value Error. Not matching sum." (correctOutputsPoolState_ValuesAndDatums ctx) &&
    


    -- Check that the tx range interval of validity. Can't be infinitum and need to be lees than ppValidTimeRange Pool Param . 
    traceIfFalse "Validate User Invest Message: Can't Invest in Pool, tx validity time range is not valid" (OnChainHelpers.isValidRange pParams info) &&

    -- Check if this the invest created at date is correct.
    traceIfFalse "Validate User Invest Message: Invest Created at Date is not Valid" (OnChainHelpers.correctInvestCreatedAt ruiCreatedAt info) &&

    -- Check if this the invest deadline date is correct.
    traceIfFalse "Validate User Invest Message: Invest Deadline Date is not Valid" (OnChainHelpers.correctInvestDeadlineAt pParams ruiCreatedAt ruiDeadline info) &&

    -- Check that the Pool Deadline is not reached. That means that the Pool is still alive. 
    traceIfFalse "Validate User Invest Message: Can't Invest in Pool, Deadline already passed" (OnChainHelpers.deadlinePoolParamNotReached pParams info)

  where
    info :: LedgerContextsV1.TxInfo
    info = LedgerContextsV1.scriptContextTxInfo ctx

    -- necesito controlar que de entrada tenga un PoolState Datum y de salida el mismo PoolState Datum mas el usuario agregado y un nuevo UserState Datum
    

    inputWithPoolState :: (LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, T.PoolStateTypo)
    inputWithPoolState =  do
        let
            inputsWithPoolState = OnChainHelpers.getInputsWithPoolState ctx
        
        case inputsWithPoolState of 
            [inputWithPoolState] -> 
                --dPoolStateFromInputBeingValidated == OnChainHelpers.getInput_Datum x
                inputWithPoolState
            _ -> traceError "Validate User Invest Message: Error. Found more than one Input with PoolState Datum" 


    outputWithPoolState :: (LedgerApiV1.TxOut, T.PoolStateTypo)
    outputWithPoolState =  do
        case OnChainHelpers.getOutputsWithPoolState  ctx of
            [(x,y)] -> (x,y)
            _ -> traceError "Validate User Invest Message: Expected exactly one output with the PoolState Datum."  

    outputWithUserState :: (LedgerApiV1.TxOut, T.UserStateTypo)
    outputWithUserState =  do
        case OnChainHelpers.getOutputsWithUserState  ctx of
            [(x,y)] -> (x,y)
            _ -> traceError "Validate User Invest Message: Expected exactly one output with the UserState Datum."  


    correctInputs :: Bool
    correctInputs = do
        case inputWithPoolState of 
            x -> True    
            _ -> False

        
    correctOutputs :: Bool
    correctOutputs = do
        case outputWithPoolState of 
            (_,_) -> 
                case outputWithUserState of 
                    (_,_) -> True    
                    _ -> False
            _ -> False
  
    signedByOutputsUserStateUser :: LedgerContextsV1.TxInfo -> LedgerContextsV1.ScriptContext-> Bool
    signedByOutputsUserStateUser info ctx  = do
        
        let 
            (_, outputUserState_Datum) = outputWithUserState

        OnChainHelpers.signedByUserStateUser outputUserState_Datum info


    correctOutputsPoolState_Datum_WithNewUser :: T.PoolNFT -> T.UserNFT -> LedgerContextsV1.ScriptContext -> Bool
    correctOutputsPoolState_Datum_WithNewUser  redeemerPoolNFT redeemerUserNFT ctx = do
        let
            (_, outputWithPoolState_Datum) = outputWithPoolState

            inputWithPoolState_Datum = OnChainHelpers.getInput_Datum inputWithPoolState

            newOutputDatum = Helpers.mkPoolStateWithNewUserInvestFromPoolState inputWithPoolState_Datum redeemerPoolNFT redeemerUserNFT

        newOutputDatum == T.PoolState outputWithPoolState_Datum

    correctOutputsUserState_Datum_OfNewUser ::  T.User -> T.UserNFT -> T.Invest -> LedgerApiV1.POSIXTime -> T.Deadline -> LedgerContextsV1.ScriptContext -> Bool
    correctOutputsUserState_Datum_OfNewUser  redeemerUser redeemerUserNFT redeemerInvest redeemerCreatedAt redeemerDeadline ctx = do
        let
            (_, outputUserState_Datum) = outputWithUserState

            newOutputDatum = T.mkUserState  redeemerUser redeemerUserNFT redeemerInvest  redeemerCreatedAt redeemerDeadline 0 0 Nothing

        newOutputDatum == T.UserState outputUserState_Datum

    correctOutputPoolState_Value_WithSameValue :: LedgerContextsV1.ScriptContext -> Bool
    correctOutputPoolState_Value_WithSameValue ctx = do
        let
            (outputWithPoolState_TxOut, _) = outputWithPoolState

            inputWithPoolState_TxOut = OnChainHelpers.getInput_TxOut  inputWithPoolState
            inputWithPoolState_Value = LedgerApiV1.txOutValue inputWithPoolState_TxOut

        inputWithPoolState_Value == LedgerApiV1.txOutValue outputWithPoolState_TxOut

    correctOutputUserState_Value_OfNewUser ::  T.Invest -> LedgerContextsV1.ScriptContext -> Bool
    correctOutputUserState_Value_OfNewUser redeemerInvest ctx = do
        let
            (outputWithUserState_TxOut, _) = outputWithUserState

            valueTotal = LedgerAda.lovelaceValueOf redeemerInvest

        valueTotal == LedgerApiV1.txOutValue outputWithUserState_TxOut   

    correctOutputPoolState_ValueAndDatum ::  (LedgerApiV1.TxOut, T.PoolStateTypo) -> Bool
    correctOutputPoolState_ValueAndDatum   (outputWithPoolState_TxOut, outputWithPoolState_Datum)  = do    
        let  
            
            masterFunders = T.psMasterFunders outputWithPoolState_Datum 

            sumFunds = sum $ T.mfFund <$> masterFunders

            valueFund  = LedgerAda.lovelaceValueOf sumFunds <> negate (LedgerAda.lovelaceValueOf $ T.psChashedOut outputWithPoolState_Datum)

        LedgerAda.fromValue valueFund == LedgerAda.fromValue (LedgerApiV1.txOutValue outputWithPoolState_TxOut)

    correctOutputsPoolState_ValuesAndDatums :: LedgerContextsV1.ScriptContext -> Bool
    correctOutputsPoolState_ValuesAndDatums   ctx  = do

        correctOutputPoolState_ValueAndDatum outputWithPoolState



{-# INLINABLE validateUserGetInvest #-}
validateUserGetInvest :: T.PoolParams -> T.UserStateTypo -> T.RedeemUserGetInvestTypo -> LedgerContextsV1.ScriptContext -> Bool
validateUserGetInvest pParams dPoolStateFromInputBeingValidated T.RedeemUserGetInvestTypo{..} ctx  =
    True
  where
    info :: LedgerContextsV1.TxInfo
    info = LedgerContextsV1.scriptContextTxInfo ctx


{-# INLINABLE validateUserGetRewards #-}

validateUserGetRewards :: T.PoolParams -> T.ValidatorDatum -> T.RedeemUserGetRewardsTypo -> LedgerContextsV1.ScriptContext -> Bool
validateUserGetRewards pParams _ T.RedeemUserGetRewardsTypo{..} ctx  =

    -- TODO: 

    -- de entrada tengo uno o varios PoolState de donde voy a sacar dinero y un UserState
    -- de salida tengo un la misma cantidad de  PoolState con el Value menos el claim Value y con el campo de CashedOut actualizado
    -- y un UserState con mismo Value pero con cashedout y claimedat actualizados

    traceIfFalse "Validate User Get Rewards Message: Wrong Input" correctInputs && 

    traceIfFalse "Validate User Get Rewards Message: Wrong Outpus" correctOutputs && 

    -- Check if the redeemerPoolNFT was minted with the right minting policy.
    traceIfFalse "Validate User Get Rewards Message: Wrong Pool NFT Minting Policy " (OnChainHelpers.correctMintigPolicyUsedForNFT pParams rugrPoolNFT  ) &&
    -- Check if the redeemerUserNFT was minted with the right minting policy
    traceIfFalse "Validate User Get Rewards Message: Wrong User NFT Minting Policy " (OnChainHelpers.correctMintigPolicyUsedForNFT pParams rugrUserNFT  ) &&
    -- Check if the redeemerUserNFT is going to the user wallet. He must keep it in order to claim rewards from the invest. 
    -- He can give it away to other user to seel the benefitis.
    traceIfFalse "Validate User Get Rewards Message: Redeemer User NFT not found in User's Output Value" (OnChainHelpers.isUserNFTInUserOutputValue rugrUserNFT rugrUser info)  &&
    -- Check if the redeemerPoolNFT is the same that identifies the Pool in the PoolParams.
    traceIfFalse "Validate User Get Rewards Message: Pool Param's NFT dosen't matches Redeemer Pool NFT" (OnChainHelpers.isPoolNFTParam pParams rugrPoolNFT ) &&

    -- Check if the redeemerPoolNFT is the same in all the inputs PoolState Datums
    traceIfFalse "Validate User Get Rewards Message: Input PoolState Datum's NFTs dosen't matches Redeemer Pool NFT" (OnChainHelpers.isPoolNFTDatum rugrPoolNFT ctx ) &&

    -- Check if the redeemerPoolNFT is the same in all the inputs UserState Datums
    traceIfFalse "Validate User Get Rewards Message: Input UserState Datum's NFTs dosen't matches Redeemer User NFT" (OnChainHelpers.isUserNFTDatum rugrUserNFT ctx ) &&
    
    -- -- Check if there is one single input UserState Datum to redeem
    -- traceIfFalse "Validate User Get Rewards Message: Need to use one single UserState Input" (isUnicSingleUserStateInput ctx ) &&
    
    -- Check if this tx was signed by the User specified in the redeemerUser.
    traceIfFalse "Validate User Get Rewards Message: Redeemer User's signature missing" (OnChainHelpers.signedByUser rugrUser info) &&
    
    -- Check if this tx was signed by User in the UserState Datum produced by this tx. 
    -- Inside is going to fail if the outputs at the script aren't just two, one with the PoolState Datum and the other with the UserStateDatum.
    traceIfFalse "Validate User Get Rewards Message: Output UserState Datum User's signature missing" (signedByOutputsUserStateUser info ctx) &&

     -- Check if the user already invested in the Pool
    traceIfFalse "Validate User Get Rewards Message: Redeemer User NFT not found in PoolState Datum UserNFTs" (isUserNFTInPoolState rugrPoolNFT rugrUserNFT  ctx)   &&

    -- Check if this the claim is more than the minimun claim.
    traceIfFalse "Validate User Get Rewards Message: Amount to withdraw does not reach minimum." (OnChainHelpers.isMoreThanMinimunClaim pParams rugrClaim) &&

    -- Check if this the claim is correct.
    traceIfFalse "Validate User Get Rewards Message: Claim Value is not Valid" (correctClaimValue pParams rugrClaim rugrClaimAt ctx) &&


    -- Check if Pool NFT is present in some input and if it is present check if in the uotput where the NFT is, the value of count funds is the same.
    traceIfFalse "Validate User Get Rewards Message: Error with otxu funds count." (correctOutputWithNFTPoolState_Datum rugrPoolNFT ctx) &&

    -- check in all the outputs with PoolState that the sum of Value plus cashed out must be equal to the funds
    traceIfFalse "Validate User Get Rewards Message: Output Value Error, not matching PoolState Datums Funds" (correctOutputsPoolState_DatumAndValues ctx) && 

    -- Check if the PoolState Datum produced is correct, need to be the same that the sum of inputs PoolState
    -- Inside is going to fail if the outputs at the script aren't just two, one with the PoolState Datum and the other with the UserStateDatum.
    traceIfFalse "Validate User Get Rewards Message: Outputs PoolState Datum wrong" (correctOutputsPoolState_Datums_WithNewClaimRewards rugrPoolNFT rugrClaim ctx)   &&

    -- Check if the UserState Datum produced is correct, with the new claim
    -- TODO: 
    -- Inside is going to fail if the outputs at the script aren't just two, one with the PoolState Datum and the other with the UserStateDatum.
    traceIfFalse "Validate User Get Rewards Message: Output UserState Datum wrong" (correctOutputsUserState_Datum_WithNewClaimRewards rugrUser rugrUserNFT rugrClaim rugrClaimAt ctx)   &&
    
    -- Check if the Output Value with PoolState Datum is the same that in the Inputs with PoolSate Datum less the rewards claimed
    traceIfFalse "Validate User Get Rewards Message: PoolState's Outputs Value Error. Must be the same that in PoolState's Inputs less the rewards claimed." (correctPoolState_Values_LessClaimRewards rugrClaim ctx) &&
    
    -- Check if the Output Value with UserState Datum is the same that the input UserState.
    traceIfFalse "Validate User Get Rewards Message: UserState's Output Value Error. Must be the same than the UserState's Input." (correctOutputUserState_Value_WithNoChanges ctx) &&
    
    


     -- Check that the tx range interval of validity. Can't be infinitum and need to be lees than ppValidTimeRange Pool Param . 
    traceIfFalse "Validate User Get Rewards Message: Can't Invest in Pool, tx validity time range is not valid" (OnChainHelpers.isValidRange pParams info) &&

  
    -- Check if this the claim date at is correct.
    traceIfFalse "Validate User Get Rewards Message: Claim DateAt is not Valid" (OnChainHelpers.correctClaimDateAt rugrClaimAt info) &&

   
    -- Check that the Pool Deadline is not reached. That means that the Pool is still alive. 
    traceIfFalse "Validate User Get Rewards Message: Can't Get Rewatrds, Deadline already passed" (OnChainHelpers.deadlinePoolParamNotReached pParams info)

  where

    info :: LedgerContextsV1.TxInfo
    info = LedgerContextsV1.scriptContextTxInfo ctx

    inputsWithPoolState :: [(LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, T.PoolStateTypo)]
    inputsWithPoolState =  do
        let
            inputsWithPoolState = OnChainHelpers.getInputsWithPoolState ctx

        case inputsWithPoolState of 
            [] -> traceError "Validate User Get Rewards Message: Error. Not Found Input with PoolState Datum" 
            _ -> do
            
                let

                    -- for finding inputs PoolState where the UserNFT was register in
                    findInputPoolStateWithUserNFT inputsWithPoolState userNFT =
                        case [  inputWithPoolState  | inputWithPoolState <- inputsWithPoolState, userNFT `elem` T.psUsersNFT (OnChainHelpers.getInput_Datum inputWithPoolState)  ] of
                            [inputWithPoolState] -> inputWithPoolState
                            _ -> traceError "Validate User Get Rewards Message: Cant' Get Rewards from because can't find single PoolState utxo with UserNFT register"

                    -- find input PoolState where the UserNFT was register in
                    inputWithPoolStateWithUserNFT = findInputPoolStateWithUserNFT inputsWithPoolState rugrUserNFT

                
                    -- get the list of inputs PoolState without the one it just found
                    inputsWithPoolStateWithoutUserNFT = OnChainHelpers.removeInputByTxOutRef (OnChainHelpers.getInput_TxOutRef inputWithPoolStateWithUserNFT) inputsWithPoolState

                    -- for ordering the list of inputs by value
                    compareValueOfInputs ::  (LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, T.PoolStateTypo)-> (LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, T.PoolStateTypo) -> Ordering
                    compareValueOfInputs inputs1 inputs2
                        | (LedgerAda.fromValue ( LedgerApiV1.txOutValue (OnChainHelpers.getInput_TxOut inputs1)) > (LedgerAda.fromValue  ( LedgerApiV1.txOutValue ( OnChainHelpers.getInput_TxOut inputs2) ))) = LT
                        | otherwise = GT


                     -- order the list of inputs by value
                    inputsOrderedByValueWithPoolState = sortBy compareValueOfInputs inputsWithPoolStateWithoutUserNFT

                    getInputsWithEnoughValueToClaim :: [(LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, T.PoolStateTypo)] -> T.Proffit -> [(LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, T.PoolStateTypo)]
                    getInputsWithEnoughValueToClaim inputs claim = do
                    
                        if claim >0 then do
                            if length inputs ==0 then 
                                traceError "Validate User Get Rewards Message: Not enough inputs to cover the claim"
                            else do
                                -- there is remaining claim to get, so i keep adding inputs
                                let
                                    input = head inputs

                                    -- cant take all the value, i need to leave the minumum ada
    
                                    value = LedgerApiV1.txOutValue (OnChainHelpers.getInput_TxOut input)
                                    valueCanUse    = value <> negate (LedgerAda.lovelaceValueOf Helpers.minLovelace)
                                    adaFromValueCanUse = LedgerAda.getLovelace (LedgerAda.fromValue valueCanUse) 

                                    newClaim = claim - adaFromValueCanUse

                                input : getInputsWithEnoughValueToClaim (tail inputs) newClaim
                        else
                            -- there is no more remaining claim to get
                            []      

                    -- the input with the UserNFT must be included beyond its value
                    -- anyways I need to consider its value
                    -- the new claim is the result of substracting the value of this input to the original claimed valueOf
        
                    valueInputWithPoolStateWithUserNFT = LedgerApiV1.txOutValue (OnChainHelpers.getInput_TxOut  inputWithPoolStateWithUserNFT)
                    valueCanUse    = valueInputWithPoolStateWithUserNFT <> negate (LedgerAda.lovelaceValueOf Helpers.minLovelace)
                    adaFromValueCanUse = LedgerAda.getLovelace (LedgerAda.fromValue valueCanUse) 
                    -- cant take all the value, need to leave minumun ada: <> negate LedgerAda.lovelaceValueOf Helpers.minLovelace
                    claim = rugrClaim - adaFromValueCanUse
                    -- The resulting list of inputs is including in the firt the input with the userNFT and the rest is a calling getInputsWithEnoughValueToClaim wo find the minumun listo of inputs wich can cover the payment.
                    inputsWithPoolStateWithEnoughValueToClaim = inputWithPoolStateWithUserNFT : getInputsWithEnoughValueToClaim inputsOrderedByValueWithPoolState claim

                if length inputsWithPoolStateWithEnoughValueToClaim == length inputsWithPoolState then
                    inputsWithPoolStateWithEnoughValueToClaim
                else
                    traceError "Validate User Get Rewards Message: Error. Using too many Inputs"      

    inputWithPoolStateAndPoolNFT :: Maybe (LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, T.PoolStateTypo)
    inputWithPoolStateAndPoolNFT =  OnChainHelpers.getInputWithPoolStateAndPoolNFT inputsWithPoolState rugrPoolNFT 

    inputWithUserState :: (LedgerApiV1.TxOutRef, LedgerApiV1.TxOut, T.UserStateTypo)
    inputWithUserState =  do
        let 
            inputsWithUserState = OnChainHelpers.getInputsWithUserState ctx

        case inputsWithUserState of
            [x] -> x
            _ -> traceError "Validate User Get Rewards Message: Expected exactly one input with the UserState Datum."  

    outputsWithPoolState :: [(LedgerApiV1.TxOut, T.PoolStateTypo)]
    outputsWithPoolState =  do
        let 
            outputsWithPoolState = OnChainHelpers.getOutputsWithPoolState  ctx
        if length outputsWithPoolState == length inputsWithPoolState then do

            let
                -- for ordering the outputs
                compareValueOfOutpus ::  (LedgerApiV1.TxOut, T.PoolStateTypo)-> (LedgerApiV1.TxOut, T.PoolStateTypo) -> Ordering
                compareValueOfOutpus output1 output2

                    | (LedgerAda.fromValue ( LedgerApiV1.txOutValue (fst output1)) > LedgerAda.fromValue  ( LedgerApiV1.txOutValue ( fst output2)  )) = LT
                    | otherwise = GT

                -- ordering the outputs by value
                outputsOrderedByValueWithPoolState = sortBy compareValueOfOutpus outputsWithPoolState
                
                -- count elements to satisfy condition
                countBy :: (a -> Bool) -> [a] -> Integer
                countBy cond xs =
                    foldr          -- Think of this as a for loop
                        body       -- The body of our for loop
                        0          -- The initial value for cnt
                        xs         -- The list to iterate over
                    where
                        body elem currentCnt =
                            if cond elem
                                then currentCnt + 1
                                else currentCnt

                getAdaValueOf :: LedgerValueV1.Value -> Integer
                getAdaValueOf value = LedgerAda.getLovelace (LedgerAda.fromValue value)

                cond :: (LedgerApiV1.TxOut, T.PoolStateTypo) -> Bool
                cond (txOut, _) = getAdaValueOf ( LedgerApiV1.txOutValue txOut) >  Helpers.minLovelace 
                                
            if  countBy cond outputsOrderedByValueWithPoolState <=1 then
                -- count the outputs which have value greater than the minimun
                -- need to be only one... i use the minumun quiantity of inputs i need to cover the claim, 
                -- and then all the inputs i use need to be in minumun ada, just the last one can have some remaining value.
                outputsOrderedByValueWithPoolState
                
            else
                traceError "Validate User Get Rewards Message: Expected exactly one Pool State output with Value greater than Helpers.minLovelace"  
        else
            traceError "Validate User Get Rewards Message: Expected exactly the same quantity of outputs than inputs with the PoolState Datum."  


    outputWithUserState :: (LedgerApiV1.TxOut, T.UserStateTypo)
    outputWithUserState =  do
        case OnChainHelpers.getOutputsWithUserState  ctx of
            [(x,y)] -> (x,y)
            _ -> traceError "Validate User Get Rewards Message: Expected exactly one output with the UserState Datum."  

    -- de entrada tengo uno o varios PoolState de donde voy a sacar dinero y un UserState
    -- de salida tengo un la misma cantidad de  PoolState con el Value menos el claim Value y con el campo de CashedOut actualizado
    -- y un UserState con mismo Value pero con cashedout y claimedat actualizados

    correctInputs :: Bool
    correctInputs = do
        case inputsWithPoolState of 
            x -> 
                case inputWithUserState of 
                    x -> True
                    _ -> False
            _ -> False

        
    correctOutputs :: Bool
    correctOutputs = do
        case outputsWithPoolState of 
            (x:xs) -> 
                case outputWithUserState of 
                    (_,_) -> True    
                    _ -> traceError "Validate User Get Rewards Message: wrong outputWithUserState."  
            [] -> traceError "Validate User Get Rewards Message: wrong outputsWithPoolState." 
  
    signedByOutputsUserStateUser :: LedgerContextsV1.TxInfo -> LedgerContextsV1.ScriptContext-> Bool
    signedByOutputsUserStateUser info ctx  = do
        
        let 
            (_, outputUserState_Datum) = outputWithUserState

        OnChainHelpers.signedByUserStateUser outputUserState_Datum info


    isUserNFTInPoolState :: T.PoolNFT-> T.UserNFT -> LedgerContextsV1.ScriptContext-> Bool
    isUserNFTInPoolState rugrPoolNFT rugrUserNFT  ctx = do
        
        let 
            inputsWithPoolState_Datum = OnChainHelpers.getInput_Datum <$> inputsWithPoolState

            newDummyDatum = Helpers.mkDummyPoolStateFromPoolStateList inputsWithPoolState_Datum rugrPoolNFT          


        rugrUserNFT `elem` T.psUsersNFT (Helpers.fromJust $ Helpers.getPoolStateFromDatum newDummyDatum)

        --any (==rugrUserNFT) (T.psUsersNFT newOutputDatum)
    
    correctClaimValue :: T.PoolParams -> T.Proffit -> LedgerApiV1.POSIXTime   -> LedgerContextsV1.ScriptContext -> Bool
    correctClaimValue pParams rugrClaim rugrClaimAt ctx = do
        let
            (_, outputUserState_Datum) = outputWithUserState

            inputWithUserState_Datum = OnChainHelpers.getInput_Datum inputWithUserState

            rewards = Helpers.getRewardsPerInvest (T.usLastClaimAt inputWithUserState_Datum) rugrClaimAt (T.usCreatedAt  inputWithUserState_Datum )  (T.usInvest inputWithUserState_Datum ) 
            totalNewRewards = rewards  + T.usRewardsNotClaimed inputWithUserState_Datum

        rugrClaim >= T.ppMinimunClaim pParams && rugrClaim <= totalNewRewards

    -- Check if Pool NFT is present in some input and if it is present check if in the uotput where the NFT is, the value of count funds is the same.
    correctOutputWithNFTPoolState_Datum  ::  T.PoolNFT -> LedgerContextsV1.ScriptContext -> Bool
    correctOutputWithNFTPoolState_Datum redeemerPoolNFT ctx  = do
        case inputWithPoolStateAndPoolNFT of
            Nothing -> True
            Just inputWithPoolStateAndPoolNFT -> do
                let 
                    outputWithPoolStateAndPoolNFT = OnChainHelpers.getOutputWithPoolStateAndPoolNFT outputsWithPoolState rugrPoolNFT

                case outputWithPoolStateAndPoolNFT of
                    Nothing -> traceError "Validate User Get Rewards Message: Not found NFT in output Pool State."  
                    Just outputWithPoolStateAndPoolNFT -> do
                        let 
                            inputWithPoolStateAndPoolNFT_Datum = OnChainHelpers.getInput_Datum inputWithPoolStateAndPoolNFT
                            outputWithPoolStateAndPoolNFT_Datum = snd outputWithPoolStateAndPoolNFT
                
                        T.psCountTotalUtxoWithPoolState inputWithPoolStateAndPoolNFT_Datum == T.psCountTotalUtxoWithPoolState outputWithPoolStateAndPoolNFT_Datum

    correctOutputPoolState_DatumAndValue ::  (LedgerApiV1.TxOut, T.PoolStateTypo) -> Bool
    correctOutputPoolState_DatumAndValue outputWithPoolState  = do
        let
            outputWithPoolState_Datum = snd outputWithPoolState
        
        LedgerAda.getLovelace ( LedgerAda.fromValue (LedgerApiV1.txOutValue (fst outputWithPoolState) <>  LedgerAda.lovelaceValueOf (T.psChashedOut outputWithPoolState_Datum) ))  == sum [ T.mfFund psMasterFunder | psMasterFunder <-  T.psMasterFunders outputWithPoolState_Datum]

    correctOutputsPoolState_DatumAndValues :: LedgerContextsV1.ScriptContext -> Bool
    correctOutputsPoolState_DatumAndValues ctx  = 
            all correctOutputPoolState_DatumAndValue outputsWithPoolState



    correctOutputsPoolState_Datums_WithNewClaimRewards ::  T.PoolNFT -> T.Proffit -> LedgerContextsV1.ScriptContext -> Bool
    correctOutputsPoolState_Datums_WithNewClaimRewards rugrPoolNFT rugrClaim ctx = do
        let 
            inputsWithPoolState_Datums = OnChainHelpers.getInput_Datum <$> inputsWithPoolState

            outputsWithPoolState_Datums = snd <$> outputsWithPoolState

            newDummyOutputDatum = Helpers.mkDummyPoolStateFromPoolStateList outputsWithPoolState_Datums rugrPoolNFT          

            newDummyInputDatum = Helpers.mkDummyPoolStateWithNewClaimRewardsFromPoolStateList inputsWithPoolState_Datums rugrPoolNFT rugrClaim    

        newDummyInputDatum == newDummyOutputDatum



    correctOutputsUserState_Datum_WithNewClaimRewards :: T.User -> T.UserNFT -> T.Proffit -> LedgerApiV1.POSIXTime   -> LedgerContextsV1.ScriptContext -> Bool
    correctOutputsUserState_Datum_WithNewClaimRewards redeemerUser redeemerUserNFT rugrClaim rugrClaimAt ctx = do
        let
            (_, outputUserState_Datum) = outputWithUserState

            inputWithUserState_Datum = OnChainHelpers.getInput_Datum inputWithUserState

            rewards = Helpers.getRewardsPerInvest (T.usLastClaimAt inputWithUserState_Datum) rugrClaimAt  (T.usCreatedAt  inputWithUserState_Datum )  (T.usInvest inputWithUserState_Datum ) 
            totalNewRewards = rewards  + T.usRewardsNotClaimed inputWithUserState_Datum
            rewardsNotClaimed = totalNewRewards - rugrClaim
            totalRewardsCashedOut = T.usChashedOut inputWithUserState_Datum + rugrClaim 

            newOutputDatum = T.mkUserState redeemerUser redeemerUserNFT 
                            (T.usInvest inputWithUserState_Datum ) 
                            (T.usCreatedAt  inputWithUserState_Datum ) 
                            (T.usDeadline  inputWithUserState_Datum )  
                            totalRewardsCashedOut
                            rewardsNotClaimed
                            (Just rugrClaimAt)

        newOutputDatum == T.UserState outputUserState_Datum

    {-Check if the Output Value with PoolState Datum is the same that in the Inputs with PoolSate Datum less the rewards claimed -}
    correctPoolState_Values_LessClaimRewards ::  T.Proffit -> LedgerContextsV1.ScriptContext -> Bool
    correctPoolState_Values_LessClaimRewards rugrClaim ctx = do
        let

            inputsWithPoolState_TxOut = OnChainHelpers.getInput_TxOut <$> inputsWithPoolState

            inputsWithPoolState_Values = [ LedgerApiV1.txOutValue inputTxOut | inputTxOut <- inputsWithPoolState_TxOut] 

            valueInputsTotal = foldl (<>)  (LedgerAda.lovelaceValueOf 0) inputsWithPoolState_Values


            outputsWithPoolState_TxOut = fst <$> outputsWithPoolState

            outputsWithPoolState_Values = [ LedgerApiV1.txOutValue outputTxOut | outputTxOut <- outputsWithPoolState_TxOut] 

            valueOutputsTotal = foldl (<>)  (LedgerAda.lovelaceValueOf 0) outputsWithPoolState_Values


        valueInputsTotal == valueOutputsTotal <> LedgerAda.lovelaceValueOf rugrClaim


    {-Check if the Output Value with UserState Datum is the same that the input UserState. -}
    correctOutputUserState_Value_WithNoChanges :: LedgerContextsV1.ScriptContext -> Bool
    correctOutputUserState_Value_WithNoChanges ctx = do
        let
            (outputWithUserState_TxOut, _) = outputWithUserState

            inputWithUserState_TxOut  = OnChainHelpers.getInput_TxOut inputWithUserState

            inputWithUserState_Value = LedgerApiV1.txOutValue inputWithUserState_TxOut 

        inputWithUserState_Value == LedgerApiV1.txOutValue outputWithUserState_TxOut



{-# INLINABLE validateUserInvestRewards #-}
validateUserInvestRewards :: T.PoolParams -> T.UserStateTypo -> T.RedeemUserInvestRewardsTypo -> LedgerContextsV1.ScriptContext -> Bool
validateUserInvestRewards pParams dPoolStateFromInputBeingValidated T.RedeemUserInvestRewardsTypo{..} ctx  =
    True
  where
    info :: LedgerContextsV1.TxInfo
    info = LedgerContextsV1.scriptContextTxInfo ctx


typedValidator :: T.PoolParams -> UtilsTypedScriptsValidatorsV1.TypedValidator ValidatorScriptV1
typedValidator  pParams = UtilsTypedScriptsValidatorsV1.mkTypedValidator @ValidatorScriptV1  
    (
      $$(PlutusTx.compile [|| mkValidator ||]) 
      `PlutusTx.applyCode` 
      PlutusTx.liftCode pParams
    )
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = UtilsTypedScriptsValidatorsV1.mkUntypedValidator

codeValidator :: T.PoolParams ->LedgerScriptsV1.Validator
codeValidator pParams = UtilsTypedScriptsValidatorsV1.validatorScript $ typedValidator pParams     

hashValidator :: T.PoolParams ->LedgerScriptsV1.ValidatorHash
hashValidator pParams = UtilsTypedScriptsValidatorsV1.validatorHash $ typedValidator pParams 

addressValidator :: T.PoolParams ->LedgerAddressV1.Address
addressValidator pParams = UtilsTypedScriptsValidatorsV1.validatorAddress $ typedValidator pParams 