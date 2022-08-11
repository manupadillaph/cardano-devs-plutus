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

--{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Validators.StakePlus.OnChain
    ( 
      codeValidator,
      typedValidator
    , hashValidator
    , addressValidator
    ) where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           Data.String  
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
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


--Import Internos
import  Validators.StakePlus.Typos 
import  Validators.StakePlus.Helpers 
import  Validators.StakePlus.OnChainHelpers as OnChainHelpers
import  Validators.StakePlus.OnChainNFT     (mintingNFTPolicy)


data ValidatorScript
instance Scripts.ValidatorTypes ValidatorScript where
    type instance RedeemerType ValidatorScript = ValidatorRedeemer
    type instance DatumType ValidatorScript = ValidatorDatum


{-# INLINABLE mkValidator #-}
mkValidator :: PoolParams -> ValidatorDatum -> ValidatorRedeemer -> ScriptContext -> Bool

mkValidator pParams (PoolState dPoolState) (RedeemMasterFundPool redeemMasterFundPool) ctx =
    validateMasterFundPool pParams dPoolState redeemMasterFundPool ctx

mkValidator pParams (PoolState dPoolState) (RedeemMasterFundAndMergePool redeemMasterFundAndMergePool) ctx =
    validateMasterFundAndMergePool pParams dPoolState redeemMasterFundAndMergePool ctx

mkValidator pParams (PoolState dPoolState) (RedeemMasterGetPool redeemMasterGetPool) ctx =
    validateMasterGetPool pParams dPoolState redeemMasterGetPool ctx

mkValidator pParams (PoolState dPoolState)  (RedeemUserInvest redeemUserInvest) ctx =
    validateUserInvest pParams dPoolState redeemUserInvest ctx

mkValidator pParams (UserState dUserState) (RedeemUserGetInvest redeemUserGetInvest) ctx =
    validateUserGetInvest pParams dUserState redeemUserGetInvest ctx

mkValidator pParams (UserState dUserState)  (RedeemUserGetRewards redeemUserGetRewards) ctx =
    validateUserGetRewards pParams (UserState dUserState) redeemUserGetRewards ctx

mkValidator pParams (PoolState dPoolState) (RedeemUserGetRewards redeemUserGetRewards) ctx =
    validateUserGetRewards pParams (PoolState dPoolState) redeemUserGetRewards ctx

mkValidator pParams (UserState dUserState) (RedeemUserInvestRewards redeemUserInvestRewards) ctx =
    validateUserInvestRewards pParams dUserState redeemUserInvestRewards ctx

mkValidator pParams (PoolState dPoolState) _ _ =
    traceIfFalse "Stake Pool Message: Wrong Redeemer For PoolState utxo" False 

mkValidator pParams (UserState dUserState) _ _ =
    traceIfFalse "Stake Pool Message: Wrong Redeemer For UserState utxo" False 

mkValidator _ _ _ _ =
     traceIfFalse "Stake Pool Message: Invalid Operationddd" False 


{-# INLINABLE validateMasterFundPool #-}
validateMasterFundPool :: PoolParams -> PoolStateTypo -> RedeemMasterFundPoolTypo -> ScriptContext -> Bool
validateMasterFundPool pParams dPoolStateFromInputBeingValidated RedeemMasterFundPoolTypo{..}  ctx  =
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
    traceIfFalse "Validate Master Fund Pool Message: Wrong NFT Minting Policy " (correctMintigPolicyUsedForNFT pParams rmfpPoolNFT  ) &&
    
    -- Check if the redeemerPoolNFT is included in the value of the Own's Single Output comming to the script. 
    -- The redeemerPoolNFT is going to be holded in the script until the Pool is alive.
    -- Esta verificacion es importante por que me asegura que haya al menos un PoolState que contiene todas los Fund, es un conteo.
    -- Esto se neceseita para recuperar la inversion. Alli se verificará que se usen todas las utxo con PoolState, segun el contador de utxoFunds

    traceIfFalse "Validate Master Fund Pool Message: Redeemer Pool NFT not found in Own's Output Value" (isPoolNFTInOldOutputWithPoolState rmfpPoolNFT ctx) &&

    -- Check if the redeemerPoolNFT is the same that identifies the Pool in the PoolParams.
    traceIfFalse "Validate Master Fund Pool Message: Pool Param's NFT dosen't matches Redeemer Pool NFT" (isPoolNFTParam pParams rmfpPoolNFT ) &&
    -- Check if the redeemerPoolNFT is the same in all the inputs PoolState Datums
    traceIfFalse "Validate Master Fund Pool Message: Input PoolState Datum's NFTs dosen't matches Redeemer Pool NFT" (isPoolNFTDatum rmfpPoolNFT ctx ) &&

    -- Check if this tx was signed by any of the Masters included in the PoolParams. 
    -- This are all the Masters that can interact with the script at any time. Nobody else will be able to change or redeem the funds.
    traceIfFalse "Validate Master Fund Pool Message: Pool Params Master's signature missing" (signedByPoolParamMaster pParams info) &&
    -- Check if this tx was signed by any of the Masters included in the PoolState Datum produced by this tx. 
    -- This datum includes all the Masters already interacted with the script. Are all the ones who funded the Pool.
    traceIfFalse "Validate Master Fund Pool Message: Output PoolState Datum Master's signature missing" (signedByNewOutputPoolStateMasterFunder info ctx) &&
    -- Check if this tx was signed by the Master specified in the redeemer.
    traceIfFalse "Validate Master Fund Pool Message: Redeemer Master's signature missing" (signedByMaster rmfpMaster info) &&


    traceIfFalse "Validate Master Fund Pool Message: Output NEW PoolState Datum wrong, not matching provided redeemer" (correctNewOutputPoolState_Datum rmfpPoolNFT rmfpMaster rmfpFund  ctx)   &&

    traceIfFalse "Validate Master Fund Pool Message: Output Value Error, not matching PoolState Datums Funds" (correctOutputsPoolState_Datum_Values ctx) && 

    -- Check that the tx range interval of validity. Can't be infinitum and need to be lees than ppValidTimeRange Pool Param . 
    traceIfFalse "Validate Master Fund Pool Message: Can't Fund Pool, tx validity time range is not valid" (isValidRange pParams info) &&

    -- Check that the Pool Deadline is not reached. That means that the Pool is still alive. 
    traceIfFalse "Validate Master Fund Pool Message: Can't Fund Pool, Deadline already passed" (deadlinePoolParamNotReached pParams info)
    
  where 

    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- En esta nueva version, voy a tener de entrada una utxo con PoolState datum, representado con dPoolStateFromInputBeingValidated y en el valor del redeemer rmfpUsingUtxo
    -- Eso es para obligar a que la accion de fund pase por el validador
    -- En la salida tengo que tener esa misma entrada, con el mismo datum y value, recreados, y la nueva salida con el Fund

    inputBeingConsumed :: TxOutRef
    inputBeingConsumed = rmfpUsingUtxo

    inputWithPoolState :: (TxOutRef,TxOut,PoolStateTypo)
    inputWithPoolState =  do
        let 
            inputsWithPoolState = getInputsWithPoolState ctx

        case inputsWithPoolState of 
            [inputWithPoolState] -> 
                if inputBeingConsumed == getInput_TxOutRef inputWithPoolState && dPoolStateFromInputBeingValidated == getInput_Datum inputWithPoolState then
                    inputWithPoolState
                else
                    traceError "Validate Master Fund Pool Message: Error. Found one Input with PoolState Datum but it is different to the redeemer"   

            _ -> traceError "Validate Master Fund Pool Message: Error. Found more than one Input with PoolState Datum" 

    inputWithPoolStateAndPoolNFT :: (TxOutRef,TxOut,PoolStateTypo)
    inputWithPoolStateAndPoolNFT =  case getInputWithPoolStateAndPoolNFT [inputWithPoolState] rmfpPoolNFT of
        Just inputWithPoolStateAndPoolNFT -> inputWithPoolStateAndPoolNFT
        Nothing -> traceError "Validate Master Fund And Merge Pool Message: Can't find Pool NFT on Inputs."  

    {- | Gets, if there is only one, the output with same PoolState Datum and Value than the input PoolState Datum plus 1 in the count of utxo with Pool State. -} 
    getOutputPoolStateDatumSameDatumAndValueThanInputPlus1CountUxto :: PoolNFT -> PoolStateTypo -> ScriptContext -> (TxOut,PoolStateTypo)
    getOutputPoolStateDatumSameDatumAndValueThanInputPlus1CountUxto redeemerPoolNFT datumInputPoolState ctx = do
        let
            oldPoolState = mkPoolStateWithNewCountFundsFromPoolState datumInputPoolState redeemerPoolNFT

            outputsWithPoolState = getOutputsWithPoolState ctx

            oldTxOuts =  [(outputWithPoolState_TxOut, outputWithPoolState_Datum) | (outputWithPoolState_TxOut, outputWithPoolState_Datum) <- outputsWithPoolState , PoolState outputWithPoolState_Datum == oldPoolState]

            oldTxOutsWithSameValue =  [(outputWithPoolState_TxOut, outputWithPoolState_Datum) | (outputWithPoolState_TxOut, outputWithPoolState_Datum) <- outputsWithPoolState , txOutValue outputWithPoolState_TxOut ==  (txOutValue $ getInput_TxOut inputWithPoolState) ]

        case oldTxOutsWithSameValue of
            [oldTxOutWithSameValue] -> oldTxOutWithSameValue
            _ -> traceError "Validate Master Fund Pool Message: Expected exactly one output with the same PoolState Datum and value than the input."
        
    getOutputPoolStateDatumDiffThan :: (TxOut,PoolStateTypo) -> ScriptContext -> (TxOut,PoolStateTypo)
    getOutputPoolStateDatumDiffThan outputSameThanInput ctx = do
        let
            outputsWithPoolState = getOutputsWithPoolState ctx

            newtxOuts =  [txOutAndPoolState | txOutAndPoolState <- outputsWithPoolState ,txOutAndPoolState PlutusTx.Prelude./= outputSameThanInput]
        
        case newtxOuts of
            [newtxOut] -> newtxOut
            _ -> traceError "Validate Master Fund Pool Message: Expected exactly one new output with PoolState Datum."
    

    oldOutputWithPoolState ::  (TxOut,PoolStateTypo)
    oldOutputWithPoolState =  getOutputPoolStateDatumSameDatumAndValueThanInputPlus1CountUxto rmfpPoolNFT dPoolStateFromInputBeingValidated ctx

    newOutputWithPoolState ::  (TxOut,PoolStateTypo)
    newOutputWithPoolState  =  getOutputPoolStateDatumDiffThan oldOutputWithPoolState  ctx

    outputsWithPoolState ::  [(TxOut,PoolStateTypo)]
    outputsWithPoolState  =  [oldOutputWithPoolState ,newOutputWithPoolState ]

    correctInputs :: Bool
    correctInputs = do
        case inputWithPoolState of 
            x -> True
            _ -> False    

    correctOutputs :: Bool
    correctOutputs = do
        case outputsWithPoolState of 
            (x:xs) -> True
            _ -> False   

    isPoolNFTInOldOutputWithPoolState :: PoolNFT ->  ScriptContext -> Bool
    isPoolNFTInOldOutputWithPoolState  redeemerPoolNFT ctx  = do
        OnChainHelpers.isPoolNFTInSomeOutputWithPoolState [oldOutputWithPoolState] redeemerPoolNFT ctx

    signedByNewOutputPoolStateMasterFunder :: TxInfo -> ScriptContext-> Bool
    signedByNewOutputPoolStateMasterFunder info ctx  = do
        
        let 
            (_, outputWithPoolState_Datum) = newOutputWithPoolState

        signedByPoolStateMasterFunder outputWithPoolState_Datum info


    correctNewOutputPoolState_Datum :: PoolNFT -> Master -> Fund -> ScriptContext -> Bool
    correctNewOutputPoolState_Datum  redeemerPoolNFT redeemerMaster redeemerFund ctx = do
        let

            (_, outputWithPoolState_Datum) = newOutputWithPoolState

            countTotalUtxoWithPoolState = 0

            newOutputDatum = mkPoolStateWithNewFundFromPoolStateList [] redeemerPoolNFT redeemerMaster redeemerFund countTotalUtxoWithPoolState

        newOutputDatum == PoolState outputWithPoolState_Datum
        

    correctOldOutputPoolState_Datum :: PoolNFT -> Bool
    correctOldOutputPoolState_Datum  rmfpPoolNFT  = do
        let

            (_, outputWithPoolState_Datum) = oldOutputWithPoolState

            inputWithPoolStateAndPoolNFT_Datum = getInput_Datum inputWithPoolStateAndPoolNFT

            newOutputDatum = mkPoolStateWithNewCountFundsFromPoolState inputWithPoolStateAndPoolNFT_Datum rmfpPoolNFT     

        newOutputDatum == PoolState outputWithPoolState_Datum
        

    correctOutputPoolState_Datum_Value ::  (TxOut,PoolStateTypo) -> Bool
    correctOutputPoolState_Datum_Value   (outputWithPoolState_TxOut, outputWithPoolState_Datum)  = do    
        let  
            
            masterFunders = psMasterFunders outputWithPoolState_Datum 

            sumFunds = PlutusTx.Prelude.sum $ mfFund <$> masterFunders

            valueFund  = Ada.lovelaceValueOf sumFunds <> negate (Ada.lovelaceValueOf $ psChashedOut outputWithPoolState_Datum)

        Ada.fromValue valueFund == Ada.fromValue (txOutValue outputWithPoolState_TxOut)

    correctOutputsPoolState_Datum_Values ::  ScriptContext -> Bool
    correctOutputsPoolState_Datum_Values   ctx  = do
        PlutusTx.Prelude.all  correctOutputPoolState_Datum_Value outputsWithPoolState
            

{-# INLINABLE validateMasterFundAndMergePool #-}
validateMasterFundAndMergePool :: PoolParams -> PoolStateTypo -> RedeemMasterFundAndMergePoolTypo -> ScriptContext -> Bool
validateMasterFundAndMergePool pParams dPoolStateFromInputBeingValidated RedeemMasterFundAndMergePoolTypo{..}  ctx  =

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
    traceIfFalse "Validate Master Fund And Merge Pool Message: Wrong NFT Minting Policy " (correctMintigPolicyUsedForNFT pParams rmfampPoolNFT  ) &&

    -- Check if the redeemerPoolNFT is included in the value of the Own's Single Output comming to the script. 
    -- The redeemerPoolNFT is going to be holded in the script until the Pool is alive.
    -- Es necesario que este la PoolState que tenía el NFT por que ese es el unico que estará siempre actualizado con la cantidad de utxo con PoolState Datum

    traceIfFalse "Validate Master Fund Pool Message: Redeemer Pool NFT not found in Own's Output Value" (isPoolNFTInSomeOutputWithPoolState rmfampPoolNFT ctx) &&

    -- Check if the redeemerPoolNFT is the same that identifies the Pool in the PoolParams.
    traceIfFalse "Validate Master Fund And Merge Pool Message: Pool Param's NFT dosen't matches Redeemer Pool NFT" (isPoolNFTParam pParams rmfampPoolNFT ) &&
    -- Check if the redeemerPoolNFT is the same in all the inputs PoolState Datums
    traceIfFalse "Validate Master Fund And Merge Pool Message: Input PoolState Datum's NFTs dosen't matches Redeemer Pool NFT" (isPoolNFTDatum rmfampPoolNFT ctx ) &&

    -- Check if this tx was signed by any of the Masters included in the PoolParams. 
    -- This are all the Masters that can interact with the script at any time. Nobody else will be able to change or redeem the funds.
    traceIfFalse "Validate Master Fund And Merge Pool Message: Pool Params Master's signature missing" (signedByPoolParamMaster pParams info) &&
    -- Check if this tx was signed by any of the Masters included in the PoolState Datum produced by this tx. 
    -- This datum includes all the Masters already interacted with the script. Are all the ones who funded the Pool.
    traceIfFalse "Validate Master Fund And Merge Pool Message: Output PoolState Datum Master's signature missing" (signedByOutputPoolStateMasterFunder info ctx) &&
    -- Check if this tx was signed by the Master specified in the redeemer.
    traceIfFalse "Validate Master Fund And Merge Pool Message: Redeemer Master's signature missing" (signedByMaster rmfampMaster info) &&

    traceIfFalse "Validate Master Fund And Merge Pool Message: Output NEW PoolState Datum wrong, not matching provided redeemer" (correctOutputPoolState_Datum rmfampPoolNFT rmfampMaster rmfampFund  ctx)   &&

    traceIfFalse "Validate Master Fund And Merge Pool Message: Output Value Error, not matching PoolState Datums Funds" (correctOutputsPoolState_Datum_Values ctx) && 

    -- Check that the tx range interval of validity. Can't be infinitum and need to be lees than ppValidTimeRange Pool Param . 
    traceIfFalse "Validate Master Fund And Merge Pool Message: Can't Fund Pool, tx validity time range is not valid" (isValidRange pParams info) &&

    -- Check that the Pool Deadline is not reached. That means that the Pool is still alive. 
    traceIfFalse "Validate Master Fund And Merge Pool Message: Can't Fund Pool, Deadline already passed" (deadlinePoolParamNotReached pParams info)
    
  where 

    info :: TxInfo
    info = scriptContextTxInfo ctx

    inputsBeingConsumed = rmfampUtxoToMerge 

    inputsWithPoolState :: [(TxOutRef,TxOut,PoolStateTypo)]
    inputsWithPoolState =  do
        let 
            inputsWithPoolState = getInputsWithPoolState ctx
            inputsWithPoolState_UtxoRef = getInput_TxOutRef <$> inputsWithPoolState

        if HASKELL.all (==True) [ input `elem` inputsWithPoolState_UtxoRef  | input <- inputsBeingConsumed] &&
         HASKELL.all (==True) [ input `elem` inputsBeingConsumed  | input <- inputsWithPoolState_UtxoRef] then
            inputsWithPoolState
        else
            traceError "Validate Master Fund And Merge Pool Message: Not Matching redeemer UtxoRef to merge with inputs."  

    inputWithPoolStateAndPoolNFT :: (TxOutRef,TxOut,PoolStateTypo)
    inputWithPoolStateAndPoolNFT =  case getInputWithPoolStateAndPoolNFT inputsWithPoolState rmfampPoolNFT of
        Just inputWithPoolStateAndPoolNFT -> inputWithPoolStateAndPoolNFT
        Nothing -> traceError "Validate Master Fund And Merge Pool Message: Can't find Pool NFT on Inputs."  

    outputWithPoolState :: (TxOut,PoolStateTypo)
    outputWithPoolState =  do
        case getOutputsWithPoolState  ctx of
            [(x,y)] -> (x,y)
            _ -> traceError "Validate Master Fund And Merge Pool Message: Expected exactly one output with the PoolState Datum."  

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
    
    isPoolNFTInSomeOutputWithPoolState :: PoolNFT ->  ScriptContext -> Bool
    isPoolNFTInSomeOutputWithPoolState  redeemerPoolNFT ctx  = do
        OnChainHelpers.isPoolNFTInSomeOutputWithPoolState [outputWithPoolState] redeemerPoolNFT ctx

    signedByOutputPoolStateMasterFunder :: TxInfo -> ScriptContext-> Bool
    signedByOutputPoolStateMasterFunder info ctx  = do
        
        let 
            (_, outputWithPoolState_Datum) = outputWithPoolState

        signedByPoolStateMasterFunder outputWithPoolState_Datum info

    correctOutputPoolState_Datum :: PoolNFT -> Master -> Fund -> ScriptContext -> Bool
    correctOutputPoolState_Datum  redeemerPoolNFT redeemerMaster redeemerFund ctx = do
        let
            (_, outputWithPoolState_Datum) = outputWithPoolState

            inputWithPoolStateAndPoolNFT_Datum = getInput_Datum inputWithPoolStateAndPoolNFT

            inputsWithPoolState_Datums = getInput_Datum <$> inputsWithPoolState

            countTotalUtxoWithPoolState = psCountTotalUtxoWithPoolState inputWithPoolStateAndPoolNFT_Datum - PlutusTx.Prelude.length inputsWithPoolState + 1 

            newOutputDatum = mkPoolStateWithNewFundFromPoolStateList inputsWithPoolState_Datums redeemerPoolNFT redeemerMaster redeemerFund countTotalUtxoWithPoolState

        newOutputDatum == PoolState outputWithPoolState_Datum

    correctOutputPoolState_Datum_Value ::  (TxOut,PoolStateTypo) -> Bool
    correctOutputPoolState_Datum_Value   (outputWithPoolState_TxOut, outputWithPoolState_Datum)  = do    
        let  
            
            masterFunders = psMasterFunders outputWithPoolState_Datum 

            sumFunds = PlutusTx.Prelude.sum $ mfFund <$> masterFunders

            valueFund  = Ada.lovelaceValueOf sumFunds <> negate (Ada.lovelaceValueOf $ psChashedOut outputWithPoolState_Datum)

        Ada.fromValue valueFund == Ada.fromValue (txOutValue outputWithPoolState_TxOut)

    correctOutputsPoolState_Datum_Values ::  ScriptContext -> Bool
    correctOutputsPoolState_Datum_Values   ctx  = do
        --correctOutputPoolState_Datum_Value outputWithPoolState
        True


{-# INLINABLE validateMasterGetPool #-}
validateMasterGetPool :: PoolParams -> PoolStateTypo -> RedeemMasterGetPoolTypo -> ScriptContext -> Bool
validateMasterGetPool pParams dPoolStateFromInputBeingValidated RedeemMasterGetPoolTypo{..}  ctx  =
    True
--     -- TODO: 

--     -- tiene como entrada varias PoolState y de salida una sola Pool State con la suma de todos los funds y el VALUE tambien la suma menos lo recuperado

--     traceIfFalse "Validate Master Get Fund Pool Message: Wrong Input" correctInputs &&

--     traceIfFalse "Validate Master Get Fund Pool Message: Wrong Outpus" correctOutputs && 

--     -- Verificar que solo modifique el valor del inversor que esta firmando esta transacion, o sea que no pueda moficiar el valor de otro inversor
--     -- traceIfFalse "Validate Master Fund Pool Message: Can't change other Master's Fund" (changingOnlyMyselfFund redeemerMaster ctx) &&

--     -- Check if the redeemerPoolNFT was minted with the right minting policy.
--     traceIfFalse "Validate Master Get Fund Pool Message: Wrong NFT Minting Policy " (correctMintigPolicyUsedForNFT pParams rmgpPoolNFT  ) &&

--     -- Check if the redeemerPoolNFT is included in the value of the Own's Single Output comming to the script. 
--     -- The redeemerPoolNFT is going to be holded in the script until the Pool is alive.
--     -- es importante que este el NFT para asegurarme que tengo la version mas actualizada de la cantidad de funds
--     -- ademas luego tendre que verificar que esten todos los utxo aqui antes de hacer el get y unirlos
--     traceIfFalse "Validate Master Fund Pool Message: Redeemer Pool NFT not found in Own's Output Value" (isPoolNFTInSomeOutputWithPoolState rmgpPoolNFT ctx) &&

--     -- Check if the redeemerPoolNFT is the same that identifies the Pool in the PoolParams.
--     traceIfFalse "Validate Master Get Fund Pool Message: Pool Param's NFT dosen't matches Redeemer Pool NFT" (isPoolNFTParam pParams rmgpPoolNFT ) &&
--     -- Check if the redeemerPoolNFT is the same in all the inputs PoolState Datums
--     traceIfFalse "Validate Master Get Fund Pool Message: Input PoolState Datum's NFTs dosen't matches Redeemer Pool NFT" (isPoolNFTDatum rmgpPoolNFT ctx ) &&

--     -- Check if this tx was signed by any of the Masters included in the PoolParams. 
--     -- This are all the Masters that can interact with the script at any time. Nobody else will be able to change or redeem the funds.
--     traceIfFalse "Validate Master Get Fund Pool Message: Pool Params Master's signature missing" (signedByPoolParamMaster pParams info) &&
--     -- Check if this tx was signed by any of the Masters included in the PoolState Datum produced by this tx. 
--     -- This datum includes all the Masters already interacted with the script. Are all the ones who funded the Pool.
--     traceIfFalse "Validate Master Get Fund Pool Message: Output PoolState Datum Master's signature missing" (signedByOutputPoolStateMasterFunder info ctx) &&
--     -- Check if this tx was signed by the Master specified in the redeemer.
--     traceIfFalse "Validate Master Get Fund Pool Message: Redeemer Master's signature missing" (signedByMaster rmgpMaster info) &&

--     traceIfFalse "Validate Master Get Fund Pool Message: Output PoolState Datum wrong, not matching provided redeemer" (correctOutputPoolState_Datum rmgpPoolNFT rmgpMaster rmgpGetFund  ctx)   &&

--     traceIfFalse "Validate Master Get Fund Pool Message: Output Value Error, not matching PoolState Datums Funds" (correctOutputsPoolState_Datum_Values ctx) && 

--     -- Check that the tx range interval of validity. Can't be infinitum and need to be lees than ppValidTimeRange Pool Param . 
--     traceIfFalse "Validate Master Get Fund Pool Message: Can't Fund Pool, tx validity time range is not valid" (isValidRange pParams info) &&

--     -- Check that the Pool Deadline is not reached. That means that the Pool is still alive. 
--     traceIfFalse "Validate Master Get Fund Pool Message: Can't Fund Pool, Deadline already passed" (deadlinePoolParamNotReached pParams info)
    
--   where 

--     info :: TxInfo
--     info = scriptContextTxInfo ctx

--     -- En esta nueva version, puedo querer unir varios PoolState Datums en uno solo.
--     -- Tendrá entocnes muchas entradas de OLD Pool State Datums y como salida tendrá una Sola con el Datum que une a todos esos y suma el nuevo Fund

--     inputsBeingConsumed :: [TxOutRef]
--     inputsBeingConsumed = rmgpUtxoToMerge

--     inputsWithPoolState :: [(TxOutRef,TxOut,PoolStateTypo)]
--     inputsWithPoolState =  do
--         let 
--             inputsWithPoolState = getInputsWithPoolState ctx
--             inputsWithPoolState_UtxoRef = getInput_TxOutRef <$> inputsWithPoolState

--         if HASKELL.all (==True) [ input `elem` inputsWithPoolState_UtxoRef  | input <- inputsBeingConsumed] &&
--          HASKELL.all (==True) [ input `elem` inputsBeingConsumed  | input <- inputsWithPoolState_UtxoRef] then
--             inputsWithPoolState
--         else
--             traceError "Validate Master Get Fund Pool Message: Not Matching redeemer UtxoRef to merge with inputs."  

--     outputWithPoolState :: (TxOut,PoolStateTypo)
--     outputWithPoolState =  do
--         case getOutputsWithPoolState  ctx of
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
    
--     isPoolNFTInSomeOutputWithPoolState :: PoolNFT ->  ScriptContext -> Bool
--     isPoolNFTInSomeOutputWithPoolState  redeemerPoolNFT ctx  = do
--         OnChainHelpers.isPoolNFTInSomeOutputWithPoolState [outputWithPoolState] redeemerPoolNFT ctx
        
--     signedByOutputPoolStateMasterFunder :: TxInfo -> ScriptContext-> Bool
--     signedByOutputPoolStateMasterFunder info ctx  = do
        
--         let 
--             (_, outputWithPoolState_Datum) = outputWithPoolState

--         signedByPoolStateMasterFunder outputWithPoolState_Datum info

--     correctOutputPoolState_Datum :: PoolNFT -> Master -> Fund -> ScriptContext -> Bool
--     correctOutputPoolState_Datum  redeemerPoolNFT redeemerMaster redeemerFund ctx = do
--         let
--             (_, outputWithPoolState_Datum) = outputWithPoolState

--             inputsWithPoolState_Datums = getInput_Datum <$> inputsWithPoolState

--             newOutputDatum = mkPoolStateWithNewFundFromPoolStateList inputsWithPoolState_Datums redeemerPoolNFT redeemerMaster redeemerFund

--         newOutputDatum == PoolState outputWithPoolState_Datum

--     correctOutputPoolState_Datum_Value ::  (TxOut,PoolStateTypo) -> Bool
--     correctOutputPoolState_Datum_Value   (outputWithPoolState_TxOut, outputWithPoolState_Datum)  = do    
--         let  
            
--             masterFunders = psMasterFunders outputWithPoolState_Datum 

--             sumFunds = PlutusTx.Prelude.sum $ mfFund <$> masterFunders

--             valueFund  = Ada.lovelaceValueOf sumFunds <> negate (Ada.lovelaceValueOf $ psChashedOut outputWithPoolState_Datum)

--         Ada.fromValue valueFund == Ada.fromValue (txOutValue outputWithPoolState_TxOut)

--     correctOutputsPoolState_Datum_Values ::  ScriptContext -> Bool
--     correctOutputsPoolState_Datum_Values   ctx  = do

--         correctOutputPoolState_Datum_Value outputWithPoolState
    

{-# INLINABLE validateUserInvest #-}
validateUserInvest :: PoolParams -> PoolStateTypo ->  RedeemUserInvestTypo -> ScriptContext -> Bool
validateUserInvest pParams dPoolStateFromInputBeingValidated RedeemUserInvestTypo{..} ctx  =
       
    -- TODO: 

    -- necesito controlar que de entrada tenga un PoolState Datum y de salida el mismo PoolState Datum mas el usuario agregado y un nuevo UserState Datum

    traceIfFalse "Validate User Invest Message: Wrong Input" correctInputs && 

    traceIfFalse "Validate User Invest Message: Wrong Outpus" correctOutputs && 

    -- Chekear que se agrega el redeemerUserNFT a la lista de userNFT en PoolState Datum de salida.
    -- Que la salida con PoolState Datum sea unica y tenga el valor sumado de todas las PoolState de entrada

    -- Que redeemerInvest sea mayor que la minima inversion en pParams
    -- Que la fecha sea dentro de la deadline del Pool en pParams

    -- que el redeemerUserNFT creado este en la salida que vuelve al usuario y valueOf == 1

    -- Check if the redeemerPoolNFT was minted with the right minting policy.
    traceIfFalse "Validate User Invest Message: Wrong NFT Minting Policy " (correctMintigPolicyUsedForNFT pParams ruiPoolNFT  ) &&
    -- Check que la txtoutRef usada en el redeemerUserNFT este como entrada en el script, para asegurarme que es gastada  
    traceIfFalse "Validate User Invest Message: Redeemer TxOutRef used for NFT not found in Inputs" (hasInputUTxO info ruiUserNFTTxOutRef ) &&
    -- Check if the redeemerUserNFT was minted with the right minting policy
    traceIfFalse "Validate User Invest Message: Wrong NFT Minting Policy " (correctMintigPolicyUsedForNFT pParams ruiUserNFT  ) &&
    -- Check if the redeemerUserNFT was minted with the right quantity, just 1.
    -- TODO: this is not really needed, because the minting policy checks that too and it checked before if this NFT was minted with that specific minting policy.
    traceIfFalse "Validate User Invest Message: User NFT is not valid. Wrong Minted quantity, must be equal 1" (isMintedNFTValid info ruiUserNFT ) &&
    -- Check if the redeemerUserNFT is going to the user wallet. He must keep it in order to claim rewards from the invest. 
    -- He can give it away to other user to seel the benefitis.
    -- TODO: this is not really needed, because all minted value are going to somewhere after all. If the tx was built in incorrects ways is not my problem.
    traceIfFalse "Validate User Invest Message: Redeemer User NFT not found in User's Output Value" (isUserNFTInUserOutputValue ruiUserNFT ruiUser info)  &&
    -- Check if the redeemerPoolNFT is the same that identifies the Pool in the PoolParams.
    traceIfFalse "Validate User Invest Message: Pool Param's NFT dosen't matches Redeemer Pool NFT" (isPoolNFTParam pParams ruiPoolNFT ) &&
    -- Check if the redeemerPoolNFT is the same in all the inputs PoolState Datums
    traceIfFalse "Validate User Invest Message: Input PoolState Datum's NFTs dosen't matches Redeemer Pool NFT" (isPoolNFTDatum ruiPoolNFT ctx ) &&

    -- Check if this tx was signed by the User specified in the redeemerUser.
    traceIfFalse "Validate User Invest Message: Redeemer User's signature missing" (signedByUser ruiUser info) &&
    
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
    traceIfFalse "Validate User Invest Message: Can't Invest in Pool, tx validity time range is not valid" (isValidRange pParams info) &&

    -- Check if this the invest created at date is correct.
    traceIfFalse "Validate User Invest Message: Invest Created at Date is not Valid" (correctInvestCreatedAt ruiCreatedAt info) &&

    -- Check if this the invest deadline date is correct.
    traceIfFalse "Validate User Invest Message: Invest Deadline Date is not Valid" (correctInvestDeadlineAt pParams ruiCreatedAt ruiDeadline info) &&

    -- Check that the Pool Deadline is not reached. That means that the Pool is still alive. 
    traceIfFalse "Validate User Invest Message: Can't Invest in Pool, Deadline already passed" (deadlinePoolParamNotReached pParams info)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- necesito controlar que de entrada tenga un PoolState Datum y de salida el mismo PoolState Datum mas el usuario agregado y un nuevo UserState Datum
    

    inputWithPoolState :: (TxOutRef,TxOut,PoolStateTypo)
    inputWithPoolState =  do
        let
            inputsWithPoolState = getInputsWithPoolState ctx
        
        case inputsWithPoolState of 
            [inputWithPoolState] -> 
                --dPoolStateFromInputBeingValidated == getInput_Datum x
                inputWithPoolState
            _ -> traceError "Validate User Invest Message: Error. Found more than one Input with PoolState Datum" 


    outputWithPoolState :: (TxOut,PoolStateTypo)
    outputWithPoolState =  do
        case getOutputsWithPoolState  ctx of
            [(x,y)] -> (x,y)
            _ -> traceError "Validate User Invest Message: Expected exactly one output with the PoolState Datum."  

    outputWithUserState :: (TxOut,UserStateTypo)
    outputWithUserState =  do
        case getOutputsWithUserState  ctx of
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
                    (_,_) ->  True    
                    _ -> False
            _ -> False
  
    signedByOutputsUserStateUser :: TxInfo -> ScriptContext-> Bool
    signedByOutputsUserStateUser info ctx  = do
        
        let 
            (_, outputUserState_Datum) = outputWithUserState

        signedByUserStateUser outputUserState_Datum info


    correctOutputsPoolState_Datum_WithNewUser :: PoolNFT -> UserNFT ->  ScriptContext -> Bool
    correctOutputsPoolState_Datum_WithNewUser  redeemerPoolNFT redeemerUserNFT ctx = do
        let
            (_, outputWithPoolState_Datum) = outputWithPoolState

            inputWithPoolState_Datum = getInput_Datum inputWithPoolState

            newOutputDatum = mkPoolStateWithNewUserInvestFromPoolState inputWithPoolState_Datum redeemerPoolNFT redeemerUserNFT

        newOutputDatum == PoolState outputWithPoolState_Datum

    correctOutputsUserState_Datum_OfNewUser ::  User -> UserNFT ->  Invest -> POSIXTime ->  Deadline ->  ScriptContext -> Bool
    correctOutputsUserState_Datum_OfNewUser  redeemerUser redeemerUserNFT redeemerInvest redeemerCreatedAt redeemerDeadline ctx = do
        let
            (_, outputUserState_Datum) = outputWithUserState

            newOutputDatum = mkUserState  redeemerUser redeemerUserNFT redeemerInvest  redeemerCreatedAt redeemerDeadline 0 0 Nothing

        newOutputDatum == UserState outputUserState_Datum

    correctOutputPoolState_Value_WithSameValue ::  ScriptContext -> Bool
    correctOutputPoolState_Value_WithSameValue ctx = do
        let
            (outputWithPoolState_TxOut, _) = outputWithPoolState

            inputWithPoolState_TxOut = getInput_TxOut  inputWithPoolState
            inputWithPoolState_Value = txOutValue inputWithPoolState_TxOut

        inputWithPoolState_Value == txOutValue outputWithPoolState_TxOut

    correctOutputUserState_Value_OfNewUser ::  Invest -> ScriptContext -> Bool
    correctOutputUserState_Value_OfNewUser redeemerInvest ctx = do
        let
            (outputWithUserState_TxOut, _) = outputWithUserState

            valueTotal = Ada.lovelaceValueOf redeemerInvest

        valueTotal == txOutValue outputWithUserState_TxOut   

    correctOutputPoolState_ValueAndDatum ::  (TxOut,PoolStateTypo) -> Bool
    correctOutputPoolState_ValueAndDatum   (outputWithPoolState_TxOut, outputWithPoolState_Datum)  = do    
        let  
            
            masterFunders = psMasterFunders outputWithPoolState_Datum 

            sumFunds = PlutusTx.Prelude.sum $ mfFund <$> masterFunders

            valueFund  = Ada.lovelaceValueOf sumFunds <> negate (Ada.lovelaceValueOf $ psChashedOut outputWithPoolState_Datum)

        Ada.fromValue valueFund == Ada.fromValue (txOutValue outputWithPoolState_TxOut)

    correctOutputsPoolState_ValuesAndDatums ::  ScriptContext -> Bool
    correctOutputsPoolState_ValuesAndDatums   ctx  = do

        correctOutputPoolState_ValueAndDatum outputWithPoolState



{-# INLINABLE validateUserGetInvest #-}
validateUserGetInvest :: PoolParams -> UserStateTypo -> RedeemUserGetInvestTypo -> ScriptContext -> Bool
validateUserGetInvest pParams dPoolStateFromInputBeingValidated RedeemUserGetInvestTypo{..} ctx  =
    True
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx


{-# INLINABLE validateUserGetRewards #-}

validateUserGetRewards :: PoolParams -> ValidatorDatum -> RedeemUserGetRewardsTypo -> ScriptContext -> Bool
validateUserGetRewards pParams _ RedeemUserGetRewardsTypo{..} ctx  =

    -- TODO: 

    -- de entrada tengo uno o varios PoolState de donde voy a sacar dinero y un UserState
    -- de salida tengo un la misma cantidad de  PoolState con el Value menos el claim Value y con el campo de CashedOut actualizado
    -- y un UserState con mismo Value pero con cashedout y claimedat actualizados

    traceIfFalse "Validate User Get Rewards Message: Wrong Input" correctInputs && 

    traceIfFalse "Validate User Get Rewards Message: Wrong Outpus" correctOutputs && 

    -- Check if the redeemerPoolNFT was minted with the right minting policy.
    traceIfFalse "Validate User Get Rewards Message: Wrong Pool NFT Minting Policy " (correctMintigPolicyUsedForNFT pParams rugrPoolNFT  ) &&
    -- Check if the redeemerUserNFT was minted with the right minting policy
    traceIfFalse "Validate User Get Rewards Message: Wrong User NFT Minting Policy " (correctMintigPolicyUsedForNFT pParams rugrUserNFT  ) &&
    -- Check if the redeemerUserNFT is going to the user wallet. He must keep it in order to claim rewards from the invest. 
    -- He can give it away to other user to seel the benefitis.
    traceIfFalse "Validate User Get Rewards Message: Redeemer User NFT not found in User's Output Value" (isUserNFTInUserOutputValue rugrUserNFT rugrUser info)  &&
    -- Check if the redeemerPoolNFT is the same that identifies the Pool in the PoolParams.
    traceIfFalse "Validate User Get Rewards Message: Pool Param's NFT dosen't matches Redeemer Pool NFT" (isPoolNFTParam pParams rugrPoolNFT ) &&

    -- Check if the redeemerPoolNFT is the same in all the inputs PoolState Datums
    traceIfFalse "Validate User Get Rewards Message: Input PoolState Datum's NFTs dosen't matches Redeemer Pool NFT" (isPoolNFTDatum rugrPoolNFT ctx ) &&

    -- Check if the redeemerPoolNFT is the same in all the inputs UserState Datums
    traceIfFalse "Validate User Get Rewards Message: Input UserState Datum's NFTs dosen't matches Redeemer User NFT" (isUserNFTDatum rugrUserNFT ctx ) &&
    
    -- -- Check if there is one single input UserState Datum to redeem
    -- traceIfFalse "Validate User Get Rewards Message: Need to use one single UserState Input" (isUnicSingleUserStateInput ctx ) &&
    
    -- Check if this tx was signed by the User specified in the redeemerUser.
    traceIfFalse "Validate User Get Rewards Message: Redeemer User's signature missing" (signedByUser rugrUser info) &&
    
    -- Check if this tx was signed by User in the UserState Datum produced by this tx. 
    -- Inside is going to fail if the outputs at the script aren't just two, one with the PoolState Datum and the other with the UserStateDatum.
    traceIfFalse "Validate User Get Rewards Message: Output UserState Datum User's signature missing" (signedByOutputsUserStateUser info ctx) &&

     -- Check if the user already invested in the Pool
    traceIfFalse "Validate User Get Rewards Message: Redeemer User NFT not found in PoolState Datum UserNFTs" (isUserNFTInPoolState rugrPoolNFT rugrUserNFT  ctx)   &&

    -- Check if this the claim is more than the minimun claim.
    traceIfFalse "Validate User Get Rewards Message: Amount to withdraw does not reach minimum." (isMoreThanMinimunClaim pParams rugrClaim) &&

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
    traceIfFalse "Validate User Get Rewards Message: Can't Invest in Pool, tx validity time range is not valid" (isValidRange pParams info) &&

  
    -- Check if this the claim date at is correct.
    traceIfFalse "Validate User Get Rewards Message: Claim DateAt is not Valid" (correctClaimDateAt rugrClaimAt info) &&

   
    -- Check that the Pool Deadline is not reached. That means that the Pool is still alive. 
    traceIfFalse "Validate User Get Rewards Message: Can't Get Rewatrds, Deadline already passed" (deadlinePoolParamNotReached pParams info)

  where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    inputsWithPoolState :: [(TxOutRef,TxOut,PoolStateTypo)]
    inputsWithPoolState =  do
        let
            inputsWithPoolState = getInputsWithPoolState ctx

        case inputsWithPoolState of 
            [] -> traceError "Validate User Get Rewards Message: Error. Not Found Input with PoolState Datum" 
            _ -> do
            
                let

                    -- for finding inputs PoolState where the UserNFT was register in
                    findInputPoolStateWithUserNFT inputsWithPoolState userNFT =
                        case [  inputWithPoolState  | inputWithPoolState <- inputsWithPoolState, userNFT `elem` psUsersNFT (getInput_Datum inputWithPoolState)  ] of
                            [inputWithPoolState] -> inputWithPoolState
                            _ -> traceError "Validate User Get Rewards Message: Cant' Get Rewards from because can't find single PoolState utxo with UserNFT register"

                    -- find input PoolState where the UserNFT was register in
                    inputWithPoolStateWithUserNFT = findInputPoolStateWithUserNFT inputsWithPoolState rugrUserNFT

                
                    -- get the list of inputs PoolState without the one it just found
                    inputsWithPoolStateWithoutUserNFT = removeInputByTxOutRef (getInput_TxOutRef inputWithPoolStateWithUserNFT) inputsWithPoolState

                    -- for ordering the list of inputs by value
                    compareValueOfInputs ::  (TxOutRef,TxOut,PoolStateTypo)-> (TxOutRef,TxOut,PoolStateTypo) -> Ordering
                    compareValueOfInputs inputs1 inputs2
                        | (Ada.fromValue ( txOutValue (getInput_TxOut inputs1)) > (Ada.fromValue  ( txOutValue ( getInput_TxOut inputs2) ))) = LT
                        | otherwise = GT


                     -- order the list of inputs by value
                    inputsOrderedByValueWithPoolState = sortBy compareValueOfInputs inputsWithPoolStateWithoutUserNFT

                    getInputsWithEnoughValueToClaim :: [(TxOutRef,TxOut,PoolStateTypo)] -> Proffit -> [(TxOutRef,TxOut,PoolStateTypo)]
                    getInputsWithEnoughValueToClaim inputs claim = do
                    
                        if claim >0 then do
                            if PlutusTx.Prelude.length inputs ==0 then 
                                traceError "Validate User Get Rewards Message: Not enough inputs to cover the claim"
                            else do
                                -- there is remaining claim to get, so i keep adding inputs
                                let
                                    input = head inputs

                                    -- cant take all the value, i need to leave the minumum ada
    
                                    value = txOutValue (getInput_TxOut input)
                                    valueCanUse    = value <> negate (Ada.lovelaceValueOf minLovelace)
                                    adaFromValueCanUse = Ada.getLovelace (Ada.fromValue valueCanUse) 

                                    newClaim = claim - adaFromValueCanUse

                                input : getInputsWithEnoughValueToClaim (tail inputs) newClaim
                        else
                            -- there is no more remaining claim to get
                            []      

                    -- the input with the UserNFT must be included beyond its value
                    -- anyways I need to consider its value
                    -- the new claim is the result of substracting the value of this input to the original claimed valueOf
        
                    valueInputWithPoolStateWithUserNFT = txOutValue (getInput_TxOut  inputWithPoolStateWithUserNFT)
                    valueCanUse    = valueInputWithPoolStateWithUserNFT <> negate (Ada.lovelaceValueOf minLovelace)
                    adaFromValueCanUse = Ada.getLovelace (Ada.fromValue valueCanUse) 
                    -- cant take all the value, need to leave minumun ada: <> negate Ada.lovelaceValueOf minLovelace
                    claim = rugrClaim - adaFromValueCanUse
                    -- The resulting list of inputs is including in the firt the input with the userNFT and the rest is a calling getInputsWithEnoughValueToClaim wo find the minumun listo of inputs wich can cover the payment.
                    inputsWithPoolStateWithEnoughValueToClaim = inputWithPoolStateWithUserNFT : getInputsWithEnoughValueToClaim inputsOrderedByValueWithPoolState claim

                if PlutusTx.Prelude.length inputsWithPoolStateWithEnoughValueToClaim == PlutusTx.Prelude.length inputsWithPoolState then
                    inputsWithPoolStateWithEnoughValueToClaim
                else
                    traceError "Validate User Get Rewards Message: Error. Using too many Inputs"      

    inputWithPoolStateAndPoolNFT :: Maybe (TxOutRef,TxOut,PoolStateTypo)
    inputWithPoolStateAndPoolNFT =  getInputWithPoolStateAndPoolNFT inputsWithPoolState rugrPoolNFT 

    inputWithUserState :: (TxOutRef,TxOut,UserStateTypo)
    inputWithUserState =  do
        let 
            inputsWithUserState = getInputsWithUserState ctx

        case inputsWithUserState of
            [x] -> x
            _ -> traceError "Validate User Get Rewards Message: Expected exactly one input with the UserState Datum."  

    outputsWithPoolState :: [(TxOut,PoolStateTypo)]
    outputsWithPoolState =  do
        let 
            outputsWithPoolState = getOutputsWithPoolState  ctx
        if PlutusTx.Prelude.length outputsWithPoolState == PlutusTx.Prelude.length inputsWithPoolState then do

            let
                -- for ordering the outputs
                compareValueOfOutpus ::  (TxOut,PoolStateTypo)-> (TxOut,PoolStateTypo) -> Ordering
                compareValueOfOutpus output1 output2

                    | (Ada.fromValue ( txOutValue (fst output1)) > Ada.fromValue  ( txOutValue ( fst output2)  )) = LT
                    | otherwise = GT

                -- ordering the outputs by value
                outputsOrderedByValueWithPoolState = sortBy compareValueOfOutpus outputsWithPoolState
                
                -- count elements to satisfy condition
                countBy :: (a -> Bool) -> [a] -> Integer
                countBy cond xs =
                    PlutusTx.Prelude.foldr          -- Think of this as a for loop
                        body       -- The body of our for loop
                        0          -- The initial value for cnt
                        xs         -- The list to iterate over
                    where
                        body elem currentCnt =
                            if cond elem
                                then currentCnt + 1
                                else currentCnt

                getAdaValueOf :: Ledger.Value -> Integer
                getAdaValueOf value = Ada.getLovelace (Ada.fromValue value)

                cond :: (TxOut,PoolStateTypo) -> Bool
                cond (txOut, _) = getAdaValueOf ( txOutValue txOut) >  minLovelace 
                                
            if  countBy cond outputsOrderedByValueWithPoolState <=1 then
                -- count the outputs which have value greater than the minimun
                -- need to be only one... i use the minumun quiantity of inputs i need to cover the claim, 
                -- and then all the inputs i use need to be in minumun ada, just the last one can have some remaining value.
                outputsOrderedByValueWithPoolState
                
            else
                traceError "Validate User Get Rewards Message: Expected exactly one Pool State output with Value greater than minLovelace"  
        else
            traceError "Validate User Get Rewards Message: Expected exactly the same quantity of outputs than inputs with the PoolState Datum."  


    outputWithUserState :: (TxOut,UserStateTypo)
    outputWithUserState =  do
        case getOutputsWithUserState  ctx of
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
                    (_,_) ->  True    
                    _ -> traceError "Validate User Get Rewards Message: wrong outputWithUserState."  
            [] -> traceError "Validate User Get Rewards Message: wrong outputsWithPoolState." 
  
    signedByOutputsUserStateUser :: TxInfo -> ScriptContext-> Bool
    signedByOutputsUserStateUser info ctx  = do
        
        let 
            (_, outputUserState_Datum) = outputWithUserState

        signedByUserStateUser outputUserState_Datum info


    isUserNFTInPoolState :: PoolNFT-> UserNFT -> ScriptContext-> Bool
    isUserNFTInPoolState rugrPoolNFT rugrUserNFT  ctx = do
        
        let 
            inputsWithPoolState_Datum = getInput_Datum <$> inputsWithPoolState

            newDummyDatum = mkDummyPoolStateFromPoolStateList inputsWithPoolState_Datum rugrPoolNFT          


        rugrUserNFT `elem` psUsersNFT (fromJust $ getPoolStateFromDatum newDummyDatum)

        --HASKELL.any (==rugrUserNFT) (psUsersNFT newOutputDatum)
    
    correctClaimValue :: PoolParams ->  Proffit ->  POSIXTime   -> ScriptContext -> Bool
    correctClaimValue pParams rugrClaim rugrClaimAt ctx = do
        let
            (_, outputUserState_Datum) = outputWithUserState

            inputWithUserState_Datum = getInput_Datum inputWithUserState

            rewards = getRewardsPerInvest (usLastClaimAt inputWithUserState_Datum) rugrClaimAt (usCreatedAt  inputWithUserState_Datum )  (usInvest inputWithUserState_Datum ) 
            totalNewRewards = rewards  + usRewardsNotClaimed inputWithUserState_Datum

        rugrClaim >= ppMinimunClaim pParams && rugrClaim <= totalNewRewards

    -- Check if Pool NFT is present in some input and if it is present check if in the uotput where the NFT is, the value of count funds is the same.
    correctOutputWithNFTPoolState_Datum  ::  PoolNFT ->  ScriptContext -> Bool
    correctOutputWithNFTPoolState_Datum redeemerPoolNFT ctx  = do
        case inputWithPoolStateAndPoolNFT of
            Nothing -> True
            Just inputWithPoolStateAndPoolNFT -> do
                let 
                    outputWithPoolStateAndPoolNFT = getOutputWithPoolStateAndPoolNFT outputsWithPoolState rugrPoolNFT

                case outputWithPoolStateAndPoolNFT of
                    Nothing -> traceError "Validate User Get Rewards Message: Not found NFT in output Pool State."  
                    Just outputWithPoolStateAndPoolNFT -> do
                        let 
                            inputWithPoolStateAndPoolNFT_Datum = getInput_Datum inputWithPoolStateAndPoolNFT
                            outputWithPoolStateAndPoolNFT_Datum = snd outputWithPoolStateAndPoolNFT
                
                        psCountTotalUtxoWithPoolState inputWithPoolStateAndPoolNFT_Datum == psCountTotalUtxoWithPoolState outputWithPoolStateAndPoolNFT_Datum

    correctOutputPoolState_DatumAndValue ::  (TxOut,PoolStateTypo) ->  Bool
    correctOutputPoolState_DatumAndValue outputWithPoolState  = do
        let
            outputWithPoolState_Datum = snd outputWithPoolState
        
        Ada.getLovelace ( Ada.fromValue (txOutValue (fst outputWithPoolState) <>  Ada.lovelaceValueOf (psChashedOut outputWithPoolState_Datum) ))  == PlutusTx.Prelude.sum [ mfFund psMasterFunder | psMasterFunder <-  psMasterFunders outputWithPoolState_Datum]

    correctOutputsPoolState_DatumAndValues ::   ScriptContext -> Bool
    correctOutputsPoolState_DatumAndValues ctx  = 
            HASKELL.all correctOutputPoolState_DatumAndValue outputsWithPoolState



    correctOutputsPoolState_Datums_WithNewClaimRewards ::  PoolNFT -> Proffit -> ScriptContext -> Bool
    correctOutputsPoolState_Datums_WithNewClaimRewards rugrPoolNFT rugrClaim ctx = do
        let 
            inputsWithPoolState_Datums = getInput_Datum <$> inputsWithPoolState

            outputsWithPoolState_Datums = snd <$> outputsWithPoolState

            newDummyOutputDatum = mkDummyPoolStateFromPoolStateList outputsWithPoolState_Datums rugrPoolNFT          

            newDummyInputDatum = mkDummyPoolStateWithNewClaimRewardsFromPoolStateList inputsWithPoolState_Datums rugrPoolNFT rugrClaim    

        newDummyInputDatum == newDummyOutputDatum



    correctOutputsUserState_Datum_WithNewClaimRewards :: User -> UserNFT -> Proffit ->  POSIXTime   -> ScriptContext -> Bool
    correctOutputsUserState_Datum_WithNewClaimRewards redeemerUser redeemerUserNFT rugrClaim rugrClaimAt ctx = do
        let
            (_, outputUserState_Datum) = outputWithUserState

            inputWithUserState_Datum = getInput_Datum inputWithUserState

            rewards = getRewardsPerInvest (usLastClaimAt inputWithUserState_Datum) rugrClaimAt  (usCreatedAt  inputWithUserState_Datum )  (usInvest inputWithUserState_Datum ) 
            totalNewRewards = rewards  + usRewardsNotClaimed inputWithUserState_Datum
            rewardsNotClaimed = totalNewRewards - rugrClaim
            totalRewardsCashedOut = usChashedOut inputWithUserState_Datum + rugrClaim 

            newOutputDatum = mkUserState redeemerUser redeemerUserNFT 
                            (usInvest inputWithUserState_Datum ) 
                            (usCreatedAt  inputWithUserState_Datum ) 
                            (usDeadline  inputWithUserState_Datum )  
                            totalRewardsCashedOut
                            rewardsNotClaimed
                            (Just rugrClaimAt)

        newOutputDatum == UserState outputUserState_Datum

    {-Check if the Output Value with PoolState Datum is the same that in the Inputs with PoolSate Datum less the rewards claimed -}
    correctPoolState_Values_LessClaimRewards ::  Proffit -> ScriptContext -> Bool
    correctPoolState_Values_LessClaimRewards rugrClaim ctx = do
        let

            inputsWithPoolState_TxOut = getInput_TxOut <$> inputsWithPoolState

            inputsWithPoolState_Values = [ txOutValue inputTxOut | inputTxOut <- inputsWithPoolState_TxOut] 

            valueInputsTotal = HASKELL.foldl (<>)  (Ada.lovelaceValueOf 0) inputsWithPoolState_Values


            outputsWithPoolState_TxOut = fst <$> outputsWithPoolState

            outputsWithPoolState_Values = [ txOutValue outputTxOut | outputTxOut <- outputsWithPoolState_TxOut] 

            valueOutputsTotal = HASKELL.foldl (<>)  (Ada.lovelaceValueOf 0) outputsWithPoolState_Values


        valueInputsTotal == valueOutputsTotal <> Ada.lovelaceValueOf rugrClaim


    {-Check if the Output Value with UserState Datum is the same that the input UserState. -}
    correctOutputUserState_Value_WithNoChanges ::  ScriptContext -> Bool
    correctOutputUserState_Value_WithNoChanges ctx = do
        let
            (outputWithUserState_TxOut, _) = outputWithUserState

            inputWithUserState_TxOut  = getInput_TxOut inputWithUserState

            inputWithUserState_Value = txOutValue inputWithUserState_TxOut 

        inputWithUserState_Value == txOutValue outputWithUserState_TxOut



{-# INLINABLE validateUserInvestRewards #-}
validateUserInvestRewards :: PoolParams -> UserStateTypo -> RedeemUserInvestRewardsTypo -> ScriptContext -> Bool
validateUserInvestRewards pParams dPoolStateFromInputBeingValidated RedeemUserInvestRewardsTypo{..} ctx  =
    True
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx




typedValidator :: PoolParams -> Scripts.TypedValidator ValidatorScript
typedValidator  pParams = Scripts.mkTypedValidator @ValidatorScript  
    (
      $$(PlutusTx.compile [|| mkValidator ||]) 
      `PlutusTx.applyCode` 
      PlutusTx.liftCode pParams
    )
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator  @ValidatorDatum @ValidatorRedeemer



codeValidator :: PoolParams -> Validator
codeValidator pParams = Scripts.validatorScript $ typedValidator  pParams 

hashValidator :: PoolParams ->Ledger.ValidatorHash
hashValidator   pParams = Scripts.validatorHash  $ typedValidator  pParams 

addressValidator :: PoolParams -> Ledger.Address
addressValidator  pParams = scriptHashAddress $ hashValidator  pParams 
