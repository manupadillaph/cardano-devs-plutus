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

module Validators.StakeSimple.OnChain
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
import  Validators.StakeSimple.Typos 
import  Validators.StakeSimple.Helpers 
import  Validators.StakeSimple.OnChainHelpers 
import  Validators.StakeSimple.OnChainNFT     (mintingNFTPolicy)


data ValidatorScript
instance Scripts.ValidatorTypes ValidatorScript where
    type instance RedeemerType ValidatorScript = ValidatorRedeemer
    type instance DatumType ValidatorScript = ValidatorDatum


{-# INLINABLE mkValidator #-}
mkValidator :: PoolParams -> ValidatorDatum -> ValidatorRedeemer -> ScriptContext -> Bool

mkValidator pParams (PoolState dPoolState) (RedeemMasterFundPool redeemMasterFundPool) ctx =
    validateMasterFundPool pParams dPoolState redeemMasterFundPool ctx

mkValidator pParams (PoolState dPoolState) (RedeemMasterGetPool redeemMasterGetPool) ctx =
    validateMasterGetPool pParams dPoolState redeemMasterGetPool ctx

mkValidator pParams (PoolState dPoolState)  (RedeemUserInvest redeemUserInvest) ctx =
    validateUserInvest pParams dPoolState redeemUserInvest ctx


mkValidator pParams (UserState dUserState) (RedeemUserGetInvest redeemUserGetInvest) ctx =
    validateUserGetInvest pParams dUserState redeemUserGetInvest ctx


-- mkValidator pParams (UserState dUserState) (RedeemUserGetRewards redeemUserGetRewards) ctx =
--     validateUserGetRewards pParams dUserState redeemUserGetRewards ctx
-- mkValidator pParams (PoolState dPoolState)  (RedeemUserGetRewards redeemUserGetRewards) ctx =
--     validateUserGetRewards pParams dPoolState redeemUserGetRewards ctx
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
     traceIfFalse "Stake Pool Message: Invalid Operation" False 


{-# INLINABLE validateMasterFundPool #-}
validateMasterFundPool :: PoolParams -> PoolStateTypo -> RedeemMasterFundPoolTypo -> ScriptContext -> Bool
validateMasterFundPool pParams dPoolStateFromInputBeingValidated RedeemMasterFundPoolTypo{..}  ctx  =
    -- TODO: 

    -- Verificar que solo modifique el valor del inversor que esta firmando esta transacion, o sea que no pueda moficiar el valor de otro inversor
    -- traceIfFalse "Validate Master Fund Pool Message: Can't change other Master's Fund" (changingOnlyMyselfFund redeemerMaster ctx) &&

    -- Check if the redeemerPoolNFT was minted with the right minting policy.
    traceIfFalse "Validate Master Fund Pool Message: Wrong NFT Minting Policy " (correctMintigPolicyUsedForNFT pParams rmfpPoolNFT  ) &&
    -- Check if the redeemerPoolNFT is included in the value of the Own's Single Output comming to the script. 
    -- The redeemerPoolNFT is going to be holded in the script until the Pool is alive.
    traceIfFalse "Validate Master Fund Pool Message: Redeemer Pool NFT not found in Own's Single Output Value" (isPoolNFTInSingleOwnOutputValue rmfpPoolNFT ctx) &&
    -- Check if the redeemerPoolNFT is the same that identifies the Pool in the PoolParams.
    traceIfFalse "Validate Master Fund Pool Message: Pool Param's NFT dosen't matches Redeemer Pool NFT" (isPoolNFTParam pParams rmfpPoolNFT ) &&
    -- Check if the redeemerPoolNFT is the same in all the inputs PoolState Datums
    traceIfFalse "Validate Master Fund Message: Input PoolState Datum's NFTs dosen't matches Redeemer Pool NFT" (isPoolNFTDatum rmfpPoolNFT ctx ) &&

    -- Check if this tx was signed by any of the Masters included in the PoolParams. 
    -- This are all the Masters that can interact with the script at any time. Nobody else will be able to change or redeem the funds.
    traceIfFalse "Validate Master Fund Pool Message: Pool Params Master's signature missing" (signedByPoolParamMaster pParams info) &&
    -- Check if this tx was signed by any of the Masters included in the PoolState Datum produced by this tx. 
    -- This datum includes all the Masters already interacted with the script. Are all the ones who funded the Pool.
    traceIfFalse "Validate Master Fund Pool Message: Output PoolState Datum Master's signature missing" (signedBySingleOwnOutputPoolStateMasterFunder info ctx) &&
    -- Check if this tx was signed by the Master specified in the redeemer.
    traceIfFalse "Validate Master Fund Pool Message: Redeemer Master's signature missing" (signedByMaster rmfpMaster info) &&

    -- Check if the PoolState datum produced is correct. 
    -- It must include the PoolState datums in all the inputs, calculate the sum of all the funds in them.
    -- It must include in the sum of the redeemerMaster that is calling this tx the new fund of his (redeemerFund).
    -- It must include the correct redeemerPoolNFT.
    traceIfFalse "Validate Master Fund Pool Message: Output PoolState Datum wrong" (correctSingleOwnOutputPoolState_Datum_WithNewFund rmfpPoolNFT rmfpMaster rmfpFund  ctx)   &&

    -- Check if the Single Own Output Value is the correct sum of all the Values in the Inputs with PoolState Datums, plus the new fund (redeemerFund)
    traceIfFalse "Validate Master Fund Pool Message: Output Value Error, Wrong sum of Input Values" (correctSingleOwnOutputPoolState_Value_WithSumValuesAndNewFund rmfpFund ctx) &&
    -- Check if the Single Own Output Value is the correct sum of all the funds in the PoolState Datums in the inputs, plus the new fund (redeemerFund)
    -- The output Value can be also be cheked against the new PoolState datum produced, because was already checked that the datum had the correct sum if the inputs datums. 
    traceIfFalse "Validate Master Fund Pool Message: Output Value Error, Wrong sum of PoolState Datums" (correctSingleOwnOutputPoolState_Datum_Value_WithSumDatumsAndNewFund rmfpFund  ctx) && 

    -- Check that the tx range interval of validity. Can't be infinitum and need to be lees than ppValidTimeRange Pool Param . 
    traceIfFalse "Validate Master Fund Pool Message: Can't Fund Pool, tx validity time range is not valid" (isValidRange pParams info) &&

    -- Check that the Pool Deadline is not reached. That means that the Pool is still alive. 
    traceIfFalse "Validate Master Fund Pool Message: Can't Fund Pool, Deadline already passed" (deadlinePoolParamNotReached pParams info)
  where 
    info :: TxInfo
    info = scriptContextTxInfo ctx

{-# INLINABLE validateMasterGetPool #-}
validateMasterGetPool :: PoolParams -> PoolStateTypo -> RedeemMasterGetPoolTypo -> ScriptContext -> Bool
validateMasterGetPool pParams dPoolStateFromInputBeingValidated RedeemMasterGetPoolTypo{..}  ctx  =
    True
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

{-# INLINABLE validateUserInvest #-}
validateUserInvest :: PoolParams -> PoolStateTypo ->  RedeemUserInvestTypo -> ScriptContext -> Bool
validateUserInvest pParams dPoolStateFromInputBeingValidated RedeemUserInvestTypo{..} ctx  =
       
    -- TODO: 
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
    traceIfFalse "Validate User Invest Message: Output UserState Datum User's signature missing" (signedByDoubleOwnOutputsUserStateUser info ctx) &&

    -- Check if the PoolState Datum produced is correct, with the new User added and all the rest the same than before.
    -- Inside is going to fail if the outputs at the script aren't just two, one with the PoolState Datum and the other with the UserStateDatum.
    traceIfFalse "Validate User Invest Message: Output PoolState Datum wrong" (correctDoubleOwnOutputsPoolState_Datum_WithNewUser ruiPoolNFT ruiUserNFT ctx)   &&

    -- Check if the UserState Datum produced is correct, with the right User
    -- TODO: 
        -- usUser, the same that redeemerUser. After is checked that the redeemerUser is signing this tx.
        -- usUserNFT, the same that redeemerUserNFT,
        -- usInvest, the same that redeemerInvest,
        -- usCreatedAt, time can't be in the past. 
        -- usDeadline, time can't be before usCreatedAt and with a minimut distance 
        -- usTotal, the total proffit to win, calculated with the interest of the Pool.
        -- usChashedOut, need to be 0.
        -- usLastClain, need to be nothing.
    -- Inside is going to fail if the outputs at the script aren't just two, one with the PoolState Datum and the other with the UserStateDatum.
    traceIfFalse "Validate User Invest Message: Output UserState Datum wrong" (correctDoubleOwnOutputsUserState_Datum_OfNewUser ruiUser ruiUserNFT ruiInvest ruiCreatedAt ruiDeadline ctx)   &&
    
    -- Check if the Output Value with PoolState Datum is the same that in the Inputs with PoolSate Datum
    traceIfFalse "Validate User Invest Message: PoolState's Output Value Error. Must be the same that in PoolState's Inputs." (correctDoubleOwnOutputPoolState_Value_WithSumValues ctx) &&
    
    -- Check if the Output Value with UserState Datum is the same that in redeemerInvest, witch is at the same time the usInvest in the UserState Datum, cheked before.
    traceIfFalse "Validate User Invest Message: UserState's Output Value Error. Must be the same that redeemerInvest." (correctDoubleOwnOutputUserState_Value_OfNewUser ruiInvest ctx) &&
    
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


{-# INLINABLE validateUserGetInvest #-}
validateUserGetInvest :: PoolParams -> UserStateTypo -> RedeemUserGetInvestTypo -> ScriptContext -> Bool
validateUserGetInvest pParams dPoolStateFromInputBeingValidated RedeemUserGetInvestTypo{..} ctx  =
    True
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx




{-# INLINABLE validateUserGetRewards #-}
-- validateUserGetRewards :: PoolParams -> UserStateTypo -> RedeemUserGetRewardsTypo -> ScriptContext -> Bool
-- validateUserGetRewards pParams dPoolStateFromInputBeingValidated RedeemUserGetRewardsTypo{..} ctx  =
validateUserGetRewards :: PoolParams -> ValidatorDatum -> RedeemUserGetRewardsTypo -> ScriptContext -> Bool
validateUserGetRewards pParams _ RedeemUserGetRewardsTypo{..} ctx  =

    -- TODO: 
    
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
    
    -- Check if there is one single input UserState Datum to redeem
    traceIfFalse "Validate User Get Rewards Message: Need to use one single UserState Input" (isUnicSingleUserStateInput ctx ) &&
    
    -- Check if this tx was signed by the User specified in the redeemerUser.
    traceIfFalse "Validate User Get Rewards Message: Redeemer User's signature missing" (signedByUser rugrUser info) &&
    
    -- Check if this tx was signed by User in the UserState Datum produced by this tx. 
    -- Inside is going to fail if the outputs at the script aren't just two, one with the PoolState Datum and the other with the UserStateDatum.
    traceIfFalse "Validate User Get Rewards Message: Output UserState Datum User's signature missing" (signedByDoubleOwnOutputsUserStateUser info ctx) &&


    -- Check if this the claim is correct.
    traceIfFalse "Validate User Get Rewards Message: Claim Value is not Valid" (correctClaimValue pParams rugrClaim rugrClaimAt ctx) &&

    -- Check if this the claim is more than the minimun claim.
    traceIfFalse "Validate User Get Rewards Message: Amount to withdraw does not reach minimum." (isMoreThanMinimunClaim pParams rugrClaim) &&

    -- Check if the PoolState Datum produced is correct, need to be the same that the sum of inputs PoolState
    -- Inside is going to fail if the outputs at the script aren't just two, one with the PoolState Datum and the other with the UserStateDatum.
    traceIfFalse "Validate User Get Rewards Message: Output PoolState Datum wrong" (correctDoubleOwnOutputsPoolState_Datum_WithNoChanges rugrPoolNFT  ctx)   &&

    -- Check if the UserState Datum produced is correct, with the new claim
    -- TODO: 
    -- Inside is going to fail if the outputs at the script aren't just two, one with the PoolState Datum and the other with the UserStateDatum.
    traceIfFalse "Validate User Get Rewards Message: Output UserState Datum wrong" (correctDoubleOwnOutputsUserState_Datum_WithNewClaimRewards rugrUser rugrUserNFT rugrClaim rugrClaimAt ctx)   &&
    
    -- Check if the Output Value with PoolState Datum is the same that in the Inputs with PoolSate Datum less the rewards claimed
    traceIfFalse "Validate User Get Rewards Message: PoolState's Output Value Error. Must be the same that in PoolState's Inputs less the rewards claimed." (correctDoubleOwnOutputPoolState_Value_LessClaimRewards rugrClaim ctx) &&
    
    -- Check if the Output Value with UserState Datum is the same that the input UserState.
    traceIfFalse "Validate User Get Rewards Message: UserState's Output Value Error. Must be the same than the UserState's Input." (correctDoubleOwnOutputUserState_Value_WithNoChanges ctx) &&
    
    


     -- Check that the tx range interval of validity. Can't be infinitum and need to be lees than ppValidTimeRange Pool Param . 
    traceIfFalse "Validate User Get Rewards Message: Can't Invest in Pool, tx validity time range is not valid" (isValidRange pParams info) &&

  
    -- Check if this the claim date at is correct.
    traceIfFalse "Validate User Get Rewards Message: Claim DateAt is not Valid" (correctClaimDateAt rugrClaimAt info) &&

   
    -- Check that the Pool Deadline is not reached. That means that the Pool is still alive. 
    traceIfFalse "Validate User Get Rewards Message: Can't Get Rewatrds, Deadline already passed" (deadlinePoolParamNotReached pParams info)


  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

{-# INLINABLE validateUserInvestRewards #-}
validateUserInvestRewards :: PoolParams -> UserStateTypo -> RedeemUserInvestRewardsTypo -> ScriptContext -> Bool
validateUserInvestRewards pParams dPoolStateFromInputBeingValidated RedeemUserInvestRewardsTypo{..} ctx  =
    True
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

-- {-# INLINABLE validateUserGetRewards #-}
-- validateUserGetRewards :: PoolParams -> UserStateTypo -> ValidatorRedeemer -> ScriptContext -> Bool
-- validateUserGetRewards pParams dUserState rUserGetRewards ctx  =
--     traceIfFalse "Stake Pool Message: Beneficiary's signature missing" (signedByUser dUserState info)  &&
--     traceIfFalse "Stake Pool Message: Dont have Rewards yet" (getAcumulatedRewars > 0)
--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx


-- {-# INLINABLE validateUserInvestRewards #-}
-- validateUserInvestRewards :: PoolParams -> UserStateTypo -> ValidatorRedeemer -> ScriptContext -> Bool
-- validateUserInvestRewards pParams dUserState rUserInvestRewards ctx  =
--     traceIfFalse "Stake Pool Message: Beneficiary's signature missing" (signedByUser dUserState info)  &&
--     traceIfFalse "Stake Pool Message: Dont have Rewards yet" (getAcumulatedRewars > 0) &&
--     traceIfFalse "Stake Pool Message: Minimun ReInvest not exceded" (getAcumulatedRewars > ppMinumunCompoundInvest pParams)

--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx



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
