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
import qualified Prelude              as P 
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
-- mkValidator pParams (PoolState dPoolState) (RedeemMasterFundPool redeemerPoolNFT  redeemerPoolNFTTokenName redeemerPoolNFTTxOutRef redeemerMaster redeemerFund) ctx =
--     validateMasterFundPool pParams dPoolState RedeemerPoolNFT redeemerPoolNFTTokenName redeemerPoolNFTTxOutRef  redeemerMaster redeemerFund ctx
mkValidator pParams (PoolState dPoolState) (RedeemMasterFundPool redeemMasterFundPool) ctx =
    validateMasterFundPool pParams dPoolState redeemMasterFundPool ctx




-- mkValidator pParams (PoolState dPoolState) RedeemMasterGetPool ctx =
--     validateMasterGetPool pParams dPoolState RedeemMasterGetPool ctx

-- mkValidator pParams (PoolState dPoolState)  (RedeemUserInvest redeemerPoolNFT redeemerUserNFT redeemerUserNFTTokenName redeemerUserNFTTxOutRef redeemerUser redeemerInvest redeemerCreatedAt redeemerDeadline) ctx =
--     validateUserInvest pParams dPoolState redeemerPoolNFT redeemerUserNFT redeemerUserNFTTokenName redeemerUserNFTTxOutRef redeemerUser redeemerInvest redeemerCreatedAt redeemerDeadline ctx
mkValidator pParams (PoolState dPoolState)  (RedeemUserInvest redeemUserInvest) ctx =
    validateUserInvest pParams dPoolState redeemUserInvest ctx


-- mkValidator pParams (PoolState dPoolState) _ _ =
--     traceIfFalse "Stake Pool Message: Wrong Redeemer For Master" False 
-- mkValidator pParams (UserState dUserState) RedeemUserGetInvest ctx =
--     validateUserGetInvest pParams dUserState RedeemUserGetInvest ctx
-- mkValidator pParams (UserState dUserState) RedeemUserGetRewards ctx =
--     validateUserGetRewards pParams dUserState RedeemUserGetRewards ctx
-- mkValidator pParams (UserState dUserState) RedeemUserInvestRewards ctx =
--     validateUserInvestRewards pParams dUserState RedeemUserInvestRewards ctx
-- mkValidator pParams (UserState dUserState) _ _ =
--     traceIfFalse "Stake Pool Message: Wrong Redeemer For User" False 
mkValidator _ _ _ _ =
     traceIfFalse "Stake Pool Message: Invalid Operation" False 


{-# INLINABLE validateMasterFundPool #-}
-- validateMasterFundPool :: PoolParams -> PoolStateTypo -> PoolNFT -> TokenName -> TxOutRef -> Master -> Fund -> ScriptContext -> Bool
-- validateMasterFundPool pParams dPoolStateFromInputBeingValidated redeemerPoolNFT redeemerPoolNFTTokenName redeemerPoolNFTTxOutRef redeemerMaster redeemerFund  ctx  =
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
    traceIfFalse "Validate Master Fund Pool Message: Output PoolState Datum wrong" (correctSingleOwnOutputPoolStateDatumWithNewFund rmfpPoolNFT rmfpMaster rmfpFund  ctx)   &&

    -- Check if the Single Own Output Value is the correct sum of all the Values in the Inputs with PoolState Datums, plus the new fund (redeemerFund)
    traceIfFalse "Validate Master Fund Pool Message: Output Value Error, Wrong sum of Input Values" (correctSumInputValuesInSingleOwnOutputValue rmfpFund ctx) &&
    -- Check if the Single Own Output Value is the correct sum of all the funds in the PoolState Datums in the inputs, plus the new fund (redeemerFund)
    -- The output Value can be also be cheked against the new PoolState datum produced, because was already checked that the datum had the correct sum if the inputs datums. 
    traceIfFalse "Validate Master Fund Pool Message: Output Value Error, Wrong sum of PoolState Datums" (correctSumInputPoolStateDatumsInSingleOwnOutputValue rmfpFund  ctx) && 

    -- Check that the Pool Deadline is not reached. That means that the Pool is still alive. 
    traceIfFalse "Validate Master Fund Pool Message: Can't Fund Pool, Deadline already passed" (deadlinePoolParamNotReached pParams info)
  where 
    info :: TxInfo
    info = scriptContextTxInfo ctx

   

{-# INLINABLE validateUserInvest #-}
-- validateUserInvest :: PoolParams -> PoolStateTypo ->  PoolNFT ->  UserNFT ->  TokenName -> TxOutRef -> User -> Invest -> POSIXTime ->  Deadline -> ScriptContext -> Bool
-- validateUserInvest pParams dPoolStateFromInputBeingValidated redeemerPoolNFT redeemerUserNFT redeemerUserNFTTokenName redeemerUserNFTTxOutRef redeemerUser redeemerInvest  redeemerCreatedAt redeemerDeadline ctx  =
    
validateUserInvest :: PoolParams -> PoolStateTypo ->  RedeemUserInvestTypo -> ScriptContext -> Bool
validateUserInvest pParams dPoolStateFromInputBeingValidated RedeemUserInvestTypo{..} ctx  =
       
    --TODO: 
    -- Chekear que se agrega el redeemerUserNFT a la lista de userNFT en PoolState Datum de salida.
    -- Que la salida con PoolState Datum sea unica y tenga el valor sumado de todas las PoolState de entrada

    -- Que redeemerInvest sea mayor que la minima inversion en pParams
    -- Que la fecha sea dentro de la deadline del Pool en pParams

    -- que el redeemerUserNFT creado este en la salida que vuelve al usuario y valueOf == 1
    -- que la txtoutRef usada en el redeemerUserNFT este como entrada en el script, para asegurarme que es gastada

    
    traceIfFalse "Validate User Invest Message: Redeemer TxOutRef used for NFT not found in Inputs" (hasUTxO info ruiUserNFTTxOutRef ) &&
    -- Check if the redeemerUserNFT was minted with the right minting policy
    traceIfFalse "Validate User Invest Message: Wrong NFT Minting Policy " (correctMintigPolicyUsedForNFT pParams ruiUserNFT  ) &&
    -- Check if the redeemerUserNFT was minted with the right quantity, just 1.
    -- TODO: this is not really needed, because the minting policy checks that too and it checked before if this NFT was minted with that specific minting policy.
    traceIfFalse "Validate User Invest Message: User NFT is not valid. Wrong Minted quantity, must be equal 1" (isMintedNFTValid info ruiUserNFT ) &&
    -- Check if the redeemerUserNFT is going to the user wallet. He must keep it in order to claim rewards from the invest. 
    -- He can give it away to other user to seel the benefitis.
    -- TODO: this is not really needed, because all minted value are going to somewhere after all. If the tx was built in incorrects ways is not my problem.
    traceIfFalse "Validate User Invest Message: Redeemer User NFT not found in User's Output Value" (isUserNFTInUserOutputValue ruiUserNFT ruiUser info)  &&
    
    -- Check if this tx was signed by the User specified in the redeemerUser.
    traceIfFalse "Validate User Invest Message: Redeemer User's signature missing" (signedByUser ruiUser info) &&
    
    -- Check if this tx was signed by User in the UserState Datum produced by this tx. 
    -- Inside is going to fail if the outputs at the script aren't just two, one with the PoolState Datum and the other with the UserStateDatum.
    traceIfFalse "Validate User Invest Message: Output UserState Datum User's signature missing" (signedByDoubleOwnOutputsUserStateUser info ctx) &&

    -- Check if the PoolState Datum produced is correct, with the new User added and all the rest the same than before.
    -- Inside is going to fail if the outputs at the script aren't just two, one with the PoolState Datum and the other with the UserStateDatum.
    traceIfFalse "Validate User Invest Message: Output PoolState Datum wrong" (correctDoubleOwnOutputsPoolStateDatumWithNewUser ruiPoolNFT ruiUserNFT ctx)   &&

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
    traceIfFalse "Validate User Invest Message: Output UserState Datum wrong" (correctDoubleOwnOutputsUserStateDatumOfNewUser ruiUser ruiUserNFT ruiInvest ruiCreatedAt ruiDeadline ctx)   &&
    
    -- Check if the Output Value with PoolState Datum is the same that in the Inputs with PoolSate Datum
    traceIfFalse "Validate User Invest Message: PoolState's Output Value Error. Must be the same that in PoolState's Inputs." (correctDoubleOwnOutputPoolStateValue ctx) &&
    
    -- Check if the Output Value with UserState Datum is the same that in redeemerInvest, witch is at the same time the usInvest in the UserState Datum, cheked before.
    traceIfFalse "Validate User Invest Message: UserState's Output Value Error. Must be the same that redeemerInvest." (correctDoubleOwnOutputUserStateValue ruiInvest ctx)
    

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx


-- {-# INLINABLE validateMasterGetPool #-}
-- validateMasterGetPool :: PoolParams -> PoolStateTypo -> ValidatorRedeemer -> ScriptContext -> Bool
-- validateMasterGetPool pParams dPoolState rMasterGetPool ctx  =
--     traceIfFalse "Stake Pool Message: Investor's signature missing" (signedByPoolStateMasterInvestor dPoolState info) &&
--     traceIfFalse "Stake Pool Message: Pool Deadline not reached" (deadlinePoolParamReached pParams info)
--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx


-- {-# INLINABLE validateUserGetInvest #-}
-- validateUserGetInvest :: PoolParams -> UserStateTypo -> ValidatorRedeemer -> ScriptContext -> Bool
-- validateUserGetInvest pParams dUserState rUserGetInvest ctx  =
--     traceIfFalse "Stake Pool Message: Beneficiary's signature missing" (signedByUser dUserState info)  &&
--     traceIfFalse "Stake Pool Message: Invest Deadline not reached" (deadlineUserStateReached dUserState info) &&
--     traceIfFalse "Stake Pool Message: Minimun Invest not exceded" (getInvest > spMinumunInvest pParams)
--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx


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
--     traceIfFalse "Stake Pool Message: Minimun ReInvest not exceded" (getAcumulatedRewars > spMinumunCompoundInvest pParams)

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
