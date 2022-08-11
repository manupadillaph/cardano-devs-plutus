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

module Validators.StakeSimple.OffChain
 where

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

import           Data.Void            (Void)


--Import Internos
import  Validators.StakeSimple.Typos  
import  Validators.StakeSimple.OffChainHelpers     
import  Validators.StakeSimple.Helpers     
import  Validators.StakeSimple.OnChain     (typedValidator, codeValidator, addressValidator)
import  Validators.StakeSimple.OnChainNFT     (mintingNFTPolicy)

minLovelace :: Integer
minLovelace = 2000000


masterCreatePool ::  MasterCreatePoolParams -> Contract w s Text ()
masterCreatePool MasterCreatePoolParams{..} = do
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @HASKELL.String $ printf "--------------------------- Master Create Pool ---------------------------------------------"  
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"   
    

    master <- ownPaymentPubKeyHash

    now   <- currentTime
    logInfo @HASKELL.String $ printf "Time: %s" (HASKELL.show now)

    let 
        masterAdds = Ledger.pubKeyHashAddress master Nothing
    
    utxosMaster <- utxosAt masterAdds

    logInfo @HASKELL.String $ printf "utxosMaster List: %s" (HASKELL.show utxosMaster)

    let 

        poolNFTTxOutRef = mcpPoolNFTTxOutRef
        poolNFTTokenName    =  mcpPoolNFTTokenName
        valuePoolNFT     = assetClassValue (ppPoolNFT mcpPoolParam) 1
       
        valueForPoolState = Ada.lovelaceValueOf mcpFund <> valuePoolNFT

        masterFunders_others = [mkMasterFunder masterParam 0 | masterParam <- ppMasters mcpPoolParam ,  masterParam /= master] 
        masterFundersNew = mkMasterFunder master mcpFund
        masterFunders = masterFundersNew:masterFunders_others

        userNFTs = []

        dPoolState = mkPoolState (ppPoolNFT mcpPoolParam) masterFunders userNFTs

        redeemerMinting   = Redeemer $ PlutusTx.toBuiltinData $ MintingRedeemer { mrTokenName = poolNFTTokenName, mrTxOutRef = poolNFTTxOutRef} 
        
        ---

        validityRange        = Ledger.interval now (now + ppValidTimeRange mcpPoolParam)

        ---

        lookupsInit = 
            -- This script is goint to use all the uxto from the user
            Constraints.unspentOutputs utxosMaster HASKELL.<> 
            -- Is sending value to script, it needs the typedValidatorLookups
            Constraints.typedValidatorLookups (typedValidator mcpPoolParam) HASKELL.<> 
             -- Is going to Mint the User NFT:
            Constraints.mintingPolicy mintingNFTPolicy 
        tx = 
            -- Is going to create an utxo at the script with the PoolState 
            Constraints.mustPayToTheScript dPoolState valueForPoolState HASKELL.<> 
            -- Is going to spend the master uxto assinged to the NFT
            Constraints.mustSpendPubKeyOutput poolNFTTxOutRef HASKELL.<> 
            -- Is going to Mint the Pool NFT
            Constraints.mustMintValueWithRedeemer redeemerMinting valuePoolNFT HASKELL.<> 
            -- Is goint create the valid range based in ppValidTimeRange Pool Param
            Constraints.mustValidateIn validityRange

    submittedTx <- submitTxConstraintsWith lookupsInit tx
    void $ awaitTxConfirmed $ getCardanoTxId submittedTx

    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @HASKELL.String $ printf "--------------------------- Master Create Pool ---------------------------------------------"  
    logInfo @HASKELL.String $ printf "Param: %s" (HASKELL.show mcpPoolParam)

    logInfo @HASKELL.String $ printf "PoolState Datum: %s" (HASKELL.show dPoolState)

    logInfo @HASKELL.String $ printf "PoolState Value: %s" (HASKELL.show valueForPoolState)

    --logInfo @HASKELL.String $ printf "SubmittedTx: %s" (HASKELL.show submittedTx)    
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------" 
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"


masterFundPool ::  MasterFundPoolParams -> Contract w s Text ()
masterFundPool MasterFundPoolParams{..} = do
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @HASKELL.String $ printf "--------------------------- Master Fund Pool ----------------------------------------------"  
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"  
    
    master <- ownPaymentPubKeyHash

    now   <- currentTime
    logInfo @HASKELL.String $ printf "Time: %s" (HASKELL.show now)

    let 
        masterAdds = Ledger.pubKeyHashAddress master Nothing

    utxosAtMaster <- utxosAt masterAdds

    utxosListAtScriptWithPoolState <- getUtxoListWithValidPoolStateInScript (addressValidator mspPoolParam)

    -- logInfo @HASKELL.String $ printf "utxosListAtScriptWithPoolState List: %s" (HASKELL.show utxosListAtScriptWithPoolState)

    case utxosListAtScriptWithPoolState of
        [] -> do
            logInfo @HASKELL.String $ printf "Cant' Fund Pool because can't find any utxo with PoolState Datum at the Script Address"
            return ()
        _ -> do

            let

                listValuesEnUtxosPoolStateList = [ getValueFromChainIndexTxOut $ snd utxo | utxo <- utxosListAtScriptWithPoolState] 
                
                valueFundForPoolState  = Ada.lovelaceValueOf mspFund

                valueTotalForPoolState = HASKELL.foldl (<>) valueFundForPoolState listValuesEnUtxosPoolStateList 

                dPoolState = mkPoolStateWithNewFundFromUtxoList utxosListAtScriptWithPoolState (ppPoolNFT mspPoolParam) master mspFund 



                redeemerValidator = Redeemer $ PlutusTx.toBuiltinData (mkRedeemMasterFundPool (ppPoolNFT mspPoolParam)  master mspFund )
                
                txOutRefsPoolState   = fst <$> utxosListAtScriptWithPoolState
                
                ---

                validityRange        = Ledger.interval now (now + ppValidTimeRange mspPoolParam)

                ---

                lookupsInit = 
                    -- This script is goint to use all the uxto from the user
                    -- TODO: no es necesario poner esto, se hace automaticamente
                    --       se necesita poner cuando uso mustSpendPubKeyOutput
                    Constraints.unspentOutputs utxosAtMaster HASKELL.<> 
                    -- Is also going to use the utxos at the script with PoolState    
                    Constraints.unspentOutputs (Map.fromList utxosListAtScriptWithPoolState)      HASKELL.<>
                    -- Is sending value to script, it needs the typedValidatorLookups
                    Constraints.typedValidatorLookups (typedValidator mspPoolParam) HASKELL.<>
                    -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum
                    -- for speending it needs the code of the validator
                    Constraints.otherScript (codeValidator mspPoolParam)

                tx      = 
                    -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum with the new fund
                    mconcat [Constraints.mustSpendScriptOutput txOutRef redeemerValidator | txOutRef <- txOutRefsPoolState] HASKELL.<> 
                    -- Is going to create an utxo at the script with the new PoolState and the new value
                    Constraints.mustPayToTheScript dPoolState valueTotalForPoolState HASKELL.<> 
                    -- Is goint create the valid range based in ppValidTimeRange Pool Param
                    Constraints.mustValidateIn validityRange    


            submittedTx <- submitTxConstraintsWith lookupsInit tx
            void $ awaitTxConfirmed $ getCardanoTxId submittedTx

            logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
            logInfo @HASKELL.String $ printf "--------------------------- Master Fund Pool -- -------------------------------------------" 

            logInfo @HASKELL.String $ printf "Param: %s" (HASKELL.show mspPoolParam)

            logInfo @HASKELL.String $ printf "PoolState Datum: %s" (HASKELL.show dPoolState)

            logInfo @HASKELL.String $ printf "PoolState Value Fund: %s" (HASKELL.show valueFundForPoolState)

            logInfo @HASKELL.String $ printf "PoolState Value: %s" (HASKELL.show valueTotalForPoolState)

            --logInfo @HASKELL.String $ printf "SubmittedTx: %s" (HASKELL.show submittedTx)    
            logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------" 
            logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"  

masterGetBackFund ::  MasterGetBackFundParams -> Contract w s Text ()
masterGetBackFund MasterGetBackFundParams{..} = do
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @HASKELL.String $ printf "--------------------------- Master GetBack Fund -------------------------------------------"  
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"     

    -- master <- ownPaymentPubKeyHash
    -- now   <- currentTime
    -- utxosFromMaster <- findUtxosFromMasters (addressValidator mgpPoolParam) master 

    -- logInfo @HASKELL.String $ printf "findUtxoFromMaster: %s" (HASKELL.show utxosFromMaster)

    -- case utxosFromMaster of

    --     [] -> Plutus.Contract.throwError "Pool From Master Not Found" 

    --     _  -> do

    --         let 
    --             redeemer      = Redeemer $ PlutusTx.toBuiltinData mkRedeemMasterGetPool

    --             txOutRefs   = fst <$> utxosFromMaster
                
    --             lookupsInit = Constraints.unspentOutputs (Map.fromList utxosFromMaster)      HASKELL.<>
    --                     Constraints.typedValidatorLookups (typedValidator mgpPoolParam) HASKELL.<>
    --                     Constraints.otherScript (codeValidator mgpPoolParam)

                
    --             tx      = mconcat [Constraints.mustSpendScriptOutput txOutRef redeemer | txOutRef <- txOutRefs]
    --                 HASKELL.<> Constraints.mustValidateIn (from now)
    --             -- Constraints.mustPayToPubKey master vGetADA  

    --         submittedTx <- submitTxConstraintsWith lookupsInit tx
    --         void $ awaitTxConfirmed $ getCardanoTxId submittedTx

    --         logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    --         logInfo @HASKELL.String $ printf "--------------------------- Master GetBack Fund -------------------------------------------" 
    --         logInfo @HASKELL.String $ printf "Param: %s " (HASKELL.show mgpPoolParam) 
    --         logInfo @HASKELL.String $ printf "submittedTx: %s" (HASKELL.show submittedTx)    
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------" 
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"  

userInvest ::  UserInvestParams -> Contract w s Text ()
userInvest UserInvestParams{..} = do
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @HASKELL.String $ printf "--------------------------- User Invest ----------------------------------------------------"  
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"      

    user <- ownPaymentPubKeyHash

    now   <- currentTime
    logInfo @HASKELL.String $ printf "Time: %s" (HASKELL.show now)

    let 
        userAdds = Ledger.pubKeyHashAddress user Nothing

    utxosAtUser <- utxosAt userAdds

    utxosListAtScriptWithPoolState <- getUtxoListWithValidPoolStateInScript (addressValidator uipPoolParam) 

    -- logInfo @HASKELL.String $ printf "utxosListAtScriptWithPoolState List: %s" (HASKELL.show utxosListAtScriptWithPoolState)

    case utxosListAtScriptWithPoolState of
        [] -> do
            logInfo @HASKELL.String $ printf "Cant' Invest in Pool because can't find any utxo with PoolState Datum at the Script Address"
            return ()
        _ -> do
            let 
                
                -- Create User NFT 

                userNFTTxOutRef  = uiUserNFTTxOutRef
                userNFTTokenName  = uiUserNFTTokenName
                userNFT = assetClass (curSymbol mintingNFTPolicy) userNFTTokenName

                valueNFTForUser     = assetClassValue userNFT 1
                valueNFTPlusMinimunAdaForUser = valueNFTForUser <>  Ada.lovelaceValueOf minLovelace

                valueForUserState = Ada.lovelaceValueOf uipInvest 

                -- Creates UserState Datum

                dUserState = mkUserState user userNFT uipInvest uipCreatedAt uipDeadline 0 0 Nothing

                -- Creates PoolState Datum with the New User

                poolNFT = ppPoolNFT uipPoolParam    

                listValuesEnUtxosPoolStateList = [ getValueFromChainIndexTxOut $ snd utxo | utxo <- utxosListAtScriptWithPoolState] 
    
                valueForPoolState = HASKELL.foldl (<>) (Ada.lovelaceValueOf 0) listValuesEnUtxosPoolStateList 

                dPoolState = mkPoolStateWithNewUserInvestFromUtxoList utxosListAtScriptWithPoolState poolNFT userNFT

                txOutRefsPoolState   = fst <$> utxosListAtScriptWithPoolState
                
                -- Creates Redeemer

                redeemerValidator = Redeemer $ PlutusTx.toBuiltinData (mkRedeemUserInvest poolNFT userNFT  userNFTTokenName userNFTTxOutRef  user uipInvest uipCreatedAt uipDeadline )
                redeemerMinting   = Redeemer $ PlutusTx.toBuiltinData $ MintingRedeemer { mrTokenName = userNFTTokenName, mrTxOutRef = userNFTTxOutRef} 

                ---

                validityRange        = Ledger.interval uipCreatedAt (uipCreatedAt + ppValidTimeRange uipPoolParam)

                ---

                lookupsInit = 
                    -- This script is goint to use all the uxto from the user
                    Constraints.unspentOutputs utxosAtUser HASKELL.<> 
                    -- Is also going to use the utxo at the script with PoolState    
                    Constraints.unspentOutputs (Map.fromList utxosListAtScriptWithPoolState)  HASKELL.<> 
                     -- Is going to Mint the User NFT: 
                    Constraints.mintingPolicy mintingNFTPolicy    HASKELL.<> 
                    -- Is sending value to script, it needs the typedValidatorLookups
                    Constraints.typedValidatorLookups (typedValidator uipPoolParam) HASKELL.<> 
                    -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum
                    -- for speending it needs the code of the validator
                    Constraints.otherScript (codeValidator uipPoolParam)

                tx = 
                    -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum
                    mconcat [Constraints.mustSpendScriptOutput txOutRef redeemerValidator | txOutRef <- txOutRefsPoolState] HASKELL.<> 
                    -- Is going to create an utxo at the script with the new UserState and the value of the New Invest
                    Constraints.mustPayToTheScript dUserState valueForUserState HASKELL.<> 
                    -- Is going to create an utxo at the script with the new PoolState with the New UserNFT and the value is the actual value at the Script
                    Constraints.mustPayToTheScript dPoolState valueForPoolState  HASKELL.<> 
                    -- Is goint to send the mint userNFT to the user wallet 
                    Constraints.mustPayToPubKey user valueNFTPlusMinimunAdaForUser  HASKELL.<> 
                    -- Is going to spend the user uxto assinged to the NFT
                    Constraints.mustSpendPubKeyOutput userNFTTxOutRef HASKELL.<> 
                    -- Is going to Mint the User NFT:
                    Constraints.mustMintValueWithRedeemer redeemerMinting valueNFTForUser HASKELL.<> 
                    -- Is goint create the valid range based in ppValidTimeRange Pool Param
                    Constraints.mustValidateIn validityRange

            submittedTx <- submitTxConstraintsWith lookupsInit tx
            void $ awaitTxConfirmed $ getCardanoTxId submittedTx

            logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
            logInfo @HASKELL.String $ printf "--------------------------- User Invest ----------------------------------------------------"  
            logInfo @HASKELL.String $ printf "Param: %s" (HASKELL.show uipPoolParam)

            logInfo @HASKELL.String $ printf "PoolState Datum: %s" (HASKELL.show dPoolState)
            logInfo @HASKELL.String $ printf "UserState Datum: %s" (HASKELL.show dUserState)

            logInfo @HASKELL.String $ printf "PoolState Value: %s" (HASKELL.show valueForPoolState)
            logInfo @HASKELL.String $ printf "UserState Value: %s" (HASKELL.show valueForUserState)
            
            logInfo @HASKELL.String $ printf "User Wallet NFT Value: %s" (HASKELL.show valueNFTPlusMinimunAdaForUser)    

            -- logInfo @HASKELL.String $ printf "SubmittedTx: %s" (HASKELL.show submittedTx)   
            logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"      
            logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"      

userGetBackInvest ::  UserGetBackInvestParams -> Contract w s Text ()
userGetBackInvest UserGetBackInvestParams{..} = do
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @HASKELL.String $ printf "--------------------------- User GetBack Invest --------------------------------------------" 
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"      

    -- user <- ownPaymentPubKeyHash
    -- now   <- currentTime
    -- utxosFromUserWithDeadline <- findUtxosFromUserWithDeadline (addressValidator ugipPoolParam) user ugipDeadline 

    -- logInfo @HASKELL.String $ printf "findUtxosFromUserWithDeadline: %s" (HASKELL.show utxosFromUserWithDeadline)

    -- case utxosFromUserWithDeadline of

    --     [] -> Plutus.Contract.throwError "User Invest Not Found" 

    --     _  -> do

    --         let 
    --             redeemer      = Redeemer $ PlutusTx.toBuiltinData mkRedeemUserGetInvest

    --             txOutRefs   = fst <$> utxosFromUserWithDeadline
                
    --             lookupsInit = Constraints.unspentOutputs (Map.fromList utxosFromUserWithDeadline)      HASKELL.<>
    --                     Constraints.typedValidatorLookups (typedValidator ugipPoolParam) HASKELL.<>
    --                     Constraints.otherScript (codeValidator ugipPoolParam)

    --             tx      = mconcat [Constraints.mustSpendScriptOutput txOutRef redeemer | txOutRef <- txOutRefs]
    --                 HASKELL.<> Constraints.mustValidateIn (from now)
    --             -- Constraints.mustPayToPubKey user vGetADA  

    --         submittedTx <- submitTxConstraintsWith lookupsInit tx
    --         void $ awaitTxConfirmed $ getCardanoTxId submittedTx

    --         logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    --         logInfo @HASKELL.String $ printf "--------------------------- User GetBack Invest --------------------------------------------"  
    --         logInfo @HASKELL.String $ printf "Param: %s " (HASKELL.show ugipPoolParam) 
    --         logInfo @HASKELL.String $ printf "submittedTx: %s" (HASKELL.show submittedTx)   
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"      
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"      

userGetRewards ::  UserGetRewardsParams -> Contract w s Text ()
userGetRewards UserGetRewardsParams{..} = do

    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @HASKELL.String $ printf "--------------------------- User Get Rewards -----------------------------------------------"  
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"    

    user <- ownPaymentPubKeyHash
    
    now   <- currentTime
    logInfo @HASKELL.String $ printf "Time: %s" (HASKELL.show now)
    
    let 
        userAdds = Ledger.pubKeyHashAddress user Nothing

        userNFTTxOutRef  = ugrpUserNFTTxOutRef
        userNFTTokenName  = ugrpUserNFTTokenName
        userNFT = assetClass (curSymbol mintingNFTPolicy) userNFTTokenName
        
    utxosAtUser <- utxosAt userAdds

    utxosListAtScriptWithPoolState <- getUtxoListWithValidPoolStateInScript (addressValidator ugrpPoolParam) 
    -- logInfo @HASKELL.String $ printf "utxosListAtScriptWithPoolState List: %s" (HASKELL.show utxosListAtScriptWithPoolState)

    utxosListAtScriptWithUserState <- getUtxoListWithValidUserStateInScript (addressValidator ugrpPoolParam) userNFT 
    -- logInfo @HASKELL.String $ printf "utxosListAtScriptWithUserState List: %s" (HASKELL.show utxosListAtScriptWithPoolState)

    case (utxosListAtScriptWithPoolState,utxosListAtScriptWithUserState) of
        ([], _) -> do
            logInfo @HASKELL.String $ printf "Cant' Get Rewards from Pool because can't find any utxo with PoolState Datum at the Script Address"
            return ()
        (_, []) -> do
            logInfo @HASKELL.String $ printf "Cant' Get Rewards from because can't find any utxo with UserState Datum with this UserNFT at the Script Address"
            return ()
        (_, x:[y]) -> do
            logInfo @HASKELL.String $ printf "Cant' Get Rewards from because can't find unic single utxo with UserState Datum with this UserNFT at the Script Address"
            return ()
        (_, [utxoAtScriptWithUserState]) -> do
            let 
                
                valueNFTForUser    = assetClassValue userNFT 1
                valueNFTPlusMinimunAdaForUser  = valueNFTForUser <>  Ada.lovelaceValueOf minLovelace

                valueClaimRewardsForUser = Ada.lovelaceValueOf ugrpClaim

                dUserStateOLD = getUserStateFromUtxo utxoAtScriptWithUserState

                valueForUserState =  getValueFromChainIndexTxOut $ snd utxoAtScriptWithUserState 

                -- Creates UserState Datum

                rewards = getRewardsPerInvest (usLastClaimAt dUserStateOLD) now  (usCreatedAt  dUserStateOLD )  (usInvest dUserStateOLD ) 
                totalNewRewards = rewards  + usRewardsNotClaimed dUserStateOLD
                rewardsNotClaimed = totalNewRewards - ugrpClaim
                totalRewardsCashedOut = usChashedOut dUserStateOLD + ugrpClaim 

            logInfo @HASKELL.String $ printf "CALCULATED Claiming " ++ HASKELL.show ugrpClaim
            logInfo @HASKELL.String $ printf "CALCULATED New Rewards " ++ HASKELL.show rewards
            logInfo @HASKELL.String $ printf "CALCULATED usRewardsNotClaimed OLD " ++ HASKELL.show (usRewardsNotClaimed dUserStateOLD)
            logInfo @HASKELL.String $ printf "CALCULATED totalNewRewards " ++ HASKELL.show totalNewRewards
            logInfo @HASKELL.String $ printf "CALCULATED rewardsNotClaimed " ++ HASKELL.show rewardsNotClaimed
            logInfo @HASKELL.String $ printf "CALCULATED totalRewardsCashedOut " ++ HASKELL.show totalRewardsCashedOut

            let

                -- TODO: throwError "ERROR " si es menor que cero

                dUserState = mkUserState user userNFT 
                    (usInvest dUserStateOLD)
                    (usCreatedAt dUserStateOLD)
                    (usDeadline dUserStateOLD)
                    (totalRewardsCashedOut)
                    (rewardsNotClaimed)
                    (Just now)

                txOutRefUserState   = fst utxoAtScriptWithUserState

                -- Creates PoolState Datum

                poolNFT = ppPoolNFT ugrpPoolParam    

                listValuesEnUtxosPoolStateList = [ getValueFromChainIndexTxOut $ snd utxo | utxo <- utxosListAtScriptWithPoolState] 

                poolStateDatums = getPoolStateListFromUtxoList utxosListAtScriptWithPoolState 

                valueForPoolState = HASKELL.foldl (<>) (negate (Ada.lovelaceValueOf ugrpClaim)) listValuesEnUtxosPoolStateList 

    
                dPoolState = mkPoolStateFromPoolStateList poolStateDatums poolNFT   

                txOutRefsPoolState   = fst <$> utxosListAtScriptWithPoolState
                
                -- Creates Redeemer

                redeemerValidator = Redeemer $ PlutusTx.toBuiltinData (mkRedeemUserGetRewards poolNFT userNFT  user ugrpClaim now )
                
                ---

                validityRange        = Ledger.interval now (now + ppValidTimeRange ugrpPoolParam)

                ---

                lookupsInit = 
                    -- This script is goint to use all the uxto from the user 
                    Constraints.unspentOutputs utxosAtUser HASKELL.<> 
                    -- Is also going to use the utxo at the script with PoolState 
                    Constraints.unspentOutputs (Map.fromList utxosListAtScriptWithPoolState)  HASKELL.<> 
                    -- Is also going to use the utxo at the script with UserState  
                    Constraints.unspentOutputs (Map.fromList [utxoAtScriptWithUserState])  HASKELL.<> 
                    -- Is sending value to script, it needs the typedValidatorLookups
                    Constraints.typedValidatorLookups (typedValidator ugrpPoolParam) HASKELL.<> 
                    -- Is going to spend the uxto at the script with PoolState Datums and UserState Datums
                    -- for speending it needs the code of the validator
                    Constraints.otherScript (codeValidator ugrpPoolParam)

                tx = 
                    -- Is going to spend the uxto at the script with PoolState Datums, because is taking the rewards from there
                    mconcat [Constraints.mustSpendScriptOutput txOutRef redeemerValidator | txOutRef <- txOutRefsPoolState] HASKELL.<> 
                    -- Is going to spend the uxto at the script with UserState Datums, because creating a new UserState with the new Claim
                    mconcat [Constraints.mustSpendScriptOutput txOutRefUserState redeemerValidator | txOutRef <- [txOutRefUserState]] HASKELL.<> 

                    -- Is going to create an utxo at the script with the new UserState and the value of the New Invest
                    Constraints.mustPayToTheScript dUserState valueForUserState HASKELL.<> 
                    -- Is going to create an utxo at the script with the new PoolState with the New UserNFT and the value is the actual value at the Script
                    Constraints.mustPayToTheScript dPoolState valueForPoolState  HASKELL.<> 
                    -- Is goint to send the NFT back again to the user wallet
                    Constraints.mustPayToPubKey user valueNFTPlusMinimunAdaForUser  HASKELL.<> 
                    -- Is goint to send the claimed rewards to the user wallet
                    Constraints.mustPayToPubKey user valueClaimRewardsForUser  HASKELL.<> 
                    -- Is goint create the valid range based in ppValidTimeRange Pool Param
                    Constraints.mustValidateIn validityRange

            submittedTx <- submitTxConstraintsWith lookupsInit tx
            void $ awaitTxConfirmed $ getCardanoTxId submittedTx

            logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
            logInfo @HASKELL.String $ printf "--------------------------- User Get Rewards ----------------------------------------------------"  
            logInfo @HASKELL.String $ printf "Param: %s" (HASKELL.show ugrpPoolParam)

            logInfo @HASKELL.String $ printf "PoolState Datum: %s" (HASKELL.show dPoolState)
            logInfo @HASKELL.String $ printf "UserState Datum: %s" (HASKELL.show dUserState)

            logInfo @HASKELL.String $ printf "PoolState Value: %s" (HASKELL.show valueForPoolState)

            logInfo @HASKELL.String $ printf "UserState Value: %s" (HASKELL.show valueForUserState)

            logInfo @HASKELL.String $ printf "User Wallet NFT Value: %s" (HASKELL.show valueNFTPlusMinimunAdaForUser)    

            logInfo @HASKELL.String $ printf "User Wallet Claimed Rewards Value: %s" (HASKELL.show valueClaimRewardsForUser)    
            
            -- logInfo @HASKELL.String $ printf "SubmittedTx: %s" (HASKELL.show submittedTx)   
            logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"      
            logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"        

userInvestRewards ::  UserInvestRewardsParams -> Contract w s Text ()
userInvestRewards UserInvestRewardsParams{..} = do
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @HASKELL.String $ printf "--------------------------- User Invest Rewards --------------------------------------------"  
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"      

    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @HASKELL.String $ printf "--------------------------- User Invest Rewards --------------------------------------------" 
    -- logInfo @HASKELL.String $ printf "Param: %s Datum: %s Value: %s" (HASKELL.show mcpPoolParam) (HASKELL.show dPoolState) (HASKELL.show value) 
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"      
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"      



type ValidatorSchema =
        Endpoint "masterCreatePool" MasterCreatePoolParams .\/ 
        Endpoint "masterFundPool" MasterFundPoolParams .\/ 
        Endpoint "masterGetBackFund" MasterGetBackFundParams .\/ 
        Endpoint "userInvest" UserInvestParams .\/ 
        Endpoint "userGetBackInvest" UserGetBackInvestParams .\/ 
        Endpoint "userGetRewards" UserGetRewardsParams .\/ 
        Endpoint "userInvestRewards" UserInvestRewardsParams
        
endpoints :: Contract () ValidatorSchema Text ()
endpoints = awaitPromise (masterCreatePool' `select` masterFundPool' `select` masterGetBackFund' `select` userInvest' `select` userGetBackInvest' `select` userGetRewards' `select` userInvestRewards') >> endpoints
  where
    masterCreatePool' = endpoint @"masterCreatePool" masterCreatePool
    masterFundPool' = endpoint @"masterFundPool" masterFundPool
    masterGetBackFund' = endpoint @"masterGetBackFund" masterGetBackFund
    userInvest' = endpoint @"userInvest" userInvest
    userGetBackInvest' = endpoint @"userGetBackInvest" userGetBackInvest
    userGetRewards' = endpoint @"userGetRewards" userGetRewards
    userInvestRewards' = endpoint @"userInvestRewards" userInvestRewards

mkSchemaDefinitions ''ValidatorSchema

