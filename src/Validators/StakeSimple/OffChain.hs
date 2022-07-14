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
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @P.String $ printf "--------------------------- Master Create Pool ---------------------------------------------"  
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"   
    
    master <- ownPaymentPubKeyHash

    let 
        masterAdds = Ledger.pubKeyHashAddress master Nothing

    utxosMaster <- utxosAt masterAdds
    logInfo @P.String $ printf "utxosMaster List: %s" (P.show utxosMaster)
    let 

        poolNFTTxOutRef = mcpPoolNFTTxOutRef
        --poolNFTTokenName    = TokenName $ clearString mcpPoolNFTTokenName
        poolNFTTokenName    =  mcpPoolNFTTokenName
        --poolNFTTokenName = "PoolNFT"
        --valuePoolNFT     = Value.singleton (curSymbol (mintingNFTPolicy poolNFT poolNFTTokenName)) poolNFTTokenName 1
        valuePoolNFT     = assetClassValue (spPoolNFT mcpPoolParam) 1
       
        value = Ada.lovelaceValueOf mcpFund <> valuePoolNFT

        masterFunders_others = [mkMasterFunder masterParam 0 | masterParam <- spMasters mcpPoolParam ,  masterParam /= master] 
        masterFundersNew = mkMasterFunder master mcpFund
        masterFunders = masterFundersNew:masterFunders_others

        userNFTs = []

        dPoolState = mkPoolState (spPoolNFT mcpPoolParam) masterFunders userNFTs

        redeemerMinting   = Redeemer $ PlutusTx.toBuiltinData $ MintingRedeemer { mrTokenName = poolNFTTokenName, mrTxOutRef = poolNFTTxOutRef} 
        
        lookupsInit = 
            Constraints.typedValidatorLookups (typedValidator mcpPoolParam) P.<> 
            Constraints.unspentOutputs utxosMaster P.<> 
            --Constraints.mintingPolicy (mintingNFTPolicy poolNFTTxOutRef poolNFTTokenName)  
            Constraints.mintingPolicy mintingNFTPolicy 
        tx = 
            Constraints.mustPayToTheScript dPoolState value P.<> 
            Constraints.mustSpendPubKeyOutput poolNFTTxOutRef P.<> 
            --Constraints.mustMintValue valuePoolNFT 
            Constraints.mustMintValueWithRedeemer redeemerMinting valuePoolNFT

    submittedTx <- submitTxConstraintsWith lookupsInit tx
    void $ awaitTxConfirmed $ getCardanoTxId submittedTx

    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @P.String $ printf "--------------------------- Master Create Pool ---------------------------------------------"  
    logInfo @P.String $ printf "Param: %s" (P.show mcpPoolParam)
    logInfo @P.String $ printf "Datum: %s" (P.show dPoolState)
    logInfo @P.String $ printf "Value: %s" (P.show value)
    --logInfo @P.String $ printf "SubmittedTx: %s" (P.show submittedTx)    
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------" 
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"


masterFundPool ::  MasterFundPoolParams -> Contract w s Text ()
masterFundPool MasterFundPoolParams{..} = do
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @P.String $ printf "--------------------------- Master Fund Pool ----------------------------------------------"  
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"  
    
    master <- ownPaymentPubKeyHash
    now   <- currentTime

    utxosPoolStateList <- getUtxoListWithValidPoolStateInScript (addressValidator mspPoolParam)

    -- logInfo @P.String $ printf "utxosPoolStateList List: %s" (P.show utxosPoolStateList)

    -- let 
    --     utxosPoolStateList = []

    case utxosPoolStateList of
        [] -> do
            logInfo @P.String $ printf "Cant' Fund Pool because can't find any utxo with PoolState Datum at the Script Address"
            return ()
        _ -> do

            let

                listValuesEnUtxosPoolStateList = [ getValueFromChainIndexTxOut $ snd utxo | utxo <- utxosPoolStateList] 
                
                valueFund  = Ada.lovelaceValueOf mspFund

                valueTotal = P.foldl (<>) valueFund listValuesEnUtxosPoolStateList 

                dPoolState = mkPoolStateWithNewFundFromUtxoList utxosPoolStateList (spPoolNFT mspPoolParam) master mspFund 

                redeemerValidator = Redeemer $ PlutusTx.toBuiltinData (redeemMasterFundPool (spPoolNFT mspPoolParam) mspPoolNFTTokenName mspPoolNFTTxOutRef master mspFund )
                
                txOutRefsPoolState   = fst <$> utxosPoolStateList
                
                lookupsInit = 
                    Constraints.unspentOutputs (Map.fromList utxosPoolStateList)      P.<>
                    Constraints.typedValidatorLookups (typedValidator mspPoolParam) P.<>
                    Constraints.otherScript (codeValidator mspPoolParam)

                tx      = 
                    mconcat [Constraints.mustSpendScriptOutput txOutRef redeemerValidator | txOutRef <- txOutRefsPoolState] P.<> 
                    Constraints.mustValidateIn (from now) P.<> 
                    Constraints.mustPayToTheScript dPoolState valueTotal

            submittedTx <- submitTxConstraintsWith lookupsInit tx
            void $ awaitTxConfirmed $ getCardanoTxId submittedTx

            logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"
            logInfo @P.String $ printf "--------------------------- Master Fund Pool -- -------------------------------------------" 
            logInfo @P.String $ printf "Param: %s" (P.show mspPoolParam)
            logInfo @P.String $ printf "Datum: %s" (P.show dPoolState)
            logInfo @P.String $ printf "Value Fund: %s" (P.show valueFund)
            logInfo @P.String $ printf "Value Total: %s" (P.show valueTotal)
            --logInfo @P.String $ printf "SubmittedTx: %s" (P.show submittedTx)    
            logInfo @P.String $ printf "--------------------------------------------------------------------------------------------" 
            logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"  

masterGetBackFund ::  MasterGetBackFundParams -> Contract w s Text ()
masterGetBackFund MasterGetBackFundParams{..} = do
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @P.String $ printf "--------------------------- Master GetBack Fund -------------------------------------------"  
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"     

    -- master <- ownPaymentPubKeyHash
    -- now   <- currentTime
    -- utxosFromMaster <- findUtxosFromMasters (addressValidator mgpPoolParam) master 

    -- logInfo @P.String $ printf "findUtxoFromMaster: %s" (P.show utxosFromMaster)

    -- case utxosFromMaster of

    --     [] -> Plutus.Contract.throwError "Pool From Master Not Found" 

    --     _  -> do

    --         let 
    --             redeemer      = Redeemer $ PlutusTx.toBuiltinData redeemMasterGetPool

    --             txOutRefs   = fst <$> utxosFromMaster
                
    --             lookupsInit = Constraints.unspentOutputs (Map.fromList utxosFromMaster)      P.<>
    --                     Constraints.typedValidatorLookups (typedValidator mgpPoolParam) P.<>
    --                     Constraints.otherScript (codeValidator mgpPoolParam)

                
    --             tx      = mconcat [Constraints.mustSpendScriptOutput txOutRef redeemer | txOutRef <- txOutRefs]
    --                 P.<> Constraints.mustValidateIn (from now)
    --             -- Constraints.mustPayToPubKey master vGetADA  

    --         submittedTx <- submitTxConstraintsWith lookupsInit tx
    --         void $ awaitTxConfirmed $ getCardanoTxId submittedTx

    --         logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"
    --         logInfo @P.String $ printf "--------------------------- Master GetBack Fund -------------------------------------------" 
    --         logInfo @P.String $ printf "Param: %s " (P.show mgpPoolParam) 
    --         logInfo @P.String $ printf "submittedTx: %s" (P.show submittedTx)    
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------" 
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"  

userInvest ::  UserInvestParams -> Contract w s Text ()
userInvest UserInvestParams{..} = do
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @P.String $ printf "--------------------------- User Invest ----------------------------------------------------"  
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"      

    user <- ownPaymentPubKeyHash
    now   <- currentTime

    let 
        userAdds = Ledger.pubKeyHashAddress user Nothing

    utxosUser <- utxosAt userAdds

    utxosPoolStateList <- getUtxoListWithValidPoolStateInScript (addressValidator uipPoolParam) 

    -- logInfo @P.String $ printf "utxosPoolStateList List: %s" (P.show utxosPoolStateList)

    -- let :r
    --     utxosPoolStateList = []

    case utxosPoolStateList of
        [] -> do
            logInfo @P.String $ printf "Cant' Invest in Pool because can't find any utxo with PoolState Datum at the Script Address"
            return ()
        _ -> do
            let 
                
                -- Create User NFT 

                userNFTTxOutRef  = uiUserNFTTxOutRef
                userNFTTokenName  = uiUserNFTTokenName
                --userNFTTokenName  = TokenName $ clearString uiUserNFTTokenName
                --userNFTTokenName = "UserNFT"
                --userNFT = assetClass (curSymbol (mintingNFTPolicy userNFTTxOutRef userNFTTokenName)) userNFTTokenName
                userNFT = assetClass (curSymbol mintingNFTPolicy) userNFTTokenName
                --valueUserNFT     = Value.singleton (curSymbol (mintingNFTPolicy userNFTTxOutRef userNFTTokenName)) userNFTTokenName 1
        
                valueUserNFT     = assetClassValue userNFT 1
                valueUserNFTPlusMinimunAda = valueUserNFT <>  Ada.lovelaceValueOf minLovelace

                valueUser = Ada.lovelaceValueOf uipInvest 

                -- Creates UserState Datum
                dUserState = mkUserState user userNFT uipInvest now uipDeadline 0 0 Nothing

                -- Creates PoolState Datum with the New User

                poolNFT = spPoolNFT uipPoolParam    

                listValuesEnUtxosPoolStateList = [ getValueFromChainIndexTxOut $ snd utxo | utxo <- utxosPoolStateList] 
    
                valuePool = P.foldl (<>) (Ada.lovelaceValueOf 0) listValuesEnUtxosPoolStateList 

                dPoolState = mkPoolStateWithNewUserInvestFromUtxoList utxosPoolStateList poolNFT userNFT

                txOutRefsPoolState   = fst <$> utxosPoolStateList
                
                -- Creates Redeemer
                redeemerValidator = Redeemer $ PlutusTx.toBuiltinData (redeemUserInvest poolNFT userNFT  userNFTTokenName userNFTTxOutRef  user uipInvest now uipDeadline )
                redeemerMinting   = Redeemer $ PlutusTx.toBuiltinData $ MintingRedeemer { mrTokenName = userNFTTokenName, mrTxOutRef = userNFTTxOutRef} 
        
                lookupsInit = 
                    -- This script is goint to use all the uxto from the user and the script with PoolState Datums:
                    Constraints.unspentOutputs utxosUser P.<> 
                    Constraints.unspentOutputs (Map.fromList utxosPoolStateList)  P.<> 
                     -- Is going to Mint the User NFT: Constraints.mustMintValue valueUserNFT  
                    --Constraints.mintingPolicy (mintingNFTPolicy userNFTTxOutRef userNFTTokenName)  P.<> 
                    Constraints.mintingPolicy mintingNFTPolicy    P.<> 
                    -- Is sending value to script, it needs the typedValidatorLookups
                    Constraints.typedValidatorLookups (typedValidator uipPoolParam) P.<> 
                    -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum
                    -- for speending it needs the code of the validator
                    Constraints.otherScript (codeValidator uipPoolParam)

                tx = 
                    -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum
                    mconcat [Constraints.mustSpendScriptOutput txOutRef redeemerValidator | txOutRef <- txOutRefsPoolState] P.<> 
                    -- Is going to create an utxo at the script with the new UserState and the value of the New Invest
                    Constraints.mustPayToTheScript dUserState valueUser P.<> 
                    -- Is going to create an utxo at the script with the new PoolState with the New UserNFT and the value is the actual value at the Script
                    Constraints.mustPayToTheScript dPoolState valuePool  P.<> 
                    -- Is goint to send the mint userNFT to the user wallet 
                    Constraints.mustPayToPubKey user valueUserNFTPlusMinimunAda  P.<> 
                    -- Is going to spend the user uxto assinged to the NFT
                    Constraints.mustSpendPubKeyOutput userNFTTxOutRef P.<> 
                    -- Is going to Mint the User NFT: Constraints.mustMintValue valueUserNFT  
                    --Constraints.mustMintValue valueUserNFT 
                    Constraints.mustMintValueWithRedeemer redeemerMinting valueUserNFT

            submittedTx <- submitTxConstraintsWith lookupsInit tx
            void $ awaitTxConfirmed $ getCardanoTxId submittedTx

            logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"
            logInfo @P.String $ printf "--------------------------- User Invest ----------------------------------------------------"  
            -- logInfo @P.String $ printf "Param: %s Datum: %s Value: %s" (P.show uipPoolParam) (P.show datumUser) (P.show value)
            logInfo @P.String $ printf "Param: %s" (P.show uipPoolParam)
            logInfo @P.String $ printf "UserState Datum: %s" (P.show dUserState)
            logInfo @P.String $ printf "PoolState Datum: %s" (P.show dPoolState)
            logInfo @P.String $ printf "UserState Value: %s" (P.show valueUser)
            logInfo @P.String $ printf "PoolState Value: %s" (P.show valuePool)
            -- logInfo @P.String $ printf "SubmittedTx: %s" (P.show submittedTx)   
            logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"      
            logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"      

userGetBackInvest ::  UserGetBackInvestParams -> Contract w s Text ()
userGetBackInvest UserGetBackInvestParams{..} = do
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @P.String $ printf "--------------------------- User GetBack Invest --------------------------------------------" 
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"      

    -- user <- ownPaymentPubKeyHash
    -- now   <- currentTime
    -- utxosFromUserWithDeadline <- findUtxosFromUserWithDeadline (addressValidator ugipPoolParam) user ugipDeadline 

    -- logInfo @P.String $ printf "findUtxosFromUserWithDeadline: %s" (P.show utxosFromUserWithDeadline)

    -- case utxosFromUserWithDeadline of

    --     [] -> Plutus.Contract.throwError "User Invest Not Found" 

    --     _  -> do

    --         let 
    --             redeemer      = Redeemer $ PlutusTx.toBuiltinData redeemUserGetInvest

    --             txOutRefs   = fst <$> utxosFromUserWithDeadline
                
    --             lookupsInit = Constraints.unspentOutputs (Map.fromList utxosFromUserWithDeadline)      P.<>
    --                     Constraints.typedValidatorLookups (typedValidator ugipPoolParam) P.<>
    --                     Constraints.otherScript (codeValidator ugipPoolParam)

    --             tx      = mconcat [Constraints.mustSpendScriptOutput txOutRef redeemer | txOutRef <- txOutRefs]
    --                 P.<> Constraints.mustValidateIn (from now)
    --             -- Constraints.mustPayToPubKey user vGetADA  

    --         submittedTx <- submitTxConstraintsWith lookupsInit tx
    --         void $ awaitTxConfirmed $ getCardanoTxId submittedTx

    --         logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"
    --         logInfo @P.String $ printf "--------------------------- User GetBack Invest --------------------------------------------"  
    --         logInfo @P.String $ printf "Param: %s " (P.show ugipPoolParam) 
    --         logInfo @P.String $ printf "submittedTx: %s" (P.show submittedTx)   
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"      
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"      

userGetRewards ::  UserGetRewardsParams -> Contract w s Text ()
userGetRewards UserGetRewardsParams{..} = do
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @P.String $ printf "--------------------------- User Get Rewards -----------------------------------------------"  
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"    

    -- user <- ownPaymentPubKeyHash
    -- now   <- currentTime
    -- utxosFromUserWithDeadline <- findUtxosFromUserWithDeadline (addressValidator ugrpPoolParam) user ugipDeadline 

    -- logInfo @P.String $ printf "findUtxosFromUserWithDeadline: %s" (P.show utxosFromUserWithDeadline)

    -- case utxosFromUserWithDeadline of

    --     [] -> Plutus.Contract.throwError "User Invest Not Found" 

    --     _  -> do

    --         let 
    --             redeemer      = Redeemer $ PlutusTx.toBuiltinData redeemUserGetInvest

    --             txOutRefs   = fst <$> utxosFromUserWithDeadline
                
    --             lookupsInit = Constraints.unspentOutputs (Map.fromList utxosFromUserWithDeadline)      P.<>
    --                     Constraints.typedValidatorLookups (typedValidator ugrpPoolParam) P.<>
    --                     Constraints.otherScript (codeValidator ugrpPoolParam)

    --             tx      = mconcat [Constraints.mustSpendScriptOutput txOutRef redeemer | txOutRef <- txOutRefs]
    --                 P.<> Constraints.mustValidateIn (from now)
    --             -- Constraints.mustPayToPubKey user vGetADA  

    --         submittedTx <- submitTxConstraintsWith lookupsInit tx
    --         void $ awaitTxConfirmed $ getCardanoTxId submittedTx

    --         logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"
    --         logInfo @P.String $ printf "--------------------------- User GetBack Invest --------------------------------------------"  
    --         logInfo @P.String $ printf "Param: %s " (P.show ugrpPoolParam) 
    --         logInfo @P.String $ printf "submittedTx: %s" (P.show submittedTx)  
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"    
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"      

userInvestRewards ::  UserInvestRewardsParams -> Contract w s Text ()
userInvestRewards UserInvestRewardsParams{..} = do
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @P.String $ printf "--------------------------- User Invest Rewards --------------------------------------------"  
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"      

    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @P.String $ printf "--------------------------- User Invest Rewards --------------------------------------------" 
    -- logInfo @P.String $ printf "Param: %s Datum: %s Value: %s" (P.show mcpPoolParam) (P.show dPoolState) (P.show value) 
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"      
    logInfo @P.String $ printf "--------------------------------------------------------------------------------------------"      



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



-- start ::  StartParams -> Contract w s Text ()
-- start StartParams{..} = do
--     pkh <- ownPaymentPubKeyHash
--     oref <- findUtxoInValidator pkh spName
--     case oref of
--         Nothing -> do
--             let a = ValidatorData
--                     { 
--                     aCreator   = pkh
--                     , aDeadline = spDeadline
--                     , aName = spName
--                     , aAdaQty   = spAdaQty
--                     }
--                 d = ValidatorDatum
--                     { dData    = a
--                     }
--                 v = Ada.lovelaceValueOf spAdaQty
--                 tx = Constraints.mustPayToTheScript d v
--             ledgerTx <- submitTxConstraints typedValidator tx
--             void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
--             logInfo @P.String $ printf  "--------------------------- Started plazo fijo %s for token %s" (P.show a) (P.show v)
--         _ -> logInfo @P.String $ printf "--------------------------- Plazo fijo con ese nombre ya existe" 

-- get :: forall w s. GetParams ->  Contract w s Text ()
-- get GetParams{..} = do
--     pkh <- ownPaymentPubKeyHash
--     now   <- currentTime
--     oref <- findUtxoInValidator pkh gpName
--     --logInfo @P.String $ printf "findUtxoInValidator 222222222222222222 plazo fijo utxo with datum %s" (P.show out)
--     case oref of
--         Nothing ->logInfo @P.String $ printf "--------------------------- Plazo Fijo NOT FOUND"
--         --TxOutRef ->logInfo @P.String $ printf "FOUND 222222222222222222 plazo fijo utxo with datum %s" (P.show oref)
--         Just oref -> do
--             logInfo @P.String $ printf "--------------------------- Plazo Fijo FOUND utxo %s" (P.show oref)
--             (oref2,o) <- getFromValidator oref
--             let 
--                 vGet       = gpAdaQty
                
--                 Just dOld = getDatumm (oref2,o) 

--                 redeemerGet = RedeemGet

--                 r      = Redeemer $ PlutusTx.toBuiltinData redeemerGet

--                 vChange       = aAdaQty (dData dOld) - vGet
                
                
--                 a = ValidatorData
--                     { 
--                     aCreator   = pkh
--                     , aDeadline = aDeadline $ dData dOld
--                     , aName = aName $ dData dOld
--                     , aAdaQty   = vChange
--                     }
--                 d = ValidatorDatum
--                     { 
--                     dData    = a
--                     }

--                 seller = pkh

--                 vGetADA       = Ada.lovelaceValueOf vGet
--                 vChangeADA       = Ada.lovelaceValueOf vChange

--                 lookups = Constraints.typedValidatorLookups typedValidator P.<>
--                   Constraints.otherScript codeValidator                P.<>
--                   Constraints.unspentOutputs (Map.singleton oref2 o)

--                 tx
--                  |  vChange >= minLovelace = Constraints.mustPayToPubKey pkh vGetADA  <>
--                                     --Constraints.mustValidateIn (from $ aDeadline adAuction)                    <>
--                                    -- Constraints.mustValidateIn (from $ aDeadline $ dData dOld)<>
--                                     Constraints.mustValidateIn (from now)<>
--                                     Constraints.mustSpendScriptOutput oref2 r <>
--                                     Constraints.mustPayToTheScript d vChangeADA
--                  | otherwise = Constraints.mustPayToPubKey pkh vGetADA  <>
--                                     --Constraints.mustValidateIn (from $ aDeadline $ dData dOld)<>
--                                    Constraints.mustValidateIn (from now)<>
--                                     Constraints.mustSpendScriptOutput oref2 r 

--             logInfo @P.String $ printf "--------------------------- Monto Anterior: %s - Get: %s - Cambio: %s " (P.show (aAdaQty (dData dOld))) (P.show vGet) (P.show vChange)

--             ledgerTx <- submitTxConstraintsWith lookups tx
--             void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
--             logInfo @P.String $ printf "--------------------------- Get Plazo Fijo "
   

        
