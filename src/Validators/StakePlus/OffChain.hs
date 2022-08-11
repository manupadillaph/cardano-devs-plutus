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

module Validators.StakePlus.OffChain
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
import          Cardano.Api 
import Cardano.Api.Shelley

--Import Internos
import  Validators.StakePlus.Typos  
import  Validators.StakePlus.OffChainHelpers     
import  Validators.StakePlus.Helpers     
import  Validators.StakePlus.OnChain     (typedValidator, codeValidator, addressValidator)
import  Validators.StakePlus.OnChainNFT     (mintingNFTPolicy)




masterCreatePool ::  MasterCreatePoolParams -> Contract w s Text ()
masterCreatePool MasterCreatePoolParams{..} = do
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @HASKELL.String $ printf "--------------------------- Master Create Pool : Init ---------------------------------------"  
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"   
    

    master <- ownPaymentPubKeyHash

    now   <- currentTime
    logInfo @HASKELL.String $ printf "Time: %s" (HASKELL.show now)

    let 
        masterAdds = Ledger.pubKeyHashAddress master Nothing
    
    utxosMaster <- utxosAt masterAdds

    logInfo @HASKELL.String $ printf "utxosMaster List: %s" (HASKELL.show utxosMaster)

    let 

        poolNFTTxOutRef = pmcpPoolNFTTxOutRef
        poolNFTTokenName    =  pmcpPoolNFTTokenName
        valuePoolNFT     = assetClassValue (ppPoolNFT pmcpPoolParam) 1
       
        valueForPoolState = Ada.lovelaceValueOf pmcpFund <> valuePoolNFT

        masterFunders_others = [mkMasterFunder masterParam 0 | masterParam <- ppMasters pmcpPoolParam ,  masterParam /= master] 
        masterFundersNew = mkMasterFunder master pmcpFund
        masterFunders = masterFundersNew:masterFunders_others

        userNFTs = []

        countPoolState = 1 -- estoy creando, hay uno solo
        cashedOut = 0

        dPoolState = mkPoolState (ppPoolNFT pmcpPoolParam) masterFunders userNFTs cashedOut countPoolState

        hashDatumPoolState = Cardano.Api.hashScriptData $ Cardano.Api.Shelley.fromPlutusData $  PlutusTx.builtinDataToData  $  PlutusTx.toBuiltinData  dPoolState

        redeemerMinting   = Redeemer $ PlutusTx.toBuiltinData $ MintingRedeemer { mrTokenName = poolNFTTokenName, mrTxOutRef = poolNFTTxOutRef} 
        
        ---

        validityRange        = Ledger.interval now (now + ppValidTimeRange pmcpPoolParam)

        ---

        lookupsInit = 
            -- This script is goint to use all the uxto from the user
            Constraints.unspentOutputs utxosMaster HASKELL.<> 
            -- Is sending value to script, it needs the typedValidatorLookups
            Constraints.typedValidatorLookups (typedValidator pmcpPoolParam) HASKELL.<> 
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

    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @HASKELL.String $ printf "--------------------------- Master Create Pool : Ending ------------------------------------"  
    logInfo @HASKELL.String $ printf "Param: %s" (HASKELL.show pmcpPoolParam)

    logInfo @HASKELL.String $ printf "PoolState Datum Hash: %s" (HASKELL.show hashDatumPoolState)
    logInfo @HASKELL.String $ printf "PoolState Datum: %s" (HASKELL.show dPoolState)


    logInfo @HASKELL.String $ printf "PoolState Value: %s" (HASKELL.show valueForPoolState)

    --logInfo @HASKELL.String $ printf "SubmittedTx: %s" (HASKELL.show submittedTx)    
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------" 
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"

    submittedTx <- submitTxConstraintsWith lookupsInit tx
    void $ awaitTxConfirmed $ getCardanoTxId submittedTx

-- construye una tx con inputs:
-- desde el script aquella que tiene el NFT 
-- del usuario
-- como salida

masterFundPool ::  MasterFundPoolParams -> Contract w s Text ()
masterFundPool MasterFundPoolParams{..} = do
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @HASKELL.String $ printf "--------------------------- Master Fund Pool : Init ----------------------------------------"  
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"  
    
    master <- ownPaymentPubKeyHash

    now   <- currentTime
    logInfo @HASKELL.String $ printf "Time: %s" (HASKELL.show now)

    let 
        masterAdds = Ledger.pubKeyHashAddress master Nothing

    utxosAtMaster <- utxosAt masterAdds

    utxosListAtScriptWithPoolState <- getUtxoListWithValidPoolStateInScript (addressValidator pmfpPoolParam)

    -- logInfo @HASKELL.String $ printf "utxosListAtScriptWithPoolState List: %s" (HASKELL.show utxosListAtScriptWithPoolState)

    case utxosListAtScriptWithPoolState of
        [] -> do
            logInfo @HASKELL.String $ printf "Cant' Fund Pool because can't find any utxo with PoolState Datum at the Script Address. It need to be created first."
            return ()
        _ -> do

            let
                
                utxoAtScriptWithPoolStateAndNFT = getUtxoWithNFTInList utxosListAtScriptWithPoolState (ppPoolNFT pmfpPoolParam)

            case utxoAtScriptWithPoolStateAndNFT of 

                Nothing -> do

                    logInfo @HASKELL.String $ printf "Cant' Fund Pool because can't find any utxo with PoolState Datum and Pool NFT at the Script Address"
                    return ()

                Just utxoAtScriptWithPoolStateAndNFT -> do

                    let 
                        valuesEnUtxosPoolStateOLD = getValueFromChainIndexTxOut $ snd utxoAtScriptWithPoolStateAndNFT
                        
                        valueFundForPoolState  = Ada.lovelaceValueOf pmfpFund

                        --valueTotalForPoolState = HASKELL.foldl (<>) valueFundForPoolState listValuesEnUtxosPoolStateList 

                        dPoolStateOLD = mkPoolStateWithNewCountFundsFromUtxo utxoAtScriptWithPoolStateAndNFT (ppPoolNFT pmfpPoolParam)
                        hashDatumPoolStateOLD = Cardano.Api.hashScriptData $ Cardano.Api.Shelley.fromPlutusData $  PlutusTx.builtinDataToData  $  PlutusTx.toBuiltinData  dPoolStateOLD

                        masterFunderNew = mkMasterFunder master pmfpFund

                        countPoolState = 0 -- prefiero que esten en cero todos menos el que tiene el NFT, alli la cuenta estará bien siempre
                        cashedOut = 0

                        dPoolState = mkPoolState (ppPoolNFT pmfpPoolParam) [masterFunderNew] [] cashedOut countPoolState
                        hashDatumPoolState = Cardano.Api.hashScriptData $ Cardano.Api.Shelley.fromPlutusData $  PlutusTx.builtinDataToData  $  PlutusTx.toBuiltinData  dPoolState

                        txOutRefsPoolState   = fst <$> [utxoAtScriptWithPoolStateAndNFT]

                        redeemerValidator = Redeemer $ PlutusTx.toBuiltinData (mkRedeemMasterFundPool (ppPoolNFT pmfpPoolParam)  master pmfpFund (head txOutRefsPoolState))
                        
                        ---

                        validityRange        = Ledger.interval now (now + ppValidTimeRange pmfpPoolParam)

                        ---

                        lookupsInit = 
                            -- This script is goint to use all the uxto from the user
                            -- TODO: no es necesario poner esto, se hace automaticamente
                            --       se necesita poner cuando uso mustSpendPubKeyOutput
                            Constraints.unspentOutputs utxosAtMaster HASKELL.<> 
                            -- Is also going to use the utxos at the script with PoolState    
                            Constraints.unspentOutputs (Map.fromList utxosListAtScriptWithPoolState)      HASKELL.<>
                            -- Is sending value to script, it needs the typedValidatorLookups
                            Constraints.typedValidatorLookups (typedValidator pmfpPoolParam) HASKELL.<>
                            -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum
                            -- for speending it needs the code of the validator
                            Constraints.otherScript (codeValidator pmfpPoolParam)

                        tx      = 
                            -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum with the new fund
                            mconcat [Constraints.mustSpendScriptOutput txOutRef redeemerValidator | txOutRef <- txOutRefsPoolState] HASKELL.<> 
                            -- Is going to create an utxo at the script with the new PoolState and the new value
                            Constraints.mustPayToTheScript dPoolState valueFundForPoolState HASKELL.<> 
                            -- Is going to create an utxo at the script with the OLD PoolState and the OLD value, not changed
                            Constraints.mustPayToTheScript dPoolStateOLD valuesEnUtxosPoolStateOLD HASKELL.<> 
                            -- Is goint create the valid range based in ppValidTimeRange Pool Param
                            Constraints.mustValidateIn validityRange    


                    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
                    logInfo @HASKELL.String $ printf "--------------------------- Master Fund Pool : Ending ---------------------------------------" 

                    logInfo @HASKELL.String $ printf "Param: %s" (HASKELL.show pmfpPoolParam)

                    logInfo @HASKELL.String $ printf "OLD PoolState Datum Hash: %s" (HASKELL.show hashDatumPoolStateOLD)
                    logInfo @HASKELL.String $ printf "OLD PoolState Datum: %s" (HASKELL.show dPoolStateOLD)
                    logInfo @HASKELL.String $ printf "OLD PoolState Value Fund: %s" (HASKELL.show valuesEnUtxosPoolStateOLD)

                    logInfo @HASKELL.String $ printf "NEW PoolState Datum Hash: %s" (HASKELL.show hashDatumPoolState)
                    logInfo @HASKELL.String $ printf "NEW PoolState Datum: %s" (HASKELL.show dPoolState)
                    logInfo @HASKELL.String $ printf "NEW PoolState Value Fund: %s" (HASKELL.show valueFundForPoolState)

                    --logInfo @HASKELL.String $ printf "SubmittedTx: %s" (HASKELL.show submittedTx)    
                    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------" 
                    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"  

                    submittedTx <- submitTxConstraintsWith lookupsInit tx
                    void $ awaitTxConfirmed $ getCardanoTxId submittedTx


masterFundAndMergePool ::  MasterFundAndMergePoolParams -> Contract w s Text ()
masterFundAndMergePool MasterFundAndMergePoolParams{..} = do
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @HASKELL.String $ printf "--------------------------- Master Fund And Merge Pool : Init ------------------------------"  
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"  
    
    master <- ownPaymentPubKeyHash

    now   <- currentTime
    logInfo @HASKELL.String $ printf "Time: %s" (HASKELL.show now)

    let 
        masterAdds = Ledger.pubKeyHashAddress master Nothing

    utxosAtMaster <- utxosAt masterAdds

    utxosListAtScriptWithPoolState <- getUtxoListWithValidPoolStateInScript (addressValidator pmfampPoolParam)

    logInfo @HASKELL.String $ printf "utxosListAtScriptWithPoolState List: %s" (HASKELL.show utxosListAtScriptWithPoolState)

    case utxosListAtScriptWithPoolState of
        [] -> do
            logInfo @HASKELL.String $ printf "Cant' Fund Pool because can't find any utxo with PoolState Datum at the Script Address. It need to be created firts."
            return ()
        _ -> do

            let
                
                utxosListAtScriptWithPoolStateToMerge = [ (txOutRef, chainIndexTxOut)  |  (txOutRef, chainIndexTxOut) <- utxosListAtScriptWithPoolState, txOutRef `elem` pmfampUtxoToMerge]
                

            logInfo @HASKELL.String $ printf "utxosListAtScriptWithPoolStateToMerge List: %s" (HASKELL.show utxosListAtScriptWithPoolState)

            let
                
                utxoAtScriptWithPoolStateAndNFT = getUtxoWithNFTInList utxosListAtScriptWithPoolStateToMerge (ppPoolNFT pmfampPoolParam)

            case utxoAtScriptWithPoolStateAndNFT of 

                Nothing -> do

                    logInfo @HASKELL.String $ printf "Cant' Fund And Merge Pool because can't find any utxo with PoolState Datum and Pool NFT at the chossen uxtos to merge"
                    return ()

                Just utxoAtScriptWithPoolStateAndNFT -> do

                    let
                        
                        poolStateDatumOfUtxoWithNFT = getPoolStateFromUtxo utxoAtScriptWithPoolStateAndNFT
                        countPoolState = psCountTotalUtxoWithPoolState poolStateDatumOfUtxoWithNFT - PlutusTx.Prelude.length utxosListAtScriptWithPoolStateToMerge + 1

                        listValuesEnUtxosPoolStateList = [ getValueFromChainIndexTxOut $ snd utxo | utxo <- utxosListAtScriptWithPoolStateToMerge] 
                        
                        valueFundForPoolState  = Ada.lovelaceValueOf pmfampFund

                        valueTotalForPoolState = HASKELL.foldl (<>) valueFundForPoolState listValuesEnUtxosPoolStateList 

                        dPoolState = mkPoolStateWithNewFundFromUtxoList utxosListAtScriptWithPoolStateToMerge (ppPoolNFT pmfampPoolParam) master pmfampFund countPoolState
                        hashDatumPoolState = Cardano.Api.hashScriptData $ Cardano.Api.Shelley.fromPlutusData $  PlutusTx.builtinDataToData  $  PlutusTx.toBuiltinData  dPoolState

                        redeemerValidator = Redeemer $ PlutusTx.toBuiltinData (mkRedeemMasterFundAndMergePool (ppPoolNFT pmfampPoolParam)  master pmfampFund  (fst <$> utxosListAtScriptWithPoolStateToMerge))
                        
                        txOutRefsPoolState   = fst <$> utxosListAtScriptWithPoolStateToMerge
                        
                        ---

                        validityRange        = Ledger.interval now (now + ppValidTimeRange pmfampPoolParam)

                        ---

                        lookupsInit = 
                            -- This script is goint to use all the uxto from the user
                            -- TODO: no es necesario poner esto, se hace automaticamente
                            --       se necesita poner cuando uso mustSpendPubKeyOutput
                            Constraints.unspentOutputs utxosAtMaster HASKELL.<> 
                            -- Is also going to use the utxos at the script with PoolState    
                            Constraints.unspentOutputs (Map.fromList utxosListAtScriptWithPoolStateToMerge)      HASKELL.<>
                            -- Is sending value to script, it needs the typedValidatorLookups
                            Constraints.typedValidatorLookups (typedValidator pmfampPoolParam) HASKELL.<>
                            -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum
                            -- for speending it needs the code of the validator
                            Constraints.otherScript (codeValidator pmfampPoolParam)

                        tx      = 
                            -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum with the new fund
                            mconcat [Constraints.mustSpendScriptOutput txOutRef redeemerValidator | txOutRef <- txOutRefsPoolState] HASKELL.<> 
                            -- Is going to create an utxo at the script with the new PoolState and the new value
                            Constraints.mustPayToTheScript dPoolState valueTotalForPoolState HASKELL.<> 
                            -- Is goint create the valid range based in ppValidTimeRange Pool Param
                            Constraints.mustValidateIn validityRange    


                    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
                    logInfo @HASKELL.String $ printf "--------------------------- Master Fund And Merge Pool : Ending -----------------------------" 

                    logInfo @HASKELL.String $ printf "Param: %s" (HASKELL.show pmfampPoolParam)

                    logInfo @HASKELL.String $ printf "PoolState Datum Hash: %s" (HASKELL.show hashDatumPoolState)
                    logInfo @HASKELL.String $ printf "PoolState Datum: %s" (HASKELL.show dPoolState)
                    logInfo @HASKELL.String $ printf "PoolState Value Fund: %s" (HASKELL.show valueFundForPoolState)
                    logInfo @HASKELL.String $ printf "PoolState Value: %s" (HASKELL.show valueTotalForPoolState)

                    --logInfo @HASKELL.String $ printf "SubmittedTx: %s" (HASKELL.show submittedTx)    
                    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------" 
                    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"  

                    submittedTx <- submitTxConstraintsWith lookupsInit tx
                    void $ awaitTxConfirmed $ getCardanoTxId submittedTx

masterGetBackFund ::  MasterGetBackFundParams -> Contract w s Text ()
masterGetBackFund MasterGetBackFundParams{..} = do
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @HASKELL.String $ printf "--------------------------- Master GetBack Fund : Init -------------------------------------"  
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"     

    -- master <- ownPaymentPubKeyHash
    -- now   <- currentTime
    -- utxosFromMaster <- findUtxosFromMasters (addressValidator pmgbfPoolParam) master 

    -- logInfo @HASKELL.String $ printf "findUtxoFromMaster: %s" (HASKELL.show utxosFromMaster)

    -- case utxosFromMaster of

    --     [] -> Plutus.Contract.throwError "Pool From Master Not Found" 

    --     _  -> do

    --         let 
    --             redeemer      = Redeemer $ PlutusTx.toBuiltinData mkRedeemMasterGetPool

    --             txOutRefs   = fst <$> utxosFromMaster
                
    --             lookupsInit = Constraints.unspentOutputs (Map.fromList utxosFromMaster)      HASKELL.<>
    --                     Constraints.typedValidatorLookups (typedValidator pmgbfPoolParam) HASKELL.<>
    --                     Constraints.otherScript (codeValidator pmgbfPoolParam)

                
    --             tx      = mconcat [Constraints.mustSpendScriptOutput txOutRef redeemer | txOutRef <- txOutRefs]
    --                 HASKELL.<> Constraints.mustValidateIn (from now)
    --             -- Constraints.mustPayToPubKey master vGetADA  

    --         submittedTx <- submitTxConstraintsWith lookupsInit tx
    --         void $ awaitTxConfirmed $ getCardanoTxId submittedTx

    --         logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    --         logInfo @HASKELL.String $ printf "--------------------------- Master GetBack Fund -------------------------------------------" 
    --         logInfo @HASKELL.String $ printf "Param: %s " (HASKELL.show pmgbfPoolParam) 
    --         logInfo @HASKELL.String $ printf "submittedTx: %s" (HASKELL.show submittedTx)    
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------" 
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"  

userInvest ::  UserInvestParams -> Contract w s Text ()
userInvest UserInvestParams{..} = do
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @HASKELL.String $ printf "--------------------------- User Invest : Init ----------------------------------------------"  
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"      

    user <- ownPaymentPubKeyHash

    now   <- currentTime
    logInfo @HASKELL.String $ printf "Time: %s" (HASKELL.show now)

    let 
        userAdds = Ledger.pubKeyHashAddress user Nothing

    utxosAtUser <- utxosAt userAdds

    utxosListAtScriptWithPoolState <- getUtxoListWithValidPoolStateInScript (addressValidator puiPoolParam) 

    -- logInfo @HASKELL.String $ printf "utxosListAtScriptWithPoolState List: %s" (HASKELL.show utxosListAtScriptWithPoolState)

    case utxosListAtScriptWithPoolState of
        [] -> do
            logInfo @HASKELL.String $ printf "Cant' Invest in Pool because can't find any utxo with PoolState Datum at the Script Address"
            return ()
        _ -> do
            let 
                
                -- Create User NFT 

                userNFTTxOutRef  = puiUserNFTTxOutRef
                userNFTTokenName  = puiUserNFTTokenName
                userNFT = assetClass (curSymbol mintingNFTPolicy) userNFTTokenName

                valueNFTForUser     = assetClassValue userNFT 1
                valueNFTPlusMinimunAdaForUser = valueNFTForUser <>  Ada.lovelaceValueOf minLovelace

                valueForUserState = Ada.lovelaceValueOf puiInvest 

                -- Creates UserState Datum

                dUserState = mkUserState user userNFT puiInvest puiCreatedAt puiDeadline 0 0 Nothing
                hashDatumUserState = Cardano.Api.hashScriptData $ Cardano.Api.Shelley.fromPlutusData $  PlutusTx.builtinDataToData  $  PlutusTx.toBuiltinData  dUserState
                
                -- Creates PoolState Datum with the New User

                poolNFT = ppPoolNFT puiPoolParam    

                utxoAtScriptWithPoolState = head utxosListAtScriptWithPoolState

                valueForPoolState =  getValueFromChainIndexTxOut $ snd utxoAtScriptWithPoolState 

                dPoolState = mkPoolStateWithNewUserInvestFromUtxo utxoAtScriptWithPoolState poolNFT userNFT
                hashDatumPoolState = Cardano.Api.hashScriptData $ Cardano.Api.Shelley.fromPlutusData $  PlutusTx.builtinDataToData  $  PlutusTx.toBuiltinData  dPoolState

                txOutRefPoolState   = fst utxoAtScriptWithPoolState

                -- listValuesEnUtxosPoolStateList = [ getValueFromChainIndexTxOut $ snd utxo | utxo <- utxosListAtScriptWithPoolState] 
    
                -- valueForPoolState = HASKELL.foldl (<>) (Ada.lovelaceValueOf 0) listValuesEnUtxosPoolStateList 

                -- dPoolState = mkPoolStateWithNewUserInvestFromUtxoList utxosListAtScriptWithPoolState poolNFT userNFT

                -- txOutRefsPoolState   = fst <$> utxosListAtScriptWithPoolState
                
                -- Creates Redeemer

                redeemerValidator = Redeemer $ PlutusTx.toBuiltinData (mkRedeemUserInvest poolNFT userNFT  userNFTTokenName userNFTTxOutRef  user puiInvest puiCreatedAt puiDeadline )
                redeemerMinting   = Redeemer $ PlutusTx.toBuiltinData $ MintingRedeemer { mrTokenName = userNFTTokenName, mrTxOutRef = userNFTTxOutRef} 

                ---

                validityRange        = Ledger.interval puiCreatedAt (puiCreatedAt + ppValidTimeRange puiPoolParam)

                ---

                lookupsInit = 
                    -- This script is goint to use all the uxto from the user
                    Constraints.unspentOutputs utxosAtUser HASKELL.<> 
                    -- Is also going to use the utxo at the script with PoolState    
                    Constraints.unspentOutputs (Map.fromList utxosListAtScriptWithPoolState)  HASKELL.<> 
                     -- Is going to Mint the User NFT: 
                    Constraints.mintingPolicy mintingNFTPolicy    HASKELL.<> 
                    -- Is sending value to script, it needs the typedValidatorLookups
                    Constraints.typedValidatorLookups (typedValidator puiPoolParam) HASKELL.<> 
                    -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum
                    -- for speending it needs the code of the validator
                    Constraints.otherScript (codeValidator puiPoolParam)

                tx = 
                    -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum
                    Constraints.mustSpendScriptOutput txOutRefPoolState redeemerValidator HASKELL.<> 
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



            logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
            logInfo @HASKELL.String $ printf "--------------------------- User Invest : Ending -------------------------------------------"  
            logInfo @HASKELL.String $ printf "Param: %s" (HASKELL.show puiPoolParam)

            logInfo @HASKELL.String $ printf "PoolState Datum Hash: %s" (HASKELL.show hashDatumPoolState)
            logInfo @HASKELL.String $ printf "PoolState Datum: %s" (HASKELL.show dPoolState)
            logInfo @HASKELL.String $ printf "PoolState Value: %s" (HASKELL.show valueForPoolState)

            logInfo @HASKELL.String $ printf "UserState Datum Hash: %s" (HASKELL.show hashDatumUserState)
            logInfo @HASKELL.String $ printf "UserState Datum: %s" (HASKELL.show dUserState)
            logInfo @HASKELL.String $ printf "UserState Value: %s" (HASKELL.show valueForUserState)
            
            
            
            logInfo @HASKELL.String $ printf "User Wallet NFT Value: %s" (HASKELL.show valueNFTPlusMinimunAdaForUser)    

            -- logInfo @HASKELL.String $ printf "SubmittedTx: %s" (HASKELL.show submittedTx)   
            logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"      
            logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"    

            submittedTx <- submitTxConstraintsWith lookupsInit tx
            void $ awaitTxConfirmed $ getCardanoTxId submittedTx  

userGetBackInvest ::  UserGetBackInvestParams -> Contract w s Text ()
userGetBackInvest UserGetBackInvestParams{..} = do
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @HASKELL.String $ printf "--------------------------- User GetBack Invest : Init --------------------------------------" 
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"      

    -- user <- ownPaymentPubKeyHash
    -- now   <- currentTime
    -- utxosFromUserWithDeadline <- findUtxosFromUserWithDeadline (addressValidator pugbiPoolParam) user pugbiDeadline 

    -- logInfo @HASKELL.String $ printf "findUtxosFromUserWithDeadline: %s" (HASKELL.show utxosFromUserWithDeadline)

    -- case utxosFromUserWithDeadline of

    --     [] -> Plutus.Contract.throwError "User Invest Not Found" 

    --     _  -> do

    --         let 
    --             redeemer      = Redeemer $ PlutusTx.toBuiltinData mkRedeemUserGetInvest

    --             txOutRefs   = fst <$> utxosFromUserWithDeadline
                
    --             lookupsInit = Constraints.unspentOutputs (Map.fromList utxosFromUserWithDeadline)      HASKELL.<>
    --                     Constraints.typedValidatorLookups (typedValidator pugbiPoolParam) HASKELL.<>
    --                     Constraints.otherScript (codeValidator pugbiPoolParam)

    --             tx      = mconcat [Constraints.mustSpendScriptOutput txOutRef redeemer | txOutRef <- txOutRefs]
    --                 HASKELL.<> Constraints.mustValidateIn (from now)
    --             -- Constraints.mustPayToPubKey user vGetADA  

    --         submittedTx <- submitTxConstraintsWith lookupsInit tx
    --         void $ awaitTxConfirmed $ getCardanoTxId submittedTx

    --         logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    --         logInfo @HASKELL.String $ printf "--------------------------- User GetBack Invest --------------------------------------------"  
    --         logInfo @HASKELL.String $ printf "Param: %s " (HASKELL.show pugbiPoolParam) 
    --         logInfo @HASKELL.String $ printf "submittedTx: %s" (HASKELL.show submittedTx)   
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"      
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"      

userGetRewards ::  UserGetRewardsParams -> Contract w s Text ()
userGetRewards UserGetRewardsParams{..} = do

    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @HASKELL.String $ printf "--------------------------- User Get Rewards : Init ----------------------------------------"  
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"    

    user <- ownPaymentPubKeyHash
    
    now   <- currentTime
    logInfo @HASKELL.String $ printf "Time: %s" (HASKELL.show now)
    
    let 
        userAdds = Ledger.pubKeyHashAddress user Nothing
        -- Re creates NFT por comparing and redeemer parameter
        userNFTTxOutRef  = pugrUserNFTTxOutRef
        userNFTTokenName  = pugrUserNFTTokenName
        userNFT = assetClass (curSymbol mintingNFTPolicy) userNFTTokenName
    
        
    utxosAtUser <- utxosAt userAdds

    utxosListAtScriptWithPoolState <- getUtxoListWithValidPoolStateInScript (addressValidator pugrPoolParam) 
    logInfo @HASKELL.String $ printf "utxosListAtScriptWithPoolState List: %s" (HASKELL.show utxosListAtScriptWithPoolState)

    logInfo @HASKELL.String $ printf "userNFT: %s" (HASKELL.show userNFT)
    utxosListAtScriptWithUserState <- getUtxoListWithValidUserStateInScript (addressValidator pugrPoolParam) userNFT 
    logInfo @HASKELL.String $ printf "utxosListAtScriptWithUserState List: %s" (HASKELL.show utxosListAtScriptWithUserState)

   

    case (utxosListAtScriptWithPoolState,utxosListAtScriptWithUserState) of
        ([], _) -> do
            logInfo @HASKELL.String $ printf "Cant' Get Rewards from Pool because can't find any utxo with PoolState Datum at the Script Address"
            return ()
        (_, []) -> do
            logInfo @HASKELL.String $ printf "Cant' Get Rewards from because can't find any utxo with UserState Datum with this UserNFT at the Script Address"
            return ()
        (_, _:[_]) -> do
            logInfo @HASKELL.String $ printf "Cant' Get Rewards from because can't find unic single utxo with UserState Datum with this UserNFT at the Script Address"
            return ()
        (_, [utxoAtScriptWithUserState]) -> do

            let 
                
                
                valueNFTForUser    = assetClassValue userNFT 1
                valueNFTPlusMinimunAdaForUser  = valueNFTForUser <>  Ada.lovelaceValueOf minLovelace

                valueClaimRewardsForUser = Ada.lovelaceValueOf pugrClaim

                dUserStateOLD = getUserStateFromUtxo utxoAtScriptWithUserState

                valueForUserState =  getValueFromChainIndexTxOut $ snd utxoAtScriptWithUserState 

                -- Creates UserState Datum

                rewards = getRewardsPerInvest (usLastClaimAt dUserStateOLD) now  (usCreatedAt  dUserStateOLD )  (usInvest dUserStateOLD ) 
                totalNewRewards = rewards  + usRewardsNotClaimed dUserStateOLD
                rewardsNotClaimed = totalNewRewards - pugrClaim
                totalRewardsCashedOut = usChashedOut dUserStateOLD + pugrClaim 

            logInfo @HASKELL.String $ printf "CALCULATED Claiming " ++ HASKELL.show pugrClaim
            logInfo @HASKELL.String $ printf "CALCULATED New Rewards " ++ HASKELL.show rewards
            logInfo @HASKELL.String $ printf "CALCULATED usRewardsNotClaimed OLD " ++ HASKELL.show (usRewardsNotClaimed dUserStateOLD)
            logInfo @HASKELL.String $ printf "CALCULATED totalNewRewards " ++ HASKELL.show totalNewRewards
            logInfo @HASKELL.String $ printf "CALCULATED rewardsNotClaimed " ++ HASKELL.show rewardsNotClaimed
            logInfo @HASKELL.String $ printf "CALCULATED totalRewardsCashedOut " ++ HASKELL.show totalRewardsCashedOut

            if  pugrClaim > totalNewRewards then do
                logInfo @HASKELL.String $ printf "Trying to get too many rewards... wait some time"
                return ()
            else do
                let

                    -- TODO: throwError "ERROR " si es menor que cero

                    dUserState = mkUserState user userNFT 
                        (usInvest dUserStateOLD)
                        (usCreatedAt dUserStateOLD)
                        (usDeadline dUserStateOLD)
                        totalRewardsCashedOut
                        rewardsNotClaimed
                        (Just now)

                    hashDatumUserState = Cardano.Api.hashScriptData $ Cardano.Api.Shelley.fromPlutusData $  PlutusTx.builtinDataToData  $  PlutusTx.toBuiltinData  dUserState
                    

                    txOutRefUserState   = fst utxoAtScriptWithUserState

                    -- Creates PoolState Datum

                    poolNFT = ppPoolNFT pugrPoolParam    

                    -- find utxo PoolState where the UserNFT was register in
                    findUxtoPoolStateWithUserNFT :: [(TxOutRef, ChainIndexTxOut)] -> UserNFT -> [(TxOutRef, ChainIndexTxOut)]
                    findUxtoPoolStateWithUserNFT utxosListAtScriptWithPoolState userNFT =
                            [  utxoAtScriptWithPoolState  | utxoAtScriptWithPoolState <- utxosListAtScriptWithPoolState, userNFT `elem` psUsersNFT (getPoolStateFromUtxo utxoAtScriptWithPoolState  )] 
                            
    
                    -- find utxo PoolState where the UserNFT was register in
                    utxoAtScriptWithPoolStateWithUserNFT = findUxtoPoolStateWithUserNFT utxosListAtScriptWithPoolState userNFT
                
                logInfo @HASKELL.String $ printf "utxoAtScriptWithPoolStateWithUserNFT List: %s" (HASKELL.show $ fst <$> utxoAtScriptWithPoolStateWithUserNFT)

                case utxoAtScriptWithPoolStateWithUserNFT of 
                     
                    [utxoAtScriptWithPoolStateWithUserNFT] -> do

                        let 
                
                            -- get the list of utxo PoolState without the one it just found
                            utxosListAtScriptWithPoolStateWithoutUserNFT = removeUxtoByTxOutRef (fst utxoAtScriptWithPoolStateWithUserNFT) utxosListAtScriptWithPoolState
                            
                        logInfo @HASKELL.String $ printf "utxosListAtScriptWithPoolStateWithoutUserNFT List: %s" (HASKELL.show $ fst <$> utxosListAtScriptWithPoolStateWithoutUserNFT)

                        let 
                            -- for ordering a list of uxto with chainindex
                            compareValueOfUtxoList :: (TxOutRef, ChainIndexTxOut) -> (TxOutRef, ChainIndexTxOut) -> Ordering
                            compareValueOfUtxoList utxo1 utxo2
                                | Ada.fromValue (getValueFromChainIndexTxOut(snd utxo1)) > Ada.fromValue (getValueFromChainIndexTxOut( snd utxo2) )= LT
                                | otherwise = GT

                            -- order the list of uxto by value
                            utxosOrderedListAtScriptWithPoolState = sortBy compareValueOfUtxoList utxosListAtScriptWithPoolStateWithoutUserNFT
                        logInfo @HASKELL.String $ printf "utxosOrderedListAtScriptWithPoolState List: %s" (HASKELL.show $ fst <$> utxosOrderedListAtScriptWithPoolState)

                        let 
  
                            isEnoughToClaimInUxtoList :: [(TxOutRef, ChainIndexTxOut)] -> Proffit -> Bool
                            isEnoughToClaimInUxtoList utxosListAtScriptWithPoolState claim = do
                               
                               claim <=0 || ( 
                                    (PlutusTx.Prelude.length utxosListAtScriptWithPoolState > 0) && (
                                        do                
                                            -- this means that there is still claim to cover... i need to keep adding utxo if there is more in the list
                                            let 
                                                utxo = head utxosListAtScriptWithPoolState
                                                -- cant take the full value of the utxo, because i need to leave the min ada in the utxo, that is why i rest minLovelace (<> negate Ada.lovelaceValueOf minLovelace)
                                                value = getValueFromChainIndexTxOut $ snd utxo
                                                valueCanUse    = value <> negate (Ada.lovelaceValueOf minLovelace)
                                                adaFromValueCanUse = Ada.getLovelace (Ada.fromValue valueCanUse) 
                                                -- if the uxto have 5ada, i just consider 5ADA - 2ADA (min) = 3ADA i can use
                                                newClaim = claim - adaFromValueCanUse

                                            isEnoughToClaimInUxtoList (tail utxosListAtScriptWithPoolState) newClaim
                                    )
                                )
                           
                            -- get the small list of uxto which can cover the paymet of the reward claimed
                            getUtxoListWithEnoughValueToClaim :: [(TxOutRef, ChainIndexTxOut)] -> Proffit -> [(TxOutRef, ChainIndexTxOut)]
                            getUtxoListWithEnoughValueToClaim utxosListAtScriptWithPoolState claim = do
                                
                                if (claim > 0) && (PlutusTx.Prelude.length utxosListAtScriptWithPoolState > 0) then do
                                    -- this means that there is still claim to cover... i need to keep adding utxo if there is more in the list
                                    let 
                                        utxo = head utxosListAtScriptWithPoolState
                                        -- cant take the full value of the utxo, because i need to leave the min ada in the utxo, that is why i rest minLovelace (<> negate Ada.lovelaceValueOf minLovelace)
                                        value = getValueFromChainIndexTxOut $ snd utxo
                                        valueCanUse    = value <> negate (Ada.lovelaceValueOf minLovelace)
                                        adaFromValueCanUse = Ada.getLovelace (Ada.fromValue valueCanUse) 
                                        -- if the uxto have 5ada, i just consider 5ADA - 2ADA (min) = 3ADA i can use

                                        newClaim = claim - adaFromValueCanUse

                                    utxo : getUtxoListWithEnoughValueToClaim (tail utxosListAtScriptWithPoolState) newClaim
                                else
                                    -- all the claim is cover, dont need to add more utxo
                                    -- could be that the list of utxo finih and i couldnt cover all
                                    []            

                        let
                            -- the uxto with the UserNFT must be included beyonf its value
                            -- anyways I need to consider its value
                            -- the new claim is the result of substracting the value of this uxto to the original claimed valueOf
                            valueUtxoAtScriptWithPoolStateWithUserNFT = getValueFromChainIndexTxOut $ snd utxoAtScriptWithPoolStateWithUserNFT
                            valueCanUse    = valueUtxoAtScriptWithPoolStateWithUserNFT <> negate (Ada.lovelaceValueOf minLovelace)
                            adaFromValueCanUse = Ada.getLovelace (Ada.fromValue valueCanUse) 
                            -- cant take the full value of the utxo, because i need to leave the min ada in the utxo, that is why i rest minLovelace (<> negate Ada.lovelaceValueOf minLovelace)    
                            claim = pugrClaim - adaFromValueCanUse
                            -- The resulting list of utxo is including in the firts utxo the utxo with the userNFT and the rest is a calling getUtxoListWithEnoughValueToClaim wo find the minumun listo of utxo wich can cover the payment.
                            utxosListAtScriptWithPoolStateWithEnoughValueToClaim = utxoAtScriptWithPoolStateWithUserNFT : getUtxoListWithEnoughValueToClaim utxosOrderedListAtScriptWithPoolState claim
                        
                        logInfo @HASKELL.String $ printf "utxosListAtScriptWithPoolStateWithEnoughValueToClaim List: %s" (HASKELL.show $ fst <$> utxosListAtScriptWithPoolStateWithEnoughValueToClaim)

                         -- could be possible that there is no enough utxo to cover the claim 
                        if isEnoughToClaimInUxtoList utxosListAtScriptWithPoolStateWithEnoughValueToClaim pugrClaim then do
                            
                            let 
                                -- it creates the PoolState Datum, hash and value to each of the uxto selected
                                getPoolStateListwithNewValues :: [(TxOutRef, ChainIndexTxOut)] -> Proffit -> Contract w0 s0 e0 ([(ValidatorDatum, Cardano.Api.Hash Cardano.Api.ScriptData, Ledger.Value)])
                                getPoolStateListwithNewValues utxosListAtScriptWithPoolStateWithEnoughValueToClaim claim = do
                                    
                                    logInfo @HASKELL.String $ printf "Looking for utxo in: %s" (HASKELL.show $ fst <$> utxosListAtScriptWithPoolStateWithEnoughValueToClaim)
                                    logInfo @HASKELL.String $ printf "Looking for utxo to claim: %s" (HASKELL.show  claim)


                                    if (claim > 0) && (PlutusTx.Prelude.length utxosListAtScriptWithPoolStateWithEnoughValueToClaim > 0)  then do
                                        let 
                                            utxo = head utxosListAtScriptWithPoolStateWithEnoughValueToClaim

                                            poolStateDatum = getPoolStateFromUtxo utxo

                                            -- cant take the full value, i cant let in 0ADA, need to leave at least 2ADA
                                            value = getValueFromChainIndexTxOut $ snd utxo
                                            valueCanUse    = value <> negate (Ada.lovelaceValueOf minLovelace)
                                            adaFromValueCanUse = Ada.getLovelace (Ada.fromValue valueCanUse) 

                                        logInfo @HASKELL.String $ printf "utxo: %s" (HASKELL.show  utxo)
                                        logInfo @HASKELL.String $ printf "poolStateDatum: %s" (HASKELL.show  poolStateDatum)
                                        logInfo @HASKELL.String $ printf "value: %s" (HASKELL.show  value)
                                        logInfo @HASKELL.String $ printf "valueCanUse: %s" (HASKELL.show  valueCanUse)
                                        logInfo @HASKELL.String $ printf "adaFromValueCanUse: %s" (HASKELL.show  adaFromValueCanUse)

                                        if adaFromValueCanUse - claim >= 0 then do
                                            -- this means that with this uxto i cover all the claim, dont need to keep adding utxo
                                            let     
                                                -- newValue add the minimun ada i sustracted before
                                                newValue = value  <> negate ( Ada.lovelaceValueOf claim)
                                                newOutputDatum = mkPoolStateWithNewClaimRewardsFromPoolState poolStateDatum poolNFT claim
                                            logInfo @HASKELL.String $ printf "newOutputDatum1: %s" (HASKELL.show  newOutputDatum)
                                            logInfo @HASKELL.String $ printf "new value: %s" (HASKELL.show  newValue)
                                            let
                                                hashDatumPoolState = Cardano.Api.hashScriptData $ Cardano.Api.Shelley.fromPlutusData $  PlutusTx.builtinDataToData  $  PlutusTx.toBuiltinData  newOutputDatum
                                
                                            return [(newOutputDatum,hashDatumPoolState, newValue)]
                                        else do
                                            -- this means that remaining claim is bigger than the value, so i need to keep adding utxo
                                            -- ill take all i can from this utxo
                                            let     
                                               
                                                newValue = value <> negate ( Ada.lovelaceValueOf    adaFromValueCanUse )
                                                newOutputDatum = mkPoolStateWithNewClaimRewardsFromPoolState poolStateDatum poolNFT adaFromValueCanUse   

                                            logInfo @HASKELL.String $ printf "newOutputDatum2: %s" (HASKELL.show  newOutputDatum)
                                            logInfo @HASKELL.String $ printf "new value: %s" (HASKELL.show  newValue)
                                            
                                            let
                                                hashDatumPoolState = Cardano.Api.hashScriptData $ Cardano.Api.Shelley.fromPlutusData $  PlutusTx.builtinDataToData  $  PlutusTx.toBuiltinData  newOutputDatum
                                
                                                newClaim = claim - adaFromValueCanUse
                                                
                                                new = (newOutputDatum,hashDatumPoolState, newValue)

                                            others <- getPoolStateListwithNewValues (tail utxosListAtScriptWithPoolStateWithEnoughValueToClaim) newClaim  

                                            return (new : others)
                                    else
                                        return []            

                                -- --listValuesEnUtxosPoolStateList = [ getValueFromChainIndexTxOut $ snd utxo | utxo <- utxosListAtScriptWithPoolState] 
                                -- --poolStateDatums = getPoolStateListFromUtxoList utxosListAtScriptWithPoolState 
                                -- --valueForPoolState = HASKELL.foldl (<>) (negate (Ada.lovelaceValueOf pugrClaim)) listValuesEnUtxosPoolStateList 
                                -- --dPoolState = mkPoolStateFromPoolStateList poolStateDatums poolNFT   

                                -- for all the listo of chossen uxto I need to create a list of PoolState datum to sit with each one of them, with the cashedOut Value updated.
                                -- all the uxto final values plus the cashedOut in the datum need to be equal to the sum of the fundings in the datums
                            dPoolStateListWithNewValues <- getPoolStateListwithNewValues utxosListAtScriptWithPoolStateWithEnoughValueToClaim pugrClaim

                            let
                                txOutRefsPoolState   = fst <$> utxosListAtScriptWithPoolStateWithEnoughValueToClaim

                            logInfo @HASKELL.String $ printf "txOutRefsPoolState List: %s" (HASKELL.show txOutRefsPoolState)
                            logInfo @HASKELL.String $ printf "dPoolStateListWithNewValues List: %s" (HASKELL.show dPoolStateListWithNewValues)
                            

                            let  
                                newDummyInputDatum = mkDummyPoolStateWithNewClaimRewardsFromPoolStateList (getPoolStateFromUtxo <$> utxosListAtScriptWithPoolStateWithEnoughValueToClaim) poolNFT pugrClaim    
                                newDummyOutputDatum = mkDummyPoolStateFromPoolStateList ((fromJust.  getPoolStateFromDatum . get1st) <$> dPoolStateListWithNewValues) poolNFT          

                               
                            logInfo @HASKELL.String $ printf "newDummyInputDatum List: %s" (HASKELL.show newDummyInputDatum)
                            logInfo @HASKELL.String $ printf "newDummyOutputDatum List: %s" (HASKELL.show newDummyOutputDatum)
                            
                            logInfo @HASKELL.String $ printf "newDummyOutputDatum EQ: %s" (HASKELL.show (newDummyOutputDatum == newDummyInputDatum))

                            let    
                                -- Creates Redeemer

                                redeemerValidator = Redeemer $ PlutusTx.toBuiltinData (mkRedeemUserGetRewards poolNFT userNFT  user pugrClaim now )
                                
                                ---

                                validityRange        = Ledger.interval now (now + ppValidTimeRange pugrPoolParam)

                                ---

                                lookupsInit = 
                                    -- This script is goint to use all the uxto from the user 
                                    Constraints.unspentOutputs utxosAtUser HASKELL.<> 
                                    -- Is also going to use the utxo at the script with PoolState 
                                    Constraints.unspentOutputs (Map.fromList utxosListAtScriptWithPoolState)  HASKELL.<> 
                                    -- Is also going to use the utxo at the script with UserState  
                                    Constraints.unspentOutputs (Map.fromList [utxoAtScriptWithUserState])  HASKELL.<> 
                                    -- Is sending value to script, it needs the typedValidatorLookups
                                    Constraints.typedValidatorLookups (typedValidator pugrPoolParam) HASKELL.<> 
                                    -- Is going to spend the uxto at the script with PoolState Datums and UserState Datums
                                    -- for speending it needs the code of the validator
                                    Constraints.otherScript (codeValidator pugrPoolParam)

                                tx = 
                                    -- Is going to spend the uxto at the script with PoolState Datums, because is taking the rewards from there
                                    mconcat [Constraints.mustSpendScriptOutput txOutRef redeemerValidator | txOutRef <- txOutRefsPoolState] HASKELL.<> 
                                    -- Is going to spend the uxto at the script with UserState Datums, because creating a new UserState with the new Claim
                                    mconcat [Constraints.mustSpendScriptOutput txOutRefUserState redeemerValidator | txOutRef <- [txOutRefUserState]] HASKELL.<> 

                                    -- Is going to create an utxo at the script with the new UserState and the value of the New Invest
                                    Constraints.mustPayToTheScript dUserState valueForUserState HASKELL.<> 

                                    -- -- Is going to create an utxo at the script with the new PoolState with the New UserNFT and the value is the actual value at the Script
                                    -- Constraints.mustPayToTheScript dPoolState valueForPoolState  HASKELL.<> 
                                    mconcat [Constraints.mustPayToTheScript dPoolState valueForPoolState | (dPoolState,_,valueForPoolState) <- dPoolStateListWithNewValues] HASKELL.<> 

                                    -- Is goint to send the NFT back again to the user wallet
                                    Constraints.mustPayToPubKey user valueNFTPlusMinimunAdaForUser  HASKELL.<> 
                                    -- Is goint to send the claimed rewards to the user wallet
                                    Constraints.mustPayToPubKey user valueClaimRewardsForUser  HASKELL.<> 
                                    -- Is goint create the valid range based in ppValidTimeRange Pool Param
                                    Constraints.mustValidateIn validityRange

                            logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
                            logInfo @HASKELL.String $ printf "--------------------------- User Get Rewards : Ending ---------------------------------------"  
                            logInfo @HASKELL.String $ printf "Param: %s" (HASKELL.show pugrPoolParam)

                            let 

                                formatPoolStateOutputs = concat [ ( "PoolState Datum Hash: " ++ (HASKELL.show hashDatumPoolState)): ( "PoolState Datum: " ++ (HASKELL.show  dPoolState)): ( "PoolState Value: " ++ (HASKELL.show valueForPoolState)):[] | (dPoolState,hashDatumPoolState,valueForPoolState) <- dPoolStateListWithNewValues] 


                            PlutusTx.Prelude.mapM_ (logInfo @HASKELL.String  ) formatPoolStateOutputs

                        
                            logInfo @HASKELL.String $ printf "UserState Datum Hash: %s" (HASKELL.show hashDatumUserState)
                            logInfo @HASKELL.String $ printf "UserState Datum: %s" (HASKELL.show dUserState)
                            logInfo @HASKELL.String $ printf "UserState Value: %s" (HASKELL.show valueForUserState)
                            logInfo @HASKELL.String $ printf "User Wallet NFT Value: %s" (HASKELL.show valueNFTPlusMinimunAdaForUser)    

                            logInfo @HASKELL.String $ printf "User Wallet Claimed Rewards Value: %s" (HASKELL.show valueClaimRewardsForUser)    
                            
                            -- logInfo @HASKELL.String $ printf "SubmittedTx: %s" (HASKELL.show submittedTx)   
                            logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"      
                            logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"        


                            submittedTx <- submitTxConstraintsWith lookupsInit tx
                            void $ awaitTxConfirmed $ getCardanoTxId submittedTx

                        else do
                            logInfo @HASKELL.String $ printf "Cant' Get Rewards because there is not enough funds to cover the claim..."
                            return ()

                    [] -> do 
                        logInfo @HASKELL.String $ printf "Cant' Get Rewards from because can't find PoolState utxo with UserNFT register"
                        return ()
                    (_:_) -> do 
                        logInfo @HASKELL.String $ printf "Cant' Get Rewards from because can't find single PoolState utxo with UserNFT register"
                        return ()   


userInvestRewards ::  UserInvestRewardsParams -> Contract w s Text ()
userInvestRewards UserInvestRewardsParams{..} = do
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @HASKELL.String $ printf "--------------------------- User Invest Rewards : Init -------------------------------------"  
    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"      

    logInfo @HASKELL.String $ printf "--------------------------------------------------------------------------------------------"
    logInfo @HASKELL.String $ printf "--------------------------- User Invest Rewards : Engind -----------------------------------" 
    -- logInfo @HASKELL.String $ printf "Param: %s Datum: %s Value: %s" (HASKELL.show pmcpPoolParam) (HASKELL.show dPoolState) (HASKELL.show value) 
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

