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

module Validators.StakePlusV1.OffChain where

--Import Externos

import qualified Cardano.Api                         as CardanoApi  
import qualified Cardano.Api.Shelley                 as ApiShelley
import qualified Control.Monad                       as Monad (void)
import qualified Data.Map                            as DataMap
import qualified Data.Text                           as DataText ( Text)
import qualified Ledger                              (getCardanoTxId, pubKeyHashAddress) --PaymentPubKeyHash, --POSIXTime(POSIXTime), --, unPaymentPubKeyHash, valuePaidTo, Slot(Slot)
import qualified Ledger.Ada                          as LedgerAda
import qualified Ledger.Constraints                  as LedgerConstraints
import qualified Ledger.Tx                           as LedgerTx (ChainIndexTxOut (..))
import qualified Playground.Contract                 (mkSchemaDefinitions)
import qualified Plutus.Contract                     as PlutusContract
import qualified Plutus.V1.Ledger.Api                as LedgerApiV1
import qualified Plutus.V1.Ledger.Interval           as LedgerIntervalV1 (interval) --, contains,  , from, member
import qualified Plutus.V1.Ledger.Value              as LedgerValueV1
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Text.Printf                         as TextPrintf (printf)

--Import Internos

import qualified Validators.StakePlusV1.Helpers         as Helpers
import qualified Validators.StakePlusV1.OffChainHelpers as OffChainHelpers  
import qualified Validators.StakePlusV1.OnChain         as OnChain (typedValidator, codeValidator, addressValidator)
import qualified Validators.StakePlusV1.OnChainNFT      as OnChainNFT (mintingNFTPolicy)
import qualified Validators.StakePlusV1.Typos           as T
  
-- Modulo:

masterCreatePool :: T.MasterCreatePoolParams -> PlutusContract.Contract w s DataText.Text ()
masterCreatePool T.MasterCreatePoolParams{..} = do
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Create Pool : Init ---------------------------------------"  
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"   

    master <- PlutusContract.ownFirstPaymentPubKeyHash

    now <- PlutusContract.currentTime
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)

    let 
        masterAdds = Ledger.pubKeyHashAddress master Nothing
    
    utxosMaster <- PlutusContract.utxosAt masterAdds

    PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosMaster List: %s" (P.show utxosMaster)

    let 

        poolNFTTxOutRef = pmcpPoolNFTTxOutRef
        poolNFTTokenName    =  pmcpPoolNFTTokenName
        valuePoolNFT     = LedgerValueV1.assetClassValue (T.ppPoolNFT pmcpPoolParam) 1
       
        valueForPoolState = LedgerAda.lovelaceValueOf pmcpFund <> valuePoolNFT

        masterFunders_others = [T.mkMasterFunder masterParam 0 | masterParam <- T.ppMasters pmcpPoolParam ,  masterParam /= master] 
        masterFundersNew = T.mkMasterFunder master pmcpFund
        masterFunders = masterFundersNew:masterFunders_others

        userNFTs = []

        countPoolState = 1 -- estoy creando, hay uno solo
        cashedOut = 0

        dPoolState = T.mkPoolState (T.ppPoolNFT pmcpPoolParam) masterFunders userNFTs cashedOut countPoolState

        hashDatumPoolState = CardanoApi.hashScriptData $ ApiShelley.fromPlutusData $  PlutusTx.builtinDataToData  $  PlutusTx.toBuiltinData  dPoolState

        redeemerMinting   = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData $ T.MintingRedeemer { mrTokenName = poolNFTTokenName, mrTxOutRef = poolNFTTxOutRef} 
        
        ---

        validityRange        = LedgerIntervalV1.interval now (now + T.ppValidTimeRange pmcpPoolParam)

        ---

        lookupsInit = 
            -- This script is goint to use all the uxto from the user
            LedgerConstraints.unspentOutputs utxosMaster P.<> 
            -- Is sending value to script, it needs the typedValidatorLookups
            LedgerConstraints.plutusV1TypedValidatorLookups (OnChain.typedValidator pmcpPoolParam) P.<> 
             -- Is going to Mint the User NFT:
            LedgerConstraints.plutusV1MintingPolicy OnChainNFT.mintingNFTPolicy 
        tx = 
            -- Is going to create an utxo at the script with the PoolState 
            LedgerConstraints.mustPayToTheScript dPoolState valueForPoolState P.<> 
            -- Is going to spend the master uxto assinged to the NFT
            LedgerConstraints.mustSpendPubKeyOutput poolNFTTxOutRef P.<> 
            -- Is going to Mint the Pool NFT
            LedgerConstraints.mustMintValueWithRedeemer redeemerMinting valuePoolNFT P.<> 
            -- Is goint create the valid range based in T.ppValidTimeRange Pool Param
            LedgerConstraints.mustValidateIn validityRange

    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Create Pool : Ending ------------------------------------"  
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Param: %s" (P.show pmcpPoolParam)

    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolState Datum Hash: %s" (P.show hashDatumPoolState)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolState Datum: %s" (P.show dPoolState)


    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolState Value: %s" (P.show valueForPoolState)

    --PlutusContract.logInfo @P.String $ TextPrintf.printf "SubmittedTx: %s" (P.show submittedTx)    
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------" 
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"

    submittedTx <- PlutusContract.submitTxConstraintsWith lookupsInit tx
    Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId submittedTx

-- construye una tx con inputs:
-- desde el script aquella que tiene el NFT 
-- del usuario
-- como salida

masterFundPool :: T.MasterFundPoolParams -> PlutusContract.Contract w s DataText.Text ()
masterFundPool T.MasterFundPoolParams{..} = do
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Fund Pool : Init ----------------------------------------"  
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"  
    
    master <- PlutusContract.ownFirstPaymentPubKeyHash

    now <- PlutusContract.currentTime
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)

    let 
        masterAdds = Ledger.pubKeyHashAddress master Nothing

    utxosAtMaster <- PlutusContract.utxosAt masterAdds

    utxosListAtScriptWithPoolState <- OffChainHelpers.getUtxoListWithValidPoolStateInScript (OnChain.addressValidator pmfpPoolParam)

    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosListAtScriptWithPoolState List: %s" (P.show utxosListAtScriptWithPoolState)

    case utxosListAtScriptWithPoolState of
        [] -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Cant' Fund Pool because can't find any utxo with PoolState Datum at the Script Address. It need to be created first."
            return ()
        _ -> do

            let
                
                utxoAtScriptWithPoolStateAndNFT = OffChainHelpers.getUtxoWithNFTInList utxosListAtScriptWithPoolState (T.ppPoolNFT pmfpPoolParam)

            case utxoAtScriptWithPoolStateAndNFT of 

                Nothing -> do

                    PlutusContract.logInfo @P.String $ TextPrintf.printf "Cant' Fund Pool because can't find any utxo with PoolState Datum and Pool NFT at the Script Address"
                    return ()

                Just utxoAtScriptWithPoolStateAndNFT -> do

                    let 
                        valuesEnUtxosPoolStateOLD = OffChainHelpers.getValueFromChainIndexTxOut $ snd utxoAtScriptWithPoolStateAndNFT
                        
                        valueFundForPoolState  = LedgerAda.lovelaceValueOf pmfpFund

                        --valueTotalForPoolState = foldl (<>) valueFundForPoolState listValuesEnUtxosPoolStateList 

                        dPoolStateOLD = OffChainHelpers.mkPoolStateWithNewCountFundsFromUtxo utxoAtScriptWithPoolStateAndNFT (T.ppPoolNFT pmfpPoolParam)
                        hashDatumPoolStateOLD = CardanoApi.hashScriptData $ ApiShelley.fromPlutusData $  PlutusTx.builtinDataToData  $  PlutusTx.toBuiltinData  dPoolStateOLD

                        masterFunderNew = T.mkMasterFunder master pmfpFund

                        countPoolState = 0 -- prefiero que esten en cero todos menos el que tiene el NFT, alli la cuenta estará bien siempre
                        cashedOut = 0

                        dPoolState = T.mkPoolState (T.ppPoolNFT pmfpPoolParam) [masterFunderNew] [] cashedOut countPoolState
                        hashDatumPoolState = CardanoApi.hashScriptData $ ApiShelley.fromPlutusData $  PlutusTx.builtinDataToData  $  PlutusTx.toBuiltinData  dPoolState

                        txOutRefsPoolState   = fst <$> [utxoAtScriptWithPoolStateAndNFT]

                        redeemerValidator = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData (T.mkRedeemMasterFundPool (T.ppPoolNFT pmfpPoolParam)  master pmfpFund (head txOutRefsPoolState))
                        
                        ---

                        validityRange        = LedgerIntervalV1.interval now (now + T.ppValidTimeRange pmfpPoolParam)

                        ---

                        lookupsInit = 
                            -- This script is goint to use all the uxto from the user
                            -- TODO: no es necesario poner esto, se hace automaticamente
                            --       se necesita poner cuando uso mustSpendPubKeyOutput
                            LedgerConstraints.unspentOutputs utxosAtMaster P.<> 
                            -- Is also going to use the utxos at the script with PoolState    
                            LedgerConstraints.unspentOutputs (DataMap.fromList utxosListAtScriptWithPoolState)      P.<>
                            -- Is sending value to script, it needs the typedValidatorLookups
                            LedgerConstraints.plutusV1TypedValidatorLookups (OnChain.typedValidator pmfpPoolParam) P.<>
                            -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum
                            -- for speending it needs the code of the validator
                            LedgerConstraints.plutusV1OtherScript (OnChain.codeValidator pmfpPoolParam)

                        tx      = 
                            -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum with the new fund
                            mconcat [LedgerConstraints.mustSpendScriptOutput txOutRef redeemerValidator | txOutRef <- txOutRefsPoolState] P.<> 
                            -- Is going to create an utxo at the script with the new PoolState and the new value
                            LedgerConstraints.mustPayToTheScript dPoolState valueFundForPoolState P.<> 
                            -- Is going to create an utxo at the script with the OLD T.PoolState and the OLD value, not changed
                            LedgerConstraints.mustPayToTheScript dPoolStateOLD valuesEnUtxosPoolStateOLD P.<> 
                            -- Is goint create the valid range based in T.ppValidTimeRange Pool Param
                            LedgerConstraints.mustValidateIn validityRange    


                    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Fund Pool : Ending ---------------------------------------" 

                    PlutusContract.logInfo @P.String $ TextPrintf.printf "Param: %s" (P.show pmfpPoolParam)

                    PlutusContract.logInfo @P.String $ TextPrintf.printf "OLD PoolState Datum Hash: %s" (P.show hashDatumPoolStateOLD)
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "OLD PoolState Datum: %s" (P.show dPoolStateOLD)
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "OLD PoolState Value Fund: %s" (P.show valuesEnUtxosPoolStateOLD)

                    PlutusContract.logInfo @P.String $ TextPrintf.printf "NEW PoolState Datum Hash: %s" (P.show hashDatumPoolState)
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "NEW PoolState Datum: %s" (P.show dPoolState)
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "NEW PoolState Value Fund: %s" (P.show valueFundForPoolState)

                    --PlutusContract.logInfo @P.String $ TextPrintf.printf "SubmittedTx: %s" (P.show submittedTx)    
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------" 
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"  

                    submittedTx <- PlutusContract.submitTxConstraintsWith lookupsInit tx
                    Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId submittedTx


masterFundAndMergePool :: T.MasterFundAndMergePoolParams -> PlutusContract.Contract w s DataText.Text ()
masterFundAndMergePool T.MasterFundAndMergePoolParams{..} = do
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Fund And Merge Pool : Init ------------------------------"  
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"  
    
    master <- PlutusContract.ownFirstPaymentPubKeyHash

    now <- PlutusContract.currentTime
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)

    let 
        masterAdds = Ledger.pubKeyHashAddress master Nothing

    utxosAtMaster <- PlutusContract.utxosAt masterAdds

    utxosListAtScriptWithPoolState <- OffChainHelpers.getUtxoListWithValidPoolStateInScript (OnChain.addressValidator pmfampPoolParam)

    PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosListAtScriptWithPoolState List: %s" (P.show utxosListAtScriptWithPoolState)

    case utxosListAtScriptWithPoolState of
        [] -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Cant' Fund Pool because can't find any utxo with PoolState Datum at the Script Address. It need to be created firts."
            return ()
        _ -> do

            let
                
                utxosListAtScriptWithPoolStateToMerge = [ (txOutRef, chainIndexTxOut)  |  (txOutRef, chainIndexTxOut) <- utxosListAtScriptWithPoolState, txOutRef `elem` pmfampUtxoToMerge]
                

            PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosListAtScriptWithPoolStateToMerge List: %s" (P.show utxosListAtScriptWithPoolState)

            let
                
                utxoAtScriptWithPoolStateAndNFT = OffChainHelpers.getUtxoWithNFTInList utxosListAtScriptWithPoolStateToMerge (T.ppPoolNFT pmfampPoolParam)

            case utxoAtScriptWithPoolStateAndNFT of 

                Nothing -> do

                    PlutusContract.logInfo @P.String $ TextPrintf.printf "Cant' Fund And Merge Pool because can't find any utxo with PoolState Datum and Pool NFT at the chossen uxtos to merge"
                    return ()

                Just utxoAtScriptWithPoolStateAndNFT -> do

                    let
                        
                        poolStateDatumOfUtxoWithNFT = OffChainHelpers.getPoolStateFromUtxo utxoAtScriptWithPoolStateAndNFT
                        countPoolState = T.psCountTotalUtxoWithPoolState poolStateDatumOfUtxoWithNFT - length utxosListAtScriptWithPoolStateToMerge + 1

                        listValuesEnUtxosPoolStateList = [ OffChainHelpers.getValueFromChainIndexTxOut $ snd utxo | utxo <- utxosListAtScriptWithPoolStateToMerge] 
                        
                        valueFundForPoolState  = LedgerAda.lovelaceValueOf pmfampFund

                        valueTotalForPoolState = foldl (<>) valueFundForPoolState listValuesEnUtxosPoolStateList 

                        dPoolState = OffChainHelpers.mkPoolStateWithNewFundFromUtxoList utxosListAtScriptWithPoolStateToMerge (T.ppPoolNFT pmfampPoolParam) master pmfampFund countPoolState
                        hashDatumPoolState = CardanoApi.hashScriptData $ ApiShelley.fromPlutusData $  PlutusTx.builtinDataToData  $  PlutusTx.toBuiltinData  dPoolState

                        redeemerValidator = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData (T.mkRedeemMasterFundAndMergePool (T.ppPoolNFT pmfampPoolParam)  master pmfampFund  (fst <$> utxosListAtScriptWithPoolStateToMerge))
                        
                        txOutRefsPoolState   = fst <$> utxosListAtScriptWithPoolStateToMerge
                        
                        ---

                        validityRange        = LedgerIntervalV1.interval now (now + T.ppValidTimeRange pmfampPoolParam)

                        ---

                        lookupsInit = 
                            -- This script is goint to use all the uxto from the user
                            -- TODO: no es necesario poner esto, se hace automaticamente
                            --       se necesita poner cuando uso mustSpendPubKeyOutput
                            LedgerConstraints.unspentOutputs utxosAtMaster P.<> 
                            -- Is also going to use the utxos at the script with PoolState    
                            LedgerConstraints.unspentOutputs (DataMap.fromList utxosListAtScriptWithPoolStateToMerge)      P.<>
                            -- Is sending value to script, it needs the typedValidatorLookups
                            LedgerConstraints.plutusV1TypedValidatorLookups (OnChain.typedValidator pmfampPoolParam) P.<>
                            -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum
                            -- for speending it needs the code of the validator
                            LedgerConstraints.plutusV1OtherScript (OnChain.codeValidator pmfampPoolParam)

                        tx      = 
                            -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum with the new fund
                            mconcat [LedgerConstraints.mustSpendScriptOutput txOutRef redeemerValidator | txOutRef <- txOutRefsPoolState] P.<> 
                            -- Is going to create an utxo at the script with the new PoolState and the new value
                            LedgerConstraints.mustPayToTheScript dPoolState valueTotalForPoolState P.<> 
                            -- Is goint create the valid range based in T.ppValidTimeRange Pool Param
                            LedgerConstraints.mustValidateIn validityRange    


                    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Fund And Merge Pool : Ending -----------------------------" 

                    PlutusContract.logInfo @P.String $ TextPrintf.printf "Param: %s" (P.show pmfampPoolParam)

                    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolState Datum Hash: %s" (P.show hashDatumPoolState)
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolState Datum: %s" (P.show dPoolState)
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolState Value Fund: %s" (P.show valueFundForPoolState)
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolState Value: %s" (P.show valueTotalForPoolState)

                    --PlutusContract.logInfo @P.String $ TextPrintf.printf "SubmittedTx: %s" (P.show submittedTx)    
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------" 
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"  

                    submittedTx <- PlutusContract.submitTxConstraintsWith lookupsInit tx
                    Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId submittedTx

masterGetBackFund ::  T.MasterGetBackFundParams -> PlutusContract.Contract w s DataText.Text ()
masterGetBackFund T.MasterGetBackFundParams{..} = do
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master GetBack Fund : Init -------------------------------------"  
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"     

    -- master <- PlutusContract.ownFirstPaymentPubKeyHash
    -- now <- PlutusContract.currentTime
    -- utxosFromMaster <- findUtxosFromMasters (OnChain.addressValidator T.pmgbfPoolParam) master 

    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "findUtxoFromMaster: %s" (P.show utxosFromMaster)

    -- case utxosFromMaster of

    --     [] -> Plutus.PlutusContract.throwError "Pool From Master Not Found" 

    --     _  -> do

    --         let 
    --             redeemer      = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData mkRedeemMasterGetPool

    --             txOutRefs   = fst <$> utxosFromMaster
                
    --             lookupsInit = LedgerConstraints.unspentOutputs (DataMap.fromList utxosFromMaster)      P.<>
    --                     LedgerConstraints.plutusV1TypedValidatorLookups (OnChain.typedValidator T.pmgbfPoolParam) P.<>
    --                     LedgerConstraints.plutusV1OtherScript (OnChain.codeValidator T.pmgbfPoolParam)

                
    --             tx      = mconcat [LedgerConstraints.mustSpendScriptOutput txOutRef redeemer | txOutRef <- txOutRefs]
    --                 P.<> LedgerConstraints.mustValidateIn (from now)
    --             -- LedgerConstraints.mustPayToPubKey master vGetADA  

    --         submittedTx <- PlutusContract.submitTxConstraintsWith lookupsInit tx
    --         Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId submittedTx

    --         PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    --         PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master GetBack Fund -------------------------------------------" 
    --         PlutusContract.logInfo @P.String $ TextPrintf.printf "Param: %s " (P.show pmgbfPoolParam) 
    --         PlutusContract.logInfo @P.String $ TextPrintf.printf "submittedTx: %s" (P.show submittedTx)    
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------" 
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"  

userInvest ::  T.UserInvestParams -> PlutusContract.Contract w s DataText.Text ()
userInvest T.UserInvestParams{..} = do
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User Invest : Init ----------------------------------------------"  
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"      

    user <- PlutusContract.ownFirstPaymentPubKeyHash

    now <- PlutusContract.currentTime
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)

    let 
        userAdds = Ledger.pubKeyHashAddress user Nothing

    utxosAtUser <- PlutusContract.utxosAt userAdds

    utxosListAtScriptWithPoolState <- OffChainHelpers.getUtxoListWithValidPoolStateInScript (OnChain.addressValidator puiPoolParam) 

    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosListAtScriptWithPoolState List: %s" (P.show utxosListAtScriptWithPoolState)

    case utxosListAtScriptWithPoolState of
        [] -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Cant' Invest in Pool because can't find any utxo with PoolState Datum at the Script Address"
            return ()
        _ -> do
            let 
                
                -- Create User NFT 

                userNFTTxOutRef  = puiUserNFTTxOutRef
                userNFTTokenName  = puiUserNFTTokenName
                userNFT = LedgerValueV1.assetClass (Helpers.curSymbol OnChainNFT.mintingNFTPolicy) userNFTTokenName

                valueNFTForUser     = LedgerValueV1.assetClassValue userNFT 1
                valueNFTPlusMinimunAdaForUser = valueNFTForUser <>  LedgerAda.lovelaceValueOf Helpers.minLovelace

                valueForUserState = LedgerAda.lovelaceValueOf puiInvest 

                -- Creates UserState Datum

                dUserState = T.mkUserState user userNFT puiInvest puiCreatedAt puiDeadline 0 0 Nothing
                hashDatumUserState = CardanoApi.hashScriptData $ ApiShelley.fromPlutusData $  PlutusTx.builtinDataToData  $  PlutusTx.toBuiltinData  dUserState
                
                -- Creates T.PoolState Datum with the New User

                poolNFT = T.ppPoolNFT puiPoolParam    

                utxoAtScriptWithPoolState = head utxosListAtScriptWithPoolState

                valueForPoolState = OffChainHelpers.getValueFromChainIndexTxOut $ snd utxoAtScriptWithPoolState 

                dPoolState = OffChainHelpers.mkPoolStateWithNewUserInvestFromUtxo utxoAtScriptWithPoolState poolNFT userNFT
                hashDatumPoolState = CardanoApi.hashScriptData $ ApiShelley.fromPlutusData $  PlutusTx.builtinDataToData  $  PlutusTx.toBuiltinData  dPoolState

                txOutRefPoolState   = fst utxoAtScriptWithPoolState

                -- listValuesEnUtxosPoolStateList = [ OffChainHelpers.getValueFromChainIndexTxOut $ snd utxo | utxo <- utxosListAtScriptWithPoolState] 
    
                -- valueForPoolState = foldl (<>) (LedgerAda.lovelaceValueOf 0) listValuesEnUtxosPoolStateList 

                -- dPoolState = T.mkPoolStateWithNewUserInvestFromUtxoList utxosListAtScriptWithPoolState poolNFT userNFT

                -- txOutRefsPoolState   = fst <$> utxosListAtScriptWithPoolState
                
                -- Creates Redeemer

                redeemerValidator = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData (T.mkRedeemUserInvest poolNFT userNFT  userNFTTokenName userNFTTxOutRef  user puiInvest puiCreatedAt puiDeadline )
                redeemerMinting   = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData $ T.MintingRedeemer { mrTokenName = userNFTTokenName, mrTxOutRef = userNFTTxOutRef} 

                ---

                validityRange        = LedgerIntervalV1.interval puiCreatedAt (puiCreatedAt + T.ppValidTimeRange puiPoolParam)

                ---

                lookupsInit = 
                    -- This script is goint to use all the uxto from the user
                    LedgerConstraints.unspentOutputs utxosAtUser P.<> 
                    -- Is also going to use the utxo at the script with PoolState    
                    LedgerConstraints.unspentOutputs (DataMap.fromList utxosListAtScriptWithPoolState)  P.<> 
                     -- Is going to Mint the User NFT: 
                    LedgerConstraints.plutusV1MintingPolicy OnChainNFT.mintingNFTPolicy    P.<> 
                    -- Is sending value to script, it needs the typedValidatorLookups
                    LedgerConstraints.plutusV1TypedValidatorLookups (OnChain.typedValidator puiPoolParam) P.<> 
                    -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum
                    -- for speending it needs the code of the validator
                    LedgerConstraints.plutusV1OtherScript (OnChain.codeValidator puiPoolParam)

                tx = 
                    -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum
                    LedgerConstraints.mustSpendScriptOutput txOutRefPoolState redeemerValidator P.<> 
                    -- Is going to create an utxo at the script with the new UserState and the value of the New T.Invest
                    LedgerConstraints.mustPayToTheScript dUserState valueForUserState P.<> 
                    -- Is going to create an utxo at the script with the new PoolState with the New T.UserNFT and the value is the actual value at the Script
                    LedgerConstraints.mustPayToTheScript dPoolState valueForPoolState  P.<> 
                    -- Is goint to send the mint userNFT to the user wallet 
                    LedgerConstraints.mustPayToPubKey user valueNFTPlusMinimunAdaForUser  P.<> 
                    -- Is going to spend the user uxto assinged to the NFT
                    LedgerConstraints.mustSpendPubKeyOutput userNFTTxOutRef P.<> 
                    -- Is going to Mint the User NFT:
                    LedgerConstraints.mustMintValueWithRedeemer redeemerMinting valueNFTForUser P.<> 
                    -- Is goint create the valid range based in T.ppValidTimeRange Pool Param
                    LedgerConstraints.mustValidateIn validityRange



            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User Invest : Ending -------------------------------------------"  
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Param: %s" (P.show puiPoolParam)

            PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolState Datum Hash: %s" (P.show hashDatumPoolState)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolState Datum: %s" (P.show dPoolState)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolState Value: %s" (P.show valueForPoolState)

            PlutusContract.logInfo @P.String $ TextPrintf.printf "UserState Datum Hash: %s" (P.show hashDatumUserState)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "UserState Datum: %s" (P.show dUserState)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "UserState Value: %s" (P.show valueForUserState)
            
            
            
            PlutusContract.logInfo @P.String $ TextPrintf.printf "User Wallet NFT Value: %s" (P.show valueNFTPlusMinimunAdaForUser)    

            -- PlutusContract.logInfo @P.String $ TextPrintf.printf "SubmittedTx: %s" (P.show submittedTx)   
            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"      
            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"    

            submittedTx <- PlutusContract.submitTxConstraintsWith lookupsInit tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId submittedTx  

userGetBackInvest ::  T.UserGetBackInvestParams -> PlutusContract.Contract w s DataText.Text ()
userGetBackInvest T.UserGetBackInvestParams{..} = do
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User GetBack Invest : Init --------------------------------------" 
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"      

    -- user <- PlutusContract.ownFirstPaymentPubKeyHash
    -- now <- PlutusContract.currentTime
    -- utxosFromUserWithDeadline <- findUtxosFromUserWithDeadline (OnChain.addressValidator T.pugbiPoolParam) user T.pugbiDeadline 

    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "findUtxosFromUserWithDeadline: %s" (P.show utxosFromUserWithDeadline)

    -- case utxosFromUserWithDeadline of

    --     [] -> Plutus.PlutusContract.throwError "User Invest Not Found" 

    --     _  -> do

    --         let 
    --             redeemer      = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData mkRedeemUserGetInvest

    --             txOutRefs   = fst <$> utxosFromUserWithDeadline
                
    --             lookupsInit = LedgerConstraints.unspentOutputs (DataMap.fromList utxosFromUserWithDeadline)      P.<>
    --                     LedgerConstraints.plutusV1TypedValidatorLookups (OnChain.typedValidator T.pugbiPoolParam) P.<>
    --                     LedgerConstraints.plutusV1OtherScript (OnChain.codeValidator T.pugbiPoolParam)

    --             tx      = mconcat [LedgerConstraints.mustSpendScriptOutput txOutRef redeemer | txOutRef <- txOutRefs]
    --                 P.<> LedgerConstraints.mustValidateIn (from now)
    --             -- LedgerConstraints.mustPayToPubKey user vGetADA  

    --         submittedTx <- PlutusContract.submitTxConstraintsWith lookupsInit tx
    --         Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId submittedTx

    --         PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    --         PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User GetBack Invest --------------------------------------------"  
    --         PlutusContract.logInfo @P.String $ TextPrintf.printf "Param: %s " (P.show pugbiPoolParam) 
    --         PlutusContract.logInfo @P.String $ TextPrintf.printf "submittedTx: %s" (P.show submittedTx)   
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"      
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"      

userGetRewards ::  T.UserGetRewardsParams -> PlutusContract.Contract w s DataText.Text ()
userGetRewards T.UserGetRewardsParams{..} = do

    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User Get Rewards : Init ----------------------------------------"  
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"    

    user <- PlutusContract.ownFirstPaymentPubKeyHash
    
    now <- PlutusContract.currentTime
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    
    let 
        userAdds = Ledger.pubKeyHashAddress user Nothing
        -- Re creates NFT por comparing and redeemer parameter
        userNFTTxOutRef  = pugrUserNFTTxOutRef
        userNFTTokenName  = pugrUserNFTTokenName
        userNFT = LedgerValueV1.assetClass (Helpers.curSymbol OnChainNFT.mintingNFTPolicy) userNFTTokenName
    
        
    utxosAtUser <- PlutusContract.utxosAt userAdds

    utxosListAtScriptWithPoolState <- OffChainHelpers.getUtxoListWithValidPoolStateInScript (OnChain.addressValidator pugrPoolParam) 
    PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosListAtScriptWithPoolState List: %s" (P.show utxosListAtScriptWithPoolState)

    PlutusContract.logInfo @P.String $ TextPrintf.printf "userNFT: %s" (P.show userNFT)
    utxosListAtScriptWithUserState <- OffChainHelpers.getUtxoListWithValidUserStateInScript (OnChain.addressValidator pugrPoolParam) userNFT 
    PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosListAtScriptWithUserState List: %s" (P.show utxosListAtScriptWithUserState)

   

    case (utxosListAtScriptWithPoolState,utxosListAtScriptWithUserState) of
        ([], _) -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Cant' Get Rewards from Pool because can't find any utxo with PoolState Datum at the Script Address"
            return ()
        (_, []) -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Cant' Get Rewards from because can't find any utxo with UserState Datum with this UserNFT at the Script Address"
            return ()
        (_, _:[_]) -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Cant' Get Rewards from because can't find unic single utxo with UserState Datum with this UserNFT at the Script Address"
            return ()
        (_, [utxoAtScriptWithUserState]) -> do

            let 
                
                
                valueNFTForUser    = LedgerValueV1.assetClassValue userNFT 1
                valueNFTPlusMinimunAdaForUser  = valueNFTForUser <>  LedgerAda.lovelaceValueOf Helpers.minLovelace

                valueClaimRewardsForUser = LedgerAda.lovelaceValueOf pugrClaim

                dUserStateOLD = OffChainHelpers.getUserStateFromUtxo utxoAtScriptWithUserState

                valueForUserState = OffChainHelpers.getValueFromChainIndexTxOut $ snd utxoAtScriptWithUserState 

                -- Creates UserState Datum

                rewards = Helpers.getRewardsPerInvest (T.usLastClaimAt dUserStateOLD) now  (T.usCreatedAt  dUserStateOLD )  (T.usInvest dUserStateOLD ) 
                totalNewRewards = rewards  + T.usRewardsNotClaimed dUserStateOLD
                rewardsNotClaimed = totalNewRewards - pugrClaim
                totalRewardsCashedOut = T.usChashedOut dUserStateOLD + pugrClaim 

            PlutusContract.logInfo @P.String $ TextPrintf.printf "CALCULATED Claiming " ++ P.show pugrClaim
            PlutusContract.logInfo @P.String $ TextPrintf.printf "CALCULATED New Rewards " ++ P.show rewards
            PlutusContract.logInfo @P.String $ TextPrintf.printf "CALCULATED usRewardsNotClaimed OLD " ++ P.show (T.usRewardsNotClaimed dUserStateOLD)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "CALCULATED totalNewRewards " ++ P.show totalNewRewards
            PlutusContract.logInfo @P.String $ TextPrintf.printf "CALCULATED rewardsNotClaimed " ++ P.show rewardsNotClaimed
            PlutusContract.logInfo @P.String $ TextPrintf.printf "CALCULATED totalRewardsCashedOut " ++ P.show totalRewardsCashedOut

            if  pugrClaim > totalNewRewards then do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "Trying to get too many rewards... wait some time"
                return ()
            else do
                let

                    -- TODO: throwError "ERROR " si es menor que cero

                    dUserState = T.mkUserState user userNFT 
                        (T.usInvest dUserStateOLD)
                        (T.usCreatedAt dUserStateOLD)
                        (T.usDeadline dUserStateOLD)
                        totalRewardsCashedOut
                        rewardsNotClaimed
                        (Just now)

                    hashDatumUserState = CardanoApi.hashScriptData $ ApiShelley.fromPlutusData $  PlutusTx.builtinDataToData  $  PlutusTx.toBuiltinData  dUserState
                    

                    txOutRefUserState   = fst utxoAtScriptWithUserState

                    -- Creates T.PoolState Datum

                    poolNFT = T.ppPoolNFT pugrPoolParam    

                    -- find utxo T.PoolState where the T.UserNFT was register in
                    findUxtoPoolStateWithUserNFT :: [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)] -> T.UserNFT -> [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]
                    findUxtoPoolStateWithUserNFT utxosListAtScriptWithPoolState userNFT =
                            [  utxoAtScriptWithPoolState  | utxoAtScriptWithPoolState <- utxosListAtScriptWithPoolState, userNFT `elem` T.psUsersNFT (OffChainHelpers.getPoolStateFromUtxo utxoAtScriptWithPoolState  )] 
                            
    
                    -- find utxo T.PoolState where the T.UserNFT was register in
                    utxoAtScriptWithPoolStateWithUserNFT = findUxtoPoolStateWithUserNFT utxosListAtScriptWithPoolState userNFT
                
                PlutusContract.logInfo @P.String $ TextPrintf.printf "utxoAtScriptWithPoolStateWithUserNFT List: %s" (P.show $ fst <$> utxoAtScriptWithPoolStateWithUserNFT)

                case utxoAtScriptWithPoolStateWithUserNFT of 
                     
                    [utxoAtScriptWithPoolStateWithUserNFT] -> do

                        let 
                
                            -- get the list of utxo T.PoolState without the one it just found
                            utxosListAtScriptWithPoolStateWithoutUserNFT = OffChainHelpers.removeUxtoByTxOutRef (fst utxoAtScriptWithPoolStateWithUserNFT) utxosListAtScriptWithPoolState
                            
                        PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosListAtScriptWithPoolStateWithoutUserNFT List: %s" (P.show $ fst <$> utxosListAtScriptWithPoolStateWithoutUserNFT)

                        let 
                            -- for ordering a list of uxto with chainindex
                            compareValueOfUtxoList :: (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut) -> (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut) -> Ordering
                            compareValueOfUtxoList utxo1 utxo2
                                | LedgerAda.fromValue (OffChainHelpers.getValueFromChainIndexTxOut(snd utxo1)) > LedgerAda.fromValue (OffChainHelpers.getValueFromChainIndexTxOut( snd utxo2) )= LT
                                | otherwise = GT

                            -- order the list of uxto by value
                            utxosOrderedListAtScriptWithPoolState = sortBy compareValueOfUtxoList utxosListAtScriptWithPoolStateWithoutUserNFT
                        PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosOrderedListAtScriptWithPoolState List: %s" (P.show $ fst <$> utxosOrderedListAtScriptWithPoolState)

                        let 
  
                            isEnoughToClaimInUxtoList :: [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)] -> T.Proffit -> Bool
                            isEnoughToClaimInUxtoList utxosListAtScriptWithPoolState claim = do
                               
                               claim <=0 || ( 
                                    (length utxosListAtScriptWithPoolState > 0) && (
                                        do                
                                            -- this means that there is still claim to cover... i need to keep adding utxo if there is more in the list
                                            let 
                                                utxo = head utxosListAtScriptWithPoolState
                                                -- cant take the full value of the utxo, because i need to leave the min ada in the utxo, that is why i rest Helpers.minLovelace (<> negate LedgerAda.lovelaceValueOf Helpers.minLovelace)
                                                value = OffChainHelpers.getValueFromChainIndexTxOut $ snd utxo
                                                valueCanUse    = value <> negate (LedgerAda.lovelaceValueOf Helpers.minLovelace)
                                                adaFromValueCanUse = LedgerAda.getLovelace (LedgerAda.fromValue valueCanUse) 
                                                -- if the uxto have 5ada, i just consider 5ADA - 2ADA (min) = 3ADA i can use
                                                newClaim = claim - adaFromValueCanUse

                                            isEnoughToClaimInUxtoList (tail utxosListAtScriptWithPoolState) newClaim
                                    )
                                )
                           
                            -- get the small list of uxto which can cover the paymet of the reward claimed
                            getUtxoListWithEnoughValueToClaim :: [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)] -> T.Proffit -> [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]
                            getUtxoListWithEnoughValueToClaim utxosListAtScriptWithPoolState claim = do
                                
                                if (claim > 0) && (length utxosListAtScriptWithPoolState > 0) then do
                                    -- this means that there is still claim to cover... i need to keep adding utxo if there is more in the list
                                    let 
                                        utxo = head utxosListAtScriptWithPoolState
                                        -- cant take the full value of the utxo, because i need to leave the min ada in the utxo, that is why i rest Helpers.minLovelace (<> negate LedgerAda.lovelaceValueOf Helpers.minLovelace)
                                        value = OffChainHelpers.getValueFromChainIndexTxOut $ snd utxo
                                        valueCanUse    = value <> negate (LedgerAda.lovelaceValueOf Helpers.minLovelace)
                                        adaFromValueCanUse = LedgerAda.getLovelace (LedgerAda.fromValue valueCanUse) 
                                        -- if the uxto have 5ada, i just consider 5ADA - 2ADA (min) = 3ADA i can use

                                        newClaim = claim - adaFromValueCanUse

                                    utxo : getUtxoListWithEnoughValueToClaim (tail utxosListAtScriptWithPoolState) newClaim
                                else
                                    -- all the claim is cover, dont need to add more utxo
                                    -- could be that the list of utxo finih and i couldnt cover all
                                    []            

                        let
                            -- the uxto with the T.UserNFT must be included beyonf its value
                            -- anyways I need to consider its value
                            -- the new claim is the result of substracting the value of this uxto to the original claimed valueOf
                            valueUtxoAtScriptWithPoolStateWithUserNFT = OffChainHelpers.getValueFromChainIndexTxOut $ snd utxoAtScriptWithPoolStateWithUserNFT
                            valueCanUse    = valueUtxoAtScriptWithPoolStateWithUserNFT <> negate (LedgerAda.lovelaceValueOf Helpers.minLovelace)
                            adaFromValueCanUse = LedgerAda.getLovelace (LedgerAda.fromValue valueCanUse) 
                            -- cant take the full value of the utxo, because i need to leave the min ada in the utxo, that is why i rest Helpers.minLovelace (<> negate LedgerAda.lovelaceValueOf Helpers.minLovelace)    
                            claim = pugrClaim - adaFromValueCanUse
                            -- The resulting list of utxo is including in the firts utxo the utxo with the userNFT and the rest is a calling getUtxoListWithEnoughValueToClaim wo find the minumun listo of utxo wich can cover the payment.
                            utxosListAtScriptWithPoolStateWithEnoughValueToClaim = utxoAtScriptWithPoolStateWithUserNFT : getUtxoListWithEnoughValueToClaim utxosOrderedListAtScriptWithPoolState claim
                        
                        PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosListAtScriptWithPoolStateWithEnoughValueToClaim List: %s" (P.show $ fst <$> utxosListAtScriptWithPoolStateWithEnoughValueToClaim)

                         -- could be possible that there is no enough utxo to cover the claim 
                        if isEnoughToClaimInUxtoList utxosListAtScriptWithPoolStateWithEnoughValueToClaim pugrClaim then do
                            
                            let 
                                -- it creates the PoolState Datum, hash and value to each of the uxto selected
                                getPoolStateListwithNewValues :: [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)] -> T.Proffit -> PlutusContract.Contract w0 s0 e0 ([(T.ValidatorDatum, CardanoApi.Hash CardanoApi.ScriptData, LedgerValueV1.Value)])
                                getPoolStateListwithNewValues utxosListAtScriptWithPoolStateWithEnoughValueToClaim claim = do
                                    
                                    PlutusContract.logInfo @P.String $ TextPrintf.printf "Looking for utxo in: %s" (P.show $ fst <$> utxosListAtScriptWithPoolStateWithEnoughValueToClaim)
                                    PlutusContract.logInfo @P.String $ TextPrintf.printf "Looking for utxo to claim: %s" (P.show  claim)


                                    if (claim > 0) && (length utxosListAtScriptWithPoolStateWithEnoughValueToClaim > 0)  then do
                                        let 
                                            utxo = head utxosListAtScriptWithPoolStateWithEnoughValueToClaim

                                            poolStateDatum = OffChainHelpers.getPoolStateFromUtxo utxo

                                            -- cant take the full value, i cant let in 0ADA, need to leave at least 2ADA
                                            value = OffChainHelpers.getValueFromChainIndexTxOut $ snd utxo
                                            valueCanUse    = value <> negate (LedgerAda.lovelaceValueOf Helpers.minLovelace)
                                            adaFromValueCanUse = LedgerAda.getLovelace (LedgerAda.fromValue valueCanUse) 

                                        PlutusContract.logInfo @P.String $ TextPrintf.printf "utxo: %s" (P.show  utxo)
                                        PlutusContract.logInfo @P.String $ TextPrintf.printf "poolStateDatum: %s" (P.show  poolStateDatum)
                                        PlutusContract.logInfo @P.String $ TextPrintf.printf "value: %s" (P.show  value)
                                        PlutusContract.logInfo @P.String $ TextPrintf.printf "valueCanUse: %s" (P.show  valueCanUse)
                                        PlutusContract.logInfo @P.String $ TextPrintf.printf "adaFromValueCanUse: %s" (P.show  adaFromValueCanUse)

                                        if adaFromValueCanUse - claim >= 0 then do
                                            -- this means that with this uxto i cover all the claim, dont need to keep adding utxo
                                            let     
                                                -- newValue add the minimun ada i sustracted before
                                                newValue = value  <> negate ( LedgerAda.lovelaceValueOf claim)
                                                newOutputDatum = Helpers.mkPoolStateWithNewClaimRewardsFromPoolState poolStateDatum poolNFT claim
                                            PlutusContract.logInfo @P.String $ TextPrintf.printf "newOutputDatum1: %s" (P.show  newOutputDatum)
                                            PlutusContract.logInfo @P.String $ TextPrintf.printf "new value: %s" (P.show  newValue)
                                            let
                                                hashDatumPoolState = CardanoApi.hashScriptData $ ApiShelley.fromPlutusData $  PlutusTx.builtinDataToData  $  PlutusTx.toBuiltinData  newOutputDatum
                                
                                            return [(newOutputDatum,hashDatumPoolState, newValue)]
                                        else do
                                            -- this means that remaining claim is bigger than the value, so i need to keep adding utxo
                                            -- ill take all i can from this utxo
                                            let     
                                               
                                                newValue = value <> negate ( LedgerAda.lovelaceValueOf    adaFromValueCanUse )
                                                newOutputDatum = Helpers.mkPoolStateWithNewClaimRewardsFromPoolState poolStateDatum poolNFT adaFromValueCanUse   

                                            PlutusContract.logInfo @P.String $ TextPrintf.printf "newOutputDatum2: %s" (P.show  newOutputDatum)
                                            PlutusContract.logInfo @P.String $ TextPrintf.printf "new value: %s" (P.show  newValue)
                                            
                                            let
                                                hashDatumPoolState = CardanoApi.hashScriptData $ ApiShelley.fromPlutusData $  PlutusTx.builtinDataToData  $  PlutusTx.toBuiltinData  newOutputDatum
                                
                                                newClaim = claim - adaFromValueCanUse
                                                
                                                new = (newOutputDatum,hashDatumPoolState, newValue)

                                            others <- getPoolStateListwithNewValues (tail utxosListAtScriptWithPoolStateWithEnoughValueToClaim) newClaim  

                                            return (new : others)
                                    else
                                        return []            

                                -- --listValuesEnUtxosPoolStateList = [ OffChainHelpers.getValueFromChainIndexTxOut $ snd utxo | utxo <- utxosListAtScriptWithPoolState] 
                                -- --poolStateDatums = getPoolStateListFromUtxoList utxosListAtScriptWithPoolState 
                                -- --valueForPoolState = foldl (<>) (negate (LedgerAda.lovelaceValueOf pugrClaim)) listValuesEnUtxosPoolStateList 
                                -- --dPoolState = T.mkPoolStateFromPoolStateList poolStateDatums poolNFT   

                                -- for all the listo of chossen uxto I need to create a list of T.PoolState datum to sit with each one of them, with the cashedOut Value updated.
                                -- all the uxto final values plus the cashedOut in the datum need to be equal to the sum of the fundings in the datums
                            dPoolStateListWithNewValues <- getPoolStateListwithNewValues utxosListAtScriptWithPoolStateWithEnoughValueToClaim pugrClaim

                            let
                                txOutRefsPoolState   = fst <$> utxosListAtScriptWithPoolStateWithEnoughValueToClaim

                            PlutusContract.logInfo @P.String $ TextPrintf.printf "txOutRefsPoolState List: %s" (P.show txOutRefsPoolState)
                            PlutusContract.logInfo @P.String $ TextPrintf.printf "dPoolStateListWithNewValues List: %s" (P.show dPoolStateListWithNewValues)
                            

                            let  
                                newDummyInputDatum = Helpers.mkDummyPoolStateWithNewClaimRewardsFromPoolStateList (OffChainHelpers.getPoolStateFromUtxo <$> utxosListAtScriptWithPoolStateWithEnoughValueToClaim) poolNFT pugrClaim    
                                newDummyOutputDatum = Helpers.mkDummyPoolStateFromPoolStateList ((Helpers.fromJust.  Helpers.getPoolStateFromDatum . Helpers.get1st) <$> dPoolStateListWithNewValues) poolNFT          

                               
                            PlutusContract.logInfo @P.String $ TextPrintf.printf "newDummyInputDatum List: %s" (P.show newDummyInputDatum)
                            PlutusContract.logInfo @P.String $ TextPrintf.printf "newDummyOutputDatum List: %s" (P.show newDummyOutputDatum)
                            
                            PlutusContract.logInfo @P.String $ TextPrintf.printf "newDummyOutputDatum EQ: %s" (P.show (newDummyOutputDatum == newDummyInputDatum))

                            let    
                                -- Creates Redeemer

                                redeemerValidator = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData (T.mkRedeemUserGetRewards poolNFT userNFT  user pugrClaim now )
                                
                                ---

                                validityRange        = LedgerIntervalV1.interval now (now + T.ppValidTimeRange pugrPoolParam)

                                ---

                                lookupsInit = 
                                    -- This script is goint to use all the uxto from the user 
                                    LedgerConstraints.unspentOutputs utxosAtUser P.<> 
                                    -- Is also going to use the utxo at the script with PoolState 
                                    LedgerConstraints.unspentOutputs (DataMap.fromList utxosListAtScriptWithPoolState)  P.<> 
                                    -- Is also going to use the utxo at the script with UserState  
                                    LedgerConstraints.unspentOutputs (DataMap.fromList [utxoAtScriptWithUserState])  P.<> 
                                    -- Is sending value to script, it needs the typedValidatorLookups
                                    LedgerConstraints.plutusV1TypedValidatorLookups (OnChain.typedValidator pugrPoolParam) P.<> 
                                    -- Is going to spend the uxto at the script with PoolState Datums and UserState Datums
                                    -- for speending it needs the code of the validator
                                    LedgerConstraints.plutusV1OtherScript (OnChain.codeValidator pugrPoolParam)

                                tx = 
                                    -- Is going to spend the uxto at the script with PoolState Datums, because is taking the rewards from there
                                    mconcat [LedgerConstraints.mustSpendScriptOutput txOutRef redeemerValidator | txOutRef <- txOutRefsPoolState] P.<> 
                                    -- Is going to spend the uxto at the script with UserState Datums, because creating a new UserState with the new Claim
                                    mconcat [LedgerConstraints.mustSpendScriptOutput txOutRefUserState redeemerValidator | txOutRef <- [txOutRefUserState]] P.<> 

                                    -- Is going to create an utxo at the script with the new UserState and the value of the New T.Invest
                                    LedgerConstraints.mustPayToTheScript dUserState valueForUserState P.<> 

                                    -- -- Is going to create an utxo at the script with the new PoolState with the New T.UserNFT and the value is the actual value at the Script
                                    -- LedgerConstraints.mustPayToTheScript dPoolState valueForPoolState  P.<> 
                                    mconcat [LedgerConstraints.mustPayToTheScript dPoolState valueForPoolState | (dPoolState,_,valueForPoolState) <- dPoolStateListWithNewValues] P.<> 

                                    -- Is goint to send the NFT back again to the user wallet
                                    LedgerConstraints.mustPayToPubKey user valueNFTPlusMinimunAdaForUser  P.<> 
                                    -- Is goint to send the claimed rewards to the user wallet
                                    LedgerConstraints.mustPayToPubKey user valueClaimRewardsForUser  P.<> 
                                    -- Is goint create the valid range based in T.ppValidTimeRange Pool Param
                                    LedgerConstraints.mustValidateIn validityRange

                            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
                            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User Get Rewards : Ending ---------------------------------------"  
                            PlutusContract.logInfo @P.String $ TextPrintf.printf "Param: %s" (P.show pugrPoolParam)

                            let 

                                formatPoolStateOutputs = concat [ ( "PoolState Datum Hash: " ++ (P.show hashDatumPoolState)): ( "PoolState Datum: " ++ (P.show  dPoolState)): ( "PoolState Value: " ++ (P.show valueForPoolState)):[] | (dPoolState,hashDatumPoolState,valueForPoolState) <- dPoolStateListWithNewValues] 


                            mapM_ (PlutusContract.logInfo @P.String  ) formatPoolStateOutputs

                        
                            PlutusContract.logInfo @P.String $ TextPrintf.printf "UserState Datum Hash: %s" (P.show hashDatumUserState)
                            PlutusContract.logInfo @P.String $ TextPrintf.printf "UserState Datum: %s" (P.show dUserState)
                            PlutusContract.logInfo @P.String $ TextPrintf.printf "UserState Value: %s" (P.show valueForUserState)
                            PlutusContract.logInfo @P.String $ TextPrintf.printf "User Wallet NFT Value: %s" (P.show valueNFTPlusMinimunAdaForUser)    

                            PlutusContract.logInfo @P.String $ TextPrintf.printf "User Wallet Claimed Rewards Value: %s" (P.show valueClaimRewardsForUser)    
                            
                            -- PlutusContract.logInfo @P.String $ TextPrintf.printf "SubmittedTx: %s" (P.show submittedTx)   
                            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"      
                            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"        


                            submittedTx <- PlutusContract.submitTxConstraintsWith lookupsInit tx
                            Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId submittedTx

                        else do
                            PlutusContract.logInfo @P.String $ TextPrintf.printf "Cant' Get Rewards because there is not enough funds to cover the claim..."
                            return ()

                    [] -> do 
                        PlutusContract.logInfo @P.String $ TextPrintf.printf "Cant' Get Rewards from because can't find PoolState utxo with UserNFT register"
                        return ()
                    (_:_) -> do 
                        PlutusContract.logInfo @P.String $ TextPrintf.printf "Cant' Get Rewards from because can't find single PoolState utxo with UserNFT register"
                        return ()   


userInvestRewards ::  T.UserInvestRewardsParams -> PlutusContract.Contract w s DataText.Text ()
userInvestRewards T.UserInvestRewardsParams{..} = do
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User Invest Rewards : Init -------------------------------------"  
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"      

    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User Invest Rewards : Engind -----------------------------------" 
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "Param: %s Datum: %s Value: %s" (P.show pmcpPoolParam) (P.show dPoolState) (P.show value) 
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"      
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"      

type ValidatorSchema =
        PlutusContract.Endpoint "masterCreatePool" T.MasterCreatePoolParams PlutusContract..\/ 
        PlutusContract.Endpoint "masterFundPool" T.MasterFundPoolParams PlutusContract..\/ 
        PlutusContract.Endpoint "masterGetBackFund" T.MasterGetBackFundParams PlutusContract..\/ 
        PlutusContract.Endpoint "userInvest" T.UserInvestParams PlutusContract..\/ 
        PlutusContract.Endpoint "userGetBackInvest" T.UserGetBackInvestParams PlutusContract..\/ 
        PlutusContract.Endpoint "userGetRewards" T.UserGetRewardsParams PlutusContract..\/ 
        PlutusContract.Endpoint "userInvestRewards" T.UserInvestRewardsParams
        
endpoints :: PlutusContract.Contract () ValidatorSchema DataText.Text ()
endpoints = PlutusContract.awaitPromise (masterCreatePool' `PlutusContract.select` masterFundPool' `PlutusContract.select` masterGetBackFund' `PlutusContract.select` userInvest' `PlutusContract.select` userGetBackInvest' `PlutusContract.select` userGetRewards' `PlutusContract.select` userInvestRewards') >> endpoints
  where
    masterCreatePool' = PlutusContract.endpoint @"masterCreatePool" masterCreatePool
    masterFundPool' = PlutusContract.endpoint @"masterFundPool" masterFundPool
    masterGetBackFund' = PlutusContract.endpoint @"masterGetBackFund" masterGetBackFund
    userInvest' = PlutusContract.endpoint @"userInvest" userInvest
    userGetBackInvest' = PlutusContract.endpoint @"userGetBackInvest" userGetBackInvest
    userGetRewards' = PlutusContract.endpoint @"userGetRewards" userGetRewards
    userInvestRewards' = PlutusContract.endpoint @"userInvestRewards" userInvestRewards

Playground.Contract.mkSchemaDefinitions ''ValidatorSchema
