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

module Validators.StakeSimpleV1.OffChain where

-- import           Control.Monad        hiding (fmap)
-- import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
-- import           Data.List.NonEmpty   (NonEmpty (..))
-- import           Data.Map             as Map
-- import           Data.Text            (pack, Text)
-- import           Data.String  
-- import qualified GHC.Generics                        as GHCGenerics (Generic)
-- import           Ledger               hiding (singleton)
-- import qualified Ledger.Constraints   as Constraints
-- import qualified Ledger.Typed.Scripts as Scripts
-- import           LedgerValueV1.Value         as Value
-- import           Ledger.Ada           as Ada
-- import           Playground.Contract  (IO, ensureKnownCurrencies, printSchemas, stage, printJson)
-- import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
-- import           Playground.Types     (KnownCurrency (..))
-- import           Plutus.Contract
-- import qualified PlutusTx
-- import           PlutusTx.Prelude     hiding (unless)
-- import qualified Prelude              as P 
-- import qualified Schema                              (ToSchema)
-- import     qualified      Data.OpenApi.Schema         (ToSchema)
-- import           Text.Printf          (printf)
-- import Data.Typeable

-- import          Plutus.Trace.Emulator  as Emulator
-- import          Wallet.Emulator.Wallet
-- import          Data.Default
-- import          Ledger.TimeSlot 

-- --Import Nuevos

-- import           Data.Void            (Void)


-- --Import Internos
-- import qualified Validators.StakeSimpleV1.Typos  
-- import qualified Validators.StakeSimpleV1.OffChainHelpers     
-- import qualified Validators.StakeSimpleV1.Helpers     
-- import qualified Validators.StakeSimpleV1.OnChain     (typedValidator, codeValidator, addressValidator)
-- import qualified Validators.StakeSimpleV1.OnChainNFT     (mintingNFTPolicy)


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

import qualified Validators.StakeSimpleV1.Helpers         as Helpers
import qualified Validators.StakeSimpleV1.OffChainHelpers as OffChainHelpers  
import qualified Validators.StakeSimpleV1.OnChain         as OnChain (typedValidator, codeValidator, addressValidator)
import qualified Validators.StakeSimpleV1.OnChainNFT      as OnChainNFT (mintingNFTPolicy)
import qualified Validators.StakeSimpleV1.Typos           as T
  
-- Modulo:

masterCreatePool :: T.MasterCreatePoolParams -> PlutusContract.Contract w s DataText.Text ()
masterCreatePool T.MasterCreatePoolParams{..} = do
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Create Pool ---------------------------------------------"  
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"   
    

    master <- PlutusContract.ownFirstPaymentPubKeyHash

    now <- PlutusContract.currentTime
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)

    let 
        masterAdds = Ledger.pubKeyHashAddress master Nothing
    
    utxosMaster <- PlutusContract.utxosAt masterAdds

    PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosMaster List: %s" (P.show utxosMaster)

    let 

        poolNFTTxOutRef = mcpPoolNFTTxOutRef
        poolNFTTokenName    =  mcpPoolNFTTokenName
        valuePoolNFT     = LedgerValueV1.assetClassValue (T.ppPoolNFT mcpPoolParam) 1
       
        valueForPoolState = LedgerAda.lovelaceValueOf mcpFund <> valuePoolNFT

        masterFunders_others = [T.mkMasterFunder masterParam 0 | masterParam <- T.ppMasters mcpPoolParam ,  masterParam /= master] 
        masterFundersNew = T.mkMasterFunder master mcpFund
        masterFunders = masterFundersNew:masterFunders_others

        userNFTs = []

        dPoolState = T.mkPoolState (T.ppPoolNFT mcpPoolParam) masterFunders userNFTs

        redeemerMinting   = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData $ T.MintingRedeemer { T.mrTokenName = poolNFTTokenName, T.mrTxOutRef = poolNFTTxOutRef} 
        
        ---

        validityRange        = LedgerIntervalV1.interval now (now + T.ppValidTimeRange mcpPoolParam)

        ---

        lookupsInit = 
            -- This script is goint to use all the uxto from the user
            LedgerConstraints.unspentOutputs utxosMaster P.<> 
            -- Is sending value to script, it needs the typedValidatorLookups
            LedgerConstraints.plutusV1TypedValidatorLookups (OnChain.typedValidator mcpPoolParam) P.<> 
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

    submittedTx <- PlutusContract.submitTxConstraintsWith lookupsInit tx
    Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId submittedTx

    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Create Pool ---------------------------------------------"  
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Param: %s" (P.show mcpPoolParam)

    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolState Datum: %s" (P.show dPoolState)

    PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolState Value: %s" (P.show valueForPoolState)

    --PlutusContract.logInfo @P.String $ TextPrintf.printf "SubmittedTx: %s" (P.show submittedTx)    
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------" 
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"


masterFundPool :: T.MasterFundPoolParams -> PlutusContract.Contract w s DataText.Text ()
masterFundPool T.MasterFundPoolParams{..} = do
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Fund Pool ----------------------------------------------"  
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"  
    
    master <- PlutusContract.ownFirstPaymentPubKeyHash

    now <- PlutusContract.currentTime
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)

    let 
        masterAdds = Ledger.pubKeyHashAddress master Nothing

    utxosAtMaster <- PlutusContract.utxosAt masterAdds

    utxosListAtScriptWithPoolState <- OffChainHelpers.getUtxoListWithValidPoolStateInScript (OnChain.addressValidator mspPoolParam)

    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosListAtScriptWithPoolState List: %s" (P.show utxosListAtScriptWithPoolState)

    case utxosListAtScriptWithPoolState of
        [] -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Cant' Fund Pool because can't find any utxo with PoolState Datum at the Script Address"
            return ()
        _ -> do

            let

                listValuesEnUtxosPoolStateList = [ OffChainHelpers.getValueFromChainIndexTxOut $ snd utxo | utxo <- utxosListAtScriptWithPoolState] 
                
                valueFundForPoolState  = LedgerAda.lovelaceValueOf mspFund

                valueTotalForPoolState = foldl (<>) valueFundForPoolState listValuesEnUtxosPoolStateList 

                dPoolState = OffChainHelpers.mkPoolStateWithNewFundFromUtxoList utxosListAtScriptWithPoolState (T.ppPoolNFT mspPoolParam) master mspFund 



                redeemerValidator = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData (T.mkRedeemMasterFundPool (T.ppPoolNFT mspPoolParam)  master mspFund )
                
                txOutRefsPoolState   = fst <$> utxosListAtScriptWithPoolState
                
                ---

                validityRange        = LedgerIntervalV1.interval now (now + T.ppValidTimeRange mspPoolParam)

                ---

                lookupsInit = 
                    -- This script is goint to use all the uxto from the user
                    -- TODO: no es necesario poner esto, se hace automaticamente
                    --       se necesita poner cuando uso mustSpendPubKeyOutput
                    LedgerConstraints.unspentOutputs utxosAtMaster P.<> 
                    -- Is also going to use the utxos at the script with PoolState    
                    LedgerConstraints.unspentOutputs (DataMap.fromList utxosListAtScriptWithPoolState)      P.<>
                    -- Is sending value to script, it needs the typedValidatorLookups
                    LedgerConstraints.plutusV1TypedValidatorLookups (OnChain.typedValidator mspPoolParam) P.<>
                    -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum
                    -- for speending it needs the code of the validator
                    LedgerConstraints.plutusV1OtherScript (OnChain.codeValidator mspPoolParam)

                tx      = 
                    -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum with the new fund
                    mconcat [LedgerConstraints.mustSpendScriptOutput txOutRef redeemerValidator | txOutRef <- txOutRefsPoolState] P.<> 
                    -- Is going to create an utxo at the script with the new PoolState and the new value
                    LedgerConstraints.mustPayToTheScript dPoolState valueTotalForPoolState P.<> 
                    -- Is goint create the valid range based in T.ppValidTimeRange Pool Param
                    LedgerConstraints.mustValidateIn validityRange    


            submittedTx <- PlutusContract.submitTxConstraintsWith lookupsInit tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId submittedTx

            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master Fund Pool -- -------------------------------------------" 

            PlutusContract.logInfo @P.String $ TextPrintf.printf "Param: %s" (P.show mspPoolParam)

            PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolState Datum: %s" (P.show dPoolState)

            PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolState Value Fund: %s" (P.show valueFundForPoolState)

            PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolState Value: %s" (P.show valueTotalForPoolState)

            --PlutusContract.logInfo @P.String $ TextPrintf.printf "SubmittedTx: %s" (P.show submittedTx)    
            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------" 
            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"  

masterGetBackFund :: T.MasterGetBackFundParams -> PlutusContract.Contract w s DataText.Text ()
masterGetBackFund T.MasterGetBackFundParams{..} = do
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master GetBack Fund -------------------------------------------"  
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"     

    -- master <- PlutusContract.ownFirstPaymentPubKeyHash
    -- now <- PlutusContract.currentTime
    -- utxosFromMaster <- findUtxosFromMasters (OnChain.addressValidator mgpPoolParam) master 

    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "findUtxoFromMaster: %s" (P.show utxosFromMaster)

    -- case utxosFromMaster of

    --     [] -> Plutus.PlutusContract.throwError "Pool From Master Not Found" 

    --     _ -> do

    --         let 
    --             redeemer      = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData mkRedeemMasterGetPool

    --             txOutRefs   = fst <$> utxosFromMaster
                
    --             lookupsInit = LedgerConstraints.unspentOutputs (DataMap.fromList utxosFromMaster)      P.<>
    --                     LedgerConstraints.plutusV1TypedValidatorLookups (OnChain.typedValidator mgpPoolParam) P.<>
    --                     LedgerConstraints.plutusV1OtherScript (OnChain.codeValidator mgpPoolParam)

                
    --             tx      = mconcat [LedgerConstraints.mustSpendScriptOutput txOutRef redeemer | txOutRef <- txOutRefs]
    --                 P.<> LedgerConstraints.mustValidateIn (from now)
    --             -- LedgerConstraints.mustPayToPubKey master vGetADA  

    --         submittedTx <- PlutusContract.submitTxConstraintsWith lookupsInit tx
    --         Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId submittedTx

    --         PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    --         PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Master GetBack Fund -------------------------------------------" 
    --         PlutusContract.logInfo @P.String $ TextPrintf.printf "Param: %s " (P.show mgpPoolParam) 
    --         PlutusContract.logInfo @P.String $ TextPrintf.printf "submittedTx: %s" (P.show submittedTx)    
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------" 
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"  

userInvest :: T.UserInvestParams -> PlutusContract.Contract w s DataText.Text ()
userInvest T.UserInvestParams{..} = do
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User Invest ----------------------------------------------------"  
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"      

    user <- PlutusContract.ownFirstPaymentPubKeyHash

    now <- PlutusContract.currentTime
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)

    let 
        userAdds = Ledger.pubKeyHashAddress user Nothing

    utxosAtUser <- PlutusContract.utxosAt userAdds

    utxosListAtScriptWithPoolState <- OffChainHelpers.getUtxoListWithValidPoolStateInScript (OnChain.addressValidator uipPoolParam) 

    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosListAtScriptWithPoolState List: %s" (P.show utxosListAtScriptWithPoolState)

    case utxosListAtScriptWithPoolState of
        [] -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Cant' Invest in Pool because can't find any utxo with PoolState Datum at the Script Address"
            return ()
        _ -> do
            let 
                
                -- Create User NFT 

                userNFTTxOutRef  = uiUserNFTTxOutRef
                userNFTTokenName  = uiUserNFTTokenName
                userNFT = LedgerValueV1.assetClass (Helpers.curSymbol OnChainNFT.mintingNFTPolicy) userNFTTokenName

                valueNFTForUser     = LedgerValueV1.assetClassValue userNFT 1
                valueNFTPlusMinimunAdaForUser = valueNFTForUser <>  LedgerAda.lovelaceValueOf Helpers.minLovelace

                valueForUserState = LedgerAda.lovelaceValueOf uipInvest 

                -- Creates UserState Datum

                dUserState = T.mkUserState user userNFT uipInvest uipCreatedAt uipDeadline 0 0 Nothing

                -- Creates PoolState Datum with the New User

                poolNFT = T.ppPoolNFT uipPoolParam    

                listValuesEnUtxosPoolStateList = [ OffChainHelpers.getValueFromChainIndexTxOut $ snd utxo | utxo <- utxosListAtScriptWithPoolState] 
    
                valueForPoolState = foldl (<>) (LedgerAda.lovelaceValueOf 0) listValuesEnUtxosPoolStateList 

                dPoolState = OffChainHelpers.mkPoolStateWithNewUserInvestFromUtxoList utxosListAtScriptWithPoolState poolNFT userNFT

                txOutRefsPoolState   = fst <$> utxosListAtScriptWithPoolState
                
                -- Creates Redeemer

                redeemerValidator = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData (T.mkRedeemUserInvest poolNFT userNFT  userNFTTokenName userNFTTxOutRef  user uipInvest uipCreatedAt uipDeadline )
                redeemerMinting   = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData $ T.MintingRedeemer { T.mrTokenName = userNFTTokenName, T.mrTxOutRef = userNFTTxOutRef} 

                ---

                validityRange        = LedgerIntervalV1.interval uipCreatedAt (uipCreatedAt + T.ppValidTimeRange uipPoolParam)

                ---

                lookupsInit = 
                    -- This script is goint to use all the uxto from the user
                    LedgerConstraints.unspentOutputs utxosAtUser P.<> 
                    -- Is also going to use the utxo at the script with PoolState    
                    LedgerConstraints.unspentOutputs (DataMap.fromList utxosListAtScriptWithPoolState)  P.<> 
                     -- Is going to Mint the User NFT: 
                    LedgerConstraints.plutusV1MintingPolicy OnChainNFT.mintingNFTPolicy    P.<> 
                    -- Is sending value to script, it needs the typedValidatorLookups
                    LedgerConstraints.plutusV1TypedValidatorLookups (OnChain.typedValidator uipPoolParam) P.<> 
                    -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum
                    -- for speending it needs the code of the validator
                    LedgerConstraints.plutusV1OtherScript (OnChain.codeValidator uipPoolParam)

                tx = 
                    -- Is going to spend the uxto at the script with PoolState Datums, because is creating a new PoolState Datum
                    mconcat [LedgerConstraints.mustSpendScriptOutput txOutRef redeemerValidator | txOutRef <- txOutRefsPoolState] P.<> 
                    -- Is going to create an utxo at the script with the new UserState and the value of the New Invest
                    LedgerConstraints.mustPayToTheScript dUserState valueForUserState P.<> 
                    -- Is going to create an utxo at the script with the new PoolState with the New UserNFT and the value is the actual value at the Script
                    LedgerConstraints.mustPayToTheScript dPoolState valueForPoolState  P.<> 
                    -- Is goint to send the mint userNFT to the user wallet 
                    LedgerConstraints.mustPayToPubKey user valueNFTPlusMinimunAdaForUser  P.<> 
                    -- Is going to spend the user uxto assinged to the NFT
                    LedgerConstraints.mustSpendPubKeyOutput userNFTTxOutRef P.<> 
                    -- Is going to Mint the User NFT:
                    LedgerConstraints.mustMintValueWithRedeemer redeemerMinting valueNFTForUser P.<> 
                    -- Is goint create the valid range based in T.ppValidTimeRange Pool Param
                    LedgerConstraints.mustValidateIn validityRange

            submittedTx <- PlutusContract.submitTxConstraintsWith lookupsInit tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId submittedTx

            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User Invest ----------------------------------------------------"  
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Param: %s" (P.show uipPoolParam)

            PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolState Datum: %s" (P.show dPoolState)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "UserState Datum: %s" (P.show dUserState)

            PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolState Value: %s" (P.show valueForPoolState)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "UserState Value: %s" (P.show valueForUserState)
            
            PlutusContract.logInfo @P.String $ TextPrintf.printf "User Wallet NFT Value: %s" (P.show valueNFTPlusMinimunAdaForUser)    

            -- PlutusContract.logInfo @P.String $ TextPrintf.printf "SubmittedTx: %s" (P.show submittedTx)   
            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"      
            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"      

userGetBackInvest :: T.UserGetBackInvestParams -> PlutusContract.Contract w s DataText.Text ()
userGetBackInvest T.UserGetBackInvestParams{..} = do
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User GetBack Invest --------------------------------------------" 
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"      

    -- user <- PlutusContract.ownFirstPaymentPubKeyHash
    -- now <- PlutusContract.currentTime
    -- utxosFromUserWithDeadline <- findUtxosFromUserWithDeadline (OnChain.addressValidator ugipPoolParam) user ugipDeadline 

    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "findUtxosFromUserWithDeadline: %s" (P.show utxosFromUserWithDeadline)

    -- case utxosFromUserWithDeadline of

    --     [] -> Plutus.PlutusContract.throwError "User Invest Not Found" 

    --     _ -> do

    --         let 
    --             redeemer      = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData mkRedeemUserGetInvest

    --             txOutRefs   = fst <$> utxosFromUserWithDeadline
                
    --             lookupsInit = LedgerConstraints.unspentOutputs (DataMap.fromList utxosFromUserWithDeadline)      P.<>
    --                     LedgerConstraints.plutusV1TypedValidatorLookups (OnChain.typedValidator ugipPoolParam) P.<>
    --                     LedgerConstraints.plutusV1OtherScript (OnChain.codeValidator ugipPoolParam)

    --             tx      = mconcat [LedgerConstraints.mustSpendScriptOutput txOutRef redeemer | txOutRef <- txOutRefs]
    --                 P.<> LedgerConstraints.mustValidateIn (from now)
    --             -- LedgerConstraints.mustPayToPubKey user vGetADA  

    --         submittedTx <- PlutusContract.submitTxConstraintsWith lookupsInit tx
    --         Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId submittedTx

    --         PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    --         PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User GetBack Invest --------------------------------------------"  
    --         PlutusContract.logInfo @P.String $ TextPrintf.printf "Param: %s " (P.show ugipPoolParam) 
    --         PlutusContract.logInfo @P.String $ TextPrintf.printf "submittedTx: %s" (P.show submittedTx)   
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"      
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"      

userGetRewards :: T.UserGetRewardsParams -> PlutusContract.Contract w s DataText.Text ()
userGetRewards T.UserGetRewardsParams{..} = do

    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User Get Rewards -----------------------------------------------"  
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"    

    user <- PlutusContract.ownFirstPaymentPubKeyHash
    
    now <- PlutusContract.currentTime
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    
    let 
        userAdds = Ledger.pubKeyHashAddress user Nothing

        userNFTTxOutRef  = ugrpUserNFTTxOutRef
        userNFTTokenName  = ugrpUserNFTTokenName
        userNFT = LedgerValueV1.assetClass (Helpers.curSymbol OnChainNFT.mintingNFTPolicy) userNFTTokenName
        
    utxosAtUser <- PlutusContract.utxosAt userAdds

    utxosListAtScriptWithPoolState <- OffChainHelpers.getUtxoListWithValidPoolStateInScript (OnChain.addressValidator ugrpPoolParam) 
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosListAtScriptWithPoolState List: %s" (P.show utxosListAtScriptWithPoolState)

    utxosListAtScriptWithUserState <- OffChainHelpers.getUtxoListWithValidUserStateInScript (OnChain.addressValidator ugrpPoolParam) userNFT 
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosListAtScriptWithUserState List: %s" (P.show utxosListAtScriptWithPoolState)

    case (utxosListAtScriptWithPoolState,utxosListAtScriptWithUserState) of
        ([], _) -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Cant' Get Rewards from Pool because can't find any utxo with PoolState Datum at the Script Address"
            return ()
        (_, []) -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Cant' Get Rewards from because can't find any utxo with UserState Datum with this UserNFT at the Script Address"
            return ()
        (_, x:[y]) -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Cant' Get Rewards from because can't find unic single utxo with UserState Datum with this UserNFT at the Script Address"
            return ()
        (_, [utxoAtScriptWithUserState]) -> do
            let 
                
                valueNFTForUser    = LedgerValueV1.assetClassValue userNFT 1
                valueNFTPlusMinimunAdaForUser  = valueNFTForUser <>  LedgerAda.lovelaceValueOf Helpers.minLovelace

                valueClaimRewardsForUser = LedgerAda.lovelaceValueOf ugrpClaim

                dUserStateOLD = OffChainHelpers.getUserStateFromUtxo utxoAtScriptWithUserState

                valueForUserState = OffChainHelpers.getValueFromChainIndexTxOut $ snd utxoAtScriptWithUserState 

                -- Creates UserState Datum

                rewards = Helpers.getRewardsPerInvest (T.usLastClaimAt dUserStateOLD) now  (T.usCreatedAt  dUserStateOLD )  (T.usInvest dUserStateOLD ) 
                totalNewRewards = rewards  + T.usRewardsNotClaimed dUserStateOLD
                rewardsNotClaimed = totalNewRewards - ugrpClaim
                totalRewardsCashedOut = T.usChashedOut dUserStateOLD + ugrpClaim 

            PlutusContract.logInfo @P.String $ TextPrintf.printf "CALCULATED Claiming " ++ P.show ugrpClaim
            PlutusContract.logInfo @P.String $ TextPrintf.printf "CALCULATED New Rewards " ++ P.show rewards
            PlutusContract.logInfo @P.String $ TextPrintf.printf "CALCULATED usRewardsNotClaimed OLD " ++ P.show (T.usRewardsNotClaimed dUserStateOLD)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "CALCULATED totalNewRewards " ++ P.show totalNewRewards
            PlutusContract.logInfo @P.String $ TextPrintf.printf "CALCULATED rewardsNotClaimed " ++ P.show rewardsNotClaimed
            PlutusContract.logInfo @P.String $ TextPrintf.printf "CALCULATED totalRewardsCashedOut " ++ P.show totalRewardsCashedOut

            let

                -- TODO: throwError "ERROR " si es menor que cero

                dUserState = T.mkUserState user userNFT 
                    (T.usInvest dUserStateOLD)
                    (T.usCreatedAt dUserStateOLD)
                    (T.usDeadline dUserStateOLD)
                    (totalRewardsCashedOut)
                    (rewardsNotClaimed)
                    (Just now)

                txOutRefUserState   = fst utxoAtScriptWithUserState

                -- Creates PoolState Datum

                poolNFT = T.ppPoolNFT ugrpPoolParam    

                listValuesEnUtxosPoolStateList = [ OffChainHelpers.getValueFromChainIndexTxOut $ snd utxo | utxo <- utxosListAtScriptWithPoolState] 

                poolStateDatums = OffChainHelpers.getPoolStateListFromUtxoList utxosListAtScriptWithPoolState 

                valueForPoolState = foldl (<>) (negate (LedgerAda.lovelaceValueOf ugrpClaim)) listValuesEnUtxosPoolStateList 

    
                dPoolState = Helpers.mkPoolStateFromPoolStateList poolStateDatums poolNFT   

                txOutRefsPoolState   = fst <$> utxosListAtScriptWithPoolState
                
                -- Creates Redeemer

                redeemerValidator = LedgerApiV1.Redeemer $ PlutusTx.toBuiltinData (T.mkRedeemUserGetRewards poolNFT userNFT  user ugrpClaim now )
                
                ---

                validityRange        = LedgerIntervalV1.interval now (now + T.ppValidTimeRange ugrpPoolParam)

                ---

                lookupsInit = 
                    -- This script is goint to use all the uxto from the user 
                    LedgerConstraints.unspentOutputs utxosAtUser P.<> 
                    -- Is also going to use the utxo at the script with PoolState 
                    LedgerConstraints.unspentOutputs (DataMap.fromList utxosListAtScriptWithPoolState)  P.<> 
                    -- Is also going to use the utxo at the script with UserState  
                    LedgerConstraints.unspentOutputs (DataMap.fromList [utxoAtScriptWithUserState])  P.<> 
                    -- Is sending value to script, it needs the typedValidatorLookups
                    LedgerConstraints.plutusV1TypedValidatorLookups (OnChain.typedValidator ugrpPoolParam) P.<> 
                    -- Is going to spend the uxto at the script with PoolState Datums and UserState Datums
                    -- for speending it needs the code of the validator
                    LedgerConstraints.plutusV1OtherScript (OnChain.codeValidator ugrpPoolParam)

                tx = 
                    -- Is going to spend the uxto at the script with PoolState Datums, because is taking the rewards from there
                    mconcat [LedgerConstraints.mustSpendScriptOutput txOutRef redeemerValidator | txOutRef <- txOutRefsPoolState] P.<> 
                    -- Is going to spend the uxto at the script with UserState Datums, because creating a new UserState with the new Claim
                    mconcat [LedgerConstraints.mustSpendScriptOutput txOutRefUserState redeemerValidator | txOutRef <- [txOutRefUserState]] P.<> 

                    -- Is going to create an utxo at the script with the new UserState and the value of the New Invest
                    LedgerConstraints.mustPayToTheScript dUserState valueForUserState P.<> 
                    -- Is going to create an utxo at the script with the new PoolState with the New UserNFT and the value is the actual value at the Script
                    LedgerConstraints.mustPayToTheScript dPoolState valueForPoolState  P.<> 
                    -- Is goint to send the NFT back again to the user wallet
                    LedgerConstraints.mustPayToPubKey user valueNFTPlusMinimunAdaForUser  P.<> 
                    -- Is goint to send the claimed rewards to the user wallet
                    LedgerConstraints.mustPayToPubKey user valueClaimRewardsForUser  P.<> 
                    -- Is goint create the valid range based in T.ppValidTimeRange Pool Param
                    LedgerConstraints.mustValidateIn validityRange

            submittedTx <- PlutusContract.submitTxConstraintsWith lookupsInit tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId submittedTx

            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User Get Rewards ----------------------------------------------------"  
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Param: %s" (P.show ugrpPoolParam)

            PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolState Datum: %s" (P.show dPoolState)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "UserState Datum: %s" (P.show dUserState)

            PlutusContract.logInfo @P.String $ TextPrintf.printf "PoolState Value: %s" (P.show valueForPoolState)

            PlutusContract.logInfo @P.String $ TextPrintf.printf "UserState Value: %s" (P.show valueForUserState)

            PlutusContract.logInfo @P.String $ TextPrintf.printf "User Wallet NFT Value: %s" (P.show valueNFTPlusMinimunAdaForUser)    

            PlutusContract.logInfo @P.String $ TextPrintf.printf "User Wallet Claimed Rewards Value: %s" (P.show valueClaimRewardsForUser)    
            
            -- PlutusContract.logInfo @P.String $ TextPrintf.printf "SubmittedTx: %s" (P.show submittedTx)   
            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"      
            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"        

userInvestRewards :: T.UserInvestRewardsParams -> PlutusContract.Contract w s DataText.Text ()
userInvestRewards T.UserInvestRewardsParams{..} = do
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User Invest Rewards --------------------------------------------"  
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"      

    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- User Invest Rewards --------------------------------------------" 
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "Param: %s Datum: %s Value: %s" (P.show mcpPoolParam) (P.show dPoolState) (P.show value) 
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

