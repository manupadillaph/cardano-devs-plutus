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

--Validators Main Helper Functions

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
    PlutusTx.Prelude.any (txSignedBy info ) [unPaymentPubKeyHash $ master | master <- spMasters pParams]


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


{-# INLINABLE deadlinePoolParamReached #-}
deadlinePoolParamReached :: PoolParams -> TxInfo ->Bool
deadlinePoolParamReached pParams info  = Ledger.contains (Ledger.from $ spDeadline pParams) $ txInfoValidRange info

{-# INLINABLE deadlinePoolParamNotReached #-}
deadlinePoolParamNotReached :: PoolParams -> TxInfo ->Bool
deadlinePoolParamNotReached  pParams info = not (deadlinePoolParamReached  pParams info)

{-# INLINABLE deadlineUserStateReached #-}
deadlineUserStateReached :: UserStateTypo -> TxInfo -> Bool
deadlineUserStateReached dUserState info = Ledger.contains (Ledger.from $ usDeadline dUserState) $ txInfoValidRange info

{-# INLINABLE deadlineUserStateNotReached #-}
deadlineUserStateNotReached :: UserStateTypo -> TxInfo -> Bool
deadlineUserStateNotReached dUserState info = not (deadlineUserStateReached dUserState info )

--Validator extra util helper functions

{-# INLINABLE getOnwOutputs #-}
getOnwOutputs :: ScriptContext -> [TxOut]
getOnwOutputs = getContinuingOutputs


-- | Gets the datum attached to a utxo.
{-# INLINABLE getDatumFromTxOut #-}
getDatumFromTxOut :: PlutusTx.FromData ValidatorDatum => TxOut -> ScriptContext -> Maybe ValidatorDatum
getDatumFromTxOut o ctx = txOutDatum o >>= (`findDatum` scriptContextTxInfo ctx)
                   >>= PlutusTx.fromBuiltinData . getDatum :: Maybe ValidatorDatum
                   
{-# INLINABLE getSingleOwnOutputPoolStateDatum #-}
getSingleOwnOutputPoolStateDatum :: ScriptContext -> (TxOut,PoolStateTypo)
getSingleOwnOutputPoolStateDatum ctx = 
    case getOnwOutputs ctx of
        [txOut] -> case getDatumFromTxOut txOut ctx of
            Just (PoolState dPoolState)  -> (txOut, dPoolState)
            Nothing   -> traceError "Wrong output PoolState Datum type."
        _   -> traceError "Expected exactly one output with PoolState Datum."

{-# INLINABLE getDoubleOwnOutputsPoolStateAndUSerStateDatum #-}
getDoubleOwnOutputsPoolStateAndUSerStateDatum :: ScriptContext -> ((TxOut,PoolStateTypo),(TxOut,UserStateTypo))
getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx = 
    case getOnwOutputs ctx of
        txOut1:txOut2:[] -> case (getDatumFromTxOut txOut1 ctx, getDatumFromTxOut txOut2 ctx) of
            (Just (PoolState dPoolState), Just (UserState dUserState) ) -> ((txOut1, dPoolState),(txOut2, dUserState))
            (Just (UserState dUserState), Just (PoolState dPoolState) ) -> ((txOut2, dPoolState),(txOut1, dUserState))
            (Nothing,_)   -> traceError "Wrong output PoolState Datum or UserState Datum type."
            (_,Nothing)   -> traceError "Wrong output PoolState Datum or UserState Datum type."
        _   -> traceError "Expected exactly two output. One with PoolState Datum and other with UserState Datum"


{-# INLINABLE getInputPoolStateDatums #-}
getInputPoolStateDatums :: ScriptContext -> [(TxOut,PoolStateTypo)]
getInputPoolStateDatums ctx = do
    let
        txInInfoResolveds = [txInInfoResolved  txInfoInput | txInfoInput <- txInfoInputs (scriptContextTxInfo ctx)]

        txOutAndDatums = [  (txInInfoResolved, getDatumFromTxOut txInInfoResolved ctx) | txInInfoResolved <- txInInfoResolveds]

        txOutAndJustDatums = [  (txtout, dat) | (txtout, dat) <- txOutAndDatums, isJust dat ]

        txOutAndPoolState =  [  (txtout, fromJust $ getPoolStateFromDatum dat) | (txtout, dat) <- txOutAndJustDatums,  datumIsPoolState dat ] 

    txOutAndPoolState

{-# INLINABLE getInputUserStateDatums #-}
getInputUserStateDatums :: ScriptContext -> [(TxOut,UserStateTypo)]
getInputUserStateDatums ctx = do
    let
        txInInfoResolveds = [txInInfoResolved  txInfoInput | txInfoInput <- txInfoInputs (scriptContextTxInfo ctx)]

        txOutAndDatums = [  (txInInfoResolved, getDatumFromTxOut txInInfoResolved ctx) | txInInfoResolved <- txInInfoResolveds]

        txOutAndJustDatums = [  (txtout, dat) | (txtout, dat) <- txOutAndDatums, isJust dat ]

        txOutAndUserState =  [  (txtout, fromJust $ getUserStateFromDatum dat) | (txtout, dat) <- txOutAndJustDatums,  datumIsUserState dat ] 

    txOutAndUserState

{-# INLINABLE correctSingleOwnOutputPoolStateDatumWithNewFund #-}
correctSingleOwnOutputPoolStateDatumWithNewFund :: PoolNFT -> Master -> Fund -> ScriptContext -> Bool
correctSingleOwnOutputPoolStateDatumWithNewFund  redeemerPoolNFT redeemerMaster redeemerFund ctx = do
    let
        (_, outputPoolStateDatum) = getSingleOwnOutputPoolStateDatum ctx

        inputsTxOutWithPoolStateDatum = getInputPoolStateDatums ctx
        inputPoolStateDatum = snd <$> inputsTxOutWithPoolStateDatum

        newOutputDatum = mkPoolStateWithNewFundFromPoolStateList inputPoolStateDatum  redeemerPoolNFT redeemerMaster redeemerFund

    newOutputDatum == PoolState outputPoolStateDatum


{-# INLINABLE correctDoubleOwnOutputsPoolStateDatumWithNewUser #-}
correctDoubleOwnOutputsPoolStateDatumWithNewUser :: PoolNFT -> UserNFT ->  ScriptContext -> Bool
correctDoubleOwnOutputsPoolStateDatumWithNewUser  redeemerPoolNFT redeemerUserNFT ctx = do
    let
        ((_, outputPoolStateDatum),(_, _)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx
 
        inputsTxOutWithPoolStateDatum = getInputPoolStateDatums ctx
        inputPoolStateDatum = snd <$> inputsTxOutWithPoolStateDatum

        newOutputDatum = mkPoolStateWithNewUserInvestFromPoolStateList inputPoolStateDatum  redeemerPoolNFT redeemerUserNFT

    newOutputDatum == PoolState outputPoolStateDatum

-- TODO: check all of this
-- usUser, the same that redeemerUser. After is checked that the redeemerUser is signing this tx.
-- usUserNFT, the same that redeemerUserNFT. 
-- usInvest, the same that redeemerInvest,
-- usCreatedAt, time can't be in the past. 
-- usDeadline, time can't be before usCreatedAt and with a minimut distance 
-- usTotal, the total proffit to win, calculated with the interest of the Pool.
-- usChashedOut, need to be 0.
-- usLastClain, need to be nothing.
{-# INLINABLE correctDoubleOwnOutputsUserStateDatumOfNewUser #-}
correctDoubleOwnOutputsUserStateDatumOfNewUser :: User -> UserNFT ->  Invest -> POSIXTime ->  Deadline ->  ScriptContext -> Bool
correctDoubleOwnOutputsUserStateDatumOfNewUser  redeemerUser redeemerUserNFT redeemerInvest redeemerCreatedAt redeemerDeadline ctx = do
    let
        ((_, _),(_, outputUserStateDatum)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

        --txInfoValidRange info

        newOutputDatum = mkUserState  redeemerUser redeemerUserNFT redeemerInvest  redeemerCreatedAt redeemerDeadline 0 0 Nothing

    newOutputDatum == UserState outputUserStateDatum


{-# INLINABLE correctDoubleOwnOutputPoolStateValue #-}
correctDoubleOwnOutputPoolStateValue ::  ScriptContext -> Bool
correctDoubleOwnOutputPoolStateValue ctx  = do
    let
        ((txtout, _),(_, _)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

        inputsTxOutWithPoolStateDatum = getInputPoolStateDatums ctx
        inputsTxOut = fst <$> inputsTxOutWithPoolStateDatum
        listValuesEnInputsUtxos = [ txOutValue inputTxOut | inputTxOut <- inputsTxOut] 

        valueTotal = P.foldl (<>) (Ada.lovelaceValueOf 0) listValuesEnInputsUtxos

    valueTotal == txOutValue txtout

{-# INLINABLE correctDoubleOwnOutputUserStateValue #-}
correctDoubleOwnOutputUserStateValue ::  Invest -> ScriptContext -> Bool
correctDoubleOwnOutputUserStateValue redeemerInvest ctx = do
    let
        ((_, _),(txtout, _)) = getDoubleOwnOutputsPoolStateAndUSerStateDatum ctx

        valueTotal = Ada.lovelaceValueOf redeemerInvest

    valueTotal == txOutValue txtout   


{- correctOutputValue: checks if output value is the same than the sum of inputs value plus the new fund -}
{-# INLINABLE correctSumInputValuesInSingleOwnOutputValue #-}
correctSumInputValuesInSingleOwnOutputValue :: Fund -> ScriptContext ->  Bool
correctSumInputValuesInSingleOwnOutputValue redeemerFund ctx = do
    let
        (txtout, _) = getSingleOwnOutputPoolStateDatum ctx

        inputsTxOutWithPoolStateDatum = getInputPoolStateDatums ctx
        inputsTxOut = fst <$> inputsTxOutWithPoolStateDatum
        listValuesEnInputsUtxos = [ txOutValue inputTxOut | inputTxOut <- inputsTxOut] 

        valueRedeemerFund  = Ada.lovelaceValueOf redeemerFund 

        valueTotal = P.foldl (<>) valueRedeemerFund listValuesEnInputsUtxos

    valueTotal == txOutValue txtout


{-# INLINABLE correctSumInputPoolStateDatumsInSingleOwnOutputValue #-}
correctSumInputPoolStateDatumsInSingleOwnOutputValue :: Fund ->  ScriptContext -> Bool
correctSumInputPoolStateDatumsInSingleOwnOutputValue  redeemerFund ctx  = do
    let
        (txtout, _) = getSingleOwnOutputPoolStateDatum ctx
        
        inputsTxOutWithPoolStateDatum = getInputPoolStateDatums ctx
        inputPoolStateDatum = snd <$> inputsTxOutWithPoolStateDatum

        listMasterFunders = concat [ psMasterFunders poolStateDatum | poolStateDatum <- inputPoolStateDatum] 
        listValuesEnPoolStateMasterFunders = [ Ada.lovelaceValueOf (mfFund masterFunder) | masterFunder <- listMasterFunders] 

        valueRedeemerFund  = Ada.lovelaceValueOf redeemerFund 

        valueTotal = P.foldl (<>) valueRedeemerFund listValuesEnPoolStateMasterFunders

    --TODO compara el value de ada solamente, por que las inversiones por ahora son en ada, pero esto cambiaria con el tipo de moneda que se use poara la inversion
    --ademas con esto evito sumar el valor del token nft que esta en valueTotal que viene de la salida que lleva esta tx
    --la salida lleva el token de nuevo al sctrip y este no quiero tenerlo en cuenta opara esta suma AQUI.
    
    Ada.fromValue valueTotal == Ada.fromValue (txOutValue txtout)
    
    
{-# INLINABLE isPoolNFTParam #-}
isPoolNFTParam ::  PoolParams -> PoolNFT ->  Bool
isPoolNFTParam  pParams redeemerPoolNFT   = do
    let 
        poolNFT  = spPoolNFT pParams

    redeemerPoolNFT == poolNFT


{-# INLINABLE isPoolNFTInSingleOwnOutputValue #-}
isPoolNFTInSingleOwnOutputValue :: PoolNFT ->  ScriptContext -> Bool
isPoolNFTInSingleOwnOutputValue  redeemerPoolNFT ctx  = do
    let 
        (txtout, _) = getSingleOwnOutputPoolStateDatum ctx

        outPutValue = txOutValue txtout
        outPutNFTQuantity = assetClassValueOf outPutValue redeemerPoolNFT

    outPutNFTQuantity == 1

{-# INLINABLE isUserNFTInUserOutputValue #-}
isUserNFTInUserOutputValue :: UserNFT ->  User -> TxInfo -> Bool
isUserNFTInUserOutputValue  redeemerUserNFT user info  = do
    let 
        outPutValue = valuePaidTo info (unPaymentPubKeyHash user )
        outPutNFTQuantity = assetClassValueOf outPutValue redeemerUserNFT

    outPutNFTQuantity == 1 



{-# INLINABLE hasUTxO #-}
hasUTxO :: TxInfo -> TxOutRef -> Bool
hasUTxO info redeemerNFTTxOutRef = any (\i -> txInInfoOutRef i == redeemerNFTTxOutRef) $ txInfoInputs info

{-# INLINABLE correctMintigPolicyUsedForNFT #-}
correctMintigPolicyUsedForNFT :: PoolParams-> NFT   -> Bool
correctMintigPolicyUsedForNFT  pParams redeemerNFT   = do
    let 
        currencySymbolNFT = fst (unAssetClass redeemerNFT )
        currencySymbolMintingPolicy = spCurSymbolForMintingNFTPolicy pParams

    currencySymbolNFT == currencySymbolMintingPolicy  

    
{-# INLINABLE isMintedNFTValid #-}
isMintedNFTValid :: TxInfo -> NFT  -> Bool
isMintedNFTValid info redeemerNFT  = do
    let 
        mintedValue = txInfoMint info
        mintedValueOf = assetClassValueOf mintedValue redeemerNFT
    mintedValueOf == 1





-- curSymbol :: Scripts.MintingPolicy  -> CurrencySymbol
-- curSymbol  = scriptCurrencySymbol 

-- checkMintedSymbol :: Bool
-- checkMintedSymbol = case flattenValue (txInfoMint info) of
--   [(cs', _, _)] -> cs' == ownCurrencySymbol ctx
--   _             -> False


    --hashMintingPolicy = 


--redeemerPoolNFT :: PoolNFT
--redeemerPoolNFT = assetClass (curSymbol (mintingNFTPolicy redeemerPoolNFTTxOutRef redeemerPoolNFTTokenName)) redeemerPoolNFTTokenName
    -- mintingHash = currencyMPSHash redeemerPoolNFT


    -- ass <- assetClass (curSymbol (mintingNFTPolicy redeemerPoolNFTTxOutRef redeemerPoolNFTTokenName)) redeemerPoolNFTTokenName

    -- ass <- assetClass (curSymbol (mintingNFTPolicy redeemerPoolNFTTxOutRef redeemerPoolNFTTokenName)) redeemerPoolNFTTokenName



-- {-# INLINABLE getInvest #-}
-- getInvest :: Invest
-- getInvest = 6_000_000

-- {-# INLINABLE getAcumulatedRewars #-}
-- getAcumulatedRewars :: Invest
-- getAcumulatedRewars = 6_000_000







         --[(txInInfoResolved  txInfoInput, fromJust (getPoolStateFromDatum (getDatumFromTxOu



    -- case datumsOk of
    --     [Nothing] -> False
    --     [Just (PoolState dPoolState)]  -> True
    --     [c] -> False
    --     (Nothing:d)-> case d of
    --         [Just (PoolState dPoolState)] -> False
    --         _ -> False

    --     (_:d)-> case d of
    --         [Just (PoolState dPoolState)] -> False
    --         _ -> False

    --     _:_ -> False



    --     _:[_] -> False
    --     _ -> False

    
            -- case spMinumunInvest ps of


    
    -- case datumsOk of
    --     [Just (PoolState dPoolState)] -> True
    --     _ -> False
             

    --     
    --     intputPoolStateDatum = snd <$> intputsWithPoolStateDatum

    --     newOutputDatum = mkPoolStateWithNewFundFromPoolStateList intputPoolStateDatum poolNFT master invest

    -- --TODO:  dPoolState es el datum que tiene de entrada el validador en el especifico input que esta analizando...
    -- --no lo necesito... en realidad necesito ver todos los inputs... por eso uso getInputPoolStateDatums

    -- newOutputDatum == PoolState outputPoolStateDatum
