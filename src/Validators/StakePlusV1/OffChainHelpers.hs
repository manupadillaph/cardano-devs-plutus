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

module Validators.StakePlusV1.OffChainHelpers where

--Import Externos

import qualified Control.Lens                        as Lens    
import qualified Data.Map                            as DataMap
import qualified Data.Text                           as DataText ( Text)
import qualified Ledger.Index                        as LedgerIndex
import qualified Ledger.Tx                           as LedgerTx (ChainIndexTxOut (..))
import qualified Plutus.Trace.Emulator               as TraceEmulator
import qualified Plutus.Contract                     as PlutusContract
import qualified Plutus.V1.Ledger.Address            as LedgerAddressV1
import qualified Plutus.V1.Ledger.Api                as LedgerApiV1
import qualified Plutus.V1.Ledger.Value              as LedgerValueV1
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Text.Printf                         as TextPrintf (printf)

--Import Internos

import qualified Validators.StakePlusV1.Helpers      as Helpers
import qualified Validators.StakePlusV1.Typos        as T

-- Modulo:

{- | Get the value from a ChainIndexTxOut. -}
getValueFromChainIndexTxOut :: LedgerTx.ChainIndexTxOut -> LedgerValueV1.Value
getValueFromChainIndexTxOut = LedgerTx._ciTxOutValue 

{- | Try to get the generic Datum from a ChainIndexTxOut. -}
getDatumm :: LedgerTx.ChainIndexTxOut -> P.Maybe T.ValidatorDatum
getDatumm chainIndexTxOut = do
    let
        datHashOrDatum = LedgerTx._ciTxOutScriptDatum chainIndexTxOut

    LedgerApiV1.Datum e <- P.snd datHashOrDatum

    case PlutusTx.fromBuiltinData e of
        P.Nothing -> P.Nothing
        P.Just d -> d

{- | Try to get the generic Datum from a ChainIndexTxOut. -}
getDatumFromChainIndexTxOut :: LedgerTx.ChainIndexTxOut -> P.Maybe T.ValidatorDatum
getDatumFromChainIndexTxOut scriptChainIndexTxOut = do

    let
        datHashOrDatum = LedgerTx._ciTxOutScriptDatum scriptChainIndexTxOut

    LedgerApiV1.Datum e <- P.snd datHashOrDatum

    case PlutusTx.fromBuiltinData e of
        P.Nothing -> P.Nothing
        P.Just (T.PoolState dPoolState) -> do
            -- PlutusContract.logInfo @P.String $ TextPrintf.printf "Encontrado Datumm Master: %s" (P.show dPoolState)
            P.Just (T.PoolState dPoolState)   
        P.Just (T.UserState validatorUserState) -> do
            -- PlutusContract.logInfo @P.String $ TextPrintf.printf "Encontrado Datumm User: %s" (P.show validatorUserState)
            P.Just (T.UserState validatorUserState)   

    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "getDatumFromChainIndexTxOut de: %s " (P.show $ _ciTxOutDatum o)
    
    -- case _ciTxOutDatum scriptChainIndexTxOut of
    --     P.Left _          -> do
    --         -- PlutusContract.logInfo @P.String $ TextPrintf.printf "P.Left " 
    --         P.Nothing
    --     P.Right datum -> do
    --         let 
    --             validatorDatum = PlutusTx.fromBuiltinData (LedgerScriptsV1.getDatum datum) :: P.Maybe T.ValidatorDatum
    --         case validatorDatum of
    --             P.Nothing -> do
    --                 -- PlutusContract.logInfo @P.String $ TextPrintf.printf "P.Nothing "
    --                 P.Nothing    
    --             P.Just (T.PoolState dPoolState) -> do
    --                 -- PlutusContract.logInfo @P.String $ TextPrintf.printf "Encontrado Datumm Master: %s" (P.show dPoolState)
    --                 P.Just (T.PoolState dPoolState)   
    --             P.Just (T.UserState validatorUserState) -> do
    --                 -- PlutusContract.logInfo @P.String $ TextPrintf.printf "Encontrado Datumm User: %s" (P.show validatorUserState)
    --                 P.Just (T.UserState validatorUserState)   


{- | Get the list of PoolState Datums from a list of Utxos -}
getPoolStateListFromUtxoList :: [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)] -> [T.PoolStateTypo]
getPoolStateListFromUtxoList utxosWithPoolState  = do
    [ getPoolStateFromUtxo (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- utxosWithPoolState, Helpers.datumIsPoolState  (getDatumFromChainIndexTxOut  scriptChainIndexTxOut) ]

{- | Get PoolState Datum from a Utxo -}
getPoolStateFromUtxo :: (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut) -> T.PoolStateTypo
getPoolStateFromUtxo utxoWithPoolState  = do
    let 
        scriptChainIndexTxOut = snd utxoWithPoolState
    Helpers.fromJust (Helpers.getPoolStateFromMaybeDatum (getDatumFromChainIndexTxOut  scriptChainIndexTxOut))
    
{- | Get the list of UserState Datums from a list of Utxos -}
getUserStateListFromUtxoList :: [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)] -> [T.UserStateTypo]
getUserStateListFromUtxoList utxosWithUserState  = do
    [  getUserStateFromUtxo (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- utxosWithUserState, Helpers.datumIsUserState  (getDatumFromChainIndexTxOut  scriptChainIndexTxOut) ]

{- | Get UserState Datum from a Utxos -}
getUserStateFromUtxo :: (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut) -> T.UserStateTypo
getUserStateFromUtxo utxoWithUserState  = do
    let 
        scriptChainIndexTxOut = snd utxoWithUserState
    Helpers.fromJust (Helpers.getUserStateFromMaybeDatum (getDatumFromChainIndexTxOut  scriptChainIndexTxOut))


-- mkPoolStateFromUtxoList:: [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]  -> T.PoolNFT  -> T.ValidatorDatum
-- mkPoolStateFromUtxoList utxosWithPoolState poolNFT   = do
--     let 
--         poolStateDatums = getPoolStateListFromUtxoList utxosWithPoolState

--     mkPoolStateFromPoolStateList poolStateDatums poolNFT



{- | Creates a new PoolState Datum using a Utxo whit PoolState Datum and adding the new fund to the total count of funds -}
mkPoolStateWithNewCountFundsFromUtxo :: (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)  -> T.PoolNFT -> T.ValidatorDatum
mkPoolStateWithNewCountFundsFromUtxo utxoWithPoolState poolNFT  = do
    let 
        poolStateDatum = getPoolStateFromUtxo utxoWithPoolState
    
    Helpers.mkPoolStateWithNewCountFundsFromPoolState poolStateDatum poolNFT 

{- | Creates a new PoolState Datum using a list of Utxo whit PoolState Datums and adding the new fund to the specific master masterFunder. -}
mkPoolStateWithNewFundFromUtxoList :: [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]  -> T.PoolNFT -> T.Master -> T.Fund -> Integer -> T.ValidatorDatum
mkPoolStateWithNewFundFromUtxoList utxosWithPoolState poolNFT master fund countTotalUtxoWithPoolState  = do
    let 
        poolStateDatums = getPoolStateListFromUtxoList utxosWithPoolState
    
    Helpers.mkPoolStateWithNewFundFromPoolStateList poolStateDatums poolNFT master fund countTotalUtxoWithPoolState

-- {- | Creates a new PoolState Datum using a list of Utxo with PoolState Datums and adding the new user NFT. -}
mkPoolStateWithNewUserInvestFromUtxo :: (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)  -> T.PoolNFT  -> T.UserNFT  -> T.ValidatorDatum
mkPoolStateWithNewUserInvestFromUtxo utxosWithPoolState poolNFT userNFT   = do
    let 
        poolStateDatum = getPoolStateFromUtxo utxosWithPoolState
    
    Helpers.mkPoolStateWithNewUserInvestFromPoolState poolStateDatum poolNFT userNFT 
   

-- {- | Creates a new PoolState Datum using a list of Utxo with PoolState Datums and adding the new user NFT. -}
-- mkPoolStateWithNewUserInvestFromUtxoList :: [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]  -> T.PoolNFT  -> T.UserNFT  -> T.ValidatorDatum
-- mkPoolStateWithNewUserInvestFromUtxoList utxosWithPoolState poolNFT userNFT   = do
--     let 
--         poolStateDatums = getPoolStateListFromUtxoList utxosWithPoolState
    
--     mkPoolStateWithNewUserInvestFromPoolStateList poolStateDatums poolNFT userNFT 
   
{- | Get the list of utxos with valid PoolState datum in the script address. -}
getUtxoListWithValidPoolStateInScript :: LedgerAddressV1.Address -> PlutusContract.Contract w s DataText.Text [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]
getUtxoListWithValidPoolStateInScript addressValidator = do
    utxos <- PlutusContract.utxosAt addressValidator
    PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosAt: %s" (P.show utxos)
    let 
        utxosListValid = [ (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- DataMap.toList utxos, Helpers.datumIsPoolState (getDatumFromChainIndexTxOut  scriptChainIndexTxOut) ]
        -- TODO: revisar lista de utxo, comprobar datum correcto, tienen que contener los mismos invertsores que el pool, 
        -- el mismo NFT y el NFT tiene que estar en el pool

        utxosRef = [ txOutRef | (txOutRef, scriptChainIndexTxOut) <- utxosListValid ]

    PlutusContract.logInfo @P.String $ TextPrintf.printf "Utxos List with Valid PoolState: %s" (P.show utxosRef)
    return utxosListValid

{- | Get the list of utxos with valid UserState datum in the script address. -}
getUtxoListWithValidUserStateInScript :: LedgerAddressV1.Address -> T.UserNFT -> PlutusContract.Contract w s DataText.Text [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]
getUtxoListWithValidUserStateInScript addressValidator userNFT  = do
    utxos <- PlutusContract.utxosAt addressValidator
    PlutusContract.logInfo @P.String $ TextPrintf.printf "utxosAt: %s" (P.show utxos)

    

    let 
        utxosListValidWithDatum = [ (txOutRef, scriptChainIndexTxOut, Helpers.fromJust (Helpers.getUserStateFromMaybeDatum (getDatumFromChainIndexTxOut  scriptChainIndexTxOut))) | (txOutRef, scriptChainIndexTxOut) <- DataMap.toList utxos, Helpers.datumIsUserState (getDatumFromChainIndexTxOut  scriptChainIndexTxOut) ]
            

        utxosListValid = [ (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut, dUserState) <- utxosListValidWithDatum, T.usUserNFT dUserState == userNFT]
        
        -- TODO: revisar lista de utxo, comprobar datum correcto, tienen que contener usuario registrado en el pool

        utxosRef = [ txOutRef | (txOutRef, scriptChainIndexTxOut) <- utxosListValid ]

    PlutusContract.logInfo @P.String $ TextPrintf.printf "Utxos List Valid UserState: %s" (P.show utxosRef)
    return utxosListValid

getUtxoWithNFTInList :: [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)] -> T.PoolNFT -> P.Maybe (LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)
getUtxoWithNFTInList utxos poolNFT  = do
    let
        utxoWithNFT = [utxo  | utxo <- utxos , LedgerValueV1.assetClassValueOf  (getValueFromChainIndexTxOut $ snd utxo) poolNFT > 0 ]

    case utxoWithNFT of
        [x] -> P.Just x
        _ -> P.Nothing



{- | Get the utxos in the address for use in the emulator trace. -}
getUtxoListInEmulator :: LedgerAddressV1.Address -> TraceEmulator.EmulatorTrace [(LedgerApiV1.TxOutRef, LedgerApiV1.TxOut)]
getUtxoListInEmulator addr = do
    state <- TraceEmulator.chainState
    let utxoIndex = LedgerIndex.getIndex $ state Lens.^. TraceEmulator.index 
        utxos     =  [(oref, o) | (oref, o) <- DataMap.toList utxoIndex, LedgerApiV1.txOutAddress o == addr]
    P.pure utxos   


{-# INLINABLE removeUxtoByTxOutRef #-}
removeUxtoByTxOutRef  :: LedgerApiV1.TxOutRef -> [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)] -> [(LedgerApiV1.TxOutRef, LedgerTx.ChainIndexTxOut)]
removeUxtoByTxOutRef _ []                 = []
removeUxtoByTxOutRef x (y:ys) 
            | x == fst y      = removeUxtoByTxOutRef x ys
            | otherwise       =  y : removeUxtoByTxOutRef x ys


-- manager :: PubKeyHash
-- manager = let (P.Just pkh) = PlutusTx.fromBuiltinData (mkB "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e") in pkh 
-- pk = Ledger.PaymentPubKeyHash manager
-- dm = T.mkPoolState pk
-- datadm = PlutusTx.toData dm
