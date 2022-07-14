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

module Validators.StakeSimple.OffChainHelpers where

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
--import  Validators.StakeSimple.OnChainHelpers 
import  Validators.StakeSimple.Helpers     

{- | Get the value from a ChainIndexTxOut. -}
getValueFromChainIndexTxOut :: ChainIndexTxOut -> Value
getValueFromChainIndexTxOut scriptChainIndexTxOut = scriptChainIndexTxOut ^. ciTxOutValue

{- | Try to get the generic Datum from a ChainIndexTxOut. -}
getDatumFromChainIndexTxOut :: ChainIndexTxOut ->  Maybe ValidatorDatum
getDatumFromChainIndexTxOut scriptChainIndexTxOut = do
    -- logInfo @P.String $ printf "getDatumFromChainIndexTxOut de: %s " (P.show $ _ciTxOutDatum o)
    case _ciTxOutDatum scriptChainIndexTxOut of
        Left _          -> do
            -- logInfo @P.String $ printf "Left " 
            Nothing
        Right datum -> do
            let 
                validatorDatum = PlutusTx.fromBuiltinData (getDatum datum) :: Maybe ValidatorDatum
            case validatorDatum of
                Nothing ->  do
                    -- logInfo @P.String $ printf "Nothing "
                    Nothing    
                Just (PoolState dPoolState) ->  do
                    -- logInfo @P.String $ printf "Encontrado Datumm Master: %s" (P.show dPoolState)
                    Just (PoolState dPoolState)   
                Just (UserState validatorUserState) ->  do
                    -- logInfo @P.String $ printf "Encontrado Datumm User: %s" (P.show validatorUserState)
                    Just (UserState validatorUserState)   


getPoolStateListFromUtxoList :: [(TxOutRef, ChainIndexTxOut)] -> [PoolStateTypo]
getPoolStateListFromUtxoList utxosWithPoolState  = do
    [  fromJust (getPoolStateFromDatum (getDatumFromChainIndexTxOut  scriptChainIndexTxOut)) | (txOutRef, scriptChainIndexTxOut) <- utxosWithPoolState, datumIsPoolState  (getDatumFromChainIndexTxOut  scriptChainIndexTxOut) ]



   

   
{- | Get the list of utxos with valid PoolState datum in the address. -}
getUtxoListWithValidPoolStateInScript :: Ledger.Address -> Contract w s Text [(TxOutRef, ChainIndexTxOut)]
getUtxoListWithValidPoolStateInScript addressValidator  = do
    utxos <- utxosAt addressValidator
    logInfo @P.String $ printf "utxosAt: %s" (P.show utxos)
    let 
        utxosListValid = [ (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- Map.toList utxos, datumIsPoolState (getDatumFromChainIndexTxOut  scriptChainIndexTxOut) ]
        -- TODO: revisar lista de utxo, comprobar datum correcto, tienen que contener los mismos invertsores que el pool, 
        -- el mismo NFT y el NFT tiene que estar en el pool

        utxosRef = [ txOutRef | (txOutRef, scriptChainIndexTxOut) <- utxosListValid ]

    logInfo @P.String $ printf "utxos List Valid: %s" (P.show utxosRef)
    return utxosListValid


{- | Get the list of utxos in the address for use in the emulator trace. -}
getUtxoListInEmulator :: Ledger.Address -> Trace.EmulatorTrace [(TxOutRef, TxOut)]
getUtxoListInEmulator addr = do
    state <- Trace.chainState
    let utxoIndex = getIndex $ state ^. Trace.index 
        utxos     =  [(oref, o) | (oref, o) <- Map.toList utxoIndex, txOutAddress o == addr]
    P.pure utxos   


mkPoolStateWithNewFundFromUtxoList :: [(TxOutRef, ChainIndexTxOut)]  ->  PoolNFT -> Master -> Fund -> ValidatorDatum
mkPoolStateWithNewFundFromUtxoList utxosWithPoolState poolNFT master fund  = do
    let 
        poolStateDatums = getPoolStateListFromUtxoList utxosWithPoolState
    
    mkPoolStateWithNewFundFromPoolStateList poolStateDatums poolNFT master fund 


mkPoolStateWithNewUserInvestFromUtxoList :: [(TxOutRef, ChainIndexTxOut)]  -> PoolNFT  ->  UserNFT  -> ValidatorDatum
mkPoolStateWithNewUserInvestFromUtxoList utxosWithPoolState poolNFT userNFT   = do
    let 
        poolStateDatums = getPoolStateListFromUtxoList utxosWithPoolState
    
    mkPoolStateWithNewUserInvestFromPoolStateList poolStateDatums poolNFT userNFT 




-- getDatumContract :: [(TxOutRef, ChainIndexTxOut)]  -> Contract w s Text ()
-- getDatumContract [(txOutRef, scriptChainIndexTxOut)] = do
--     logInfo @String $ printf "GetDatum %s" (P.show txOutRef)
--     let 
--         dat = getDatumFromChainIndexTxOut scriptChainIndexTxOut
--     logInfo @String $ printf "Datum %s" (P.show dat)
--     return ()
-- getDatumContract [] = return ()
-- getDatumContract ((txOutRef, scriptChainIndexTxOut):xs) = do
--     getDatumContract [(txOutRef, scriptChainIndexTxOut)]
--     getDatumContract xs



-- checkUtxoMaster  ::  ChainIndexTxOut -> Master -> Bool
-- checkUtxoMaster scriptChainIndexTxOut  master  = 
--     case getDatumFromChainIndexTxOut scriptChainIndexTxOut of
--         Nothing -> False
--         Just (PoolState dPoolState) -> PlutusTx.Prelude.any (master==) [mfMaster masterFunder | masterFunder <- psMasterFunders dPoolState]
--         _ -> False


-- checkUtxoUserWithDeadline  :: ChainIndexTxOut-> User -> Deadline-> Bool
-- checkUtxoUserWithDeadline scriptChainIndexTxOut user deadline = 
--     case getDatumFromChainIndexTxOut scriptChainIndexTxOut of
--         Nothing -> False
--         Just (UserState validatorUserState) -> usUser validatorUserState == user && deadline >= usDeadline validatorUserState  
--         _ -> False




-- findUtxosMaster :: [(TxOutRef, ChainIndexTxOut)]  -> Master  ->  [(TxOutRef, ChainIndexTxOut)]
-- findUtxosMaster [] _ = []  
-- findUtxosMaster [(txOutRef, scriptChainIndexTxOut)]  master  
--     | checkUtxoMaster scriptChainIndexTxOut master = [(txOutRef, scriptChainIndexTxOut)]
--     | otherwise = []
-- findUtxosMaster ((txOutRef, scriptChainIndexTxOut):xs) master  
--     | checkUtxoMaster scriptChainIndexTxOut master = (txOutRef, scriptChainIndexTxOut):findUtxosMaster xs master
--     | otherwise = findUtxosMaster xs master


-- findUtxosUserWithDeadline :: [(TxOutRef, ChainIndexTxOut)]  -> User  -> Deadline ->  [(TxOutRef, ChainIndexTxOut)]
-- findUtxosUserWithDeadline [] _ _ = []  
-- findUtxosUserWithDeadline [(txOutRef, scriptChainIndexTxOut)]  user  deadline
--     | checkUtxoUserWithDeadline scriptChainIndexTxOut user deadline= [(txOutRef, scriptChainIndexTxOut)]
--     | otherwise = []
-- findUtxosUserWithDeadline ((txOutRef, scriptChainIndexTxOut):xs) user  deadline
--     | checkUtxoUserWithDeadline scriptChainIndexTxOut  user deadline = (txOutRef, scriptChainIndexTxOut):findUtxosUserWithDeadline xs user deadline
--     | otherwise = findUtxosUserWithDeadline xs user deadline


-- findUtxosFromMasters :: Ledger.Address -> Master -> Contract w s Text [(TxOutRef, ChainIndexTxOut)]
-- findUtxosFromMasters addressValidator master = do
--     utxos <- utxosAt addressValidator
--     logInfo @P.String $ printf "utxosAt: %s" (P.show utxos)
--     let 
--         xs = [ (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- Map.toList utxos ]
--         utxosFromMaster = findUtxosMaster xs master 
--         utxosRef = [ txOutRef | (txOutRef, scriptChainIndexTxOut) <- utxosFromMaster ]
--     getDatumContract $ Map.toList utxos
--     logInfo @P.String $ printf "utxosFromMaster: %s" (P.show utxosRef)
--     return utxosFromMaster





-- findUtxosFromUserWithDeadline :: Ledger.Address -> User  -> Deadline -> Contract w s Text [(TxOutRef, ChainIndexTxOut)]
-- findUtxosFromUserWithDeadline addressValidator user deadline = do
--     utxos <- utxosAt addressValidator
--     logInfo @P.String $ printf "utxosAt: %s" (P.show utxos)
--     let 
--         xs = [ (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- Map.toList utxos ]
--         utxosFromUserWithDeadline = findUtxosUserWithDeadline xs user deadline
--     getDatumContract $ Map.toList utxos
--     -- logInfo @P.String $ printf "utxosFromMaster: %s" (P.show utxosFromMaster)
--     return utxosFromUserWithDeadline









-- getUtxoAndChainIndexFromUtxo :: Ledger.Address -> TxOutRef -> Contract w s Text (TxOutRef, ChainIndexTxOut)
-- getUtxoAndChainIndexFromUtxo addrs get_oref = do
--     utxos <- utxosAt addrs
--     let 
--         xs = [ (oref, o) | (oref, o) <- Map.toList utxos , get_oref == oref]
--     case xs of
--         [x] ->  return x














-- getValidatorDatumm :: (TxOutRef, ChainIndexTxOut) -> Maybe ValidatorDatum
-- getValidatorDatumm (oref,o) = case _ciTxOutDatum o of
--                         Left _          -> Nothing
--                         Right (Datum e) -> case PlutusTx.fromBuiltinData e of
--                             Nothing -> Nothing
--                             Just d@Validators.StakeSimple.Typos.ValidatorDatum{..} -> Just d

-- getPoolStateTypo :: (TxOutRef, ChainIndexTxOut) -> Maybe PoolStateTypo
-- getPoolStateTypo (txOutRef, scriptChainIndexTxOut) = case _ciTxOutDatum scriptChainIndexTxOut of
--                         Left _          -> Nothing
--                         Right (Datum eDatum) -> case PlutusTx.fromBuiltinData eDatum of
--                             Nothing -> Nothing
--                             Just datum@Validators.StakeSimple.Typos.PoolStateTypo{..}  -> Just datum

-- getPoolStateTypo1 :: PaymentPubKeyHash -> (TxOutRef, ChainIndexTxOut) -> Maybe PoolStateTypo
-- getPoolStateTypo1 p (txOutRef, scriptChainIndexTxOut) = case _ciTxOutDatum scriptChainIndexTxOut of
--                         Left _          -> Nothing
--                         Right (Datum eDatum) -> case PlutusTx.fromBuiltinData eDatum of
--                             Nothing -> Just (mkPoolStateTypo p)
--                             Just datum@Validators.StakeSimple.Typos.PoolStateTypo{..}  -> Just datum


-- getUserStateTypo :: (TxOutRef, ChainIndexTxOut) -> Maybe UserStateTypo
-- getUserStateTypo (txOutRef, scriptChainIndexTxOut) = do

--     case _ciTxOutDatum scriptChainIndexTxOut of
--         Left _          -> Nothing
--         Right (Datum eDatum) -> case PlutusTx.fromBuiltinData eDatum of
--             Nothing -> Nothing
--             Just datum@Validators.StakeSimple.Typos.UserStateTypo{..} -> Just datum 


-- {- | Get the datum from a ChainIndexTxOut, only if it is not a datum hash. -}
-- getDatumFromChainIndexTxOut ::  ChainIndexTxOut -> Maybe PoolStateTypo
-- getDatumFromChainIndexTxOut scriptChainIndexTxOut = do
--     let 
--         a = _ciTxOutDatum scriptChainIndexTxOut
--     case a of
--         Right d -> PlutusTx.fromBuiltinData $ getDatum d
--         _ -> Nothing    

--     -- case  _ciTxOutDatum scriptChainIndexTxOut of
--     --     Right d -> PlutusTx.fromBuiltinData $ getDatum d
--     --     _       -> Nothing

-- {- | Get the value from a ChainIndexTxOut. -}
-- getValueFromChainIndexTxOut :: ChainIndexTxOut -> Value
-- getValueFromChainIndexTxOut scriptChainIndexTxOut = scriptChainIndexTxOut ^. ciTxOutValue


-- -- -- | Monadic function for getting the datum from a ChainIndexTxOut.
-- -- getContractDatum :: ChainIndexTxOut -> Contract w s Text PoolStateTypo
-- -- getContractDatum  = maybe (PC.throwError "Cannot find contract datum") return . getDatumFromChainIndexTxOut
-- --     -- maybe (PC.throwError "Cannot find contract datum") return .
     


         
-- findUtxosFromMasters ::  Ledger.Address -> PaymentPubKeyHash -> Contract w s Text ([TxOutRef])
-- findUtxosFromMasters addressValidator ppkh  = do
--     utxos <- utxosAt addressValidator
--     logInfo @P.String $ printf "utxosAt %s" (P.show utxos)
--     let 
--         -- xs1 = [ getPoolStateTypo1 ppkh (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- Map.toList utxos ]

--         xs = [ (txOutRef, scriptChainIndexTxOut) | (txOutRef, scriptChainIndexTxOut) <- Map.toList utxos ]

--         utxosFromMaster = findUtxos xs ppkh

--     -- logInfo @P.String $ printf "xs1 %s" (P.show xs1)
        
--     return utxosFromMaster

-- findUtxos :: [(TxOutRef, ChainIndexTxOut)]  -> PaymentPubKeyHash  ->  [TxOutRef]
-- findUtxos [] _ = []  
-- findUtxos [(txOutRef, scriptChainIndexTxOut)]  ppkh  
--     | checkUtxo (txOutRef, scriptChainIndexTxOut)  ppkh = [txOutRef]
--     | otherwise = []
-- findUtxos ((txOutRef, scriptChainIndexTxOut):xs) ppkh  
--     | checkUtxo (txOutRef, scriptChainIndexTxOut)  ppkh = txOutRef:findUtxos xs ppkh
--     | otherwise = findUtxos xs ppkh

-- checkUtxo  :: (TxOutRef, ChainIndexTxOut) -> PaymentPubKeyHash -> Bool
-- checkUtxo (txOutRef, scriptChainIndexTxOut) ppkh  = do
--     let 

--         datum    = getDatumFromChainIndexTxOut scriptChainIndexTxOut
--     case datum of 
--         Nothing -> True
--         Just PoolStateTypo{..} -> psMasterFunders == ppkh

    -- case datum of
    --     UserState vdu             -> False
    --         -- PC.throwError
    --         -- "Expected PoolStateTypo but found UserStateTypo in staking script UTxO."
    --     PoolState vdm -> psMasterFunders vdm == ppkh

    -- case getPoolStateTypo (txOutRef, scriptChainIndexTxOut) of
    --     Nothing -> False
    --     Just d@PoolStateTypo{..}
    --         | psMasterFunders == ppkh -> True
    --         | otherwise          -> True


-- getDatumm :: (TxOutRef, ChainIndexTxOut) -> Maybe ValidatorDatum
-- getDatumm (oref,o) = case _ciTxOutDatum o of
--                         Left _          -> Nothing
--                         Right (Datum e) -> case PlutusTx.fromBuiltinData e of
--                             Nothing -> Nothing
--                             Just d@ValidatorDatum{..} -> Just d

-- checkUTXO  :: (TxOutRef, ChainIndexTxOut) -> PaymentPubKeyHash -> Integer -> Bool
-- checkUTXO (oref,o)  ppkh name = do
--     case getDatumm (oref,o) of
--         Nothing -> False
--         Just d@ValidatorDatum{..}
--             | aCreator dData == ppkh && aName dData == name -> True
--             | otherwise                                           -> False

-- findUTXO :: [(TxOutRef, ChainIndexTxOut)]  -> PaymentPubKeyHash -> Integer -> Maybe TxOutRef
-- findUTXO [] _ _ = Nothing --do  
-- findUTXO [(oref,o)]  ppkh name  = do
--     if checkUTXO (oref, o) ppkh name then 
--         return oref
--     else 
--         Nothing
-- findUTXO ((oref,o):xs) ppkh name  
--     | checkUTXO (oref ,o)  ppkh name = return oref
--     | otherwise = findUTXO xs   ppkh name

-- findUtxoInValidator :: PaymentPubKeyHash -> Integer -> Contract w s Text (Maybe TxOutRef)
-- findUtxoInValidator ppkh name = do
--     utxos <- utxosAt addressValidator
--     let 
--         xs = [ (oref, o) | (oref, o) <- Map.toList utxos ]
--         out = findUTXO xs ppkh name
--     return out
 
-- getFromValidator :: TxOutRef -> Contract w s Text (TxOutRef, ChainIndexTxOut)
-- getFromValidator get_oref= do
--     utxos <- utxosAt addressValidator
--     let 
--         xs = [ (oref, o) | (oref, o) <- Map.toList utxos , get_oref == oref]
--     case xs of
--         [x] ->  return x




-- manager :: PubKeyHash
-- manager = let (Just pkh) = PlutusTx.fromBuiltinData (mkB "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e") in pkh 
-- pk = PaymentPubKeyHash manager
-- dm = mkPoolState pk
-- datadm = PlutusTx.toData dm
