{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Main
    ( main
    ) where

import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Run                      (runWith)

import qualified          Validators.Locker.PAB                    (ValidatorContracts )
import qualified          Validators.AlwaysTrue.PAB                    (ValidatorContracts )
import qualified          Validators.AlwaysFalse.PAB                    (ValidatorContracts )
import qualified          Validators.Beneficiary.PAB                    (ValidatorContracts )
import qualified          Validators.Deadline.PAB                    (ValidatorContracts )
import qualified          Validators.Redeemer.PAB                    (ValidatorContracts )
-- import qualified          Validators.StakeSimple.PAB                    (ValidatorContracts )



main :: IO ()
main = do

    -- putStrLn "Iniciar PAB API server de Validador:"

    -- putStrLn "1: Locker"
    -- putStrLn "2: AlwaysTrue"
    -- putStrLn "3: AlwaysFalse"
    -- putStrLn "4: Beneficiary"
    -- putStrLn "5: Deadline"
    -- putStrLn "6: Redeemer"
    -- putStrLn "7: Stake Simple"

    opcion <- getLine

    case read opcion of
        1 -> do
            runWith (Builtin.handleBuiltin @Validators.Locker.PAB.ValidatorContracts)
        2 -> do
            runWith (Builtin.handleBuiltin @Validators.AlwaysTrue.PAB.ValidatorContracts)
        3 -> do
            runWith (Builtin.handleBuiltin @Validators.AlwaysFalse.PAB.ValidatorContracts)
        4 -> do
            runWith (Builtin.handleBuiltin @Validators.Beneficiary.PAB.ValidatorContracts)
        5 -> do
            runWith (Builtin.handleBuiltin @Validators.Deadline.PAB.ValidatorContracts)
        6 -> do
            runWith (Builtin.handleBuiltin @Validators.Redeemer.PAB.ValidatorContracts)
        -- 7 -> do
        --     runWith (Builtin.handleBuiltin @Validators.StakeSimple.PAB.ValidatorContracts)
        _ -> main 


