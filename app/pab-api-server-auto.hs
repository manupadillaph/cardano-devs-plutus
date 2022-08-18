{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Main where

import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Run                      (runWith)

import qualified Validators.LockerV1.PAB             (ValidatorContracts )
import qualified Validators.AlwaysTrueV1.PAB         (ValidatorContracts )
import qualified Validators.AlwaysFalseV1.PAB        (ValidatorContracts )
import qualified Validators.BeneficiaryV1.PAB        (ValidatorContracts )
import qualified Validators.DeadlineV1.PAB           (ValidatorContracts )
import qualified Validators.RedeemerV1.PAB           (ValidatorContracts )
-- import qualified Validators.StakePlusV1.PAB         (ValidatorContracts )
-- import qualified Validators.StakeSimpleV1.PAB       (ValidatorContracts )

--Modulo: 

main :: IO ()
main = do

    -- putStrLn "Iniciar PAB API server de Validador:"

    -- putStrLn "1: Locker"
    -- putStrLn "2: AlwaysTrue"
    -- putStrLn "3: AlwaysFalseV1"
    -- putStrLn "4: Beneficiary"
    -- putStrLn "5: Deadline"
    -- putStrLn "6: Redeemer"
    -- putStrLn "7: Stake Simple"

    opcion <- getLine

    case read opcion of
        1 -> do
            runWith (Builtin.handleBuiltin @Validators.LockerV1.PAB.ValidatorContracts)
        2 -> do
            runWith (Builtin.handleBuiltin @Validators.AlwaysTrueV1.PAB.ValidatorContracts)
        3 -> do
            runWith (Builtin.handleBuiltin @Validators.AlwaysFalseV1.PAB.ValidatorContracts)
        4 -> do
            runWith (Builtin.handleBuiltin @Validators.BeneficiaryV1.PAB.ValidatorContracts)
        5 -> do
            runWith (Builtin.handleBuiltin @Validators.DeadlineV1.PAB.ValidatorContracts)
        6 -> do
            runWith (Builtin.handleBuiltin @Validators.RedeemerV1.PAB.ValidatorContracts)
        -- 7 -> do
        --     runWith (Builtin.handleBuiltin @Validators.StakeSimpleV1.PAB.ValidatorContracts)
        _ -> main 


