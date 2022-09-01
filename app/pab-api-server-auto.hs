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
import qualified Validators.MarketNFTV1.PAB          (ValidatorContracts )
import qualified Validators.StakeSimpleV1.PAB        (ValidatorContracts )
import qualified Validators.StakePlusV1.PAB          (ValidatorContracts )

--Modulo: 

main :: IO ()
main = do

    -- putStrLn "Iniciar PAB API server de Validador:"

    -- putStrLn "1: LockerV1"
    -- putStrLn "2: AlwaysTrueV1"
    -- putStrLn "3: AlwaysFalseV1"
    -- putStrLn "4: BeneficiaryV1"
    -- putStrLn "5: DeadlineV1"
    -- putStrLn "6: RedeemerV1"
    -- putStrLn "7: MarketNFTV1"
    -- putStrLn "8: StakeSimpleV1"
    -- putStrLn "9: StakePlusV1"

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
        7 -> do
            runWith (Builtin.handleBuiltin @Validators.MarketNFTV1.PAB.ValidatorContracts)
        8 -> do
            runWith (Builtin.handleBuiltin @Validators.StakeSimpleV1.PAB.ValidatorContracts)
        9 -> do
            runWith (Builtin.handleBuiltin @Validators.StakePlusV1.PAB.ValidatorContracts)
        _ -> main 


