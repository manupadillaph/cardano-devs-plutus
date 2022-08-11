{-|
Module      : Staking
Description : Main module of the Staking Pool contract.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Validators.Beneficiary
    ( 
      module Validators.Beneficiary.OffChain
    , module Validators.Beneficiary.OnChain
    , module Validators.Beneficiary.PAB
    , module Validators.Beneficiary.Test
    , module Validators.Beneficiary.Typos
    )
where

import Validators.Beneficiary.OffChain
import Validators.Beneficiary.OnChain
import Validators.Beneficiary.PAB
import Validators.Beneficiary.Test
import Validators.Beneficiary.Typos
