{-|
Module      : Staking
Description : Main module of the Staking Pool contract.
Copyright   : P2P Solutions Ltd.
License     : GPL-3
Maintainer  : laurynas@adafinance.io
Stability   : develop
-}

module Validators.StakeSimple
    ( 
      module Validators.StakeSimple.OffChain
    , module Validators.StakeSimple.OnChain
    , module Validators.StakeSimple.PAB
    , module Validators.StakeSimple.Test
    , module Validators.StakeSimple.Typos
    )
where

import Validators.StakeSimple.OffChain
import Validators.StakeSimple.OnChain
import Validators.StakeSimple.PAB
import Validators.StakeSimple.Test
import Validators.StakeSimple.Typos
