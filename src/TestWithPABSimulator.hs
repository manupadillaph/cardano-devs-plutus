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

module TestWithPABSimulator  where
  
import qualified Prelude                             as P

--Import Internos

import qualified Validators.LockerV1
import qualified Validators.AlwaysTrueV1
import qualified Validators.AlwaysFalseV1
import qualified Validators.BeneficiaryV1
import qualified Validators.DeadlineV1
import qualified Validators.RedeemerV1

import qualified Validators.MarketNFTV1

import qualified Validators.StakePlusV1
import qualified Validators.StakeSimpleV1

--Modulo:

testValidatorMarketNFTV1WithPABSimulator  :: P.IO ()
testValidatorMarketNFTV1WithPABSimulator  = Validators.MarketNFTV1.testWithPABSimulator  

---------------------------

testValidatorStakeSimpleV1WithPABSimulator  :: P.IO ()
testValidatorStakeSimpleV1WithPABSimulator  = Validators.StakeSimpleV1.testWithPABSimulator

---------------------------

testValidatorStakePlusV1WithPABSimulator  :: P.IO ()
testValidatorStakePlusV1WithPABSimulator  = Validators.StakePlusV1.testWithPABSimulator
