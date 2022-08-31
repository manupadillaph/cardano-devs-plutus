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

module TestWithTraceEmulator where
  
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

import qualified MintingPolicies.FreeV1              
import qualified MintingPolicies.NFTV1      
import qualified MintingPolicies.NFTSignedV1                
import qualified MintingPolicies.SignedV1         
import qualified MintingPolicies.TokensV1            
import qualified MintingPolicies.TokensSignedV1   
--Modulo:

testValidatorLockerV1WithTraceEmulator :: P.IO ()
testValidatorLockerV1WithTraceEmulator = Validators.LockerV1.testWithTraceEmulator  

---------------------------

testValidatorAlwaysTrueV1WithTraceEmulator :: P.IO ()
testValidatorAlwaysTrueV1WithTraceEmulator = Validators.LockerV1.testWithTraceEmulator  

---------------------------

testValidatorAlwaysFalseV1WithTraceEmulator :: P.IO ()
testValidatorAlwaysFalseV1WithTraceEmulator = Validators.AlwaysFalseV1.testWithTraceEmulator

---------------------------

testValidatorBeneficiaryV1WithTraceEmulator  :: P.IO ()
testValidatorBeneficiaryV1WithTraceEmulator  = Validators.BeneficiaryV1.testWithTraceEmulator

---------------------------

testValidatorDeadlineV1WithTraceEmulator :: P.IO ()
testValidatorDeadlineV1WithTraceEmulator = Validators.DeadlineV1.testWithTraceEmulator

---------------------------

testValidatorRedeemerV1WithTraceEmulator  :: P.IO ()
testValidatorRedeemerV1WithTraceEmulator  = Validators.RedeemerV1.testWithTraceEmulator

---------------------------

-- testValidatorMarketNFTV1WithTraceEmulator  :: P.IO ()
-- testValidatorMarketNFTV1WithTraceEmulator  = Validators.MarketNFTV1.testWithTraceEmulator


-- testValidatorStakeSimpleV1WithTraceEmulator :: P.IO ()
-- testValidatorStakeSimpleV1WithTraceEmulator = Validators.StakeSimpleV1.testWithTraceEmulator

-- ---------------------------

-- testValidatorStakePlusV1WithTraceEmulator  :: P.IO ()
-- testValidatorStakePlusV1WithTraceEmulator  = Validators.StakePlusV1.testWithTraceEmulator

---------------------------

testMintingPolicyFreeV1WithTraceEmulator  :: P.IO ()
testMintingPolicyFreeV1WithTraceEmulator = MintingPolicies.FreeV1.testWithTraceEmulator 

---------------------------

testMintingPolicyNFTV1WithTraceEmulator  :: P.IO ()
testMintingPolicyNFTV1WithTraceEmulator  = MintingPolicies.NFTV1.testWithTraceEmulator

---------------------------

testMintingPolicyNFTSignedV1WithTraceEmulator  :: P.IO ()
testMintingPolicyNFTSignedV1WithTraceEmulator  = MintingPolicies.NFTSignedV1.testWithTraceEmulator

---------------------------
testMintingPolicySignedV1WithTraceEmulator  :: P.IO ()
testMintingPolicySignedV1WithTraceEmulator  = MintingPolicies.SignedV1.testWithTraceEmulator

---------------------------

testMintingPolicyTokensV1WithTraceEmulator  :: P.IO ()
testMintingPolicyTokensV1WithTraceEmulator  = MintingPolicies.TokensV1.testWithTraceEmulator

---------------------------

testMintingPolicyTokensSignedV1WithTraceEmulator  :: P.IO ()
testMintingPolicyTokensSignedV1WithTraceEmulator  = MintingPolicies.TokensSignedV1.testWithTraceEmulator
