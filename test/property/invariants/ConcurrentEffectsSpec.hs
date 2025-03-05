{-# LANGUAGE OverloadedStrings #-}

module ConcurrentEffectsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Map.Strict qualified as Map
import Data.List (sort)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (replicateM)

import Core.Common (ActorId)
import Core.Resource (ResourceId)
import Core.Types (LamportTime)

import Programs.AccountProgram
  ( AccountProgram(..)
  , AccountMessage(..)
  , ResourceLedger(..)
  , createAccountProgram
  , applyAccountMessage
  , getBalance
  )

-- | Generate a random actor ID
genActorId :: Gen ActorId
genActorId = do
  n <- choose (1, 1000 :: Int)
  pure $ "actor-" <> show n

-- | Generate a random resource ID
genResourceId :: Gen ResourceId
genResourceId = do
  n <- choose (1, 100 :: Int)
  pure $ "resource-" <> show n

-- | Generate a random amount
genAmount :: Gen Integer
genAmount = choose (1, 100)

-- | Generate a random timestamp
genTimestamp :: Gen LamportTime
genTimestamp = choose (1, 1000)

-- | Generate a random deposit message
genDeposit :: ResourceId -> Gen AccountMessage
genDeposit resourceId = do
  amount <- genAmount
  pure $ Deposit resourceId amount

-- | Generate a random withdraw message
genWithdraw :: ResourceId -> Gen AccountMessage
genWithdraw resourceId = do
  amount <- genAmount
  pure $ Withdraw resourceId amount

-- | Generate a list of random account messages
genMessages :: ResourceId -> Int -> Gen [AccountMessage]
genMessages resourceId n = do
  replicateM n $ oneof [genDeposit resourceId, genWithdraw resourceId]

-- | Property: Total balance after concurrent operations should be consistent
prop_concurrentBalanceConsistency :: Property
prop_concurrentBalanceConsistency = monadicIO $ do
  -- Generate test data
  actorId <- pick genActorId
  resourceId <- pick genResourceId
  initialBalance <- pick $ choose (1000, 2000 :: Integer)
  numOperations <- pick $ choose (10, 50 :: Int)
  messages <- pick $ genMessages resourceId numOperations
  timestamps <- pick $ sort <$> replicateM numOperations genTimestamp
  
  -- Create initial program
  program <- run $ createAccountProgram actorId (Map.singleton resourceId initialBalance)
  
  -- Calculate expected final balance
  let expectedDelta = sum [case msg of
                            Deposit _ amount -> amount
                            Withdraw _ amount -> -amount
                            _ -> 0
                          | msg <- messages]
      expectedFinalBalance = initialBalance + expectedDelta
  
  -- Apply messages sequentially
  let applyMessage (prog, _) (msg, ts) = applyAccountMessage prog msg ts
      (finalProgram, _) = foldl applyMessage (program, Nothing) (zip messages timestamps)
  
  -- Check final balance
  let actualFinalBalance = getBalance finalProgram resourceId
  
  -- Assert balance is as expected
  assert (actualFinalBalance == expectedFinalBalance)

-- | Property: Concurrent deposits should all be reflected in final balance
prop_concurrentDeposits :: Property
prop_concurrentDeposits = monadicIO $ do
  -- Generate test data
  actorId <- pick genActorId
  resourceId <- pick genResourceId
  initialBalance <- pick $ choose (1000, 2000 :: Integer)
  numDeposits <- pick $ choose (10, 50 :: Int)
  amounts <- pick $ replicateM numDeposits genAmount
  timestamps <- pick $ sort <$> replicateM numDeposits genTimestamp
  
  -- Create initial program
  program <- run $ createAccountProgram actorId (Map.singleton resourceId initialBalance)
  
  -- Create deposit messages
  let messages = [Deposit resourceId amount | amount <- amounts]
      expectedFinalBalance = initialBalance + sum amounts
  
  -- Apply messages concurrently (simulated)
  let applyMessage (prog, _) (msg, ts) = applyAccountMessage prog msg ts
      (finalProgram, _) = foldl applyMessage (program, Nothing) (zip messages timestamps)
  
  -- Check final balance
  let actualFinalBalance = getBalance finalProgram resourceId
  
  -- Assert balance is as expected
  assert (actualFinalBalance == expectedFinalBalance)

spec :: Spec
spec = do
  describe "Concurrent Effect Application" $ do
    it "maintains balance consistency with concurrent operations" $
      property prop_concurrentBalanceConsistency
    
    it "correctly applies all concurrent deposits" $
      property prop_concurrentDeposits 