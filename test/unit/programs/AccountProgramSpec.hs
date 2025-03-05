{-# LANGUAGE OverloadedStrings #-}

module AccountProgramSpec (spec) where

import Test.Hspec
import Data.Map.Strict qualified as Map
import Data.ByteString.Char8 qualified as BS

import Core.Common (ActorId)
import Core.Resource (ResourceId)
import Core.Types (LamportTime)

import Programs.AccountProgram
  ( AccountProgram(..)
  , AccountMessage(..)
  , ResourceLedger(..)
  , LockTable(..)
  , createAccountProgram
  , applyAccountMessage
  , getBalance
  , updateBalance
  , lockResource
  , unlockResource
  , isResourceLocked
  )

-- Mock implementations for testing
mockActorId :: ActorId
mockActorId = "actor-123"

mockResourceId :: ResourceId
mockResourceId = "resource-456"

mockResourceId2 :: ResourceId
mockResourceId2 = "resource-789"

-- | Create a test account program with initial balances
createTestProgram :: IO AccountProgram
createTestProgram = do
  let initialBalances = Map.fromList [(mockResourceId, 100), (mockResourceId2, 50)]
  createAccountProgram mockActorId initialBalances

spec :: Spec
spec = do
  describe "AccountProgram" $ do
    describe "createAccountProgram" $ do
      it "creates a program with the correct initial balances" $ do
        program <- createTestProgram
        getBalance program mockResourceId `shouldBe` 100
        getBalance program mockResourceId2 `shouldBe` 50
    
    describe "updateBalance" $ do
      it "correctly updates the balance of a resource" $ do
        program <- createTestProgram
        let program' = updateBalance program mockResourceId 50 1
        getBalance program' mockResourceId `shouldBe` 150
      
      it "can handle negative deltas" $ do
        program <- createTestProgram
        let program' = updateBalance program mockResourceId (-30) 1
        getBalance program' mockResourceId `shouldBe` 70
    
    describe "lockResource" $ do
      it "locks a resource" $ do
        program <- createTestProgram
        let program' = lockResource program mockResourceId 1
        isResourceLocked program' mockResourceId `shouldBe` True
      
      it "doesn't lock other resources" $ do
        program <- createTestProgram
        let program' = lockResource program mockResourceId 1
        isResourceLocked program' mockResourceId2 `shouldBe` False
    
    describe "unlockResource" $ do
      it "unlocks a locked resource" $ do
        program <- createTestProgram
        let program' = lockResource program mockResourceId 1
            program'' = unlockResource program' mockResourceId
        isResourceLocked program'' mockResourceId `shouldBe` False
    
    describe "applyAccountMessage" $ do
      it "handles Deposit messages" $ do
        program <- createTestProgram
        let (program', _) = applyAccountMessage program (Deposit mockResourceId 50) 1
        getBalance program' mockResourceId `shouldBe` 150
      
      it "handles Withdraw messages with sufficient balance" $ do
        program <- createTestProgram
        let (program', result) = applyAccountMessage program (Withdraw mockResourceId 50) 1
        getBalance program' mockResourceId `shouldBe` 50
        result `shouldSatisfy` isJust
      
      it "rejects Withdraw messages with insufficient balance" $ do
        program <- createTestProgram
        let (program', result) = applyAccountMessage program (Withdraw mockResourceId 150) 1
        getBalance program' mockResourceId `shouldBe` 100  -- Unchanged
        result `shouldSatisfy` isJust
      
      it "handles Query messages" $ do
        program <- createTestProgram
        let (program', _) = applyAccountMessage program (Query mockResourceId) 1
        program' `shouldBe` program  -- Program state unchanged
      
      it "handles LockResources messages" $ do
        program <- createTestProgram
        let (program', _) = applyAccountMessage program (LockResources [mockResourceId]) 1
        isResourceLocked program' mockResourceId `shouldBe` True
      
      it "handles UnlockResources messages" $ do
        program <- createTestProgram
        let program' = lockResource program mockResourceId 1
            (program'', _) = applyAccountMessage program' (UnlockResources [mockResourceId]) 2
        isResourceLocked program'' mockResourceId `shouldBe` False

-- Helper function for tests
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False 