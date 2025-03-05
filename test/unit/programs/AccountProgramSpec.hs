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
createTestProgram :: [(ResourceId, Integer)] -> AccountProgram
createTestProgram initialBalances = 
  let resourceLedger = ResourceLedger $ Map.fromList initialBalances
      lockTable = LockTable Map.empty
  in AccountProgram
       { actorId = mockActorId
       , resources = resourceLedger
       , locks = lockTable
       , inbox = []
       , outbox = []
       }

-- | Test specification
spec :: Spec
spec = do
  describe "AccountProgram" $ do
    it "can be created with initial balances" $ do
      let program = createTestProgram [(mockResourceId, 100)]
      getBalance program mockResourceId `shouldBe` 100
    
    it "can update balances" $ do
      let program = createTestProgram [(mockResourceId, 100)]
      let updated = updateBalance program mockResourceId 150
      getBalance updated mockResourceId `shouldBe` 150
    
    it "can lock resources" $ do
      let program = createTestProgram [(mockResourceId, 100)]
      let (updated, _) = lockResource program mockResourceId 0
      isResourceLocked updated mockResourceId `shouldBe` True
    
    it "can unlock resources" $ do
      let program = createTestProgram [(mockResourceId, 100)]
      let (locked, _) = lockResource program mockResourceId 0
      let unlocked = unlockResource locked mockResourceId
      isResourceLocked unlocked mockResourceId `shouldBe` False

-- Helper function for tests
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False 