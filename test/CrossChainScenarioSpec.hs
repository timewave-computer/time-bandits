module CrossChainScenarioSpec (spec) where

import TestUtils
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- | Test specification
spec :: Spec
spec = do
  describe "Cross-Chain Scenarios" $ do
    it "can transfer resources between chains" $ do
      -- Create account programs on two different chains
      let actorId = mockActorId
      let initialBalances1 = Map.fromList [(T.pack "token-1", 100)]
      let initialBalances2 = Map.fromList []
      
      program1 <- mockCreateAccountProgram actorId initialBalances1
      program2 <- mockCreateAccountProgram actorId initialBalances2
      
      -- Create a cross-chain transfer
      let transferAmount = 50
      transferResult <- mockCrossChainTransfer program1 program2 (T.pack "token-1") transferAmount
      
      -- Verify the transfer succeeded
      transferResult `shouldSatisfy` isRight
      
      -- Verify the balances on both chains
      case transferResult of
        Left err -> fail $ "Expected successful transfer but got error: " ++ show err
        Right (updatedProgram1, updatedProgram2) -> do
          mockGetBalance updatedProgram1 (T.pack "token-1") `shouldBe` (100 - transferAmount)
          mockGetBalance updatedProgram2 (T.pack "token-1") `shouldBe` transferAmount
    
    it "maintains causal consistency across chains" $ do
      -- Create account programs on two different chains
      let actorId = mockActorId
      let initialBalances1 = Map.fromList [(T.pack "token-1", 100)]
      let initialBalances2 = Map.fromList []
      
      program1 <- mockCreateAccountProgram actorId initialBalances1
      program2 <- mockCreateAccountProgram actorId initialBalances2
      
      -- Perform a sequence of operations across chains
      -- First transfer
      transferResult1 <- mockCrossChainTransfer program1 program2 (T.pack "token-1") 30
      transferResult1 `shouldSatisfy` isRight
      
      -- Second transfer based on the result of the first
      case transferResult1 of
        Left err -> fail $ "Expected successful transfer but got error: " ++ show err
        Right (updatedProgram1, updatedProgram2) -> do
          -- Transfer back some tokens
          transferResult2 <- mockCrossChainTransfer updatedProgram2 updatedProgram1 (T.pack "token-1") 10
          transferResult2 `shouldSatisfy` isRight
          
          -- Verify final balances
          case transferResult2 of
            Left err -> fail $ "Expected successful transfer but got error: " ++ show err
            Right (finalProgram2, finalProgram1) -> do
              mockGetBalance finalProgram1 (T.pack "token-1") `shouldBe` 80
              mockGetBalance finalProgram2 (T.pack "token-1") `shouldBe` 20
    
    it "handles cross-chain resource locking correctly" $ do
      -- Create account programs on two different chains
      let actorId = mockActorId
      let initialBalances1 = Map.fromList [(T.pack "token-1", 100)]
      
      program1 <- mockCreateAccountProgram actorId initialBalances1
      
      -- Lock a resource for cross-chain transfer
      lockResult <- mockLockResource program1 (T.pack "token-1") 40
      lockResult `shouldSatisfy` isRight
      
      -- Verify the resource is locked
      case lockResult of
        Left err -> fail $ "Expected successful lock but got error: " ++ show err
        Right lockedProgram -> do
          -- Try to withdraw any amount while locked (should fail because resource is locked)
          withdrawResult <- mockWithdrawResource lockedProgram (T.pack "token-1") 10
          withdrawResult `shouldSatisfy` isLeft
          
          -- Unlock the resource
          unlockResult <- mockUnlockResource lockedProgram (T.pack "token-1")
          unlockResult `shouldSatisfy` isRight
          
          -- Verify the resource is unlocked
          case unlockResult of
            Left err -> fail $ "Expected successful unlock but got error: " ++ show err
            Right unlockedProgram -> do
              -- Now should be able to withdraw some balance
              finalWithdrawResult <- mockWithdrawResource unlockedProgram (T.pack "token-1") 30
              finalWithdrawResult `shouldSatisfy` isRight 