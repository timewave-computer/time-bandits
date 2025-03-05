module BasicFunctionalitySpec (spec) where

import Test.Hspec
import Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import TestUtils

-- | Test specification
spec :: Spec
spec = do
  describe "Basic Functionality" $ do
    it "can create and register resources" $ do
      -- Create a resource registry
      registry <- mockResourceRegistry
      
      -- Register a new resource
      let resourceId = T.pack "new-token"
      let programId = T.pack "program-3"
      result <- mockRegisterResource registry resourceId programId
      
      -- Verify registration was successful
      result `shouldSatisfy` TestUtils.isRight
      
      -- Check that the resource is registered with the correct program
      case result of
        Left err -> fail $ "Expected successful registration but got error: " ++ show err
        Right updatedRegistry -> Map.lookup resourceId updatedRegistry `shouldBe` Just programId
    
    it "can create account programs with initial balances" $ do
      -- Create an account program with initial balances
      let actorId = mockActorId
      let initialBalances = Map.fromList [(T.pack "token-1", 100), (T.pack "token-2", 200)]
      program <- mockCreateAccountProgram actorId initialBalances
      
      -- Verify the program has the correct initial balances
      mockGetBalance program (T.pack "token-1") `shouldBe` 100
      mockGetBalance program (T.pack "token-2") `shouldBe` 200
      mockGetBalance program (T.pack "non-existent-token") `shouldBe` 0
    
    it "can update resource balances" $ do
      -- Create an account program with initial balances
      let actorId = mockActorId
      let initialBalances = Map.fromList [(T.pack "token-1", 100)]
      program <- mockCreateAccountProgram actorId initialBalances
      
      -- Deposit resources
      depositResult <- mockDepositResource program (T.pack "token-1") 50
      depositResult `shouldSatisfy` TestUtils.isRight
      
      -- Verify the balance was updated
      case depositResult of
        Left err -> fail $ "Expected successful deposit but got error: " ++ show err
        Right updatedProgram -> mockGetBalance updatedProgram (T.pack "token-1") `shouldBe` 150
      
      -- Create another program instance for withdrawal
      program2 <- mockCreateAccountProgram actorId initialBalances
      
      -- Withdraw resources
      withdrawResult <- mockWithdrawResource program2 (T.pack "token-1") 30
      withdrawResult `shouldSatisfy` TestUtils.isRight
      
      -- Verify the balance was updated
      case withdrawResult of
        Left err -> fail $ "Expected successful withdrawal but got error: " ++ show err
        Right updatedProgram -> mockGetBalance updatedProgram (T.pack "token-1") `shouldBe` 70
    
    it "can handle concurrent balance updates" $ do
      -- Create two account programs
      let actor1Id = T.pack "actor-1"
      let actor2Id = T.pack "actor-2"
      let program1Balances = Map.fromList [(T.pack "token-1", 100), (T.pack "token-2", 50)]
      let program2Balances = Map.fromList [(T.pack "token-1", 200), (T.pack "token-3", 75)]
      
      program1 <- mockCreateAccountProgram actor1Id program1Balances
      program2 <- mockCreateAccountProgram actor2Id program2Balances
      
      -- Update balances concurrently on different resources
      depositResult1 <- mockDepositResource program1 (T.pack "token-1") 25
      depositResult2 <- mockDepositResource program2 (T.pack "token-3") 25
      
      -- Verify both updates succeeded
      depositResult1 `shouldSatisfy` TestUtils.isRight
      depositResult2 `shouldSatisfy` TestUtils.isRight
      
      -- Verify the balances were updated correctly
      case depositResult1 of
        Left err -> fail $ "Expected successful deposit but got error: " ++ show err
        Right updatedProgram1 -> mockGetBalance updatedProgram1 (T.pack "token-1") `shouldBe` 125
      
      case depositResult2 of
        Left err -> fail $ "Expected successful deposit but got error: " ++ show err
        Right updatedProgram2 -> mockGetBalance updatedProgram2 (T.pack "token-3") `shouldBe` 100
    
    it "can transfer resources between programs" $ do
      -- Create two account programs
      let actor1Id = T.pack "actor-1"
      let actor2Id = T.pack "actor-2"
      let program1Balances = Map.fromList [(T.pack "token-1", 100)]
      let program2Balances = Map.fromList [(T.pack "token-1", 50)]
      
      program1 <- mockCreateAccountProgram actor1Id program1Balances
      program2 <- mockCreateAccountProgram actor2Id program2Balances
      
      -- Transfer resources from program1 to program2
      transferResult <- mockTransferResource program1 program2 (T.pack "token-1") 25
      
      -- Verify the transfer was successful
      transferResult `shouldSatisfy` TestUtils.isRight
      
      -- Verify the balances were updated correctly
      case transferResult of
        Left err -> fail $ "Expected successful transfer but got error: " ++ show err
        Right (updatedProgram1, updatedProgram2) -> do
          mockGetBalance updatedProgram1 (T.pack "token-1") `shouldBe` 75
          mockGetBalance updatedProgram2 (T.pack "token-1") `shouldBe` 75 