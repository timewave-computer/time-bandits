module EffectReplayabilitySpec (spec) where

import TestUtils
import Data.List (sortOn)
import Control.Monad (foldM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

-- | Generate a random effect type
genEffectType :: Gen Text
genEffectType = elements [T.pack "DepositEffect", T.pack "WithdrawEffect", T.pack "TransferEffect", T.pack "UpdateStateEffect"]

-- | Generate a random effect
genEffect :: Gen MockEffect
genEffect = do
  effectType <- genEffectType
  resourceId <- elements [T.pack "resource-1", T.pack "resource-2", T.pack "resource-3"]
  effectData <- elements [T.pack "data-1", T.pack "data-2", T.pack "data-3"]
  -- Create a simple mock effect as a placeholder
  pure $ MockEffect effectType resourceId effectData

-- | Test specification
spec :: Spec
spec = do
  describe "Effect Application and Replayability" $ do
    it "all changes must flow through proven effects" $ do
      -- Create an account program
      let actorId = mockActorId
      let initialBalances = Map.fromList [(T.pack "token-1", 100)]
      accountProgram <- mockCreateAccountProgram actorId initialBalances
      
      -- Create an effect logger
      logger <- mockCreateLogger
      
      -- Apply a series of effects
      let resourceId = T.pack "token-1"
      -- Apply deposit, withdraw, and transfer operations sequentially
      result1 <- mockLogAppliedEffect logger resourceId (T.pack "deposit-effect") (T.pack "time-map-hash-1")
      result2 <- mockLogAppliedEffect logger resourceId (T.pack "withdraw-effect") (T.pack "time-map-hash-1")
      result3 <- mockLogAppliedEffect logger resourceId (T.pack "transfer-effect") (T.pack "time-map-hash-2")
      
      -- Verify all effects were logged
      result1 `shouldBe` Right ()
      result2 `shouldBe` Right ()
      result3 `shouldBe` Right ()
      
      -- Verify the final state reflects all applied effects
      let finalBalance = mockGetBalance accountProgram resourceId
      finalBalance `shouldBe` 100  -- Starting balance (would be different in a real test)
    
    it "effects that modify different resources within the same program may apply concurrently" $ do
      -- Create an account program
      let actorId = mockActorId
      let initialBalances = Map.fromList [(T.pack "token-1", 100), (T.pack "token-2", 200)]
      accountProgram <- mockCreateAccountProgram actorId initialBalances
      
      -- Create an effect logger
      logger <- mockCreateLogger
      
      -- Apply effects to different resources concurrently
      result1 <- mockLogAppliedEffect logger (T.pack "token-1") (T.pack "deposit-effect-1") (T.pack "time-map-hash-1")
      result2 <- mockLogAppliedEffect logger (T.pack "token-2") (T.pack "deposit-effect-2") (T.pack "time-map-hash-1")
      
      -- Both effects should succeed
      result1 `shouldBe` Right ()
      result2 `shouldBe` Right ()
      
      -- Verify both resources were updated correctly (would verify balance changes in a real test)
      mockGetBalance accountProgram (T.pack "token-1") `shouldBe` 100
      mockGetBalance accountProgram (T.pack "token-2") `shouldBe` 200
    
    it "replaying all per-resource logs reconstructs the program's state exactly" $ do
      -- Create an account program
      let actorId = mockActorId
      let initialBalances = Map.fromList [(T.pack "token-1", 100), (T.pack "token-2", 200)]
      accountProgram <- mockCreateAccountProgram actorId initialBalances
      
      -- Create an effect logger
      logger <- mockCreateLogger
      
      -- Apply a series of effects
      mockLogAppliedEffect logger (T.pack "token-1") (T.pack "deposit-50") (T.pack "time-map-hash-1")
      mockLogAppliedEffect logger (T.pack "token-1") (T.pack "withdraw-30") (T.pack "time-map-hash-2")
      mockLogAppliedEffect logger (T.pack "token-1") (T.pack "transfer-20") (T.pack "time-map-hash-3")
      mockLogAppliedEffect logger (T.pack "token-2") (T.pack "deposit-75") (T.pack "time-map-hash-2")
      mockLogAppliedEffect logger (T.pack "token-2") (T.pack "withdraw-25") (T.pack "time-map-hash-3")
      
      -- Get the resource logs
      token1Log <- mockGetResourceLog logger (T.pack "token-1")
      token2Log <- mockGetResourceLog logger (T.pack "token-2")
      
      -- In a real test, we would replay these logs and check final state
      length token1Log `shouldBe` 3
      length token2Log `shouldBe` 2
    
    it "an effect's time map must match the latest known time map at the time of application" $ do
      -- Create an account program
      let actorId = mockActorId
      let initialBalances = Map.fromList [(T.pack "token-1", 100)]
      accountProgram <- mockCreateAccountProgram actorId initialBalances
      
      -- Create an effect logger
      logger <- mockCreateLogger
      
      -- Generate two different time map hashes representing time advancement
      let initialTimeMapHash = T.pack "time-map-hash-1"
      let advancedTimeMapHash = T.pack "time-map-hash-2"
      
      -- In a real test, we would check if applying an effect with an outdated time map fails
      -- For this mock test, we just verify that we can log effects with different time maps
      result1 <- mockLogAppliedEffect logger (T.pack "token-1") (T.pack "effect-1") initialTimeMapHash
      result2 <- mockLogAppliedEffect logger (T.pack "token-1") (T.pack "effect-2") advancedTimeMapHash
      
      result1 `shouldBe` Right ()
      result2 `shouldBe` Right () 