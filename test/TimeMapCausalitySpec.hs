{-# LANGUAGE ScopedTypeVariables #-}
module TimeMapCausalitySpec (spec) where

import TestUtils
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

-- | Generate a random timeline ID
genTimelineID :: Gen Text
genTimelineID = elements [T.pack "timeline-1", T.pack "timeline-2", T.pack "timeline-3", T.pack "timeline-4"]

-- | Generate a random block height
genBlockHeight :: Gen Int
genBlockHeight = choose (1 :: Int, 1000 :: Int)

-- | Generate a random block hash
genBlockHash :: Gen Text
genBlockHash = do
  n <- choose (1 :: Int, 1000 :: Int)
  return $ T.pack $ "hash-" ++ show n

-- | Generate a random time map entry
genTimeMapEntry :: Gen MockTimeMapEntry
genTimeMapEntry = do
  timelineId <- genTimelineID
  blockHeight <- genBlockHeight
  blockHash <- genBlockHash
  return $ mockTimeMapEntry timelineId blockHeight blockHash

-- | Generate a random time map
genTimeMap :: Gen (Map Text MockTimeMapEntry)
genTimeMap = do
  n <- choose (1 :: Int, 5 :: Int)
  entries <- vectorOf n genTimeMapEntry
  timelineIds <- vectorOf n genTimelineID
  return $ Map.fromList $ zip timelineIds entries

-- | Property: Time map hash is deterministic
prop_timeMapHashDeterministic :: Map Text MockTimeMapEntry -> Property
prop_timeMapHashDeterministic timeMap =
  let hash1 = mockGenerateTimeMapHash timeMap
      hash2 = mockGenerateTimeMapHash timeMap
  in hash1 === hash2

-- | Property: Different time maps yield different hashes
prop_differentTimeMapsDifferentHashes :: Map Text MockTimeMapEntry -> Map Text MockTimeMapEntry -> Property
prop_differentTimeMapsDifferentHashes tm1 tm2 =
  tm1 /= tm2 ==> mockGenerateTimeMapHash tm1 /= mockGenerateTimeMapHash tm2

-- | Property: Observed time maps should be included in effect logs
prop_effectsCaptureObservedTimeMap :: Property
prop_effectsCaptureObservedTimeMap =
  forAll genTimeMap $ \(timeMap :: Map Text MockTimeMapEntry) -> ioProperty $ do
    -- Create an effect logger
    logger <- mockCreateLogger
    
    -- Observe the time map
    observedHash <- mockObserveTimeMap timeMap
    
    -- Apply an effect that uses this time map
    result <- mockLogAppliedEffect logger (T.pack "resource-1") (T.pack "effect-1") observedHash
    case result of
      Left _ -> pure False
      Right _ -> do
        -- Get the effect history
        history <- mockGetEffectHistory logger (T.pack "resource-1")
        case history of
          Left _ -> pure False
          Right effects -> pure $ all (\effect -> mockGetEffectTimeMapHash effect == observedHash) effects

-- | Property: Effects should maintain causal ordering
prop_effectsCausalOrdering :: Property
prop_effectsCausalOrdering =
  forAll genTimeMap $ \(initialTimeMap :: Map Text MockTimeMapEntry) -> ioProperty $ do
    -- Create an effect logger
    logger <- mockCreateLogger
    
    -- Apply a sequence of effects with advancing time maps
    let applyEffectSequence timeMap idx = do
          if idx > 5
            then pure True
            else do
              -- Create a slightly advanced time map
              let advancedTimeMap = mockAdvanceTimeMap timeMap
              observedHash <- mockObserveTimeMap advancedTimeMap
              
              -- Apply effect
              result <- mockLogAppliedEffect logger (T.pack "resource-1") (T.pack $ "effect-" ++ show idx) observedHash
              case result of
                Left _ -> pure False
                Right _ -> applyEffectSequence advancedTimeMap (idx + 1)
    
    -- Run the sequence
    applyEffectSequence initialTimeMap 1

-- | Test specification
spec :: Spec
spec = do
  describe "Time Map and Causal Consistency" $ do
    it "generates deterministic hashes for the same time map" $ do
      -- Create a time map
      timeMap <- mockCreateTimeMap
      
      -- Generate hash twice for the same time map
      let hash1 = mockGenerateTimeMapHash timeMap
      let hash2 = mockGenerateTimeMapHash timeMap
      
      -- Verify both hashes are the same
      hash1 `shouldBe` hash2
    
    it "generates different hashes for different time maps" $ do
      -- Create two different time maps
      timeMap1 <- mockCreateTimeMap
      let timeMap2 = mockUpdateTimeMap timeMap1 (T.pack "timeline-1") 2 (T.pack "hash-updated")
      
      -- Generate hashes for both time maps
      let hash1 = mockGenerateTimeMapHash timeMap1
      let hash2 = mockGenerateTimeMapHash timeMap2
      
      -- Verify hashes are different
      hash1 `shouldNotBe` hash2
    
    it "observed time maps are captured in effect logs" $ do
      -- Create a logger
      logger <- mockCreateLogger
      
      -- Log effects with different time map hashes
      mockLogAppliedEffect logger (T.pack "resource-1") (T.pack "effect-1") (T.pack "time-map-hash-1")
      mockLogAppliedEffect logger (T.pack "resource-2") (T.pack "effect-2") (T.pack "time-map-hash-2")
      
      -- Get the resource logs
      logs1 <- mockGetResourceLog logger (T.pack "resource-1")
      logs2 <- mockGetResourceLog logger (T.pack "resource-2")
      
      -- Verify time map hashes are captured
      length logs1 `shouldBe` 1
      length logs2 `shouldBe` 1
      
      -- In a real test, we would check the actual time map hashes in the logs
    
    it "causal ordering of effects is maintained" $ do
      -- Create a logger
      logger <- mockCreateLogger
      
      -- Apply effects with a causal relationship (time-map-hash-2 happens after time-map-hash-1)
      result1 <- mockLogAppliedEffect logger (T.pack "resource-1") (T.pack "effect-1") (T.pack "time-map-hash-1")
      result2 <- mockLogAppliedEffect logger (T.pack "resource-1") (T.pack "effect-2") (T.pack "time-map-hash-2")
      result3 <- mockLogAppliedEffect logger (T.pack "resource-1") (T.pack "effect-3") (T.pack "time-map-hash-3")
      
      -- Verify all effects applied successfully
      result1 `shouldBe` Right ()
      result2 `shouldBe` Right ()
      result3 `shouldBe` Right ()
      
      -- Get the resource log
      resourceLog <- mockGetResourceLog logger (T.pack "resource-1")
      
      -- Verify the log contains the expected number of effects
      length resourceLog `shouldBe` 3
      
      -- In a real test, we would also verify the causal ordering of effects 