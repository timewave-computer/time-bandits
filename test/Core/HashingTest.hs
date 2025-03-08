{-# LANGUAGE OverloadedStrings #-}

module Core.HashingTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Control.Monad (forM_, replicateM)
import Data.List (sort)

import Core.Hashing

-- | Main test group
tests :: TestTree
tests = testGroup "Hashing Tests"
  [ testCase "Compute hash" testComputeHash
  , testCase "Compute node score" testComputeNodeScore
  , testCase "Rendezvous hashing" testRendezvousHashing
  , testCase "Find responsible node" testFindResponsibleNode
  , testCase "Deterministic node selection" testDeterministicNodeSelection
  ]

-- | Test the basic hash function
testComputeHash :: Assertion
testComputeHash = do
  -- Test with some sample inputs
  let input1 = C8.pack "test-input-1"
      input2 = C8.pack "test-input-2"
      
      hash1 = computeHash input1
      hash2 = computeHash input2
  
  -- Verify hashes are not empty
  assertBool "Hash should not be empty" (not $ BS.null hash1)
  assertBool "Hash should not be empty" (not $ BS.null hash2)
  
  -- Verify different inputs produce different hashes
  hash1 /= hash2 @? "Different inputs should produce different hashes"
  
  -- Verify same input produces same hash
  computeHash input1 @?= hash1

-- | Test computing node scores
testComputeNodeScore :: Assertion
testComputeNodeScore = do
  -- Create some test inputs
  let key = C8.pack "test-key"
      nodeId1 = C8.pack "node-1"
      nodeId2 = C8.pack "node-2"
  
  -- Compute scores
  let score1 = computeNodeScore key nodeId1
      score2 = computeNodeScore key nodeId2
  
  -- Verify scores are in range [0,1]
  assertBool "Score should be between 0 and 1" (score1 >= 0 && score1 <= 1)
  assertBool "Score should be between 0 and 1" (score2 >= 0 && score2 <= 1)
  
  -- Verify different nodes get different scores
  score1 /= score2 @? "Different nodes should get different scores"
  
  -- Verify same inputs produce same score
  computeNodeScore key nodeId1 @?= score1

-- | Test rendezvous hashing
testRendezvousHashing :: Assertion
testRendezvousHashing = do
  -- Create test data
  let key = C8.pack "test-key"
      nodes = map (C8.pack . ("node-" ++)) ["a", "b", "c", "d", "e"]
  
  -- Compute rendezvous hash
  let result = computeRendezvousHash key nodes
  
  -- Verify we got a result
  case result of
    Nothing -> assertFailure "Should return a node"
    Just selected -> do
      -- Selected node should be in the original list
      assertBool "Selected node should be in the original list" (selected `elem` nodes)
      
      -- Same key and nodes should give same result
      computeRendezvousHash key nodes @?= result

-- | Test finding responsible node
testFindResponsibleNode :: Assertion
testFindResponsibleNode = do
  -- Create test data
  let key = C8.pack "resource-key"
      nodes = map (C8.pack . ("node-" ++)) ["1", "2", "3", "4", "5"]
  
  -- Find responsible node
  let result = findResponsibleNode key nodes
  
  -- Verify we got a result
  case result of
    Nothing -> assertFailure "Should return a node"
    Just selected -> do
      -- Selected node should be in the original list
      assertBool "Selected node should be in the original list" (selected `elem` nodes)
      
      -- Same key and nodes should give same result
      findResponsibleNode key nodes @?= result
      
      -- Different keys should likely give different results
      let key2 = C8.pack "different-resource"
          result2 = findResponsibleNode key2 nodes
      
      -- Note: This could theoretically fail with a small probability
      result /= result2 @? "Different keys should usually select different nodes"

-- | Test deterministic node selection
testDeterministicNodeSelection :: Assertion
testDeterministicNodeSelection = do
  -- Create multiple keys
  let keys = map (C8.pack . ("key-" ++)) (map show [1..100])
      nodes = map (C8.pack . ("node-" ++)) ["a", "b", "c", "d", "e"]
  
  -- Find responsible node for each key
  let results = map (\k -> findResponsibleNode k nodes) keys
      validResults = [r | Just r <- results]
  
  -- All keys should map to a node
  length validResults @?= length keys
  
  -- All nodes should be used (good distribution)
  let nodeOccurrences = map (\n -> (n, countOccurrences n validResults)) nodes
      countsOnly = map snd nodeOccurrences
  
  -- Print distribution for inspection
  putStrLn $ "Node selection distribution: " ++ show nodeOccurrences
  
  -- Check that all nodes are used
  all (> 0) countsOnly @? "All nodes should be used"
  
  -- Verify node selection is stable (adding a node only affects some keys)
  let newNodes = nodes ++ [C8.pack "node-f"]
      newResults = map (\k -> findResponsibleNode k newNodes) keys
      validNewResults = [r | Just r <- newResults]
      
      -- Count how many keys were reassigned
      reassigned = length $ filter id $ zipWith (/=) validResults validNewResults
      
      -- Calculate the percentage of keys that were reassigned
      reassignedPercentage = (fromIntegral reassigned / fromIntegral (length keys)) * 100
  
  putStrLn $ "Reassigned keys: " ++ show reassigned ++ "/" ++ show (length keys) 
           ++ " (" ++ show (round reassignedPercentage :: Int) ++ "%)"
  
  -- Typically, adding 1 node to 5 should reassign about 1/6 of keys
  -- Allow a reasonable margin of error
  reassignedPercentage > 10 && reassignedPercentage < 30 
    @? "Reassignment percentage should be reasonable (roughly 1/6)"

-- | Count occurrences of an element in a list
countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences x = length . filter (== x) 