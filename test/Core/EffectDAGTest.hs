{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core.EffectDAGTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Aeson (Value(..), Object, encode, decode, toJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromJust)

import Core.Common (Hash, computeHash)
import Core.Effect (Effect(..), EffectType(..), EffectID)
import Core.Types (topoSort)

-- | Effect DAG tests
tests :: TestTree
tests = testGroup "Effect DAG Tests"
  [ testGroup "Effect Linking Tests"
      [ testCase "Link effects with parent-child relationships" testEffectLinking
      ]
  , testGroup "Content Addressing Tests"
      [ testCase "Ensure effect hashes match across serialization" testContentAddressing
      ]
  , testGroup "Multiple Parent Effects Tests"
      [ testCase "Apply effects with multiple parents" testMultipleParents
      ]
  , testGroup "DAG Traversal Tests"
      [ testCase "Walk DAG from root to tip" testDAGTraversal
      ]
  ]

-- | Test linking effects with explicit parent-child relationships
testEffectLinking :: Assertion
testEffectLinking = do
  -- Create root effect
  let rootEffect = Effect
        { effectID = "effect-1"
        , parentEffects = []
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  -- Create child effect that depends on root
  let childEffect = Effect
        { effectID = "effect-2"
        , parentEffects = ["effect-1"]
        , effectType = WithdrawEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  -- Create grandchild effect that depends on child
  let grandchildEffect = Effect
        { effectID = "effect-3"
        , parentEffects = ["effect-2"]
        , effectType = TransferEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  -- Build effect DAG
  let effectDAG = Map.fromList
        [ ("effect-1", toJSON rootEffect)
        , ("effect-2", toJSON childEffect)
        , ("effect-3", toJSON grandchildEffect)
        ]
  
  -- Check causal links
  assertEqual "Root effect should have no parents" 
    [] (parentEffects rootEffect)
    
  assertEqual "Child effect should have root as parent" 
    ["effect-1"] (parentEffects childEffect)
    
  assertEqual "Grandchild effect should have child as parent" 
    ["effect-2"] (parentEffects grandchildEffect)
  
  -- Validate full causal chain
  let childParents = parentEffects childEffect
  assertBool "Child's parent should be in DAG" 
    (all (`Map.member` effectDAG) childParents)
    
  let grandchildParents = parentEffects grandchildEffect
  assertBool "Grandchild's parent should be in DAG" 
    (all (`Map.member` effectDAG) grandchildParents)

-- | Test content addressing of effects
testContentAddressing :: Assertion
testContentAddressing = do
  -- Create an effect
  let effect = Effect
        { effectID = "effect-1"
        , parentEffects = []
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  -- Serialize and compute hash
  let serialized = encode effect
  let hash1 = computeHash (BS.toStrict serialized)
  
  -- Deserialize and compute hash again
  let deserialized = decode serialized :: Maybe Effect
  case deserialized of
    Nothing -> assertFailure "Failed to deserialize effect"
    Just deserializedEffect -> do
      let serialized2 = encode deserializedEffect
      let hash2 = computeHash (BS.toStrict serialized2)
      
      -- Assert hashes match
      assertEqual "Hashes should match across serialization/deserialization" 
        hash1 hash2

-- | Test effects with multiple parents
testMultipleParents :: Assertion
testMultipleParents = do
  -- Create two independent root effects
  let rootEffect1 = Effect
        { effectID = "effect-1"
        , parentEffects = []
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  let rootEffect2 = Effect
        { effectID = "effect-2"
        , parentEffects = []
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  -- Create effect that depends on both roots (fork resolution)
  let mergeEffect = Effect
        { effectID = "effect-3"
        , parentEffects = ["effect-1", "effect-2"]
        , effectType = TransferEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  -- Build effect DAG
  let effectDAG = Map.fromList
        [ ("effect-1", toJSON rootEffect1)
        , ("effect-2", toJSON rootEffect2)
        , ("effect-3", toJSON mergeEffect)
        ]
  
  -- Check merge effect has both parents
  assertEqual "Merge effect should have two parents" 
    2 (length (parentEffects mergeEffect))
    
  assertBool "Merge effect should have root1 as parent" 
    ("effect-1" `elem` parentEffects mergeEffect)
    
  assertBool "Merge effect should have root2 as parent" 
    ("effect-2" `elem` parentEffects mergeEffect)
  
  -- Check topological ordering would process both parents before merge
  let order = topoSort effectDAG
  
  -- In a valid topological sort, parents come before children
  assertBool "Root1 should come before merge in topological order" $
    findPosition "effect-1" order < findPosition "effect-3" order
    
  assertBool "Root2 should come before merge in topological order" $
    findPosition "effect-2" order < findPosition "effect-3" order
  where
    findPosition :: Text -> [Text] -> Int
    findPosition key = maybe (-1) id . findIndex (== key)
    
    findIndex :: (a -> Bool) -> [a] -> Maybe Int
    findIndex p = go 0
      where
        go _ [] = Nothing
        go i (x:xs)
          | p x = Just i
          | otherwise = go (i+1) xs

-- | Test DAG traversal
testDAGTraversal :: Assertion
testDAGTraversal = do
  -- Create a DAG with several paths
  --        root
  --      /     \
  --   child1   child2
  --     |        \
  --   child3     |
  --     \       /
  --      \    /
  --       merge
  
  let rootEffect = Effect
        { effectID = "root"
        , parentEffects = []
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  let child1Effect = Effect
        { effectID = "child1"
        , parentEffects = ["root"]
        , effectType = TransferEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  let child2Effect = Effect
        { effectID = "child2"
        , parentEffects = ["root"]
        , effectType = TransferEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  let child3Effect = Effect
        { effectID = "child3"
        , parentEffects = ["child1"]
        , effectType = WithdrawEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  let mergeEffect = Effect
        { effectID = "merge"
        , parentEffects = ["child3", "child2"]
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  -- Build effect DAG
  let effectDAG = Map.fromList
        [ ("root", toJSON rootEffect)
        , ("child1", toJSON child1Effect)
        , ("child2", toJSON child2Effect)
        , ("child3", toJSON child3Effect)
        , ("merge", toJSON mergeEffect)
        ]
  
  -- Test DAG traversal
  -- In a valid topological sort:
  -- 1. root must come before child1 and child2
  -- 2. child1 must come before child3
  -- 3. child2 and child3 must come before merge
  
  let order = topoSort effectDAG
  
  -- Check full traversal includes all effects
  assertEqual "Topological sort should include all effects" 
    5 (length order)
  
  -- Check every effect is visited
  let effectSet = Set.fromList order
  assertEqual "All effects should be visited" 
    5 (Set.size effectSet)
  
  -- Check causal order is preserved
  assertBool "Root should come before child1" $
    findPosition "root" order < findPosition "child1" order
    
  assertBool "Root should come before child2" $
    findPosition "root" order < findPosition "child2" order
    
  assertBool "Child1 should come before child3" $
    findPosition "child1" order < findPosition "child3" order
    
  assertBool "Child3 should come before merge" $
    findPosition "child3" order < findPosition "merge" order
    
  assertBool "Child2 should come before merge" $
    findPosition "child2" order < findPosition "merge" order
  where
    findPosition :: Text -> [Text] -> Int
    findPosition key = maybe (-1) id . findIndex (== key)
    
    findIndex :: (a -> Bool) -> [a] -> Maybe Int
    findIndex p = go 0
      where
        go _ [] = Nothing
        go i (x:xs)
          | p x = Just i
          | otherwise = go (i+1) xs 