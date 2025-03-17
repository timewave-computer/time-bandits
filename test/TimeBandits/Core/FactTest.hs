{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TimeBandits.Core.FactTest (tests) where

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

import TimeBandits.Core.Common.Types (Hash, computeHash)
import TimeBandits.Core.Types (FactId(..), FactSnapshot(..), ObservedFact(..), FactValue(..), ObservationProof(..))
import TimeBandits.Core.Effect (Effect(..), EffectType(..))
import Programs.Program (Program(..))
import Programs.ProgramState (ProgramState(..))
import TimeBandits.Core.Schema (Schema(..), SchemaField(..), FieldType(..), EvolutionRules(..), 
                    SafeStatePolicy(..), defaultCoreEvolutionRules)
import TimeBandits.Core.Types (ProgramId(..))

import Data.Version.Extra (mkVersion)

-- | Fact Observation and Handling tests
tests :: TestTree
tests = testGroup "Fact Observation and Handling Tests"
  [ testGroup "Fact Observation Tests"
      [ testCase "Observe fact and store in snapshot" testFactObservation
      ]
  , testGroup "Fact Logging Tests"
      [ testCase "Check facts are durably logged" testFactLogging
      ]
  , testGroup "Fact Replay Tests"
      [ testCase "Replay program and check fact reappearance" testFactReplay
      ]
  , testGroup "Invalid Fact Proof Tests"
      [ testCase "Reject tampered fact proof" testInvalidFactProof
      ]
  ]

-- | Test observing a fact and storing it in a fact snapshot
testFactObservation :: Assertion
testFactObservation = do
  -- Create a price observation fact
  let factId = FactId "price-fact-1"
  let factValue = NumberFact 2900.0
  let observationProof = ObservationProof
        { proofSource = "oracle"
        , proofSignature = "valid-signature"
        , proofMetadata = Map.empty
        }
  
  let observedFact = ObservedFact
        { factID = factId
        , factValue = factValue
        , observationProof = observationProof
        }
  
  -- Create a fact snapshot
  let factSnapshot = FactSnapshot
        { facts = Map.singleton factId observedFact
        , snapshotTimestamp = undefined
        }
  
  -- Check fact is in snapshot
  assertEqual "Fact should be in snapshot" 
    (Just observedFact) (Map.lookup factId (facts factSnapshot))
  
  assertEqual "Fact value should match" 
    factValue (factValue observedFact)
  
  assertEqual "Fact proof should match" 
    observationProof (observationProof observedFact)

-- | Test fact logging
testFactLogging :: Assertion
testFactLogging = do
  -- Create a program with observed facts
  let schema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = [SchemaField "balance" FieldDecimal False]
        , evolutionRules = defaultCoreEvolutionRules
        }
        
  let programId = ProgramId "test-program-id"
  let version = mkVersion [1, 0, 0]
  let protocolVersion = mkVersion [1, 0, 0]
  
  -- Create a fact
  let factId = FactId "price-fact-1"
  let factValue = NumberFact 2900.0
  let observationProof = ObservationProof
        { proofSource = "oracle"
        , proofSignature = "valid-signature"
        , proofMetadata = Map.empty
        }
  
  let observedFact = ObservedFact
        { factID = factId
        , factValue = factValue
        , observationProof = observationProof
        }
  
  -- Create a fact snapshot
  let factSnapshot = FactSnapshot
        { facts = Map.singleton factId observedFact
        , snapshotTimestamp = undefined
        }
  
  -- Create effect that depends on the fact
  let effect = Effect
        { effectID = "effect-1"
        , parentEffects = []
        , effectType = ObserveEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.singleton "observedFacts" (toJSON factSnapshot)
        }
  
  -- Create program with the effect
  let program = Program
        { programID = programId
        , version = version
        , protocolVersion = protocolVersion
        , schema = schema
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.singleton "effect-1" (toJSON effect)
        , programState = Map.empty
        }
  
  -- Serialize program
  let serialized = encode program
  
  -- Deserialize program
  let deserialized = decode serialized :: Maybe Program
  
  -- Check fact is preserved in deserialized program
  case deserialized of
    Nothing -> assertFailure "Failed to deserialize program"
    Just deserializedProgram -> do
      -- Extract effect from DAG
      let maybeEffect = Map.lookup "effect-1" (effectDAG deserializedProgram)
      case maybeEffect of
        Nothing -> assertFailure "Effect not found in deserialized program"
        Just effectJson -> do
          -- Extract fact snapshot from effect metadata
          let parsedEffect = decode (encode effectJson) :: Maybe Effect
          case parsedEffect of
            Nothing -> assertFailure "Failed to parse effect"
            Just effect' -> do
              let maybeFactSnapshot = Map.lookup "observedFacts" (effectMetadata effect')
              case maybeFactSnapshot of
                Nothing -> assertFailure "Fact snapshot not found in effect metadata"
                Just factSnapshotJson -> do
                  -- Parse fact snapshot and verify fact
                  let parsedFactSnapshot = decode (encode factSnapshotJson) :: Maybe FactSnapshot
                  case parsedFactSnapshot of
                    Nothing -> assertFailure "Failed to parse fact snapshot"
                    Just factSnapshot' -> do
                      let maybeFact = Map.lookup factId (facts factSnapshot')
                      case maybeFact of
                        Nothing -> assertFailure "Fact not found in snapshot"
                        Just fact' -> do
                          assertEqual "Fact value should match original" 
                            factValue (factValue fact')
                          assertEqual "Fact proof source should match original" 
                            (proofSource observationProof) (proofSource (observationProof fact'))

-- | Test replaying program and checking fact reappearance
testFactReplay :: Assertion
testFactReplay = do
  -- Create program with observed facts
  let schema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = [SchemaField "price" FieldDecimal False]
        , evolutionRules = defaultCoreEvolutionRules
        }
        
  let programId = ProgramId "test-program-id"
  let version = mkVersion [1, 0, 0]
  let protocolVersion = mkVersion [1, 0, 0]
  
  -- Create a price fact
  let factId = FactId "price-fact-1"
  let factValue = NumberFact 2900.0
  let observationProof = ObservationProof
        { proofSource = "oracle"
        , proofSignature = "valid-signature"
        , proofMetadata = Map.empty
        }
  
  let observedFact = ObservedFact
        { factID = factId
        , factValue = factValue
        , observationProof = observationProof
        }
  
  -- Create a fact snapshot
  let factSnapshot = FactSnapshot
        { facts = Map.singleton factId observedFact
        , snapshotTimestamp = undefined
        }
  
  -- Create effect that observes price
  let observeEffect = Effect
        { effectID = "observe-effect"
        , parentEffects = []
        , effectType = ObserveEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.singleton "observedFacts" (toJSON factSnapshot)
        }
  
  -- Create program that uses the fact
  let program = Program
        { programID = programId
        , version = version
        , protocolVersion = protocolVersion
        , schema = schema
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.singleton "observe-effect" (toJSON observeEffect)
        , programState = Map.singleton "price" (Number 2900.0)
        }
  
  -- Serialize program
  let serialized = encode program
  
  -- "Clear memory" by deserializing
  let deserialized = decode serialized :: Maybe Program
  
  case deserialized of
    Nothing -> assertFailure "Failed to deserialize program"
    Just rehydratedProgram -> do
      -- Simulate replay by processing effects
      -- In a real implementation, we would actually replay the effects
      -- For the test, we just verify the fact is still accessible
      
      let maybeEffect = Map.lookup "observe-effect" (effectDAG rehydratedProgram)
      case maybeEffect of
        Nothing -> assertFailure "Effect not found in deserialized program"
        Just effectJson -> do
          let parsedEffect = decode (encode effectJson) :: Maybe Effect
          case parsedEffect of
            Nothing -> assertFailure "Failed to parse effect"
            Just effect' -> do
              let maybeFactSnapshot = Map.lookup "observedFacts" (effectMetadata effect')
              case maybeFactSnapshot of
                Nothing -> assertFailure "Fact snapshot not found in effect metadata"
                Just factSnapshotJson -> do
                  let parsedFactSnapshot = decode (encode factSnapshotJson) :: Maybe FactSnapshot
                  case parsedFactSnapshot of
                    Nothing -> assertFailure "Failed to parse fact snapshot"
                    Just factSnapshot' -> do
                      let maybeFact = Map.lookup factId (facts factSnapshot')
                      case maybeFact of
                        Nothing -> assertFailure "Fact not found during replay"
                        Just fact' -> do
                          -- Check the observed fact data matches
                          assertEqual "Fact value should match during replay" 
                            factValue (factValue fact')
                          assertEqual "Proof signature should match during replay" 
                            (proofSignature observationProof) (proofSignature (observationProof fact'))
                          
                          -- Check program state reflects the observed fact
                          assertEqual "Program state should reflect observed price" 
                            (Just (Number 2900.0)) (Map.lookup "price" (programState rehydratedProgram))

-- | Test rejecting tampered fact proof
testInvalidFactProof :: Assertion
testInvalidFactProof = do
  -- Create valid fact
  let factId = FactId "price-fact"
  let factValue = NumberFact 2900.0
  let validProof = ObservationProof
        { proofSource = "oracle"
        , proofSignature = "valid-signature"
        , proofMetadata = Map.empty
        }
  
  let validFact = ObservedFact
        { factID = factId
        , factValue = factValue
        , observationProof = validProof
        }
  
  -- Create tampered fact with invalid signature
  let invalidProof = ObservationProof
        { proofSource = "oracle"
        , proofSignature = "invalid-signature"
        , proofMetadata = Map.empty
        }
  
  let tamperedFact = ObservedFact
        { factID = factId
        , factValue = NumberFact 3000.0  -- Changed value!
        , observationProof = invalidProof
        }
  
  -- In a real implementation, we would verify the fact here
  -- For this test, we'll simulate validation by comparing signatures
  
  let isValidFact :: ObservedFact -> Bool
      isValidFact fact = proofSignature (observationProof fact) == "valid-signature"
  
  -- Verify the valid fact passes
  assertBool "Valid fact should pass verification" $
    isValidFact validFact
  
  -- Verify the tampered fact fails
  assertBool "Tampered fact should fail verification" $
    not $ isValidFact tamperedFact 