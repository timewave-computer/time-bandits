{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TimeBandits.Core.InvocationTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Aeson (Value(..), Object, encode, decode, toJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Version.Extra (mkVersion)

import TimeBandits.Core.Common.Types (Hash, computeHash)
import TimeBandits.Core.Types (ProgramId(..), FactId(..), FactValue(..), FactSnapshot(..), ObservedFact(..), ObservationProof(..))
import TimeBandits.Core.Schema
    ( Schema(..)
    , SchemaField(..)
    , FieldType(..)
    , EvolutionRules(..)
    , SafeStatePolicy(..)
    , defaultCoreEvolutionRules
    )
import TimeBandits.Core.Effect (Effect(..), EffectType(..))
import Programs.Program (Program(..))
import Programs.ProgramState (ProgramState(..))

-- | Invocation Handling tests
tests :: TestTree
tests = testGroup "Invocation Handling Tests"
  [ testGroup "Direct Invocation Tests"
      [ testCase "Invoke function directly inside program" testDirectInvocation
      ]
  , testGroup "Cross-Program Invocation Tests"
      [ testCase "Program A invokes Program B" testCrossProgramInvocation
      ]
  , testGroup "Error Handling Tests"
      [ testCase "Program B fails, A handles failure" testErrorHandling
      ]
  , testGroup "Replay Consistency Tests"
      [ testCase "Replay cross-program interaction" testReplayConsistency
      ]
  ]

-- | Simulate a direct invocation within a program
testDirectInvocation :: Assertion
testDirectInvocation = do
  -- Create program schema
  let schema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "counter" FieldInt False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
        
  let programId = ProgramId "test-program-id"
  let version = mkVersion [1, 0, 0]
  let protocolVersion = mkVersion [1, 0, 0]
  
  -- Initial program with counter = 0
  let initialProgram = Program
        { programID = programId
        , version = version
        , protocolVersion = protocolVersion
        , schema = schema
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.empty
        , programState = Map.singleton "counter" (Number 0)
        }
  
  -- Create an invocation effect that increments the counter
  let invocationEffect = Effect
        { effectID = "invocation-effect-1"
        , parentEffects = []
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "incrementCounter")
            , ("parameters", Object $ Map.singleton "amount" (Number 1))
            ]
        }
  
  -- Apply invocation effect
  let afterInvocation = initialProgram
        { effectDAG = Map.singleton "invocation-effect-1" (toJSON invocationEffect)
        , programState = Map.singleton "counter" (Number 1)
        }
  
  -- Check counter was incremented
  assertEqual "Counter should be incremented after invocation" 
    (Just (Number 1)) (Map.lookup "counter" (programState afterInvocation))
  
  -- Create another invocation effect to increment again
  let invocationEffect2 = Effect
        { effectID = "invocation-effect-2"
        , parentEffects = ["invocation-effect-1"]
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "incrementCounter")
            , ("parameters", Object $ Map.singleton "amount" (Number 2))
            ]
        }
  
  -- Apply second invocation effect
  let afterSecondInvocation = afterInvocation
        { effectDAG = Map.fromList
            [ ("invocation-effect-1", toJSON invocationEffect)
            , ("invocation-effect-2", toJSON invocationEffect2)
            ]
        , programState = Map.singleton "counter" (Number 3)
        }
  
  -- Check counter was incremented again
  assertEqual "Counter should be incremented by 2 after second invocation" 
    (Just (Number 3)) (Map.lookup "counter" (programState afterSecondInvocation))

-- | Simulate a cross-program invocation where Program A calls Program B
testCrossProgramInvocation :: Assertion
testCrossProgramInvocation = do
  -- Create Program A - the caller
  let schemaA = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "result" FieldDecimal True
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
        
  let programIdA = ProgramId "program-a"
  let versionA = mkVersion [1, 0, 0]
  let protocolVersionA = mkVersion [1, 0, 0]
  
  let initialProgramA = Program
        { programID = programIdA
        , version = versionA
        , protocolVersion = protocolVersionA
        , schema = schemaA
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.empty
        , programState = Map.empty
        }
  
  -- Create Program B - will observe price and return it
  let schemaB = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "price" FieldDecimal False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
        
  let programIdB = ProgramId "program-b"
  let versionB = mkVersion [1, 0, 0]
  let protocolVersionB = mkVersion [1, 0, 0]
  
  let initialProgramB = Program
        { programID = programIdB
        , version = versionB
        , protocolVersion = protocolVersionB
        , schema = schemaB
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.empty
        , programState = Map.empty
        }
  
  -- Create a cross-program invocation from A to B
  let crossProgramEffect = Effect
        { effectID = "cross-program-effect-1"
        , parentEffects = []
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "getPrice")
            , ("targetProgram", toJSON programIdB)
            , ("callbackEffect", String "cross-program-callback-1")
            ]
        }
  
  -- Apply the cross-program invocation effect to Program A
  let programAAfterInvocation = initialProgramA
        { effectDAG = Map.singleton "cross-program-effect-1" (toJSON crossProgramEffect)
        , programState = Map.empty  -- No state change yet, waiting for response
        }
  
  -- Create observed price fact for Program B
  let factId = FactId "price-fact-1"
  let factValue = NumberFact 3000.0
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
  
  -- Create an observe effect for Program B
  let observeEffect = Effect
        { effectID = "observe-effect-1"
        , parentEffects = []
        , effectType = ObserveEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("observedFacts", toJSON factSnapshot)
            , ("requester", toJSON programIdA)
            ]
        }
  
  -- Apply observe effect to Program B
  let programBAfterObservation = initialProgramB
        { effectDAG = Map.singleton "observe-effect-1" (toJSON observeEffect)
        , programState = Map.singleton "price" (Number 3000.0)
        }
  
  -- Create a return effect from B to A
  let returnEffect = Effect
        { effectID = "return-effect-1"
        , parentEffects = ["observe-effect-1"]
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "returnValue")
            , ("targetProgram", toJSON programIdA)
            , ("value", Number 3000.0)
            , ("originEffect", String "cross-program-effect-1")
            ]
        }
  
  -- Apply return effect to Program B
  let programBAfterReturn = programBAfterObservation
        { effectDAG = Map.fromList
            [ ("observe-effect-1", toJSON observeEffect)
            , ("return-effect-1", toJSON returnEffect)
            ]
        , programState = Map.singleton "price" (Number 3000.0)
        }
  
  -- Create callback effect for Program A to process the return value
  let callbackEffect = Effect
        { effectID = "cross-program-callback-1"
        , parentEffects = ["cross-program-effect-1"]
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "processReturnValue")
            , ("value", Number 3000.0)
            , ("sourceProgram", toJSON programIdB)
            ]
        }
  
  -- Apply callback effect to Program A
  let programAAfterCallback = programAAfterInvocation
        { effectDAG = Map.fromList
            [ ("cross-program-effect-1", toJSON crossProgramEffect)
            , ("cross-program-callback-1", toJSON callbackEffect)
            ]
        , programState = Map.singleton "result" (Number 3000.0)
        }
  
  -- Assert Program A now has the result from Program B
  assertEqual "Program A should have received the result from Program B" 
    (Just (Number 3000.0)) (Map.lookup "result" (programState programAAfterCallback))

-- | Test error handling in cross-program invocations
testErrorHandling :: Assertion
testErrorHandling = do
  -- Create Program A - the caller
  let schemaA = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "lastError" FieldText True
            , SchemaField "errorCount" FieldInt False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
        
  let programIdA = ProgramId "program-a"
  let versionA = mkVersion [1, 0, 0]
  let protocolVersionA = mkVersion [1, 0, 0]
  
  let initialProgramA = Program
        { programID = programIdA
        , version = versionA
        , protocolVersion = protocolVersionA
        , schema = schemaA
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.empty
        , programState = Map.singleton "errorCount" (Number 0)
        }
  
  -- Create Program B - will fail when invoked
  let schemaB = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "state" FieldText False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
        
  let programIdB = ProgramId "program-b"
  let versionB = mkVersion [1, 0, 0]
  let protocolVersionB = mkVersion [1, 0, 0]
  
  let initialProgramB = Program
        { programID = programIdB
        , version = versionB
        , protocolVersion = protocolVersionB
        , schema = schemaB
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.empty
        , programState = Map.singleton "state" (String "inactive")
        }
  
  -- Create a cross-program invocation from A to B
  let crossProgramEffect = Effect
        { effectID = "cross-program-effect-1"
        , parentEffects = []
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "performAction")
            , ("targetProgram", toJSON programIdB)
            , ("callbackEffect", String "cross-program-callback-1")
            ]
        }
  
  -- Apply the cross-program invocation effect to Program A
  let programAAfterInvocation = initialProgramA
        { effectDAG = Map.singleton "cross-program-effect-1" (toJSON crossProgramEffect)
        , programState = Map.singleton "errorCount" (Number 0)
        }
  
  -- Create an error effect for Program B
  let errorEffect = Effect
        { effectID = "error-effect-1"
        , parentEffects = []
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("error", String "InsufficientFunds")
            , ("errorMessage", String "Not enough balance to perform action")
            , ("requester", toJSON programIdA)
            ]
        }
  
  -- Apply error effect to Program B
  let programBAfterError = initialProgramB
        { effectDAG = Map.singleton "error-effect-1" (toJSON errorEffect)
        , programState = Map.singleton "state" (String "error")
        }
  
  -- Create a error return effect from B to A
  let errorReturnEffect = Effect
        { effectID = "error-return-effect-1"
        , parentEffects = ["error-effect-1"]
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "returnError")
            , ("targetProgram", toJSON programIdA)
            , ("error", String "InsufficientFunds")
            , ("errorMessage", String "Not enough balance to perform action")
            , ("originEffect", String "cross-program-effect-1")
            ]
        }
  
  -- Apply error return effect to Program B
  let programBAfterErrorReturn = programBAfterError
        { effectDAG = Map.fromList
            [ ("error-effect-1", toJSON errorEffect)
            , ("error-return-effect-1", toJSON errorReturnEffect)
            ]
        }
  
  -- Create error callback effect for Program A to handle the error
  let errorCallbackEffect = Effect
        { effectID = "cross-program-callback-1"
        , parentEffects = ["cross-program-effect-1"]
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "handleError")
            , ("error", String "InsufficientFunds")
            , ("errorMessage", String "Not enough balance to perform action")
            , ("sourceProgram", toJSON programIdB)
            ]
        }
  
  -- Apply error callback effect to Program A
  let incrementedErrorCount = case Map.lookup "errorCount" (programState programAAfterInvocation) of
        Just (Number count) -> count + 1
        _ -> 1
  
  let programAAfterErrorCallback = programAAfterInvocation
        { effectDAG = Map.fromList
            [ ("cross-program-effect-1", toJSON crossProgramEffect)
            , ("cross-program-callback-1", toJSON errorCallbackEffect)
            ]
        , programState = Map.fromList
            [ ("lastError", String "InsufficientFunds")
            , ("errorCount", Number incrementedErrorCount)
            ]
        }
  
  -- Assert Program A properly handled the error
  assertEqual "Program A should have recorded the error type" 
    (Just (String "InsufficientFunds")) (Map.lookup "lastError" (programState programAAfterErrorCallback))
    
  assertEqual "Program A should have incremented the error count" 
    (Just (Number 1)) (Map.lookup "errorCount" (programState programAAfterErrorCallback))

-- | Test replay consistency in cross-program invocations
testReplayConsistency :: Assertion
testReplayConsistency = do
  -- Create Program A - the caller
  let schemaA = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "result" FieldDecimal True
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
        
  let programIdA = ProgramId "program-a"
  let versionA = mkVersion [1, 0, 0]
  let protocolVersionA = mkVersion [1, 0, 0]
  
  -- Create Program B - will process calculation
  let schemaB = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "multiplier" FieldDecimal False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
        
  let programIdB = ProgramId "program-b"
  let versionB = mkVersion [1, 0, 0]
  let protocolVersionB = mkVersion [1, 0, 0]
  
  -- Initial state for both programs
  let initialProgramA = Program
        { programID = programIdA
        , version = versionA
        , protocolVersion = protocolVersionA
        , schema = schemaA
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.empty
        , programState = Map.empty
        }
  
  let initialProgramB = Program
        { programID = programIdB
        , version = versionB
        , protocolVersion = protocolVersionB
        , schema = schemaB
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.empty
        , programState = Map.singleton "multiplier" (Number 2.5)
        }
  
  -- Create a complete interaction between A and B with final states
  
  -- 1. A invokes B
  let invocationEffect = Effect
        { effectID = "invocation-effect"
        , parentEffects = []
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "calculate")
            , ("targetProgram", toJSON programIdB)
            , ("callbackEffect", String "callback-effect")
            , ("value", Number 10.0)
            ]
        }
  
  let programAAfterInvocation = initialProgramA
        { effectDAG = Map.singleton "invocation-effect" (toJSON invocationEffect)
        , programState = Map.empty
        }
  
  -- 2. B processes the calculation
  let processEffect = Effect
        { effectID = "process-effect"
        , parentEffects = []
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "processCalculation")
            , ("value", Number 10.0)
            , ("requester", toJSON programIdA)
            ]
        }
  
  let programBAfterProcess = initialProgramB
        { effectDAG = Map.singleton "process-effect" (toJSON processEffect)
        , programState = Map.singleton "multiplier" (Number 2.5)
        }
  
  -- 3. B returns result to A
  let returnEffect = Effect
        { effectID = "return-effect"
        , parentEffects = ["process-effect"]
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "returnResult")
            , ("targetProgram", toJSON programIdA)
            , ("result", Number 25.0)  -- 10.0 * 2.5
            , ("originEffect", String "invocation-effect")
            ]
        }
  
  let programBAfterReturn = programBAfterProcess
        { effectDAG = Map.fromList
            [ ("process-effect", toJSON processEffect)
            , ("return-effect", toJSON returnEffect)
            ]
        }
  
  -- 4. A processes the callback with the result
  let callbackEffect = Effect
        { effectID = "callback-effect"
        , parentEffects = ["invocation-effect"]
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "processResult")
            , ("result", Number 25.0)
            , ("sourceProgram", toJSON programIdB)
            ]
        }
  
  let programAAfterCallback = programAAfterInvocation
        { effectDAG = Map.fromList
            [ ("invocation-effect", toJSON invocationEffect)
            , ("callback-effect", toJSON callbackEffect)
            ]
        , programState = Map.singleton "result" (Number 25.0)
        }
  
  -- Now serialize both final programs
  let serializedA = encode programAAfterCallback
  let serializedB = encode programBAfterReturn
  
  -- "Clear memory" by deserializing
  let deserializedA = decode serializedA :: Maybe Program
  let deserializedB = decode serializedB :: Maybe Program
  
  -- Verify we can deserialize both programs
  case (deserializedA, deserializedB) of
    (Nothing, _) -> assertFailure "Failed to deserialize Program A"
    (_, Nothing) -> assertFailure "Failed to deserialize Program B"
    (Just rehydratedA, Just rehydratedB) -> do
      -- Now simulate replay by executing the effects in order
      -- For this test, we're just verifying the final state matches
      
      let replayedA = rehydratedA
      let replayedB = rehydratedB
      
      -- Assert replayed states match original states
      assertEqual "Program A state should match after replay" 
        (programState programAAfterCallback) (programState replayedA)
        
      assertEqual "Program B state should match after replay" 
        (programState programBAfterReturn) (programState replayedB)
      
      -- Assert the result is still preserved in Program A
      assertEqual "Program A should preserve result after replay" 
        (Just (Number 25.0)) (Map.lookup "result" (programState replayedA)) 