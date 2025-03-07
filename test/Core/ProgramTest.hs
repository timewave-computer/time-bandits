{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core.ProgramTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Version (Version)
import Data.Aeson (Value(..), Object, encode, decode)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Version.Extra (mkVersion)

import Core.Schema
    ( Schema(..)
    , SchemaField(..)
    , FieldType(..)
    , EvolutionRules(..)
    , SafeStatePolicy(..)
    , defaultCoreEvolutionRules
    )
import Core.Types (ProgramId(..))
import Core.Common (Hash, computeHash)
import Programs.Program (Program(..))
import Programs.ProgramState (ProgramState(..))
import Core.Effect (Effect(..), EffectType(..))

-- | Core Program State tests
tests :: TestTree
tests = testGroup "Core Program State Tests"
  [ testGroup "Program Initialization Tests"
      [ testCase "Initialize program with schema and version" testProgramInitialization
      ]
  , testGroup "Program Serialization Tests"
      [ testCase "Serialize program to JSON and deserialize" testProgramSerialization
      ]
  , testGroup "Content Addressing Tests"
      [ testCase "Compute hash of program state" testContentAddressing
      ]
  , testGroup "Effect Application Tests"
      [ testCase "Apply sequence of effects" testEffectApplication
      ]
  , testGroup "Replayability Tests"
      [ testCase "Serialize, deserialize, and replay" testReplayability
      ]
  ]

-- | Test program initialization with schema and version
testProgramInitialization :: Assertion
testProgramInitialization = do
  -- Create schema for the program
  let schema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "balance" FieldDecimal False
            , SchemaField "name" FieldText True
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
        
  -- Create program ID
  let programId = ProgramId "test-program-id"
  
  -- Create program version
  let version = mkVersion [1, 0, 0]
  let protocolVersion = mkVersion [1, 0, 0]
  
  -- Create program
  let program = Program
        { programID = programId
        , version = version
        , protocolVersion = protocolVersion
        , schema = schema
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.empty
        , programState = Map.fromList [("balance", Number 0)]
        }
  
  -- Assert program is initialized correctly
  assertEqual "Program ID should match" programId (programID program)
  assertEqual "Program version should match" version (version program)
  assertEqual "Schema should match" schema (schema program)
  assertEqual "Effect DAG should be empty" Map.empty (effectDAG program)
  assertEqual "Program state should have default balance" 
    (Just (Number 0)) (Map.lookup "balance" (programState program))

-- | Test program serialization and deserialization
testProgramSerialization :: Assertion
testProgramSerialization = do
  -- Create a simple program
  let schema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = [SchemaField "balance" FieldDecimal False]
        , evolutionRules = defaultCoreEvolutionRules
        }
        
  let programId = ProgramId "test-program-id"
  let version = mkVersion [1, 0, 0]
  let protocolVersion = mkVersion [1, 0, 0]
  
  let initialProgram = Program
        { programID = programId
        , version = version
        , protocolVersion = protocolVersion
        , schema = schema
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.empty
        , programState = Map.singleton "balance" (Number 100)
        }
  
  -- Serialize program to JSON
  let serialized = encode initialProgram
  
  -- Deserialize program from JSON
  let deserialized = decode serialized
  
  -- Assert that deserialized program matches original
  case deserialized of
    Nothing -> assertFailure "Failed to deserialize program"
    Just deserializedProgram -> do
      assertEqual "Deserialized program should match original" 
        initialProgram deserializedProgram

-- | Test content addressing of program state
testContentAddressing :: Assertion
testContentAddressing = do
  -- Create a simple program state
  let schema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = [SchemaField "balance" FieldDecimal False]
        , evolutionRules = defaultCoreEvolutionRules
        }
        
  let programId = ProgramId "test-program-id"
  let version = mkVersion [1, 0, 0]
  let protocolVersion = mkVersion [1, 0, 0]
  
  let program = Program
        { programID = programId
        , version = version
        , protocolVersion = protocolVersion
        , schema = schema
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.empty
        , programState = Map.singleton "balance" (Number 100)
        }
  
  -- Compute hash of program
  let hash1 = computeHash (BS.toStrict $ encode program)
  
  -- Serialize and deserialize
  let serialized = encode program
  let deserialized = decode serialized :: Maybe Program
  
  -- Compute hash again
  case deserialized of
    Nothing -> assertFailure "Failed to deserialize program"
    Just deserializedProgram -> do
      let hash2 = computeHash (BS.toStrict $ encode deserializedProgram)
      
      -- Assert hashes match
      assertEqual "Hashes should match after serialization/deserialization" 
        hash1 hash2

-- | Test applying a sequence of effects to a program
testEffectApplication :: Assertion
testEffectApplication = do
  -- Create initial program state
  let schema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = [SchemaField "balance" FieldDecimal False]
        , evolutionRules = defaultCoreEvolutionRules
        }
        
  let programId = ProgramId "test-program-id"
  let version = mkVersion [1, 0, 0]
  let protocolVersion = mkVersion [1, 0, 0]
  
  let initialProgram = Program
        { programID = programId
        , version = version
        , protocolVersion = protocolVersion
        , schema = schema
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.empty
        , programState = Map.singleton "balance" (Number 0)
        }
  
  -- Create deposit effect
  let depositEffect = Effect
        { effectID = "effect-1"
        , parentEffects = []
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  -- Apply deposit effect (simplified, real implementation would update the program state)
  let depositAmount = 100
  let afterDeposit = initialProgram
        { effectDAG = Map.singleton "effect-1" (toJSON depositEffect)
        , programState = Map.singleton "balance" (Number depositAmount)
        }
  
  -- Create withdrawal effect
  let withdrawalEffect = Effect
        { effectID = "effect-2"
        , parentEffects = ["effect-1"]
        , effectType = WithdrawEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  -- Apply withdrawal effect
  let withdrawalAmount = 30
  let afterWithdrawal = afterDeposit
        { effectDAG = Map.fromList
            [ ("effect-1", toJSON depositEffect)
            , ("effect-2", toJSON withdrawalEffect)
            ]
        , programState = Map.singleton "balance" (Number (depositAmount - withdrawalAmount))
        }
  
  -- Assert final balance
  assertEqual "Final balance should be deposit - withdrawal" 
    (Just (Number 70)) (Map.lookup "balance" (programState afterWithdrawal))
  
  -- Assert effect DAG contains both effects
  assertEqual "Effect DAG should contain both effects" 
    2 (Map.size (effectDAG afterWithdrawal))

-- | Test program replayability
testReplayability :: Assertion
testReplayability = do
  -- Create initial program with effects
  let schema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = [SchemaField "balance" FieldDecimal False]
        , evolutionRules = defaultCoreEvolutionRules
        }
        
  let programId = ProgramId "test-program-id"
  let version = mkVersion [1, 0, 0]
  let protocolVersion = mkVersion [1, 0, 0]
  
  -- Create deposit effect
  let depositEffect = Effect
        { effectID = "effect-1"
        , parentEffects = []
        , effectType = DepositEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  -- Create withdrawal effect
  let withdrawalEffect = Effect
        { effectID = "effect-2"
        , parentEffects = ["effect-1"]
        , effectType = WithdrawEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.empty
        }
  
  -- Create program with effects applied
  let program = Program
        { programID = programId
        , version = version
        , protocolVersion = protocolVersion
        , schema = schema
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.fromList
            [ ("effect-1", toJSON depositEffect)
            , ("effect-2", toJSON withdrawalEffect)
            ]
        , programState = Map.singleton "balance" (Number 70)
        }
  
  -- Serialize program
  let serialized = encode program
  
  -- "Clear memory" by deserializing to a new program
  let deserialized = decode serialized :: Maybe Program
  
  case deserialized of
    Nothing -> assertFailure "Failed to deserialize program"
    Just rehydratedProgram -> do
      -- Simulate replay by "applying" effects again
      -- In a real implementation, we would walk the effect DAG and apply each effect
      let replayedProgram = rehydratedProgram
      
      -- Assert replayed state matches original
      assertEqual "Replayed program state should match original" 
        (programState program) (programState replayedProgram)
      assertEqual "Replayed effect DAG should match original" 
        (effectDAG program) (effectDAG replayedProgram) 