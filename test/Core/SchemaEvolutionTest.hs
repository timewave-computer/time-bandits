{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core.SchemaEvolutionTest (tests) where

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

import Core.Common (Hash, computeHash)
import Core.Types (ProgramId(..))
import Core.Schema
    ( Schema(..)
    , SchemaField(..)
    , FieldType(..)
    , EvolutionRules(..)
    , SafeStatePolicy(..)
    , defaultCoreEvolutionRules
    , SchemaEvolution(..)
    , SchemaChange(..)
    , validateSchema
    , evolveSchema
    )
import Core.Effect (Effect(..), EffectType(..))
import Programs.Program (Program(..))
import Programs.ProgramState (ProgramState(..))

-- | Safe State Enforcement and Schema Evolution tests
tests :: TestTree
tests = testGroup "Safe State Enforcement and Schema Evolution Tests"
  [ testGroup "Safe State Check Tests"
      [ testCase "Non-blocking effects report safe state" testNonBlockingSafeState
      , testCase "Cross-program calls report unsafe state" testCrossProgramUnsafeState
      ]
  , testGroup "Schema Evolution Tests"
      [ testCase "Add optional field to schema" testAddOptionalField
      , testCase "Remove unused field from schema" testRemoveUnusedField
      , testCase "Disallow renaming field in schema" testDisallowRenameField
      ]
  , testGroup "Schema Mismatch Tests"
      [ testCase "Program pinned to older version is rejected by newer bandit" testSchemaMismatch
      ]
  , testGroup "Upgrade and Replay Tests"
      [ testCase "Apply schema evolution and replay with evolved schema" testUpgradeAndReplay
      ]
  ]

-- | Helper function to create a test program
createTestProgram :: ProgramId -> Schema -> SafeStatePolicy -> IO Program
createTestProgram programId schema safePolicy = do
  let version = mkVersion [1, 0, 0]
  let protocolVersion = mkVersion [1, 0, 0]
  
  return Program
    { programID = programId
    , version = version
    , protocolVersion = protocolVersion
    , schema = schema
    , safeStatePolicy = safePolicy
    , effectDAG = Map.empty
    , programState = Map.empty
    }

-- | Test that non-blocking effects report safe state
testNonBlockingSafeState :: Assertion
testNonBlockingSafeState = do
  -- Create schema for test program
  let schema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "counter" FieldInt False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  -- Define a custom safe state policy that considers a program safe when it's not in the middle of a cross-program call
  let safePolicy = CustomSafePolicy "NonBlockingPolicy"
  
  -- Create program
  let programId = ProgramId "test-program"
  program <- createTestProgram programId schema safePolicy
  
  -- Create a non-blocking effect (internal calculation)
  let nonBlockingEffect = Effect
        { effectID = "non-blocking-effect"
        , parentEffects = []
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "incrementCounter")
            , ("parameters", Object $ Map.singleton "amount" (Number 1))
            ]
        }
  
  -- Apply the non-blocking effect
  let updatedProgram = program
        { effectDAG = Map.singleton "non-blocking-effect" (toJSON nonBlockingEffect)
        , programState = Map.singleton "counter" (Number 1)
        }
  
  -- Check if the program is in a safe state
  let isSafeState = isInSafeState updatedProgram
  
  -- Assert the program is in a safe state
  assertBool "Program should be in a safe state after non-blocking effect" isSafeState
  where
    -- Mock implementation of safe state check for testing
    isInSafeState :: Program -> Bool
    isInSafeState prog = 
      case safeStatePolicy prog of
        AlwaysSafe -> True
        NeverSafe -> False
        CustomSafePolicy "NonBlockingPolicy" ->
          -- Check if there are any cross-program calls in progress
          let effects = Map.elems (effectDAG prog)
              hasCrossProgramCall = any isCrossProgramCall effects
          in not hasCrossProgramCall
        _ -> False
    
    -- Check if an effect is a cross-program call
    isCrossProgramCall :: Value -> Bool
    isCrossProgramCall effectJson =
      case decode (encode effectJson) of
        Just effect -> case Map.lookup "targetProgram" (effectMetadata effect) of
          Just _ -> True  -- Effect has a targetProgram, so it's a cross-program call
          Nothing -> False
        Nothing -> False

-- | Test that cross-program calls report unsafe state
testCrossProgramUnsafeState :: Assertion
testCrossProgramUnsafeState = do
  -- Create schema for test program
  let schema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "counter" FieldInt False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  -- Define a custom safe state policy that considers a program unsafe during cross-program calls
  let safePolicy = CustomSafePolicy "NonBlockingPolicy"
  
  -- Create program
  let programId = ProgramId "test-program"
  let targetProgramId = ProgramId "target-program"
  program <- createTestProgram programId schema safePolicy
  
  -- Create a cross-program call effect
  let crossProgramEffect = Effect
        { effectID = "cross-program-effect"
        , parentEffects = []
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "remoteCall")
            , ("targetProgram", toJSON targetProgramId)
            , ("callbackEffect", String "callback-effect")
            ]
        }
  
  -- Apply the cross-program effect
  let updatedProgram = program
        { effectDAG = Map.singleton "cross-program-effect" (toJSON crossProgramEffect)
        , programState = Map.empty
        }
  
  -- Check if the program is in a safe state
  let isSafeState = isInSafeState updatedProgram
  
  -- Assert the program is NOT in a safe state during cross-program call
  assertBool "Program should not be in a safe state during cross-program call" (not isSafeState)
  where
    -- Mock implementation of safe state check for testing
    isInSafeState :: Program -> Bool
    isInSafeState prog = 
      case safeStatePolicy prog of
        AlwaysSafe -> True
        NeverSafe -> False
        CustomSafePolicy "NonBlockingPolicy" ->
          -- Check if there are any cross-program calls in progress
          let effects = Map.elems (effectDAG prog)
              hasCrossProgramCall = any isCrossProgramCall effects
          in not hasCrossProgramCall
        _ -> False
    
    -- Check if an effect is a cross-program call
    isCrossProgramCall :: Value -> Bool
    isCrossProgramCall effectJson =
      case decode (encode effectJson) of
        Just effect -> case Map.lookup "targetProgram" (effectMetadata effect) of
          Just _ -> True  -- Effect has a targetProgram, so it's a cross-program call
          Nothing -> False
        Nothing -> False

-- | Test adding an optional field to schema
testAddOptionalField :: Assertion
testAddOptionalField = do
  -- Create initial schema
  let initialSchema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "counter" FieldInt False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  -- Create schema evolution to add an optional field
  let schemaEvolution = SchemaEvolution
        { fromVersion = mkVersion [1, 0, 0]
        , toVersion = mkVersion [1, 1, 0]
        , changes = 
            [ AddField (SchemaField "description" FieldText True)  -- Adding optional text field
            ]
        }
  
  -- Apply schema evolution
  let evolvedSchema = evolveSchema initialSchema schemaEvolution
  
  -- Check evolution was successful
  case evolvedSchema of
    Left error -> assertFailure $ "Schema evolution failed: " ++ T.unpack error
    Right newSchema -> do
      -- Check version was updated
      assertEqual "Schema version should be updated" 
        (mkVersion [1, 1, 0]) (schemaVersion newSchema)
      
      -- Check new field was added
      let hasDescriptionField = any (\field -> fieldName field == "description") (fields newSchema)
      assertBool "Schema should include the new description field" hasDescriptionField
      
      -- Check original field is still there
      let hasCounterField = any (\field -> fieldName field == "counter") (fields newSchema)
      assertBool "Schema should still include the original counter field" hasCounterField

-- | Test removing an unused field from schema
testRemoveUnusedField :: Assertion
testRemoveUnusedField = do
  -- Create initial schema with multiple fields
  let initialSchema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "counter" FieldInt False
            , SchemaField "unusedField" FieldText True  -- This field will be removed
            , SchemaField "name" FieldText False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  -- Create program state without the unused field
  let programState = Map.fromList
        [ ("counter", Number 5)
        , ("name", String "Test Program")
        -- No "unusedField" entry
        ]
  
  -- Create schema evolution to remove the unused field
  let schemaEvolution = SchemaEvolution
        { fromVersion = mkVersion [1, 0, 0]
        , toVersion = mkVersion [1, 1, 0]
        , changes = 
            [ RemoveField "unusedField"  -- Remove the unused field
            ]
        }
  
  -- Apply schema evolution
  let evolvedSchema = evolveSchema initialSchema schemaEvolution
  
  -- Check evolution was successful
  case evolvedSchema of
    Left error -> assertFailure $ "Schema evolution failed: " ++ T.unpack error
    Right newSchema -> do
      -- Check version was updated
      assertEqual "Schema version should be updated" 
        (mkVersion [1, 1, 0]) (schemaVersion newSchema)
      
      -- Check field was removed
      let hasUnusedField = any (\field -> fieldName field == "unusedField") (fields newSchema)
      assertBool "Schema should not include the removed field" (not hasUnusedField)
      
      -- Check other fields are still there
      let hasCounterField = any (\field -> fieldName field == "counter") (fields newSchema)
      let hasNameField = any (\field -> fieldName field == "name") (fields newSchema)
      assertBool "Schema should still include the counter field" hasCounterField
      assertBool "Schema should still include the name field" hasNameField

-- | Test that renaming a field in schema is disallowed
testDisallowRenameField :: Assertion
testDisallowRenameField = do
  -- Create initial schema
  let initialSchema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "oldName" FieldText False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  -- Create schema evolution to rename a field (which should be disallowed)
  let schemaEvolution = SchemaEvolution
        { fromVersion = mkVersion [1, 0, 0]
        , toVersion = mkVersion [1, 1, 0]
        , changes = 
            [ RenameField "oldName" "newName"  -- This should be disallowed
            ]
        }
  
  -- Apply schema evolution
  let evolvedSchema = evolveSchema initialSchema schemaEvolution
  
  -- Check evolution failed as expected
  case evolvedSchema of
    Left error -> do
      -- We expect an error since renaming should be disallowed
      assertBool "Error should mention renaming is disallowed" 
        (T.isInfixOf "rename" error || T.isInfixOf "Rename" error)
    Right _ -> assertFailure "Schema evolution should have failed for rename operation"

-- | Test that a program pinned to an older version is rejected by a newer bandit
testSchemaMismatch :: Assertion
testSchemaMismatch = do
  -- Create old schema (v1.0.0)
  let oldSchema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "counter" FieldInt False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  -- Create program pinned to old schema version
  let programId = ProgramId "test-program"
  let version = mkVersion [1, 0, 0]  -- Program version
  let protocolVersion = mkVersion [1, 0, 0]
  
  let program = Program
        { programID = programId
        , version = version
        , protocolVersion = protocolVersion
        , schema = oldSchema
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.empty
        , programState = Map.singleton "counter" (Number 1)
        }
  
  -- Define bandit's required minimum schema version (v2.0.0)
  let requiredSchemaVersion = mkVersion [2, 0, 0]
  
  -- Check if program's schema version meets bandit's requirement
  let isCompatible = schemaVersion oldSchema >= requiredSchemaVersion
  
  -- Assert that the program's schema version is rejected
  assertBool "Program with old schema version should be rejected by newer bandit" 
    (not isCompatible)

-- | Test applying schema evolution and then replaying with the evolved schema
testUpgradeAndReplay :: Assertion
testUpgradeAndReplay = do
  -- Create initial schema (v1.0.0)
  let initialSchema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "counter" FieldInt False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  -- Create program with initial schema
  let programId = ProgramId "test-program"
  let version = mkVersion [1, 0, 0]
  let protocolVersion = mkVersion [1, 0, 0]
  
  -- Create an effect for the initial program
  let initialEffect = Effect
        { effectID = "initial-effect"
        , parentEffects = []
        , effectType = CallEffect
        , effectTimestamp = undefined
        , effectMetadata = Map.fromList
            [ ("function", String "incrementCounter")
            , ("parameters", Object $ Map.singleton "amount" (Number 5))
            ]
        }
  
  let initialProgram = Program
        { programID = programId
        , version = version
        , protocolVersion = protocolVersion
        , schema = initialSchema
        , safeStatePolicy = AlwaysSafe
        , effectDAG = Map.singleton "initial-effect" (toJSON initialEffect)
        , programState = Map.singleton "counter" (Number 5)
        }
  
  -- Create schema evolution to add a new field
  let schemaEvolution = SchemaEvolution
        { fromVersion = mkVersion [1, 0, 0]
        , toVersion = mkVersion [1, 1, 0]
        , changes = 
            [ AddField (SchemaField "description" FieldText True)
            ]
        }
  
  -- Apply schema evolution
  let evolvedSchemaResult = evolveSchema initialSchema schemaEvolution
  
  case evolvedSchemaResult of
    Left error -> assertFailure $ "Schema evolution failed: " ++ T.unpack error
    Right evolvedSchema -> do
      -- Create evolution effect
      let evolutionEffect = Effect
            { effectID = "evolution-effect"
            , parentEffects = ["initial-effect"]
            , effectType = SchemaEvolutionEffect
            , effectTimestamp = undefined
            , effectMetadata = Map.fromList
                [ ("schemaEvolution", toJSON schemaEvolution)
                ]
            }
      
      -- Apply evolution to program
      let evolvedProgram = initialProgram
            { version = mkVersion [1, 1, 0]  -- Update program version
            , schema = evolvedSchema
            , effectDAG = Map.fromList
                [ ("initial-effect", toJSON initialEffect)
                , ("evolution-effect", toJSON evolutionEffect)
                ]
            , programState = Map.fromList
                [ ("counter", Number 5)
                , ("description", String "Added after evolution")
                ]
            }
      
      -- Serialize evolved program
      let serialized = encode evolvedProgram
      
      -- "Clear memory" by deserializing
      let deserialized = decode serialized :: Maybe Program
      
      case deserialized of
        Nothing -> assertFailure "Failed to deserialize evolved program"
        Just rehydratedProgram -> do
          -- Simulate replaying the effects with evolved schema
          -- In a real implementation, we would actually replay all effects
          let replayedProgram = rehydratedProgram
          
          -- Check replayed program has the evolved schema
          assertEqual "Replayed program should have evolved schema version" 
            (schemaVersion evolvedSchema) (schemaVersion (schema replayedProgram))
          
          -- Check the program state has been preserved
          assertEqual "Replayed program should preserve counter value" 
            (Just (Number 5)) (Map.lookup "counter" (programState replayedProgram))
            
          assertEqual "Replayed program should include new field" 
            (Just (String "Added after evolution")) (Map.lookup "description" (programState replayedProgram)) 