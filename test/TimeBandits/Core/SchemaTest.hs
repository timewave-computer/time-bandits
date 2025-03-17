{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TimeBandits.Core.SchemaTest (
  tests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Version (Version)
import Data.Aeson (Value(..), Object)
import Data.Version.Extra (mkVersion)

import TimeBandits.Core.Schema
  ( Schema(..)
  , SchemaField(..)
  , FieldType(..)
  , EvolutionRules(..)
  , EvolutionError(..)
  , EvolutionResult(..)
  , SafeStateStatus(..)
  , applySchemaEvolution
  , defaultCoreEvolutionRules
  , checkSchemaCompatibility
  )

tests :: TestTree
tests = testGroup "Schema Tests"
  [ testGroup "Schema Evolution Tests"
    [ testCase "Add optional field" testAddOptionalField
    , testCase "Add field with default" testAddFieldWithDefault
    , testCase "Remove unused field" testRemoveUnusedField
    , testCase "Reject field removal when not allowed" testRejectRemoval
    , testCase "Reject type change when not allowed" testRejectTypeChange
    ]
  , testGroup "Schema Compatibility Tests"
    [ testCase "Check protocol compatibility" testProtocolCompatibility
    ]
  ]

-- | Test adding an optional field to a schema
testAddOptionalField :: Assertion
testAddOptionalField = do
  let oldSchema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "balance" FieldDecimal False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  let newSchema = Schema
        { schemaVersion = mkVersion [1, 1, 0]
        , fields = 
            [ SchemaField "balance" FieldDecimal False
            , SchemaField "riskTolerance" FieldDecimal True -- optional field
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  let state = Map.singleton "state" $ Map.singleton "balance" (Number 100.0)
  
  case applySchemaEvolution oldSchema newSchema state of
    Left err -> assertFailure $ "Schema evolution failed: " ++ show err
    Right evolvedState -> do
      let stateMap = Map.findWithDefault Map.empty "state" evolvedState
      -- Check that the balance is preserved
      assertEqual "Balance should be preserved" 
        (Just (Number 100.0)) 
        (Map.lookup "balance" stateMap)
      
      -- Check that optional field is added with null value
      assertEqual "riskTolerance should be added as null" 
        (Just Null) 
        (Map.lookup "riskTolerance" stateMap)

-- | Test adding a field with default value
testAddFieldWithDefault :: Assertion
testAddFieldWithDefault = do
  let oldSchema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "balance" FieldDecimal False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  let newSchema = Schema
        { schemaVersion = mkVersion [1, 1, 0]
        , fields = 
            [ SchemaField "balance" FieldDecimal False
            , SchemaField "maxSlippage" FieldDecimal False -- required field with default
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  let state = Map.singleton "state" $ Map.singleton "balance" (Number 100.0)
  
  case applySchemaEvolution oldSchema newSchema state of
    Left err -> assertFailure $ "Schema evolution failed: " ++ show err
    Right evolvedState -> do
      let stateMap = Map.findWithDefault Map.empty "state" evolvedState
      
      -- Check that maxSlippage is added with default value
      assertEqual "maxSlippage should be added with default value" 
        (Just (Number 0.0)) 
        (Map.lookup "maxSlippage" stateMap)

-- | Test removing an unused field
testRemoveUnusedField :: Assertion
testRemoveUnusedField = do
  let oldSchema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "balance" FieldDecimal False
            , SchemaField "legacyCounter" FieldInt False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  let newSchema = Schema
        { schemaVersion = mkVersion [1, 1, 0]
        , fields = 
            [ SchemaField "balance" FieldDecimal False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  let state = Map.singleton "state" $ Map.fromList [
          ("balance", Number 100.0),
          ("legacyCounter", Number 42)
        ]
  
  case applySchemaEvolution oldSchema newSchema state of
    Left err -> assertFailure $ "Schema evolution failed: " ++ show err
    Right evolvedState -> do
      let stateMap = Map.findWithDefault Map.empty "state" evolvedState
      
      -- Check that legacyCounter is removed
      assertEqual "legacyCounter should be removed" 
        Nothing
        (Map.lookup "legacyCounter" stateMap)

-- | Test rejecting field removal when not allowed
testRejectRemoval :: Assertion
testRejectRemoval = do
  let restrictiveRules = EvolutionRules
        { allowAddOptionalFields = True
        , allowAddFieldsWithDefault = True
        , allowRemoveUnusedFields = False -- Not allowing field removal
        , allowRenameFields = False
        , allowTypeChanges = False
        }
  
  let oldSchema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "balance" FieldDecimal False
            , SchemaField "legacyCounter" FieldInt False
            ]
        , evolutionRules = restrictiveRules
        }
  
  let newSchema = Schema
        { schemaVersion = mkVersion [1, 1, 0]
        , fields = 
            [ SchemaField "balance" FieldDecimal False
            ]
        , evolutionRules = restrictiveRules
        }
  
  let state = Map.singleton "state" $ Map.fromList [
          ("balance", Number 100.0),
          ("legacyCounter", Number 42)
        ]
  
  case applySchemaEvolution oldSchema newSchema state of
    Left (RemovalNotAllowed field) -> 
      assertEqual "Should reject with the correct field" "legacyCounter" field
    Left err -> assertFailure $ "Unexpected error: " ++ show err
    Right _ -> assertFailure "Should have rejected field removal"

-- | Test rejecting type change when not allowed
testRejectTypeChange :: Assertion
testRejectTypeChange = do
  let oldSchema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "price" FieldDecimal False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  let newSchema = Schema
        { schemaVersion = mkVersion [1, 1, 0]
        , fields = 
            [ SchemaField "price" FieldText False -- Changed type from Decimal to Text
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  let state = Map.singleton "state" $ Map.singleton "price" (Number 2900.0)
  
  case applySchemaEvolution oldSchema newSchema state of
    Left (TypeChangeNotAllowed field oldType newType) -> do
      assertEqual "Should reject with the correct field" "price" field
      assertEqual "Should have the correct old type" FieldDecimal oldType
      assertEqual "Should have the correct new type" FieldText newType
    Left err -> assertFailure $ "Unexpected error: " ++ show err
    Right _ -> assertFailure "Should have rejected type change"

-- | Test protocol compatibility
testProtocolCompatibility :: Assertion
testProtocolCompatibility = do
  let schema = Schema
        { schemaVersion = mkVersion [2, 0, 0]
        , fields = []
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  -- Test too old
  case checkSchemaCompatibility schema (mkVersion [0, 9, 0]) of
    Left (ProtocolTooOld _) -> return ()
    Left err -> assertFailure $ "Unexpected error: " ++ show err
    Right _ -> assertFailure "Should have rejected too old protocol"
  
  -- Test too new
  case checkSchemaCompatibility schema (mkVersion [4, 0, 0]) of
    Left (ProtocolTooNew _) -> return ()
    Left err -> assertFailure $ "Unexpected error: " ++ show err
    Right _ -> assertFailure "Should have rejected too new protocol"
  
  -- Test compatible
  case checkSchemaCompatibility schema (mkVersion [2, 0, 0]) of
    Left err -> assertFailure $ "Unexpected error: " ++ show err
    Right _ -> return () 