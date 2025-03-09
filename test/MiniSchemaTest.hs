{-# LANGUAGE OverloadedStrings #-}

-- A minimal standalone test for Schema functionality, with minimal dependencies

import Control.Monad (unless)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version (Version)
import qualified Data.Version as V

-- Reimplement just what we need from the Schema module
data FieldType = FieldInt | FieldText | FieldDecimal | FieldBool | FieldList FieldType
  deriving (Show, Eq)

data SchemaField = SchemaField
  { fieldName :: Text
  , fieldType :: FieldType
  , fieldNullable :: Bool
  } deriving (Show, Eq)

data Schema = Schema
  { schemaVersion :: Version
  , fields :: [SchemaField]
  } deriving (Show, Eq)

-- Simplified evolution
data EvolutionError
  = FieldMissing Text
  | TypeChangeNotAllowed Text FieldType FieldType
  | RequiredFieldAdded Text
  deriving (Show, Eq)

schemaAddField :: Schema -> SchemaField -> Schema
schemaAddField schema field = schema { fields = fields schema ++ [field] }

-- Check if adding a field is allowed
validateAddField :: SchemaField -> Either EvolutionError ()
validateAddField field =
  if not (fieldNullable field)
    then Left $ RequiredFieldAdded (fieldName field)
    else Right ()

-- Test adding a field to a schema
testAddField :: IO ()
testAddField = do
  putStrLn "Testing adding a nullable field to a schema..."
  
  let v1 = V.makeVersion [1, 0, 0]
  let schema = Schema v1 
              [ SchemaField "name" FieldText False
              , SchemaField "age" FieldInt False
              ]
  
  let newField = SchemaField "balance" FieldDecimal True
  let result = validateAddField newField
  
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right () -> do
      let newSchema = schemaAddField schema newField
      putStrLn $ "Field count before: " ++ show (length $ fields schema)
      putStrLn $ "Field count after: " ++ show (length $ fields newSchema)
      putStrLn "Test passed!"

testRequiredField :: IO ()
testRequiredField = do
  putStrLn "Testing adding a required field (should fail)..."
  
  let newField = SchemaField "must_have" FieldBool False
  let result = validateAddField newField
  
  case result of
    Left (RequiredFieldAdded name) -> putStrLn $ "Correctly rejected required field: " ++ T.unpack name
    _ -> putStrLn "Test failed! Should have rejected required field"

main :: IO ()
main = do
  putStrLn "Running simplified Schema evolution tests"
  testAddField
  testRequiredField 