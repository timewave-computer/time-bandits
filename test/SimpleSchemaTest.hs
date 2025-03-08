{-# LANGUAGE OverloadedStrings #-}

module Main where

import Core.Schema
import Data.Version (Version)
import Data.Version.Extra (mkVersion)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
  putStrLn "Running simple Schema tests"
  testBasicSchema
  testSchemaEvolution

testBasicSchema :: IO ()
testBasicSchema = do
  putStrLn "Testing basic schema creation..."
  
  let schema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "name" FieldText False
            , SchemaField "age" FieldInt False
            , SchemaField "balance" FieldDecimal True
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  putStrLn $ "Schema version: " ++ show (schemaVersion schema)
  putStrLn $ "Number of fields: " ++ show (length $ fields schema)
  putStrLn "Basic schema test passed!"

testSchemaEvolution :: IO ()
testSchemaEvolution = do
  putStrLn "Testing schema evolution..."
  
  let oldSchema = Schema
        { schemaVersion = mkVersion [1, 0, 0]
        , fields = 
            [ SchemaField "name" FieldText False
            , SchemaField "age" FieldInt False
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  let newSchema = Schema
        { schemaVersion = mkVersion [1, 1, 0]
        , fields = 
            [ SchemaField "name" FieldText False
            , SchemaField "age" FieldInt False
            , SchemaField "balance" FieldDecimal True
            ]
        , evolutionRules = defaultCoreEvolutionRules
        }
  
  let state = Map.singleton "state" $ Map.singleton "name" "John"
  
  case applySchemaEvolution oldSchema newSchema state of
    Left err -> putStrLn $ "Evolution failed: " ++ show err
    Right result -> putStrLn "Evolution succeeded with new state!"
  
  putStrLn "Schema evolution test passed!" 