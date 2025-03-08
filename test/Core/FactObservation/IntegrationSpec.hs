{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Core.FactObservation.IntegrationSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Either (isRight, isLeft)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.String.QQ
import Control.Monad (void, forM_)
import Control.Exception (bracket)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import System.IO.Temp (withSystemTempDirectory)

import Core.FactObservation.Rules
import Core.FactObservation.TOMLParser
import Core.FactObservation.Engine
import Core.FactObservation.Schema

spec :: Spec
spec = do
  describe "Fact Observation Integration" $ do
    around withTestEnvironment $ do
      it "parses rules, validates them, and evaluates data" $ \env -> do
        -- 1. Parse rules from TOML
        let tomlText = [s|
[[rules]]
rule_id = "integration-test-rule"
fact_type = "BalanceObservation"
proof = "NoProof"
enabled = true
description = "Integration test rule"

path.source = "ethereum"
path.selector = "account.balance"

[path.parameters]
address = "0x742d35Cc6634C0532925a3b844Bc454e4438f44e"

[[conditions]]
field = "balance"
operator = ">"
value = 1000000000000000000
|]
        
        let parseResult = parseRuleSet tomlText
        parseResult `shouldSatisfy` isRight
        
        -- 2. Validate against schema
        case parseResult of
          Left err -> fail $ "Failed to parse rule set: " ++ show err
          Right ruleSet -> do
            let ruleJson = Aeson.toJSON $ head $ rules ruleSet
            validateRuleAgainstSchema ruleJson `shouldSatisfy` isRight
            
            -- 3. Create a rule file
            let ruleFile = rulesDir env </> "integration-test.toml"
            TIO.writeFile ruleFile tomlText
            
            -- 4. Create the engine
            let config = EngineConfig
                  { configRulesDirectory = rulesDir env
                  , configFactsDirectory = factsDir env
                  , configProofsEnabled = False  -- Disable proofs for simpler testing
                  , configValidateRules = True
                  , configLogLevel = "info"
                  , configMaxConcurrent = 4
                  }
            engine <- createEngine config
            
            -- 5. Load rules
            loadResult <- loadRules engine ruleFile
            loadResult `shouldSatisfy` isRight
            
            -- 6. Create test data
            let testData = object
                  [ "ethereum" .= object
                      [ "account" .= object
                          [ "balance" .= (2000000000000000000 :: Integer)  -- 2 ETH, above threshold
                          , "address" .= ("0x742d35Cc6634C0532925a3b844Bc454e4438f44e" :: Text)
                          ]
                      ]
                  ]
            
            -- 7. Evaluate data
            results <- evaluateData engine testData
            
            -- 8. Check results
            length (filter isRight results) `shouldBe` 1
            
            -- 9. Check that a fact file was created
            factFiles <- listDirectory (factsDir env)
            let jsonFiles = filter (\f -> takeExtension f == ".json") factFiles
            length jsonFiles `shouldBe` 1
      
      it "detects missing fields and validation errors" $ \env -> do
        -- Create invalid rule with missing fields
        let invalidToml = [s|
[[rules]]
# Missing rule_id
fact_type = "BalanceObservation"
enabled = true

path.source = "ethereum"
# Missing selector
|]
        
        -- Parse the invalid rule
        let parseResult = parseRuleSet invalidToml
        parseResult `shouldSatisfy` isRight  -- Parser doesn't validate content
        
        -- Validate the rule set
        case parseResult of
          Left err -> fail $ "Failed to parse rule set: " ++ show err
          Right ruleSet -> do
            let rule = head $ rules ruleSet
            validateRule rule `shouldSatisfy` isLeft
            
            -- Create engine with validation enabled
            let config = EngineConfig
                  { configRulesDirectory = rulesDir env
                  , configFactsDirectory = factsDir env
                  , configProofsEnabled = False
                  , configValidateRules = True  -- Enable validation
                  , configLogLevel = "info"
                  , configMaxConcurrent = 4
                  }
            engine <- createEngine config
            
            -- Create a rule file
            let ruleFile = rulesDir env </> "invalid-rule.toml"
            TIO.writeFile ruleFile invalidToml
            
            -- Try to load the invalid rule
            loadResult <- loadRules engine ruleFile
            loadResult `shouldSatisfy` isLeft  -- Should fail validation
      
      it "handles multiple rules and complex conditions" $ \env -> do
        -- Create file with multiple rules
        let multiRuleToml = [s|
[[rules]]
rule_id = "rule-1"
fact_type = "BalanceObservation"
proof = "NoProof"
enabled = true

path.source = "ethereum"
path.selector = "account.balance"

[[conditions]]
field = "balance"
operator = ">"
value = 1000000000000000000

[[rules]]
rule_id = "rule-2"
fact_type = "BalanceObservation"
proof = "NoProof"
enabled = true

path.source = "ethereum"
path.selector = "account.balance"

[[conditions]]
logical_op = "AND"

[[conditions.sub_conditions]]
field = "balance"
operator = ">"
value = 5000000000000000000  # 5 ETH, won't match our data

[[conditions.sub_conditions]]
check_field = "address"
|]
        
        -- Create rule file
        let ruleFile = rulesDir env </> "multi-rule.toml"
        TIO.writeFile ruleFile multiRuleToml
        
        -- Create engine
        let config = EngineConfig
              { configRulesDirectory = rulesDir env
              , configFactsDirectory = factsDir env
              , configProofsEnabled = False
              , configValidateRules = True
              , configLogLevel = "info"
              , configMaxConcurrent = 4
              }
        engine <- createEngine config
        
        -- Load rules
        loadResult <- loadRules engine ruleFile
        loadResult `shouldSatisfy` isRight
        
        -- Create test data
        let testData = object
              [ "ethereum" .= object
                  [ "account" .= object
                      [ "balance" .= (2000000000000000000 :: Integer)  -- 2 ETH, matches rule-1 but not rule-2
                      , "address" .= ("0x742d35Cc6634C0532925a3b844Bc454e4438f44e" :: Text)
                      ]
                  ]
              ]
        
        -- Evaluate data
        results <- evaluateData engine testData
        
        -- Only one rule should match
        length (filter isRight results) `shouldBe` 1
        
        -- The fact file should be for rule-1
        factFiles <- listDirectory (factsDir env)
        let jsonFiles = filter (\f -> takeExtension f == ".json") factFiles
        length jsonFiles `shouldBe` 1
        
        -- Check the content of the fact file
        let factFile = factsDir env </> head jsonFiles
        factFileExists <- doesFileExist factFile
        factFileExists `shouldBe` True
        
        factContent <- Aeson.eitherDecodeFileStrict factFile
        factContent `shouldSatisfy` isRight
        
        case factContent of
          Left err -> fail $ "Failed to parse fact file: " ++ err
          Right value -> do
            jsonField value "rule_id" `shouldBe` Just (String "rule-1")

-- Helper types and functions

data TestEnvironment = TestEnvironment
  { rulesDir :: FilePath
  , factsDir :: FilePath
  }

withTestEnvironment :: (TestEnvironment -> IO ()) -> IO ()
withTestEnvironment action = 
  withSystemTempDirectory "fact-observation-integration" $ \tmpDir -> do
    let rulesDir = tmpDir </> "rules"
        factsDir = tmpDir </> "facts"
    
    -- Create the directories
    createDirectoryIfMissing True rulesDir
    createDirectoryIfMissing True factsDir
    
    -- Run the test action
    action TestEnvironment
      { rulesDir = rulesDir
      , factsDir = factsDir
      }

-- | Get a field from a JSON object
jsonField :: Value -> Text -> Maybe Value
jsonField (Object obj) field = Aeson.lookup field obj
jsonField _ _ = Nothing 