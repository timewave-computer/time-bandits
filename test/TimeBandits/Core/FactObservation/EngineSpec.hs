{-# LANGUAGE OverloadedStrings #-}

module TimeBandits.Core.FactObservation.EngineSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..), object, (.=))
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.FilePath ((</>))
import System.Directory (createDirectory, doesFileExist, listDirectory)
import Control.Monad (forM_)
import Control.Exception (try, SomeException)

import qualified TimeBandits.Core.FactObservation.Engine as Engine
import qualified TimeBandits.Core.FactObservation.Rules as Rules
import qualified TimeBandits.Core.FactObservation.TOMLParser as TOMLParser

-- Helper function to create a sample rule
createSampleRule :: Text -> Rules.FactType -> Rules.FactObservationRule
createSampleRule id factType = Rules.FactObservationRule
  { Rules.ruleId = id
  , Rules.ruleName = "Test Rule " <> id
  , Rules.ruleDescription = "Test rule for " <> id
  , Rules.ruleSetName = Nothing
  , Rules.factType = factType
  , Rules.factPath = Rules.JsonPath "$.test-source.price"
  , Rules.conditions = []
  , Rules.proofType = Rules.NoProof
  , Rules.priority = 1
  , Rules.enabled = True
  , Rules.metadata = Map.empty
  }

-- Helper to create a rule with conditions
createRuleWithCondition :: Text -> Rules.FactType -> Rules.Condition -> Rules.FactObservationRule
createRuleWithCondition id factType condition =
  let baseRule = createSampleRule id factType
  in baseRule { Rules.conditions = [condition] }

-- Create a standard engine config for testing
createTestConfig :: Engine.EngineConfig
createTestConfig = Engine.EngineConfig
  { Engine.ruleDirectories = ["rules"]
  , Engine.schemaDirectory = "schemas"
  , Engine.proofEnabled = False
  , Engine.logVerbosity = 0
  }

-- Helper to create properly formatted test data for the engine
createTestData :: Value -> Value
createTestData dataValue = object 
  [ "test-source" .= dataValue  -- The source name matches the rule's source
  ]

-- Helper to create a simple TOML rule file
createTOMLRuleFile :: FilePath -> Text -> Text -> IO ()
createTOMLRuleFile dir ruleId description = do
  let ruleFile = dir </> T.unpack ruleId <> ".toml"
      tomlContent = T.unlines
        [ "[rule]"
        , "id = \"" <> ruleId <> "\""
        , "description = \"" <> description <> "\""
        , "enabled = true"
        , "fact_type = \"price\""
        , ""
        , "[path]"
        , "source = \"test-source\""
        , "selector = \"price\""
        ]
  TIO.writeFile ruleFile tomlContent

-- Helper to create an invalid TOML rule file (missing required fields)
createInvalidTOMLRuleFile :: FilePath -> Text -> IO ()
createInvalidTOMLRuleFile dir ruleId = do
  let ruleFile = dir </> T.unpack ruleId <> "_invalid.toml"
      tomlContent = T.unlines
        [ "[rule]"
        , "id = \"" <> ruleId <> "\""
        , "enabled = true"
        , "# Missing required fact_type field"
        , ""
        , "# Missing path section"
        ]
  TIO.writeFile ruleFile tomlContent

spec :: Spec
spec = do
  describe "Rule Engine" $ do
    it "should evaluate a rule against data" $ do
      -- Create a test engine
      let config = createTestConfig
      engineResult <- Engine.createEngine config
      case engineResult of
        Left err -> fail $ "Failed to create engine: " ++ show err
        Right engine -> do
          -- Create a simple comparison condition
          let condition = Rules.GreaterThan (Rules.JsonPath "$.price") (Aeson.Number 100)
          
          -- Create rules with these conditions
          let passingRule = createRuleWithCondition "passing-rule" Rules.BlockHeader condition
              failingRule = createRuleWithCondition "failing-rule" Rules.BlockHeader 
                              (Rules.GreaterThan (Rules.JsonPath "$.price") (Aeson.Number 200))
          
          -- Create test data with a price of 150
          let testData = object [ "test-source" .= object [ "price" .= (150 :: Int) ] ]
          
          -- Test the passing rule
          passingResult <- Engine.evaluateDataWithRule engine passingRule testData
          case passingResult of
            Left err -> fail $ "Rule evaluation failed: " ++ show err
            Right factResult -> do
              Engine.factType factResult `shouldBe` Rules.BlockHeader
              
          -- Test the failing rule
          failingResult <- Engine.evaluateDataWithRule engine failingRule testData
          case failingResult of
            Left _ -> return () -- Expected to fail
            Right _ -> fail "Rule should have failed but passed"
    
    it "should load rules from a directory" $ withSystemTempDirectory "rules-test" $ \tempDir -> do
      -- Create a rule file in the temp directory
      let rulePath = tempDir </> "test-rule.json"
          ruleContent = "{\"ruleId\": \"test-rule\", \"ruleName\": \"Test Rule\", \"ruleDescription\": \"A test rule\", \"factType\": \"BlockHeader\", \"factPath\": {\"JsonPath\": \"$.price\"}, \"conditions\": [], \"proofType\": \"NoProof\", \"priority\": 1, \"enabled\": true, \"metadata\": {}}"
      
      TIO.writeFile rulePath ruleContent
      
      -- Create an engine with the temp directory
      let config = createTestConfig { Engine.ruleDirectories = [tempDir] }
      engineResult <- Engine.createEngine config
      case engineResult of
        Left err -> fail $ "Failed to create engine: " ++ show err
        Right engine -> do
          -- Load rules from the directory
          loadResult <- Engine.loadRulesFromDirectory engine [tempDir]
          case loadResult of
            Left err -> fail $ "Failed to load rules: " ++ show err
            Right updatedEngine -> do
              -- Check that the rule was loaded
              let ruleSet = Map.lookup "default" (Engine.ruleSets updatedEngine)
              case ruleSet of
                Nothing -> fail "Rule set not found"
                Just rs -> do
                  Map.size (Rules.ruleSetRules rs) `shouldBe` 1
    
    it "should validate rules" $ do
      -- Create a valid rule
      let validRule = createSampleRule "valid-rule" Rules.BlockHeader
      
      -- Validate the rule
      case Rules.validateRule validRule of
        Left err -> fail $ "Valid rule failed validation: " ++ show err
        Right () -> return ()
      
      -- Create an invalid rule (empty ID)
      let invalidRule = validRule { Rules.ruleId = "" }
      
      -- Validate the invalid rule
      case Rules.validateRule invalidRule of
        Left (Rules.MissingRequiredField _) -> return ()
        Left err -> fail $ "Expected MissingRequiredField error, got: " ++ show err
        Right () -> fail "Invalid rule passed validation"
    
    it "should handle rule sets" $ do
      -- Create some rules
      let rule1 = createSampleRule "rule-1" Rules.BlockHeader
          rule2 = createSampleRule "rule-2" Rules.Transaction
      
      -- Create a rule set and add the rules
      let ruleSet = Rules.createRuleSet "test-set"
          updatedSet = Rules.addRule rule1 (Rules.addRule rule2 ruleSet)
      
      -- Check that both rules were added
      Map.size (Rules.ruleSetRules updatedSet) `shouldBe` 2
      
      -- Try to add a duplicate rule (should be ignored)
      let duplicateRule = rule1 { Rules.ruleName = "Changed name" }
          finalSet = Rules.addRule duplicateRule updatedSet
      
      -- Check that no new rule was added
      Map.size (Rules.ruleSetRules finalSet) `shouldBe` 2
      
      -- Force add the duplicate rule
      let forcedSet = Rules.forceAddRule duplicateRule updatedSet
      
      -- Check that the rule was replaced
      Map.size (Rules.ruleSetRules forcedSet) `shouldBe` 2
      let replacedRule = Map.lookup (Rules.ruleId rule1) (Rules.ruleSetRules forcedSet)
      case replacedRule of
        Nothing -> fail "Rule not found after force add"
        Just r -> Rules.ruleName r `shouldBe` "Changed name"
