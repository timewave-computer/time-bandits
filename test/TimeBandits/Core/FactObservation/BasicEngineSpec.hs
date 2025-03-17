module TimeBandits.Core.FactObservation.BasicEngineSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.Text as T

import qualified TimeBandits.Core.FactObservation.Engine as Engine
import qualified TimeBandits.Core.FactObservation.Rules as Rules

spec :: Spec
spec = describe "Basic functionality of Engine" $ do
  describe "createEngine" $ do
    it "creates an engine with a valid configuration" $ do
      -- Create a minimal valid configuration
      let config = Engine.EngineConfig
            { Engine.ruleDirectories = []
            , Engine.schemaDirectory = ""
            , Engine.proofEnabled = False
            , Engine.logVerbosity = 0  -- Use an integer for verbosity
            }
      
      -- Create the engine
      engineResult <- Engine.createEngine config
      
      -- Check the result
      case engineResult of
        Left err -> fail $ "Engine creation failed: " ++ show err
        Right _ -> do
          -- Verify that engine was created successfully
          return ()
    
    it "handles invalid rule directories gracefully" $ do
      -- Create configuration with non-existent directories
      let config = Engine.EngineConfig
            { Engine.ruleDirectories = ["non-existent-dir"]
            , Engine.schemaDirectory = ""
            , Engine.proofEnabled = False
            , Engine.logVerbosity = 0  -- Use an integer for verbosity
            }
      
      -- Create the engine
      engineResult <- Engine.createEngine config
      
      -- Check that engine creation succeeds even with non-existent directories
      case engineResult of
        Left err -> fail $ "Engine creation failed: " ++ show err
        Right engine -> do
          -- Load rules from non-existent directory
          _ <- Engine.loadRulesFromDirectory engine ["non-existent-dir"]
          
          -- Check that loading fails gracefully - we expect it to succeed with warnings
          -- but not fail with an error
          return ()
          
  describe "evaluateRules" $ do
    it "evaluates rules against data" $ do
      withSystemTempDirectory "engine-test" $ \_ -> do
        -- Create a rule set
        let ruleSetName = T.pack "test-ruleset"
        let ruleSet = Rules.createRuleSet ruleSetName
        
        -- Create a simple rule
        let ruleName = T.pack "test-rule"
            factType = T.pack "test-fact"
            rule = Rules.FactObservationRule
              { Rules.ruleId = ruleName
              , Rules.ruleName = T.pack "Test rule"
              , Rules.ruleDescription = T.pack "Test rule description"
              , Rules.ruleSetName = Just ruleSetName
              , Rules.factType = Rules.Custom factType
              , Rules.factPath = Rules.JsonPath (T.pack "$.factType")
              , Rules.conditions = []
              , Rules.proofType = Rules.NoProof
              , Rules.priority = 1
              , Rules.enabled = True
              , Rules.metadata = Map.empty
              }
        
        -- Add the rule to the rule set
        let updatedRuleSet = Rules.addRule rule ruleSet
        
        -- Create an engine config
        let config = Engine.EngineConfig
              { Engine.ruleDirectories = []
              , Engine.schemaDirectory = ""
              , Engine.proofEnabled = False
              , Engine.logVerbosity = 0
              }
        
        -- Create the engine
        engineResult <- Engine.createEngine config
        
        case engineResult of
          Left err -> fail $ "Engine creation failed: " ++ show err
          Right engine -> do
            -- Create test data with the matching fact type
            let testData = Aeson.object [(Key.fromText (T.pack "factType"), Aeson.String factType)]
            
            -- Evaluate the data against the rules
            results <- Engine.evaluateDataWithRuleSet engine updatedRuleSet testData
            
            -- Check the evaluation results
            length results `shouldSatisfy` (> 0)
            
            -- Check that we don't have any errors in the results
            -- We need to check each result without using Show for FactResult
            let errorsCount = length [() | Left _ <- results]
            errorsCount `shouldBe` 0
            
            -- Check that we have at least one successful result
            let successCount = length [() | Right _ <- results]
            successCount `shouldSatisfy` (> 0) 