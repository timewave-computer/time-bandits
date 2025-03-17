module TimeBandits.Core.FactObservation.BasicRulesSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Maybe (listToMaybe)

import qualified TimeBandits.Core.FactObservation.Rules as Rules

spec :: Spec
spec = describe "Basic Rules Functionality" $ do
  it "can create a rule set" $ do
    let ruleId = T.pack "test-ruleset"
        ruleSet = Rules.createRuleSet ruleId
    Rules.ruleSetId ruleSet `shouldBe` ruleId
    Map.size (Rules.ruleSetRules ruleSet) `shouldBe` 0

  it "can add a rule to a rule set" $ do
    let ruleId = T.pack "test-ruleset"
        ruleSet = Rules.createRuleSet ruleId
        
        -- Create a simple rule
        rule = Rules.FactObservationRule
          { Rules.ruleId = T.pack "test-rule"
          , Rules.ruleName = T.pack "Test rule"
          , Rules.ruleDescription = T.pack "Test rule description"
          , Rules.ruleSetName = Nothing
          , Rules.factType = Rules.BlockHeader
          , Rules.factPath = Rules.JsonPath (T.pack "$.path")
          , Rules.conditions = []
          , Rules.proofType = Rules.NoProof
          , Rules.priority = 1
          , Rules.enabled = True
          , Rules.metadata = Map.empty
          }
        
        -- Add the rule to the rule set
        ruleSet' = Rules.addRule rule ruleSet
    
    Map.size (Rules.ruleSetRules ruleSet') `shouldBe` 1
    let rules = Rules.rules ruleSet'
    length rules `shouldBe` 1
    
    -- Use safe access for the first rule
    case listToMaybe rules of
      Just firstRule -> Rules.ruleId firstRule `shouldBe` T.pack "test-rule"
      Nothing -> expectationFailure "Expected at least one rule but got none"

  it "can find rules by fact type" $ do
    let ruleId = T.pack "test-ruleset"
        ruleSet = Rules.createRuleSet ruleId
        
        -- Create rules with different fact types
        rule1 = Rules.FactObservationRule
          { Rules.ruleId = T.pack "rule1"
          , Rules.ruleName = T.pack "Test rule 1"
          , Rules.ruleDescription = T.pack "Test rule 1 description"
          , Rules.ruleSetName = Nothing
          , Rules.factType = Rules.BlockHeader
          , Rules.factPath = Rules.JsonPath (T.pack "$.path")
          , Rules.conditions = []
          , Rules.proofType = Rules.NoProof
          , Rules.priority = 1
          , Rules.enabled = True
          , Rules.metadata = Map.empty
          }
        
        rule2 = Rules.FactObservationRule
          { Rules.ruleId = T.pack "rule2"
          , Rules.ruleName = T.pack "Test rule 2"
          , Rules.ruleDescription = T.pack "Test rule 2 description"
          , Rules.ruleSetName = Nothing
          , Rules.factType = Rules.Transaction
          , Rules.factPath = Rules.JsonPath (T.pack "$.path")
          , Rules.conditions = []
          , Rules.proofType = Rules.NoProof
          , Rules.priority = 1
          , Rules.enabled = True
          , Rules.metadata = Map.empty
          }
        
        rule3 = Rules.FactObservationRule
          { Rules.ruleId = T.pack "rule3"
          , Rules.ruleName = T.pack "Test rule 3"
          , Rules.ruleDescription = T.pack "Test rule 3 description"
          , Rules.ruleSetName = Nothing
          , Rules.factType = Rules.BlockHeader
          , Rules.factPath = Rules.JsonPath (T.pack "$.path")
          , Rules.conditions = []
          , Rules.proofType = Rules.NoProof
          , Rules.priority = 1
          , Rules.enabled = True
          , Rules.metadata = Map.empty
          }
        
        -- Create a disabled rule
        rule4 = Rules.FactObservationRule
          { Rules.ruleId = T.pack "rule4"
          , Rules.ruleName = T.pack "Test rule 4"
          , Rules.ruleDescription = T.pack "Test rule 4 description"
          , Rules.ruleSetName = Nothing
          , Rules.factType = Rules.BlockHeader
          , Rules.factPath = Rules.JsonPath (T.pack "$.path")
          , Rules.conditions = []
          , Rules.proofType = Rules.NoProof
          , Rules.priority = 1
          , Rules.enabled = False  -- This one is disabled
          , Rules.metadata = Map.empty
          }
        
        -- Add all rules to the rule set
        ruleSet' = foldr Rules.addRule ruleSet [rule1, rule2, rule3, rule4]
        
    -- Find rules by fact type
    let rulesForBlockHeader = Rules.findRulesByFactType Rules.BlockHeader ruleSet'
    let rulesForTransaction = Rules.findRulesByFactType Rules.Transaction ruleSet'
    
    -- Only enabled rules should be returned
    length rulesForBlockHeader `shouldBe` 2  -- rule1 and rule3, but not rule4 (disabled)
    length rulesForTransaction `shouldBe` 1  -- just rule2
    
    -- Rules should be returned in some order
    let ruleIds = map Rules.ruleId rulesForBlockHeader
    ruleIds `shouldContain` [T.pack "rule1"]
    ruleIds `shouldContain` [T.pack "rule3"]

  it "validates rules correctly" $ do
    -- Create a valid rule
    let validRule = Rules.FactObservationRule
          { Rules.ruleId = T.pack "valid-rule"
          , Rules.ruleName = T.pack "Valid Rule"
          , Rules.ruleDescription = T.pack "A valid rule"
          , Rules.ruleSetName = Nothing
          , Rules.factType = Rules.BlockHeader
          , Rules.factPath = Rules.JsonPath (T.pack "$.path")
          , Rules.conditions = []
          , Rules.proofType = Rules.NoProof
          , Rules.priority = 1
          , Rules.enabled = True
          , Rules.metadata = Map.empty
          }
    
    -- Create an invalid rule (empty ID)
    let invalidRule = Rules.FactObservationRule
          { Rules.ruleId = T.pack ""  -- Empty rule ID is invalid
          , Rules.ruleName = T.pack "Invalid Rule"
          , Rules.ruleDescription = T.pack "An invalid rule"
          , Rules.ruleSetName = Nothing
          , Rules.factType = Rules.BlockHeader
          , Rules.factPath = Rules.JsonPath (T.pack "$.path")
          , Rules.conditions = []
          , Rules.proofType = Rules.NoProof
          , Rules.priority = 1
          , Rules.enabled = True
          , Rules.metadata = Map.empty
          }
    
    -- Validate the rules
    case Rules.validateRule validRule of
      Left err -> expectationFailure $ "Valid rule failed validation: " ++ show err
      Right () -> return ()
    
    -- Invalid rule should fail validation
    case Rules.validateRule invalidRule of
      Left (Rules.MissingRequiredField _) -> return ()  -- Expected error
      Left err -> expectationFailure $ "Expected MissingRequiredField error, got: " ++ show err
      Right () -> expectationFailure "Invalid rule passed validation" 