{-# LANGUAGE OverloadedStrings #-}

module TimeBandits.Core.FactObservation.RulesSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Map.Strict as Map

import qualified TimeBandits.Core.FactObservation.Rules as Rules

spec :: Spec
spec = describe "Rules" $ do
  describe "Rule creation and management" $ do
    it "creates valid rules" $ do
      let rule = createSampleRule "test-rule-1" Rules.BalanceObservation
      Rules.ruleId rule `shouldBe` "test-rule-1"
      Rules.factType rule `shouldBe` Rules.BalanceObservation
    
    it "adds rules to a ruleset" $ do
      let rule1 = createSampleRule "rule-1" Rules.BalanceObservation
          rule2 = createSampleRule "rule-2" Rules.PriceObservation
          ruleSet = Rules.createRuleSet [rule1, rule2] Map.empty
      
      length (Rules.rules ruleSet) `shouldBe` 2
    
    it "detects duplicate rule IDs" $ do
      let rule1 = createSampleRule "duplicate-id" Rules.BalanceObservation
          rule2 = createSampleRule "duplicate-id" Rules.PriceObservation
          rule3 = createSampleRule "unique-id" Rules.EventObservation
          
          result = Rules.addRule (Rules.createRuleSet [rule1] Map.empty) rule2
      
      case result of
        Left _ -> True `shouldBe` True -- Expected error
        Right _ -> expectationFailure "Should have failed with duplicate ID"
      
      -- Test force adding a rule
      let forcedSet = Rules.forceAddRule (Rules.createRuleSet [rule1] Map.empty) rule2
      length (Rules.rules forcedSet) `shouldBe` 1
      Rules.factType (head $ Rules.rules forcedSet) `shouldBe` Rules.PriceObservation
    
    it "successfully adds rules with unique IDs" $ do
      let rule1 = createSampleRule "unique-id-1" Rules.BalanceObservation
          rule2 = createSampleRule "unique-id-2" Rules.PriceObservation
          
          result = Rules.addRule (Rules.createRuleSet [rule1] Map.empty) rule2
      
      case result of
        Left err -> expectationFailure $ "Should not have failed: " ++ show err
        Right ruleSet -> length (Rules.rules ruleSet) `shouldBe` 2

-- Helper to create a sample rule
createSampleRule :: Text -> Rules.FactType -> Rules.FactObservationRule
createSampleRule id factType = Rules.FactObservationRule
  { Rules.ruleId = id
  , Rules.factType = factType
  , Rules.description = Just $ "Test rule for " <> id
  , Rules.enabled = True
  , Rules.conditions = []
  , Rules.path = Rules.PathExpression 
      { Rules.source = "test-source"
      , Rules.selector = "test-selector"
      , Rules.parameters = mempty
      }
  , Rules.proof = Rules.NoProof
  }