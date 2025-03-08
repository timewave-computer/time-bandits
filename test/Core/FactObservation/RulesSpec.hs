{-# LANGUAGE OverloadedStrings #-}

module Core.FactObservation.RulesSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Either (isRight, isLeft)

import Core.FactObservation.Rules

spec :: Spec
spec = do
  describe "FactObservationRule validation" $ do
    it "validates a valid rule" $ do
      let rule = FactObservationRule
            { ruleId = "test-rule-1"
            , factType = BalanceObservation
            , path = PathExpression "ethereum" "account.balance" Map.empty
            , proof = StateProof
            , conditions = [ExistsCondition "balance"]
            , description = Just "Test rule"
            , enabled = True
            }
      validateRule rule `shouldSatisfy` isRight
    
    it "rejects a rule with empty ID" $ do
      let rule = FactObservationRule
            { ruleId = ""
            , factType = BalanceObservation
            , path = PathExpression "ethereum" "account.balance" Map.empty
            , proof = StateProof
            , conditions = [ExistsCondition "balance"]
            , description = Just "Test rule"
            , enabled = True
            }
      validateRule rule `shouldSatisfy` isLeft
    
    it "rejects a rule with invalid path (empty source)" $ do
      let rule = FactObservationRule
            { ruleId = "test-rule-1"
            , factType = BalanceObservation
            , path = PathExpression "" "account.balance" Map.empty
            , proof = StateProof
            , conditions = [ExistsCondition "balance"]
            , description = Just "Test rule"
            , enabled = True
            }
      validateRule rule `shouldSatisfy` isLeft
    
    it "rejects a rule with invalid path (empty selector)" $ do
      let rule = FactObservationRule
            { ruleId = "test-rule-1"
            , factType = BalanceObservation
            , path = PathExpression "ethereum" "" Map.empty
            , proof = StateProof
            , conditions = [ExistsCondition "balance"]
            , description = Just "Test rule"
            , enabled = True
            }
      validateRule rule `shouldSatisfy` isLeft
  
  describe "PathExpression validation" $ do
    it "validates a valid path expression" $ do
      let path = PathExpression "ethereum" "account.balance" (Map.fromList [("address", "0x123")])
      validatePathExpression path `shouldSatisfy` isRight
    
    it "rejects a path with empty source" $ do
      let path = PathExpression "" "account.balance" Map.empty
      validatePathExpression path `shouldSatisfy` isLeft
    
    it "rejects a path with empty selector" $ do
      let path = PathExpression "ethereum" "" Map.empty
      validatePathExpression path `shouldSatisfy` isLeft
  
  describe "Condition validation" $ do
    it "validates a valid comparison condition" $ do
      let condition = ComparisonCondition "balance" ">" (Number 1000)
      validateCondition condition `shouldSatisfy` isRight
    
    it "validates a valid existence condition" $ do
      let condition = ExistsCondition "balance"
      validateCondition condition `shouldSatisfy` isRight
    
    it "validates a valid logical condition" $ do
      let condition = LogicalCondition "AND" [
              ComparisonCondition "balance" ">" (Number 1000),
              ExistsCondition "lastUpdated"
            ]
      validateCondition condition `shouldSatisfy` isRight
    
    it "rejects a comparison condition with empty field" $ do
      let condition = ComparisonCondition "" ">" (Number 1000)
      validateCondition condition `shouldSatisfy` isLeft
    
    it "rejects a comparison condition with invalid operator" $ do
      let condition = ComparisonCondition "balance" "INVALID" (Number 1000)
      validateCondition condition `shouldSatisfy` isLeft
    
    it "rejects an existence condition with empty field" $ do
      let condition = ExistsCondition ""
      validateCondition condition `shouldSatisfy` isLeft
    
    it "rejects a logical condition with invalid operator" $ do
      let condition = LogicalCondition "INVALID" [ExistsCondition "balance"]
      validateCondition condition `shouldSatisfy` isLeft
    
    it "rejects a logical condition with empty sub-conditions" $ do
      let condition = LogicalCondition "AND" []
      validateCondition condition `shouldSatisfy` isLeft
  
  describe "RuleSet operations" $ do
    it "creates a valid rule set" $ do
      let rule1 = FactObservationRule
            { ruleId = "test-rule-1"
            , factType = BalanceObservation
            , path = PathExpression "ethereum" "account.balance" Map.empty
            , proof = StateProof
            , conditions = [ExistsCondition "balance"]
            , description = Just "Test rule 1"
            , enabled = True
            }
          rule2 = FactObservationRule
            { ruleId = "test-rule-2"
            , factType = TransactionObservation
            , path = PathExpression "bitcoin" "transaction" Map.empty
            , proof = InclusionProof
            , conditions = [ComparisonCondition "amount" ">" (Number 100000000)]
            , description = Just "Test rule 2"
            , enabled = False
            }
          ruleSet = createRuleSet [rule1, rule2] (Map.fromList [("version", "1.0.0")])
      
      length (rules ruleSet) `shouldBe` 2
      Map.lookup "version" (metadata ruleSet) `shouldBe` Just "1.0.0"
    
    it "adds a rule to a rule set" $ do
      let rule1 = FactObservationRule
            { ruleId = "test-rule-1"
            , factType = BalanceObservation
            , path = PathExpression "ethereum" "account.balance" Map.empty
            , proof = StateProof
            , conditions = [ExistsCondition "balance"]
            , description = Just "Test rule 1"
            , enabled = True
            }
          ruleSet1 = createRuleSet [rule1] Map.empty
          
          rule2 = FactObservationRule
            { ruleId = "test-rule-2"
            , factType = TransactionObservation
            , path = PathExpression "bitcoin" "transaction" Map.empty
            , proof = InclusionProof
            , conditions = [ComparisonCondition "amount" ">" (Number 100000000)]
            , description = Just "Test rule 2"
            , enabled = False
            }
          ruleSet2 = addRuleToSet ruleSet1 rule2
      
      length (rules ruleSet2) `shouldBe` 2
    
    it "removes a rule from a rule set" $ do
      let rule1 = FactObservationRule
            { ruleId = "test-rule-1"
            , factType = BalanceObservation
            , path = PathExpression "ethereum" "account.balance" Map.empty
            , proof = StateProof
            , conditions = [ExistsCondition "balance"]
            , description = Just "Test rule 1"
            , enabled = True
            }
          rule2 = FactObservationRule
            { ruleId = "test-rule-2"
            , factType = TransactionObservation
            , path = PathExpression "bitcoin" "transaction" Map.empty
            , proof = InclusionProof
            , conditions = [ComparisonCondition "amount" ">" (Number 100000000)]
            , description = Just "Test rule 2"
            , enabled = False
            }
          ruleSet1 = createRuleSet [rule1, rule2] Map.empty
          ruleSet2 = removeRuleFromSet ruleSet1 "test-rule-1"
      
      length (rules ruleSet2) `shouldBe` 1
      ruleId (head (rules ruleSet2)) `shouldBe` "test-rule-2"
    
    it "finds rules by fact type" $ do
      let rule1 = FactObservationRule
            { ruleId = "test-rule-1"
            , factType = BalanceObservation
            , path = PathExpression "ethereum" "account.balance" Map.empty
            , proof = StateProof
            , conditions = [ExistsCondition "balance"]
            , description = Just "Test rule 1"
            , enabled = True
            }
          rule2 = FactObservationRule
            { ruleId = "test-rule-2"
            , factType = BalanceObservation
            , path = PathExpression "ethereum" "account.balance" Map.empty
            , proof = StateProof
            , conditions = [ComparisonCondition "balance" ">" (Number 1000000000000000000)]
            , description = Just "Test rule 2"
            , enabled = True
            }
          rule3 = FactObservationRule
            { ruleId = "test-rule-3"
            , factType = TransactionObservation
            , path = PathExpression "bitcoin" "transaction" Map.empty
            , proof = InclusionProof
            , conditions = [ComparisonCondition "amount" ">" (Number 100000000)]
            , description = Just "Test rule 3"
            , enabled = False
            }
          ruleSet = createRuleSet [rule1, rule2, rule3] Map.empty
          
          balanceRules = findRulesByFactType ruleSet BalanceObservation
          transactionRules = findRulesByFactType ruleSet TransactionObservation
          eventRules = findRulesByFactType ruleSet EventObservation
      
      length balanceRules `shouldBe` 2
      length transactionRules `shouldBe` 1
      length eventRules `shouldBe` 0

-- Helper functions
data TestValue = Number Int | Text Text | Bool Bool

instance Eq TestValue where
  Number a == Number b = a == b
  Text a == Text b = a == b
  Bool a == Bool b = a == b
  _ == _ = False 