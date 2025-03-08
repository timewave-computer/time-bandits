{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Core.FactObservation.TOMLParserSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Either (isRight, isLeft)
import Data.String.QQ
import Data.Maybe (isJust)
import Control.Monad (forM_)

import Core.FactObservation.Rules
import Core.FactObservation.TOMLParser

spec :: Spec
spec = do
  describe "TOML Parser for fact observation rules" $ do
    it "parses a valid rule set" $ do
      let tomlText = [s|
[[rules]]
rule_id = "eth-balance-observation-1"
fact_type = "BalanceObservation"
proof = "StateProof"
enabled = true
description = "Test Ethereum balance observation"

path.source = "ethereum"
path.selector = "account.balance"

[path.parameters]
address = "0x742d35Cc6634C0532925a3b844Bc454e4438f44e"
network = "mainnet"

[[conditions]]
field = "balance"
operator = ">"
value = 1000000000000000000

[[conditions]]
check_field = "last_updated"

[metadata]
version = "1.0.0"
author = "Test Author"
|]
      
      let result = parseRuleSet tomlText
      result `shouldSatisfy` isRight
      
      case result of
        Left err -> fail $ "Failed to parse rule set: " ++ show err
        Right ruleSet -> do
          length (rules ruleSet) `shouldBe` 1
          let rule = head (rules ruleSet)
          
          ruleId rule `shouldBe` "eth-balance-observation-1"
          factType rule `shouldBe` BalanceObservation
          proof rule `shouldBe` StateProof
          enabled rule `shouldBe` True
          description rule `shouldBe` Just "Test Ethereum balance observation"
          
          source (path rule) `shouldBe` "ethereum"
          selector (path rule) `shouldBe` "account.balance"
          Map.lookup "address" (parameters (path rule)) `shouldBe` Just "0x742d35Cc6634C0532925a3b844Bc454e4438f44e"
          Map.lookup "network" (parameters (path rule)) `shouldBe` Just "mainnet"
          
          length (conditions rule) `shouldBe` 2
          
          Map.lookup "version" (metadata ruleSet) `shouldBe` Just "1.0.0"
          Map.lookup "author" (metadata ruleSet) `shouldBe` Just "Test Author"
    
    it "parses a rule with logical conditions" $ do
      let tomlText = [s|
[[rules]]
rule_id = "btc-transaction-observation-1"
fact_type = "TransactionObservation"
proof = "InclusionProof"
enabled = true

path.source = "bitcoin"
path.selector = "transaction"

[[conditions]]
logical_op = "AND"

[[conditions.sub_conditions]]
field = "amount"
operator = ">"
value = 100000000

[[conditions.sub_conditions]]
check_field = "confirmations"
|]
      
      let result = parseRuleSet tomlText
      result `shouldSatisfy` isRight
      
      case result of
        Left err -> fail $ "Failed to parse rule set: " ++ show err
        Right ruleSet -> do
          length (rules ruleSet) `shouldBe` 1
          let rule = head (rules ruleSet)
          
          ruleId rule `shouldBe` "btc-transaction-observation-1"
          factType rule `shouldBe` TransactionObservation
          
          length (conditions rule) `shouldBe` 1
          case head (conditions rule) of
            LogicalCondition op subConds -> do
              op `shouldBe` "AND"
              length subConds `shouldBe` 2
            _ -> fail "Expected logical condition"
    
    it "returns an error for invalid TOML" $ do
      let tomlText = [s|
# Invalid TOML (missing closing bracket)
[[rules]
rule_id = "invalid-rule"
|]
      
      let result = parseRuleSet tomlText
      result `shouldSatisfy` isLeft
    
    it "returns an error for missing required fields" $ do
      let tomlText = [s|
[[rules]]
# Missing rule_id and other required fields
fact_type = "BalanceObservation"
|]
      
      let result = parseRuleSet tomlText
      result `shouldSatisfy` isLeft
    
    it "parses a custom fact type" $ do
      let tomlText = [s|
[[rules]]
rule_id = "custom-fact-rule"
fact_type = "Custom_TokenBalance"
proof = "NoProof"
enabled = true

path.source = "ethereum"
path.selector = "token.balance"
|]
      
      let result = parseRuleSet tomlText
      result `shouldSatisfy` isRight
      
      case result of
        Left err -> fail $ "Failed to parse rule set: " ++ show err
        Right ruleSet -> do
          let rule = head (rules ruleSet)
          factType rule `shouldBe` CustomFact "TokenBalance"
    
    it "parses multiple rules in one file" $ do
      let tomlText = [s|
[[rules]]
rule_id = "rule-1"
fact_type = "BalanceObservation"
proof = "StateProof"
enabled = true

path.source = "ethereum"
path.selector = "account.balance"

[[rules]]
rule_id = "rule-2"
fact_type = "TransactionObservation"
proof = "InclusionProof"
enabled = false

path.source = "bitcoin"
path.selector = "transaction"
|]
      
      let result = parseRuleSet tomlText
      result `shouldSatisfy` isRight
      
      case result of
        Left err -> fail $ "Failed to parse rule set: " ++ show err
        Right ruleSet -> do
          length (rules ruleSet) `shouldBe` 2
          ruleId (rules ruleSet !! 0) `shouldBe` "rule-1"
          ruleId (rules ruleSet !! 1) `shouldBe` "rule-2"
  
  describe "Rule serialization" $ do
    it "serializes and deserializes a rule" $ do
      let rule = FactObservationRule
            { ruleId = "test-rule"
            , factType = BalanceObservation
            , path = PathExpression "ethereum" "account.balance" (Map.fromList [("address", "0x123")])
            , proof = StateProof
            , conditions = [ExistsCondition "balance"]
            , description = Just "Test rule"
            , enabled = True
            }
      
      let serializeResult = serializeRule rule
      serializeResult `shouldSatisfy` isRight
      
      case serializeResult of
        Left err -> fail $ "Failed to serialize rule: " ++ show err
        Right tomlText -> do
          let parseResult = parseRule tomlText
          parseResult `shouldSatisfy` isRight
          
          case parseResult of
            Left err -> fail $ "Failed to parse serialized rule: " ++ show err
            Right parsedRule -> do
              ruleId parsedRule `shouldBe` ruleId rule
              factType parsedRule `shouldBe` factType rule
              source (path parsedRule) `shouldBe` source (path rule)
              selector (path parsedRule) `shouldBe` selector (path rule)
              Map.lookup "address" (parameters (path parsedRule)) `shouldBe` Map.lookup "address" (parameters (path rule))
              proof parsedRule `shouldBe` proof rule
              enabled parsedRule `shouldBe` enabled rule
              description parsedRule `shouldBe` description rule
    
    it "handles different proof types correctly" $ do
      let proofTypes = [InclusionProof, HeaderProof, StateProof, SignatureProof, ReceiptProof, NoProof]
      
      forM_ proofTypes $ \proofType -> do
        let rule = FactObservationRule
              { ruleId = "test-rule"
              , factType = BlockObservation
              , path = PathExpression "ethereum" "block" Map.empty
              , proof = proofType
              , conditions = []
              , description = Nothing
              , enabled = True
              }
        
        let serializeResult = serializeRule rule
        serializeResult `shouldSatisfy` isRight
        
        case serializeResult of
          Left err -> fail $ "Failed to serialize rule with proof type " ++ show proofType ++ ": " ++ show err
          Right tomlText -> do
            let parseResult = parseRule tomlText
            parseResult `shouldSatisfy` isRight
            
            case parseResult of
              Left err -> fail $ "Failed to parse serialized rule with proof type " ++ show proofType ++ ": " ++ show err
              Right parsedRule -> proof parsedRule `shouldBe` proofType 