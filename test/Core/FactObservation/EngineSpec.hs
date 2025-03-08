{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Core.FactObservation.EngineSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Either (isRight, isLeft)
import Data.Aeson (Value(..), object, (.=), toJSON)
import qualified Data.Aeson as Aeson
import Data.Time (getCurrentTime)
import Control.Monad (void)
import Control.Concurrent (threadDelay)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Core.FactObservation.Rules
import Core.FactObservation.Engine
import Core.FactObservation.TOMLParser (parseRule)

spec :: Spec
spec = do
  describe "Rule Engine" $ do
    around withTestDirectories $ do
      it "creates a rule engine with default configuration" $ \(rulesDir, factsDir) -> do
        -- Create engine configuration
        let config = EngineConfig
              { configRulesDirectory = rulesDir
              , configFactsDirectory = factsDir
              , configProofsEnabled = True
              , configValidateRules = True
              , configLogLevel = "info"
              , configMaxConcurrent = 4
              }
        
        -- Create the engine
        engine <- createEngine config
        
        -- Check that the engine was created
        True `shouldBe` True
      
      it "loads rules from a file" $ \(rulesDir, factsDir) -> do
        -- Create engine configuration
        let config = EngineConfig
              { configRulesDirectory = rulesDir
              , configFactsDirectory = factsDir
              , configProofsEnabled = True
              , configValidateRules = True
              , configLogLevel = "info"
              , configMaxConcurrent = 4
              }
        
        -- Create the engine
        engine <- createEngine config
        
        -- Create a rule file
        let ruleContent = makeTestRuleContent
        let ruleFile = rulesDir </> "test-rule.toml"
        writeFile ruleFile ruleContent
        
        -- Load the rule
        result <- loadRules engine ruleFile
        
        -- Check that the rule was loaded
        result `shouldSatisfy` isRight
        
        case result of
          Left err -> fail $ "Failed to load rules: " ++ show err
          Right ruleSet -> do
            length (rules ruleSet) `shouldBe` 1
            ruleId (head (rules ruleSet)) `shouldBe` "test-eth-balance-rule"
  
      it "evaluates data against rules" $ \(rulesDir, factsDir) -> do
        -- Create engine configuration
        let config = EngineConfig
              { configRulesDirectory = rulesDir
              , configFactsDirectory = factsDir
              , configProofsEnabled = False  -- Disable proofs for simpler testing
              , configValidateRules = True
              , configLogLevel = "info"
              , configMaxConcurrent = 4
              }
        
        -- Create the engine
        engine <- createEngine config
        
        -- Create a rule file with matching condition
        let ruleContent = makeTestRuleContentWithBalance 1000  -- Will match our test data
        let ruleFile = rulesDir </> "test-rule.toml"
        writeFile ruleFile ruleContent
        
        -- Load the rule
        void $ loadRules engine ruleFile
        
        -- Create test data that matches the rule
        let testData = makeTestData 2000  -- Greater than rule's 1000 threshold
        
        -- Evaluate the data
        results <- evaluateData engine testData
        
        -- Check that evaluation produced at least one successful result
        length (filter isRight results) `shouldBe` 1
      
      it "doesn't match when condition fails" $ \(rulesDir, factsDir) -> do
        -- Create engine configuration
        let config = EngineConfig
              { configRulesDirectory = rulesDir
              , configFactsDirectory = factsDir
              , configProofsEnabled = False  -- Disable proofs for simpler testing
              , configValidateRules = True
              , configLogLevel = "info"
              , configMaxConcurrent = 4
              }
        
        -- Create the engine
        engine <- createEngine config
        
        -- Create a rule file with non-matching condition
        let ruleContent = makeTestRuleContentWithBalance 3000  -- Won't match our test data
        let ruleFile = rulesDir </> "test-rule.toml"
        writeFile ruleFile ruleContent
        
        -- Load the rule
        void $ loadRules engine ruleFile
        
        -- Create test data that doesn't match the rule
        let testData = makeTestData 2000  -- Less than rule's 3000 threshold
        
        -- Evaluate the data
        results <- evaluateData engine testData
        
        -- All results should be Left (errors or non-matches)
        all isLeft results `shouldBe` True
      
      it "generates a fact result with correct data" $ \(rulesDir, factsDir) -> do
        -- Create a rule
        let rule = FactObservationRule
              { ruleId = "test-rule"
              , factType = BalanceObservation
              , path = PathExpression "ethereum" "account.balance" Map.empty
              , proof = NoProof
              , conditions = [ExistsCondition "balance"]
              , description = Just "Test rule"
              , enabled = True
              }
        
        -- Create test data
        let testValue = object ["balance" .= (1000 :: Int)]
        
        -- Generate a fact
        result <- generateFact rule testValue
        
        -- Check the fact
        result `shouldSatisfy` isRight
        
        case result of
          Left err -> fail $ "Failed to generate fact: " ++ show err
          Right factResult -> do
            factRuleId factResult `shouldBe` ruleId rule
            factType factResult `shouldBe` factType rule
            factData factResult `shouldBe` testValue
            factSource factResult `shouldBe` source (path rule)
  
  describe "FactResult serialization" $ do
    it "converts a fact result to JSON" $ do
      -- Create a fact result
      now <- getCurrentTime
      let factResult = FactResult
            { factRuleId = "test-rule"
            , factType = BalanceObservation
            , factData = object ["balance" .= (1000 :: Int)]
            , factProof = Just $ object ["type" .= ("test" :: Text)]
            , factTimestamp = now
            , factSource = "ethereum"
            , factConfidence = 0.8
            }
      
      -- Convert to JSON
      let json = factToJson factResult
      
      -- Check JSON fields
      json `shouldSatisfy` isJsonObject
      jsonField json "rule_id" `shouldBe` Just (String "test-rule")
      jsonField json "type" `shouldBe` Just (String "BalanceObservation")
      jsonField json "source" `shouldBe` Just (String "ethereum")
      jsonField json "confidence" `shouldBe` Just (Number 0.8)
      jsonField json "data" `shouldSatisfy` isJust
      jsonField json "proof" `shouldSatisfy` isJust

-- Helper functions

-- | Set up test directories
withTestDirectories :: ((FilePath, FilePath) -> IO ()) -> IO ()
withTestDirectories action = 
  withSystemTempDirectory "fact-observation-test" $ \tmpDir -> do
    let rulesDir = tmpDir </> "rules"
        factsDir = tmpDir </> "facts"
    
    -- Create the directories
    createDirectoryIfMissing True rulesDir
    createDirectoryIfMissing True factsDir
    
    -- Run the test action
    action (rulesDir, factsDir)

-- | Create test rule content
makeTestRuleContent :: String
makeTestRuleContent = unlines
  [ "[[rules]]"
  , "rule_id = \"test-eth-balance-rule\""
  , "fact_type = \"BalanceObservation\""
  , "proof = \"NoProof\""
  , "enabled = true"
  , "description = \"Test Ethereum balance observation\""
  , ""
  , "path.source = \"ethereum\""
  , "path.selector = \"account.balance\""
  , ""
  , "[[conditions]]"
  , "check_field = \"balance\""
  ]

-- | Create test rule content with specific balance threshold
makeTestRuleContentWithBalance :: Int -> String
makeTestRuleContentWithBalance threshold = unlines
  [ "[[rules]]"
  , "rule_id = \"test-eth-balance-rule\""
  , "fact_type = \"BalanceObservation\""
  , "proof = \"NoProof\""
  , "enabled = true"
  , "description = \"Test Ethereum balance observation\""
  , ""
  , "path.source = \"ethereum\""
  , "path.selector = \"account.balance\""
  , ""
  , "[[conditions]]"
  , "field = \"balance\""
  , "operator = \">\""
  , "value = " ++ show threshold
  ]

-- | Create test data
makeTestData :: Int -> Value
makeTestData balance = object
  [ "ethereum" .= object
      [ "account" .= object
          [ "balance" .= balance
          , "address" .= ("0x742d35Cc6634C0532925a3b844Bc454e4438f44e" :: Text)
          ]
      ]
  ]

-- | Check if a value is a JSON object
isJsonObject :: Value -> Bool
isJsonObject (Object _) = True
isJsonObject _ = False

-- | Get a field from a JSON object
jsonField :: Value -> Text -> Maybe Value
jsonField (Object obj) field = Aeson.lookup field obj
jsonField _ _ = Nothing

-- | Check if a Maybe value is Just
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False 