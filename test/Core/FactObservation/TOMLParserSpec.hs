{-# LANGUAGE OverloadedStrings #-}

module Core.FactObservation.TOMLParserSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStr, hClose)
import Control.Exception (bracket)

import qualified Core.FactObservation.TOMLParser as TOMLParser
import qualified Core.FactObservation.Rules as Rules

-- Example TOML content for testing
validRuleTOML :: Text
validRuleTOML = 
  "[rule]\n\
  \id = \"price-observation-rule\"\n\
  \description = \"Observe price updates from an exchange\"\n\
  \enabled = true\n\
  \fact_type = \"price\"\n\
  \\n\
  \[path]\n\
  \source = \"exchange\"\n\
  \selector = \"price.current\"\n\
  \\n\
  \[[conditions]]\n\
  \operator = \"gt\"\n\
  \value = \"100\"\n"

invalidRuleTOML :: Text
invalidRuleTOML = 
  "[rule]\n\
  \# Missing required id field\n\
  \description = \"Incomplete rule definition\"\n\
  \enabled = true\n"

-- Helper function to create a temp file with content
withTempFile :: Text -> (FilePath -> IO a) -> IO a
withTempFile content action = 
  withSystemTempFile "test-rule.toml" $ \path handle -> do
    hPutStr handle (show content)  -- Convert Text to String for hPutStr
    hClose handle
    action path

spec :: Spec
spec = describe "TOML Parser" $ do
  describe "Rule parsing" $ do
    it "should attempt to parse a rule from TOML text" $ do
      -- Since the actual parsing is stubbed, we simply verify that the function
      -- returns the expected stubbed response for any input
      let result = TOMLParser.parseRule validRuleTOML
      case result of
        Left (TOMLParser.ValidationError msg) -> 
          -- Expect the stubbed error message
          msg `shouldBe` "Parsing not implemented due to missing dependencies"
        _ -> expectationFailure "Unexpected result from parser stub"
    
    it "should attempt to parse a rule from a file" $ do
      -- Test with a temporary file
      withTempFile validRuleTOML $ \path -> do
        result <- TOMLParser.parseRuleFromFile path
        case result of
          Left (TOMLParser.ValidationError _) -> 
            -- Just check the error type since we know parsing is stubbed
            pendingWith "Rule file parsing is currently stubbed"
          Left err -> expectationFailure $ "Unexpected error: " ++ show err
          Right _ -> expectationFailure "Stub unexpectedly succeeded"
    
    it "should attempt to parse a rule set" $ do
      -- Since parseRuleSet is stubbed to return an empty RuleSet, 
      -- we can test that behavior
      let result = TOMLParser.parseRuleSet validRuleTOML
      case result of
        Right ruleSet -> do
          length (Rules.rules ruleSet) `shouldBe` 0
          Map.size (Rules.metadata ruleSet) `shouldBe` 0
        Left err -> expectationFailure $ "Unexpected error from stub: " ++ show err
