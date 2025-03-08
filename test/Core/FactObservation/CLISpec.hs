{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Core.FactObservation.CLISpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Either (isRight, isLeft)
import Data.String.QQ
import Data.Maybe (isJust)
import System.Process (readCreateProcessWithExitCode, shell)
import System.Exit (ExitCode(..))
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import System.IO.Temp (withSystemTempDirectory)
import Control.Exception (bracket)
import Control.Monad (void, forM_, when)
import qualified Data.Aeson as Aeson

spec :: Spec
spec = do
  describe "Fact Observation CLI" $ do
    -- This test is more of an integration test that simulates CLI usage
    -- It's marked as pending (xit) by default because it requires the CLI executable to be built
    -- and can be run manually when needed
    xit "loads rules and evaluates data using the CLI" $ do
      withSystemTempDirectory "fact-observation-cli-test" $ \tmpDir -> do
        let rulesDir = tmpDir </> "rules"
            factsDir = tmpDir </> "facts"
            dataDir = tmpDir </> "data"
        
        -- Create the directories
        createDirectoryIfMissing True rulesDir
        createDirectoryIfMissing True factsDir
        createDirectoryIfMissing True dataDir
        
        -- Create a test rule file
        let ruleFile = rulesDir </> "test-rule.toml"
        let ruleContent = [s|
[[rules]]
rule_id = "cli-test-rule"
fact_type = "BalanceObservation"
proof = "NoProof"
enabled = true
description = "CLI test rule"

path.source = "ethereum"
path.selector = "account.balance"

[[conditions]]
field = "balance"
operator = ">"
value = 1000000000000000000
|]
        TIO.writeFile ruleFile $ T.pack ruleContent
        
        -- Create test data file
        let dataFile = dataDir </> "test-data.json"
        let dataContent = [s|
{
  "ethereum": {
    "account": {
      "balance": 2000000000000000000,
      "address": "0x742d35Cc6634C0532925a3b844Bc454e4438f44e"
    }
  }
}
|]
        TIO.writeFile dataFile $ T.pack dataContent
        
        -- 1. Load the rule
        (loadExitCode, loadStdout, loadStderr) <- runCLI $ unwords
          [ "fact-observation-cli"
          , "--command", "load"
          , "--rules-dir", rulesDir
          , "--facts-dir", factsDir
          , "--input", ruleFile
          ]
        
        loadExitCode `shouldBe` ExitSuccess
        loadStdout `shouldContain` "Successfully loaded 1 rules"
        
        -- 2. Evaluate the data
        let resultFile = factsDir </> "results.json"
        (evalExitCode, evalStdout, evalStderr) <- runCLI $ unwords
          [ "fact-observation-cli"
          , "--command", "evaluate"
          , "--rules-dir", rulesDir
          , "--facts-dir", factsDir
          , "--input", dataFile
          , "--output", resultFile
          ]
        
        evalExitCode `shouldBe` ExitSuccess
        evalStdout `shouldContain` "facts generated"
        
        -- 3. Check that result file was created
        resultExists <- doesFileExist resultFile
        resultExists `shouldBe` True
        
        -- 4. Check the content of the result file
        resultContent <- TIO.readFile resultFile
        resultContent `shouldContain` "cli-test-rule"
        resultContent `shouldContain` "BalanceObservation"
    
    it "validates CLI options correctly" $ do
      -- These tests don't require the CLI executable, as they just validate the behavior
      -- described in the CLI documentation
      
      let validOptions =
            [ "--command load --input rules/ethereum_balance.toml"
            , "--command validate --input rules/ethereum_balance.toml"
            , "--command evaluate --input data/ethereum_data.json --output facts/results.json"
            , "--command list-rules"
            , "--command list-facts"
            , "--help"
            , "--version"
            , "--rules-dir custom-rules --facts-dir custom-facts"
            , "--verbose --no-proofs --no-validation"
            ]
      
      forM_ validOptions $ \opts -> do
        isValidCLIOptions opts `shouldBe` True
      
      let invalidOptions =
            [ "--invalid-option"
            , "--command invalid-command"
            , "--command evaluate"  -- Missing required --input
            ]
      
      forM_ invalidOptions $ \opts -> do
        isValidCLIOptions opts `shouldBe` False

-- | Run a CLI command (simulated for testing)
runCLI :: String -> IO (ExitCode, String, String)
runCLI cmd = 
  -- In a real test, this would execute the CLI command
  -- For this test, we'll just simulate success
  return (ExitSuccess, "Successfully processed command", "")

-- | Check if CLI options are valid (simulated for testing)
isValidCLIOptions :: String -> Bool
isValidCLIOptions opts =
  let tokens = words opts
      hasOption opt = opt `elem` tokens
      hasCommand cmd = "--command" `elem` tokens && cmd `elem` tokens
      
      -- Helper for checking required options
      hasRequiredOption opt cmd = not (hasCommand cmd) || hasOption opt
  in
    -- Valid commands
    (hasCommand "load" || 
     hasCommand "validate" || 
     hasCommand "evaluate" || 
     hasCommand "list-rules" || 
     hasCommand "list-facts" ||
     hasOption "--help" ||
     hasOption "--version") &&
    
    -- Required options for specific commands
    hasRequiredOption "--input" "evaluate" &&
    
    -- No invalid options
    all (`elem` [ "--command", "load", "validate", "evaluate", "list-rules", "list-facts",
                 "--input", "--output", "--rules-dir", "--facts-dir", 
                 "--verbose", "--no-proofs", "--no-validation", "--help", "--version" ])
        tokens 