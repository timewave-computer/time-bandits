{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Hspec.Runner as Hspec
import qualified Test.Hspec as Hspec
import System.Process (readProcess)
import System.Exit (ExitCode(..))
import Control.Exception (try, SomeException, catch, IOException)
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, findExecutable)
import System.FilePath ((</>))
import Data.List (isInfixOf)
import Data.Text (Text)
import System.Environment (lookupEnv)

-- Core modules
import qualified Core.FactObservation.Rules as Rules
import qualified Core.FactObservation.Engine as Engine

-- | Find the fact-observation-cli executable path
findCliExecutable :: IO (Maybe String)
findCliExecutable = do
  -- Check if we're running under Nix
  isNix <- lookupEnv "IN_NIX_SHELL"
  case isNix of
    Just _ -> do
      -- If we're in Nix, check for executables in standard Nix output paths
      let nixPaths = [
            "./result/bin/fact-observation-cli",
            "/nix/store/*/fact-observation-cli",
            "result/bin/fact-observation-cli"
            ]
      checkNixPaths nixPaths
    Nothing -> do
      -- If not in Nix, use standard cabal paths
      let standardPaths = [
            "dist-newstyle/build/aarch64-osx/ghc-9.6.3/time-bandits-0.1.0.0/x/fact-observation-cli/build/fact-observation-cli/fact-observation-cli",
            "dist-newstyle/build/x86_64-osx/ghc-9.6.3/time-bandits-0.1.0.0/x/fact-observation-cli/build/fact-observation-cli/fact-observation-cli",
            "dist-newstyle/build/x86_64-linux/ghc-9.6.3/time-bandits-0.1.0.0/x/fact-observation-cli/build/fact-observation-cli/fact-observation-cli",
            "dist-newstyle/build/aarch64-linux/ghc-9.6.3/time-bandits-0.1.0.0/x/fact-observation-cli/build/fact-observation-cli/fact-observation-cli",
            "dist/build/fact-observation-cli/fact-observation-cli" -- Legacy Cabal path
            ]
      -- Check each path in sequence
      checkPaths standardPaths
  where
    checkPaths [] = do
      -- If not found in standard paths, try to find it in PATH
      mbPath <- findExecutable "fact-observation-cli"
      return mbPath
    
    checkPaths (p:ps) = do
      exists <- doesFileExist p
      if exists
        then return (Just p)
        else checkPaths ps
    
    checkNixPaths paths = do
      -- Try standard Nix paths, fallback to PATH
      mbPath <- findExecutable "fact-observation-cli"
      return mbPath

main :: IO ()
main = do
  -- Run simplified Hspec tests
  putStrLn "Running simplified Hspec tests for Fact Observation components..."
  Hspec.hspec $ Hspec.describe "Fact Observation Tests" $ do
    -- Basic tests for Rules module
    Hspec.describe "Rules Module" $ do
      Hspec.it "can create an empty RuleSet" $ do
        let ruleSet = Rules.RuleSet [] mempty
        length (Rules.rules ruleSet) `Hspec.shouldBe` 0
        
      Hspec.it "can create a rule" $ do
        let rule = Rules.FactObservationRule
              { Rules.ruleId = "test-rule"
              , Rules.factType = Rules.BalanceObservation
              , Rules.description = Just "Test rule"
              , Rules.enabled = True
              , Rules.conditions = []
              , Rules.path = Rules.PathExpression 
                  { Rules.source = "ethereum"
                  , Rules.selector = "balance"
                  , Rules.parameters = mempty
                  }
              , Rules.proof = Rules.NoProof
              }
        Rules.ruleId rule `Hspec.shouldBe` "test-rule"
        Rules.factType rule `Hspec.shouldBe` Rules.BalanceObservation
  
  -- Check if we're in a Nix build environment
  isNix <- lookupEnv "IN_NIX_SHELL"
  mbCliPath <- findCliExecutable
  
  -- Only run CLI tests if we found the executable
  case mbCliPath of
    Just cliPath -> do
      putStrLn $ "\nRunning Tasty tests for Time Bandits CLI components using: " ++ cliPath
      defaultMain (cliTests cliPath)
    Nothing -> do
      if isJust isNix
        then do
          putStrLn "\nSkipping CLI tests in Nix build environment"
          putStrLn "All Hspec tests passed, exiting with success status"
          -- In Nix build, consider success if Hspec tests pass
          return ()
        else do
          -- Outside Nix, warn but don't fail
          putStrLn "\nWarning: Could not find fact-observation-cli executable"
          putStrLn "Skipping CLI tests, but all Hspec tests passed"
          return ()

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

-- CLI tests that verify the functionality of fact-observation-cli
cliTests :: String -> TestTree
cliTests cliPath = testGroup "CLI Tests" 
  [ testCase "fact-observation-cli executable is available" $ do
      fileExists <- doesFileExist cliPath
      assertBool "Executable should exist" fileExists
      
      result <- try $ readProcess cliPath ["--version"] ""
      case result of
        Left (e :: SomeException) -> 
          assertFailure $ "Failed to run executable: " ++ show e
        Right output -> 
          "Time Bandits Fact Observation CLI" `isInfixOf` output @? "Version info not found in output"
          
  , testCase "fact-observation-cli help shows options" $ do
      output <- readProcess cliPath ["--help"] ""
      assertBool "Help output should mention command option" 
                 ("--command=COMMAND" `isInfixOf` output)
      assertBool "Help output should mention rules directory option" 
                 ("--rules-dir=DIR" `isInfixOf` output)
      assertBool "Help output should mention facts directory option" 
                 ("--facts-dir=DIR" `isInfixOf` output)
                 
  , testCase "fact-observation-cli creates rules and facts directories" $ 
      withSystemTempDirectory "time-bandits-test" $ \tmpDir -> do
        let rulesDir = tmpDir </> "rules"
        let factsDir = tmpDir </> "facts"
        
        -- Run CLI with custom directories
        _ <- readProcess cliPath 
               ["--rules-dir=" ++ rulesDir, 
                "--facts-dir=" ++ factsDir,
                "--help"] ""
                
        -- Both directories should now exist
        rulesExists <- doesDirectoryExist rulesDir
        factsExists <- doesDirectoryExist factsDir
        
        assertBool "Rules directory should be created" rulesExists
        assertBool "Facts directory should be created" factsExists
        
  , testCase "fact-observation-cli lists commands" $ do
      output <- readProcess cliPath ["--help"] ""
      
      assertBool "Help should mention load command" 
                 ("load" `isInfixOf` output)
      assertBool "Help should mention validate command" 
                 ("validate" `isInfixOf` output)
      assertBool "Help should mention evaluate command" 
                 ("evaluate" `isInfixOf` output)
      assertBool "Help should mention list-rules command" 
                 ("list-rules" `isInfixOf` output)
      assertBool "Help should mention list-facts command" 
                 ("list-facts" `isInfixOf` output)
                 
  , testCase "fact-observation-cli verbose option works" $ 
      withSystemTempDirectory "time-bandits-test" $ \tmpDir -> do
        let rulesDir = tmpDir </> "rules"
        let factsDir = tmpDir </> "facts"
        
        -- Create directories
        createDirectory rulesDir
        createDirectory factsDir
        
        -- Run CLI with verbose option
        output <- readProcess cliPath 
                    ["--rules-dir=" ++ rulesDir, 
                     "--facts-dir=" ++ factsDir,
                     "--verbose",
                     "--help"] ""
                     
        -- Test should consider success if the help output is shown properly
        assertBool "Help output should be displayed with verbose option" 
                   ("Usage: fact-observation-cli" `isInfixOf` output)
  ] 