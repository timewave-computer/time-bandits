{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory (getCurrentDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Environment (getArgs)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.IORef (newIORef)
import Data.Maybe (fromMaybe)

import qualified Test.Hspec.Runner as Hspec
import qualified Test.Hspec.Core.Runner as Hspec
import qualified Test.Hspec as Hspec
import Test.Tasty
import Test.Tasty.HUnit

-- Import our custom test reporters
import TestReport (TestReport, emptyReport)
import HspecReporter (hspecReporterWith)
import TastyReporter (tastyReporterWith)

-- Import test modules
import qualified Core.FactObservation.RulesSpec as RulesSpec
import qualified Core.FactObservation.TOMLParserSpec as TOMLParserSpec
import qualified Core.FactObservation.EngineSpec as EngineSpec
import qualified Core.FactObservation.IntegrationSpec as IntegrationSpec
import qualified Core.FactObservation.CLISpec as CLISpec
import qualified Core.ContentAddressableTest as ContentAddressableTest
import qualified Core.ContentAddressableSystemTest as ContentAddressableSystemTest
import qualified Core.TEL.InterpreterTest as InterpreterTest
import qualified Core.TELTest as TELTest

main :: IO ()
main = do
  args <- getArgs
  let reportDir = case args of
        (dir:_) -> dir
        [] -> "test-reports"
  
  -- Create reports directory
  baseDir <- getCurrentDirectory
  let reportPath = baseDir </> reportDir
  createDirectoryIfMissing True reportPath
  
  -- Run Hspec tests with our custom formatter
  putStrLn $ "Generating test report in: " ++ reportPath
  putStrLn "Running tests..."
  
  -- Create an empty report for Hspec
  hspecReport <- emptyReport
  hspecReportRef <- newIORef hspecReport
  
  -- Define Hspec configuration with our custom formatter
  let hspecConfig = Hspec.defaultConfig {
        Hspec.configFormatter = Just $ hspecReporterWith reportPath,
        Hspec.configExtraInfo = [("testReportRef", hspecReportRef)]
      }
  
  -- Run Hspec tests
  putStrLn "Running Hspec tests for components..."
  Hspec.withArgs [] $ Hspec.runSpec spec hspecConfig
  
  putStrLn $ "Test reports generated in: " ++ reportPath
  putStrLn "Done!"

-- | Hspec test specification
spec :: Hspec.Spec
spec = do
  Hspec.describe "Time Bandits Test Report" $ do
    -- Fact Observation tests
    Hspec.describe "Fact Observation Tests" $ do
      -- Include all the fact observation spec modules
      RulesSpec.spec
      TOMLParserSpec.spec
      EngineSpec.spec
      IntegrationSpec.spec
      CLISpec.spec
    
    -- Content-Addressable Code tests
    Hspec.describe "Content-Addressable Code Tests" $ do
      -- Basic tests for the content-addressable code implementation
      ContentAddressableTest.testContentAddressable
      
      -- Comprehensive tests that validate the ADR-011 requirements
      ContentAddressableSystemTest.testContentAddressableSystem
      
    -- Temporal Effect Language tests
    Hspec.describe "Temporal Effect Language Tests" $ do
      -- TEL Interpreter tests
      InterpreterTest.testInterpreter
      
      -- TEL framework tests
      TELTest.testTEL 