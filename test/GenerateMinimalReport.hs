{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (try, SomeException)
import Control.Monad (when)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import qualified System.Environment as Env
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)
import Data.Maybe (isJust)

-- Helper for string to text conversion
toText :: String -> Text
toText = T.pack

main :: IO ()
main = do
  -- Get output directory from args or use default
  args <- Env.getArgs
  let reportDir = case args of
        (dir:_) -> dir
        [] -> "test-reports"
  
  -- Create output directory
  baseDir <- getCurrentDirectory
  let reportPath = baseDir </> reportDir
  createDirectoryIfMissing True reportPath
  
  putStrLn $ "Generating test report in: " ++ reportPath
  putStrLn "Running tests..."
  
  -- Run tests and capture output
  (exitCode, stdout, stderr) <- runTestCommand
  
  -- Generate report from test output
  report <- generateReport exitCode stdout stderr
  
  -- Write report to file
  now <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now
      fileName = "test_report_" <> timestamp <> ".md"
      fullPath = reportPath </> fileName
      latestPath = reportPath </> "latest_report.md"
  
  -- Write to both timestamped and latest files
  TIO.writeFile fullPath report
  TIO.writeFile latestPath report
  
  putStrLn $ "Test report written to: " ++ fullPath
  putStrLn $ "Latest report available at: " ++ latestPath

-- | Run tests and capture output
runTestCommand :: IO (ExitCode, String, String)
runTestCommand = do
  -- First, check if we are in a Nix environment
  isNix <- isJust <$> Env.lookupEnv "IN_NIX_SHELL"
  
  -- Use appropriate command based on environment
  let command :: String
      command = if isNix then "nix" else "cabal"
      
      args :: [String]
      args = if isNix then ["build", ".#time-bandits-test"] else ["test"]
      
      cmdLine :: String
      cmdLine = command ++ " " ++ unwords args
  
  putStrLn $ "Running " ++ cmdLine ++ "..."
  
  -- Try to execute the command
  result <- try $ readProcessWithExitCode command args ""
  case result of
    Left (e :: SomeException) -> do
      -- If Nix command fails, try falling back to cabal
      if isNix 
        then do
          putStrLn $ "Nix command failed: " ++ show e
          putStrLn "Falling back to cabal test..."
          fallbackResult <- try $ readProcessWithExitCode "cabal" ["test"] ""
          case fallbackResult of
            Left (e2 :: SomeException) ->
              return (ExitFailure 1, "", "Failed to run tests with both nix and cabal: " ++ show e ++ "\n" ++ show e2)
            Right result' -> return result'
        else
          return (ExitFailure 1, "", "Failed to run tests: " ++ show e)
    Right output -> return output

-- | Generate a markdown report from test output
generateReport :: ExitCode -> String -> String -> IO Text
generateReport exitCode stdout stderr = do
  now <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
      status = case exitCode of
        ExitSuccess -> "✅ Tests passed"
        ExitFailure _ -> "❌ Tests failed"
      
      -- Extract test results from stdout
      testResults = extractTestResults stdout
      
      -- Generate report content
      reportContent = T.unlines
        [ "# Time Bandits Test Report"
        , ""
        , "Generated: " <> T.pack timestamp
        , ""
        , "## Summary"
        , ""
        , "Status: " <> T.pack status
        , ""
        , "## Test Results"
        , ""
        , testResults
        , ""
        , "## Raw Output"
        , ""
        , "```"
        , T.pack stdout
        , "```"
        , ""
        , "## Errors"
        , ""
        , if T.null (T.pack stderr)
            then "_No errors_"
            else "```\n" <> T.pack stderr <> "\n```"
        ]
  
  return reportContent

-- | Extract and format test results from stdout
extractTestResults :: String -> Text
extractTestResults output = 
  let
    -- Simple heuristic: look for lines containing test results
    lines = filter isTestResult (T.lines (T.pack output))
    
    -- If no test results found, return a message
    resultLines = 
      if null lines 
      then ["_No test results found in output_"] 
      else lines
    
    -- Format as a list
    formatted = T.unlines $ map (\line -> "- " <> line) resultLines
  in
    formatted

-- | Check if a line looks like a test result
isTestResult :: Text -> Bool
isTestResult line =
  let 
    indicators = ["test", "spec", "should", "✓", "✗", "PASS", "FAIL"]
  in
    any (`T.isInfixOf` line) indicators 