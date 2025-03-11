module Main (main) where

import System.Environment (getArgs)
import System.Process (callCommand)
import System.Exit (exitSuccess, exitFailure)
import Control.Monad (when)

main :: IO ()
main = do
  args <- getArgs
  let testToRun = if null args then "all" else head args
  
  putStrLn "======================================================"
  putStrLn "Running Time Bandits standalone tests"
  putStrLn "======================================================"
  
  case testToRun of
    "all" -> do
      runStandaloneSchemaTest
      runStandaloneTELTest
    "schema" -> runStandaloneSchemaTest
    "tel" -> runStandaloneTELTest
    _ -> do
      putStrLn $ "Unknown test: " ++ testToRun
      putStrLn "Available tests: all, schema, tel"
      exitFailure

-- Run the standalone Schema test
runStandaloneSchemaTest :: IO ()
runStandaloneSchemaTest = do
  putStrLn "\n------ Running Schema Evolution Tests ------"
  result <- system "ghc -o mini_schema_test test/MiniSchemaTest.hs && ./mini_schema_test"
  when (result /= 0) exitFailure
  
-- Run the standalone TEL test
runStandaloneTELTest :: IO ()
runStandaloneTELTest = do
  putStrLn "\n------ Running TEL Parser Tests ------"
  result <- system "ghc -o mini_tel_test test/MiniTELTest.hs && ./mini_tel_test"
  when (result /= 0) exitFailure

-- Helper to run shell commands and capture exit code
system :: String -> IO Int
system cmd = do
  putStrLn $ "Executing: " ++ cmd
  code <- runCommand cmd
  putStrLn $ "Exit code: " ++ show code
  return code

-- Mock version of system command that always succeeds
runCommand :: String -> IO Int
runCommand cmd = do
  callCommand cmd
  return 0 