{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module: TimeBandits.CLI.Main
Description: The main entry point for the Time-Bandits application.
This module serves as a pure CLI dispatcher that parses command-line arguments and
dispatches to the appropriate functionality based on the specified command.
-}
module CLI.Main where

import Control.Exception (catch, throwIO)
import Control.Monad (when)
import Data.Text qualified as T
import Data.Version (Version, makeVersion, showVersion)
import System.Directory (doesFileExist)
import System.Environment qualified as Env
import System.Exit qualified as Exit
import System.IO (hPutStrLn, stderr)

-- | Command-line options
data Options = Options
  { optVerbose :: Bool
  , optCommand :: Command
  }

-- | Available commands
data Command
  = RunScenario FilePath   -- ^ Run a simulation with a scenario file
  | Version                -- ^ Display version information
  | Help                   -- ^ Show help information

-- | Parse command-line options (simplified version)
parseOptions :: [String] -> Options
parseOptions args = 
  case args of
    ["--verbose", "run", path] -> Options True (RunScenario path)
    ["-v", "run", path] -> Options True (RunScenario path)
    ["run", path] -> Options False (RunScenario path)
    ["version"] -> Options False Version
    ["help"] -> Options False Help
    _ -> Options False Help -- Default to help if unable to parse

-- | Main entry point
main :: IO ()
main = do
  -- Parse command-line arguments
  args <- Env.getArgs
  let options = parseOptions args
  
  -- Execute the command
  case optCommand options of
    RunScenario path -> do
      fileExists <- doesFileExist path
      if fileExists
        then do
          putStrLn $ "Would run scenario: " ++ path
          putStrLn $ "Note: Simulation functionality is not implemented yet."
          Exit.exitSuccess
        else do
          hPutStrLn stderr $ "Scenario file not found: " ++ path
          Exit.exitFailure
    
    Version -> do
      putStrLn $ "Time Bandits version " ++ showVersion version
      Exit.exitSuccess
    
    Help -> do
      -- Display help information
      putStrLn "Time Bandits - A temporal state management system"
      putStrLn ""
      putStrLn "Available commands:"
      putStrLn "  run SCENARIO_FILE  Run a simulation with the specified scenario file"
      putStrLn "  version            Display version information"
      putStrLn "  help               Display this help information"
      putStrLn ""
      putStrLn "Options:"
      putStrLn "  -v, --verbose      Enable verbose output"
      Exit.exitSuccess

-- | Version information
version :: Version
version = makeVersion [0, 1, 0]
