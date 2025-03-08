{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : FactObservationCLI
Description : Command-line interface for fact observation
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides a command-line interface for the fact observation engine,
allowing users to load rules, evaluate data, and manage facts.
-}
module Main where

import Control.Exception (try, SomeException)
import Control.Monad (forM_, when, void)
import Data.Aeson (Value, eitherDecode, encode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Console.GetOpt
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

-- Import from TimeBandits modules
import Core.FactObservation.Rules
import Core.FactObservation.TOMLParser
import Core.FactObservation.Engine
import Core.FactObservation.Schema (validateRuleAgainstSchema, validateRuleSetAgainstSchema)

-- | CLI options
data Options = Options
  { optCommand        :: Command
  , optRulesDir       :: FilePath
  , optFactsDir       :: FilePath
  , optInputFile      :: Maybe FilePath
  , optOutputFile     :: Maybe FilePath
  , optVerbose        :: Bool
  , optProofsEnabled  :: Bool
  , optValidateRules  :: Bool
  } deriving (Show)

-- | Default options
defaultOptions :: Options
defaultOptions = Options
  { optCommand        = Help
  , optRulesDir       = "rules"
  , optFactsDir       = "facts"
  , optInputFile      = Nothing
  , optOutputFile     = Nothing
  , optVerbose        = False
  , optProofsEnabled  = True
  , optValidateRules  = True
  }

-- | CLI commands
data Command
  = LoadRules FilePath            -- ^ Load rules from a file or directory
  | ValidateRules FilePath        -- ^ Validate rules without loading them
  | EvaluateData                  -- ^ Evaluate data against loaded rules
  | ListRules                     -- ^ List all loaded rules
  | ListFacts                     -- ^ List all generated facts
  | Help                          -- ^ Show help
  | Version                       -- ^ Show version
  deriving (Show)

-- | Command-line options
options :: [OptDescr (Options -> Options)]
options =
  [ Option ['c'] ["command"]
      (ReqArg parseCommand "COMMAND")
      "Command to execute (load, validate, evaluate, list-rules, list-facts)"
  , Option ['r'] ["rules-dir"]
      (ReqArg (\d opts -> opts { optRulesDir = d }) "DIR")
      "Directory containing rule files"
  , Option ['f'] ["facts-dir"]
      (ReqArg (\d opts -> opts { optFactsDir = d }) "DIR")
      "Directory to store generated facts"
  , Option ['i'] ["input"]
      (ReqArg (\f opts -> opts { optInputFile = Just f }) "FILE")
      "Input file for data evaluation"
  , Option ['o'] ["output"]
      (ReqArg (\f opts -> opts { optOutputFile = Just f }) "FILE")
      "Output file for results"
  , Option ['v'] ["verbose"]
      (NoArg (\opts -> opts { optVerbose = True }))
      "Enable verbose output"
  , Option [] ["no-proofs"]
      (NoArg (\opts -> opts { optProofsEnabled = False }))
      "Disable proof generation"
  , Option [] ["no-validation"]
      (NoArg (\opts -> opts { optValidateRules = False }))
      "Disable rule validation"
  , Option ['h'] ["help"]
      (NoArg (\opts -> opts { optCommand = Help }))
      "Show help"
  , Option [] ["version"]
      (NoArg (\opts -> opts { optCommand = Version }))
      "Show version"
  ]

-- | Parse a command from a string
parseCommand :: String -> Options -> Options
parseCommand cmd opts = 
  case cmd of
    "load"        -> opts { optCommand = LoadRules (fromMaybe "rules" (optInputFile opts)) }
    "validate"    -> opts { optCommand = ValidateRules (fromMaybe "rules" (optInputFile opts)) }
    "evaluate"    -> opts { optCommand = EvaluateData }
    "list-rules"  -> opts { optCommand = ListRules }
    "list-facts"  -> opts { optCommand = ListFacts }
    "help"        -> opts { optCommand = Help }
    "version"     -> opts { optCommand = Version }
    _             -> opts { optCommand = Help }

-- | Parse command-line arguments
parseArgs :: [String] -> IO Options
parseArgs args = do
  case getOpt Permute options args of
    (actions, [], []) -> do
      -- Apply all options
      let opts = foldl (flip id) defaultOptions actions
      
      -- Handle special case for load command with positional argument
      let opts' = case args of
                    ("load":path:_) -> opts { optCommand = LoadRules path }
                    ("validate":path:_) -> opts { optCommand = ValidateRules path }
                    _ -> opts
      
      return opts'
      
    (_, nonOpts, []) -> do
      hPutStrLn stderr $ "Unrecognized arguments: " ++ unwords nonOpts
      hPutStrLn stderr $ usageInfo usageHeader options
      exitFailure
      
    (_, _, errs) -> do
      hPutStrLn stderr $ concat errs
      hPutStrLn stderr $ usageInfo usageHeader options
      exitFailure

-- | Usage header
usageHeader :: String
usageHeader = "Usage: fact-observation-cli [OPTIONS] COMMAND"

-- | Main entry point
main :: IO ()
main = do
  -- Parse command-line arguments
  args <- getArgs
  opts <- parseArgs args
  
  -- Create directories if they don't exist
  createDirectoryIfMissing True (optRulesDir opts)
  createDirectoryIfMissing True (optFactsDir opts)
  
  -- Create engine configuration
  let config = EngineConfig
        { configRulesDirectory = optRulesDir opts
        , configFactsDirectory = optFactsDir opts
        , configProofsEnabled  = optProofsEnabled opts
        , configValidateRules  = optValidateRules opts
        , configLogLevel       = if optVerbose opts then "debug" else "info"
        , configMaxConcurrent  = 4
        }
  
  -- Create the engine
  engine <- createEngine config
  
  -- Execute the command
  case optCommand opts of
    LoadRules path -> do
      -- Check if the path is a file or directory
      isDir <- doesDirectoryExist path
      isFile <- doesFileExist path
      
      result <- if isDir
                then loadRulesFromDirectory engine path
                else if isFile
                     then loadRules engine path
                     else do
                       hPutStrLn stderr $ "Path not found: " ++ path
                       return $ Left $ RuleLoadError "Path not found"
      
      case result of
        Left err -> do
          hPutStrLn stderr $ "Error: " ++ show err
          exitFailure
          
        Right ruleSet -> do
          putStrLn $ "Successfully loaded " ++ show (length $ rules ruleSet) ++ " rules"
          exitSuccess
    
    ValidateRules path -> do
      -- Check if the path is a file or directory
      isFile <- doesFileExist path
      
      if not isFile
        then do
          hPutStrLn stderr $ "File not found: " ++ path
          exitFailure
        else do
          -- Parse the rule set
          result <- parseRuleSetFromFile path
          case result of
            Left err -> do
              hPutStrLn stderr $ "Parse error: " ++ show err
              exitFailure
              
            Right ruleSet -> do
              -- Validate the rule set
              case validateRuleSetAgainstSchema ruleSet of
                Left valErr -> do
                  hPutStrLn stderr $ "Validation error: " ++ T.unpack valErr
                  exitFailure
                  
                Right () -> do
                  putStrLn $ "Successfully validated " ++ show (length $ rules ruleSet) ++ " rules"
                  exitSuccess
    
    EvaluateData -> do
      -- Check if input file is provided
      case optInputFile opts of
        Nothing -> do
          hPutStrLn stderr "Error: No input file specified"
          exitFailure
          
        Just inputFile -> do
          -- Read the input file
          inputResult <- try $ LBS.readFile inputFile
          case inputResult of
            Left (err :: SomeException) -> do
              hPutStrLn stderr $ "Error reading input file: " ++ show err
              exitFailure
              
            Right inputData -> do
              -- Parse the JSON
              case eitherDecode inputData of
                Left jsonErr -> do
                  hPutStrLn stderr $ "Error parsing JSON: " ++ jsonErr
                  exitFailure
                  
                Right (value :: Value) -> do
                  -- Evaluate the data
                  results <- evaluateData engine value
                  
                  -- Filter for successful results
                  let successResults = catMaybes $ map (either (const Nothing) Just) results
                  
                  -- Print the results
                  putStrLn $ "Evaluated data against rules: " ++ show (length results) ++ " rules processed, " ++ show (length successResults) ++ " facts generated"
                  
                  -- Write to output file if specified
                  case optOutputFile opts of
                    Nothing -> do
                      -- Print to stdout
                      forM_ successResults $ \result -> do
                        let json = AesonPretty.encodePretty $ factToJson result
                        LBS.putStr json
                        putStrLn ""
                        
                    Just outputFile -> do
                      -- Write to file
                      let resultsJson = AesonPretty.encodePretty $ map factToJson successResults
                      writeResult <- try $ LBS.writeFile outputFile resultsJson
                      case writeResult of
                        Left (err :: SomeException) -> do
                          hPutStrLn stderr $ "Error writing output file: " ++ show err
                          exitFailure
                          
                        Right () -> do
                          putStrLn $ "Results written to " ++ outputFile
                          exitSuccess
    
    ListRules -> do
      -- Get the current rule set
      ruleSet <- readLock engine
      
      -- Print the rules
      putStrLn $ "Rules (" ++ show (length $ rules ruleSet) ++ "):"
      forM_ (rules ruleSet) $ \rule -> do
        putStrLn $ "  - " ++ T.unpack (ruleId rule) ++ ": " ++ 
                   show (factType rule) ++ 
                   (if enabled rule then " (enabled)" else " (disabled)")
        case description rule of
          Just desc -> putStrLn $ "    " ++ T.unpack desc
          Nothing -> return ()
      
      exitSuccess
    
    ListFacts -> do
      -- List facts in the facts directory
      let factsDir = optFactsDir opts
      
      -- Check if the directory exists
      dirExists <- doesDirectoryExist factsDir
      if not dirExists
        then do
          hPutStrLn stderr $ "Facts directory not found: " ++ factsDir
          exitFailure
        else do
          -- List all JSON files
          files <- listJsonFiles factsDir
          
          -- Print the files
          putStrLn $ "Facts (" ++ show (length files) ++ "):"
          forM_ files $ \file -> do
            putStrLn $ "  - " ++ file
          
          exitSuccess
    
    Help -> do
      -- Show help
      putStrLn $ usageInfo usageHeader options
      exitSuccess
    
    Version -> do
      -- Show version
      putStrLn "Time Bandits Fact Observation CLI v1.0.0"
      exitSuccess

-- | Read the current rule set from the engine
readLock :: RuleEngine -> IO RuleSet
readLock engine = do
  -- Get the current rule set
  Lock.readLock (engineRules engine)

-- | List all JSON files in a directory
listJsonFiles :: FilePath -> IO [FilePath]
listJsonFiles dir = do
  -- List all files
  filesResult <- try $ listDirectory dir
  case filesResult of
    Left (err :: SomeException) -> do
      hPutStrLn stderr $ "Error listing directory: " ++ show err
      return []
      
    Right files -> do
      -- Filter for JSON files
      let jsonFiles = filter (\f -> takeExtension f == ".json") files
      return jsonFiles

-- | Convert a fact result to JSON
factToJson :: FactResult -> Value
factToJson FactResult{..} = Aeson.object
  [ "rule_id" Aeson..= factRuleId
  , "type" Aeson..= case factType of
               CustomFact name -> name
               other -> T.pack $ show other
  , "data" Aeson..= factData
  , "proof" Aeson..= factProof
  , "timestamp" Aeson..= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" factTimestamp
  , "source" Aeson..= factSource
  , "confidence" Aeson..= factConfidence
  ] 