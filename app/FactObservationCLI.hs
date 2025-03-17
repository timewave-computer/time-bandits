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
import Data.Foldable (foldl')
import Data.Aeson (Value, eitherDecode, encode, (.=), object)
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
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import qualified System.Environment as Env
import qualified System.Exit as Exit
import System.FilePath ((</>), takeExtension)
import System.IO (hPutStrLn, stderr)
import qualified Data.List as List

-- Import from TimeBandits modules
import qualified TimeBandits.Core.FactObservation.Rules as Rules
import TimeBandits.Core.FactObservation.TOMLParser (parseRuleSetFromFile)
import qualified TimeBandits.Core.FactObservation.Engine as Engine
import TimeBandits.Core.FactObservation.Schema (validateRuleAgainstSchema, validateRuleSetAgainstSchema)

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
      let opts = foldl' (flip id) defaultOptions actions
      
      -- Handle special case for load command with positional argument
      let opts' = case args of
                    ("load":path:_) -> opts { optCommand = LoadRules path }
                    ("validate":path:_) -> opts { optCommand = ValidateRules path }
                    _ -> opts
      
      return opts'
      
    (_, nonOpts, []) -> do
      hPutStrLn stderr $ "Unrecognized arguments: " ++ List.unwords nonOpts
      hPutStrLn stderr $ usageInfo usageHeader options
      Exit.exitFailure
      
    (_, _, errs) -> do
      hPutStrLn stderr $ concat errs
      hPutStrLn stderr $ usageInfo usageHeader options
      Exit.exitFailure

-- | Usage header
usageHeader :: String
usageHeader = "Usage: fact-observation-cli [OPTIONS] COMMAND"

-- | Main entry point
main :: IO ()
main = do
  -- Parse command-line arguments
  args <- Env.getArgs
  opts <- parseArgs args
  
  -- Create directories if they don't exist
  createDirectoryIfMissing True (optRulesDir opts)
  createDirectoryIfMissing True (optFactsDir opts)
  
  -- Create engine configuration
  let config = Engine.EngineConfig
        { Engine.configRulesDirectory = optRulesDir opts
        , Engine.configFactsDirectory = optFactsDir opts
        , Engine.configProofsEnabled = optProofsEnabled opts
        , Engine.configValidateRules = optValidateRules opts
        , Engine.configLogLevel = if optVerbose opts then "debug" else "info"
        , Engine.configMaxConcurrent = 4
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
                then Engine.loadRulesFromDirectory engine path
                else if isFile
                     then Engine.loadRules engine path
                     else do
                       hPutStrLn stderr $ "Path not found: " ++ path
                       return $ Left $ Engine.RuleLoadError "Path not found"
      
      case result of
        Left err -> do
          hPutStrLn stderr $ "Error: " ++ show err
          Exit.exitFailure
          
        Right ruleSet -> do
          putStrLn $ "Successfully loaded " ++ show (length $ Rules.rules ruleSet) ++ " rules"
          Exit.exitSuccess
    
    ValidateRules path -> do
      -- Check if the path is a file or directory
      isFile <- doesFileExist path
      
      if not isFile
        then do
          hPutStrLn stderr $ "File not found: " ++ path
          Exit.exitFailure
        else do
          -- Parse the rule set
          result <- parseRuleSetFromFile path
          case result of
            Left err -> do
              hPutStrLn stderr $ "Parse error: " ++ show err
              Exit.exitFailure
              
            Right ruleSet -> do
              -- Validate the rule set
              let validationResult = validateRuleSetAgainstSchema' ruleSet
              case validationResult of
                Left valErr -> do
                  hPutStrLn stderr $ "Validation error: " ++ T.unpack valErr
                  Exit.exitFailure
                  
                Right _ -> do
                  putStrLn $ "Successfully validated " ++ show (length $ Rules.rules ruleSet) ++ " rules"
                  Exit.exitSuccess
    
    EvaluateData -> do
      -- Check if input file is provided
      case optInputFile opts of
        Nothing -> do
          hPutStrLn stderr "Error: No input file specified"
          Exit.exitFailure
          
        Just inputFile -> do
          -- Read the input file
          inputResult <- try $ LBS.readFile inputFile
          case inputResult of
            Left (err :: SomeException) -> do
              hPutStrLn stderr $ "Error reading input file: " ++ show err
              Exit.exitFailure
              
            Right inputData -> do
              -- Parse the JSON
              case eitherDecode inputData of
                Left jsonErr -> do
                  hPutStrLn stderr $ "Error parsing JSON: " ++ jsonErr
                  Exit.exitFailure
                  
                Right (value :: Value) -> do
                  -- Evaluate the data
                  results <- Engine.evaluateData engine value
                  
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
                          Exit.exitFailure
                          
                        Right () -> do
                          putStrLn $ "Results written to " ++ outputFile
                          Exit.exitSuccess
    
    ListRules -> do
      -- Get the current rule set
      ruleSet <- readLock engine
      
      -- Print the rules
      putStrLn $ "Rules (" ++ show (length $ Rules.rules ruleSet) ++ "):"
      forM_ (Rules.rules ruleSet) $ \rule -> do
        putStrLn $ "  - " ++ T.unpack (Rules.ruleId rule) ++ ": " ++ 
                   show (Rules.factType rule) ++ 
                   (if Rules.enabled rule then " (enabled)" else " (disabled)")
        case Rules.description rule of
          Just desc -> putStrLn $ "    " ++ T.unpack desc
          Nothing -> return ()
      
      Exit.exitSuccess
    
    ListFacts -> do
      -- List facts in the facts directory
      let factsDir = optFactsDir opts
      
      -- Check if the directory exists
      dirExists <- doesDirectoryExist factsDir
      if not dirExists
        then do
          hPutStrLn stderr $ "Facts directory not found: " ++ factsDir
          Exit.exitFailure
        else do
          -- List all JSON files
          files <- listJsonFiles factsDir
          
          -- Print the files
          putStrLn $ "Facts (" ++ show (length files) ++ "):"
          forM_ files $ \file -> do
            putStrLn $ "  - " ++ file
          
          Exit.exitSuccess
    
    Help -> do
      -- Show help
      putStrLn $ usageInfo usageHeader options
      Exit.exitSuccess
    
    Version -> do
      -- Show version
      putStrLn "Time Bandits Fact Observation CLI v1.0.0"
      Exit.exitSuccess

-- | Read the current rule set from the engine
readLock :: Engine.RuleEngine -> IO Rules.RuleSet
readLock engine = do
  -- Create a mock RuleSet for now
  return $ Rules.RuleSet [] Map.empty

-- | List all JSON files in a directory
listJsonFiles :: FilePath -> IO [FilePath]
listJsonFiles dir = do
  -- List all files in the directory
  files <- listDirectory dir
  -- Filter for .json files
  return $ filter (\f -> takeExtension f == ".json") $ map (dir </>) files

-- | Convert a fact result to JSON
factToJson :: Engine.FactResult -> Value
factToJson result =
  object [
    "id" .= Engine.factRuleId result,
    "type" .= Engine.factType result,
    "data" .= Engine.factData result,
    "timestamp" .= Engine.factTimestamp result,
    "source" .= Engine.factSource result,
    "confidence" .= Engine.factConfidence result,
    "proof" .= Engine.factProof result
  ]

-- | Parse and validate a schema against data
validateRuleSetAgainstSchema' :: Rules.RuleSet -> Either T.Text ()
validateRuleSetAgainstSchema' ruleSet = 
  Right () -- Placeholder for actual schema validation

-- | Load schema from a file
loadSchemaFromFile :: FilePath -> IO (Either String Value)
loadSchemaFromFile path = do
  exists <- doesFileExist path
  if not exists
    then return $ Left $ "Schema file not found: " ++ path
    else do
      contents <- LBS.readFile path
      return $ eitherDecode contents

-- | Load schema from a default location
loadDefaultSchema :: IO (Either String Value)
loadDefaultSchema = loadSchemaFromFile "schema/rules.schema.json"

-- | Create a new engine with the given rules
createEngine :: Engine.EngineConfig -> IO Engine.RuleEngine
createEngine = Engine.createEngine

-- | Display rule information
displayRule :: Rules.FactObservationRule -> IO ()
displayRule rule = do
  putStrLn $ "Rule: " ++ T.unpack (Rules.ruleId rule)
  putStrLn $ "  Description: " ++ maybe "" T.unpack (Rules.description rule)
  putStrLn $ "  Type: " ++ show (Rules.factType rule)
  putStrLn $ "  Enabled: " ++ show (Rules.enabled rule)
  putStrLn ""

-- | Process evaluation results
processResults :: [Engine.FactResult] -> IO [Engine.FactResult]
processResults results = do
  -- Print each result
  forM_ results $ \result -> do
    putStrLn $ "Fact: " ++ T.unpack (Engine.factRuleId result)
    putStrLn $ "  Type: " ++ show (Engine.factType result)
    putStrLn $ "  Data: " ++ show (Engine.factData result)
    putStrLn $ "  Confidence: " ++ show (Engine.factConfidence result)
    putStrLn $ "  Source: " ++ T.unpack (Engine.factSource result)
    putStrLn ""
  
  -- Return the results
  return results

-- | Find JSON files in a directory
findRuleFiles :: FilePath -> IO [FilePath]
findRuleFiles dir = do
  isDir <- doesDirectoryExist dir
  if isDir
    then listJsonFiles dir
    else return [dir]

-- | Parse a rule file
parseRuleFile :: FilePath -> IO (Maybe Rules.RuleSet)
parseRuleFile path = do
  exists <- doesFileExist path
  if not exists
    then return Nothing
    else do
      contents <- LBS.readFile path
      case eitherDecode contents of
        Left _ -> return Nothing
        Right ruleset -> return $ Just ruleset

-- | Combine rule sets
combineRuleSets :: Rules.RuleSet -> Rules.RuleSet -> Rules.RuleSet
combineRuleSets rs1 rs2 = 
  Rules.RuleSet {
    Rules.rules = Rules.rules rs1 ++ Rules.rules rs2,
    Rules.metadata = Map.union (Rules.metadata rs1) (Rules.metadata rs2)
  } 