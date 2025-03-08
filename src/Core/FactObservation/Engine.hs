{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Core.FactObservation.Engine
Description : Rule engine for fact observation
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module implements the rule engine for processing fact observation rules,
evaluating blockchain data against defined rules, and generating facts.
-}
module Core.FactObservation.Engine
  ( -- * Engine Types
    RuleEngine
  , EngineConfig(..)
  , EngineError(..)
  , FactResult(..)
  
    -- * Engine Operations
  , createEngine
  , loadRules
  , loadRulesFromDirectory
  , evaluateData
  , evaluateDataWithRule
  , evaluateDataWithRuleSet
  
    -- * Fact Generation
  , generateFact
  , generateFactWithProof
  ) where

import Control.Exception (try, catch, SomeException)
import Control.Monad (forM, filterM, when, unless, void)
import Data.Aeson (Value(..), Object, (.=), object, fromJSON, Result(..))
import qualified Data.Aeson as Aeson
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Directory (listDirectory, doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>), takeExtension)
import System.IO (hPutStrLn, stderr)

-- Import from TimeBandits modules
import Core.FactObservation.Rules
import Core.FactObservation.TOMLParser
import Core.FactObservation.Schema (validateRuleAgainstSchema, validateRuleSetAgainstSchema)
import Core.Concurrency.Lock (ResourceLock, withLock)
import qualified Core.Concurrency.Lock as Lock
import Core.Effect.Log (EffectLog, logInfo, logError, logDebug)
import qualified Core.Effect.Log as Log

-- | Configuration for the rule engine
data EngineConfig = EngineConfig
  { configRulesDirectory :: FilePath        -- ^ Directory containing rule files
  , configFactsDirectory :: FilePath        -- ^ Directory to store generated facts
  , configProofsEnabled  :: Bool            -- ^ Whether to generate proofs
  , configValidateRules  :: Bool            -- ^ Whether to validate rules against schema
  , configLogLevel       :: Text            -- ^ Log level (debug, info, error)
  , configMaxConcurrent  :: Int             -- ^ Maximum concurrent rule evaluations
  } deriving (Show, Eq)

-- | Default engine configuration
defaultConfig :: EngineConfig
defaultConfig = EngineConfig
  { configRulesDirectory = "rules"
  , configFactsDirectory = "facts"
  , configProofsEnabled  = True
  , configValidateRules  = True
  , configLogLevel       = "info"
  , configMaxConcurrent  = 4
  }

-- | Error type for rule engine operations
data EngineError
  = RuleLoadError Text              -- ^ Error loading rules
  | RuleValidationError Text        -- ^ Error validating rules
  | DataEvaluationError Text        -- ^ Error evaluating data
  | FactGenerationError Text        -- ^ Error generating facts
  | ProofGenerationError Text       -- ^ Error generating proofs
  | StorageError Text               -- ^ Error storing facts or proofs
  deriving (Show, Eq)

-- | Result of fact evaluation
data FactResult = FactResult
  { factRuleId      :: Text         -- ^ ID of the rule that generated the fact
  , factType        :: FactType     -- ^ Type of the fact
  , factData        :: Value        -- ^ The fact data
  , factProof       :: Maybe Value  -- ^ Optional proof data
  , factTimestamp   :: UTCTime      -- ^ When the fact was generated
  , factSource      :: Text         -- ^ Source of the data (e.g., "ethereum")
  , factConfidence  :: Double       -- ^ Confidence level (0.0-1.0)
  } deriving (Show, Eq)

-- | The rule engine
data RuleEngine = RuleEngine
  { engineConfig    :: EngineConfig       -- ^ Engine configuration
  , engineRules     :: ResourceLock RuleSet  -- ^ Thread-safe access to rules
  , engineLogger    :: EffectLog          -- ^ Logger for engine events
  }

-- | Create a new rule engine with the given configuration
createEngine :: EngineConfig -> IO RuleEngine
createEngine config = do
  -- Create directories if they don't exist
  createDirectoryIfMissing True (configRulesDirectory config)
  createDirectoryIfMissing True (configFactsDirectory config)
  
  -- Create a logger
  logger <- Log.createLogger (configLogLevel config)
  
  -- Create an empty rule set with a lock
  rulesLock <- Lock.createLock $ RuleSet [] Map.empty
  
  -- Create the engine
  let engine = RuleEngine
        { engineConfig = config
        , engineRules = rulesLock
        , engineLogger = logger
        }
  
  -- Log engine creation
  logInfo logger "Rule engine created"
  
  return engine

-- | Load rules from a TOML file
loadRules :: RuleEngine -> FilePath -> IO (Either EngineError RuleSet)
loadRules engine filePath = do
  let logger = engineLogger engine
  let validate = configValidateRules (engineConfig engine)
  
  -- Log loading rules
  logInfo logger $ "Loading rules from " <> T.pack filePath
  
  -- Try to parse the rule set
  result <- parseRuleSetFromFile filePath
  case result of
    Left parseErr -> do
      let errMsg = "Failed to parse rules: " <> T.pack (show parseErr)
      logError logger errMsg
      return $ Left $ RuleLoadError errMsg
      
    Right ruleSet -> do
      -- Validate the rule set if enabled
      if validate
        then case validateRuleSetAgainstSchema ruleSet of
          Left valErr -> do
            let errMsg = "Rule validation failed: " <> valErr
            logError logger errMsg
            return $ Left $ RuleValidationError errMsg
            
          Right () -> do
            -- Update the engine's rules
            withLock (engineRules engine) $ \currentRules -> do
              let updatedRules = mergeRuleSets currentRules ruleSet
              logInfo logger $ "Loaded " <> T.pack (show (length $ rules ruleSet)) <> " rules"
              return (updatedRules, Right updatedRules)
        else do
          -- Update without validation
          withLock (engineRules engine) $ \currentRules -> do
            let updatedRules = mergeRuleSets currentRules ruleSet
            logInfo logger $ "Loaded " <> T.pack (show (length $ rules ruleSet)) <> " rules"
            return (updatedRules, Right updatedRules)

-- | Load all rules from a directory
loadRulesFromDirectory :: RuleEngine -> FilePath -> IO (Either EngineError RuleSet)
loadRulesFromDirectory engine dirPath = do
  let logger = engineLogger engine
  
  -- Log loading rules
  logInfo logger $ "Loading rules from directory " <> T.pack dirPath
  
  -- List all files in the directory
  filesResult <- try $ listDirectory dirPath
  case filesResult of
    Left (err :: SomeException) -> do
      let errMsg = "Failed to list directory: " <> T.pack (show err)
      logError logger errMsg
      return $ Left $ RuleLoadError errMsg
      
    Right files -> do
      -- Filter for TOML files
      let tomlFiles = filter (\f -> takeExtension f == ".toml") files
      
      -- Load each file
      results <- forM tomlFiles $ \file -> do
        let fullPath = dirPath </> file
        loadRules engine fullPath
      
      -- Combine the results
      case sequence results of
        Left err -> return $ Left err
        Right ruleSets -> do
          -- Merge all rule sets
          let finalRuleSet = foldl mergeRuleSets (RuleSet [] Map.empty) ruleSets
          
          -- Update the engine's rules
          withLock (engineRules engine) $ \_ -> do
            logInfo logger $ "Loaded " <> T.pack (show (length $ rules finalRuleSet)) <> " rules from " <> T.pack (show (length tomlFiles)) <> " files"
            return (finalRuleSet, Right finalRuleSet)

-- | Evaluate data against all enabled rules
evaluateData :: RuleEngine -> Value -> IO [Either EngineError FactResult]
evaluateData engine inputData = do
  -- Get the current rule set
  currentRuleSet <- Lock.readLock (engineRules engine)
  
  -- Filter for enabled rules
  let enabledRules = filter enabled (rules currentRuleSet)
  
  -- Evaluate against each rule
  forM enabledRules $ \rule -> do
    evaluateDataWithRule engine rule inputData

-- | Evaluate data against a specific rule
evaluateDataWithRule :: RuleEngine -> FactObservationRule -> Value -> IO (Either EngineError FactResult)
evaluateDataWithRule engine rule inputData = do
  let logger = engineLogger engine
  
  -- Log evaluation
  logDebug logger $ "Evaluating data against rule " <> ruleId rule
  
  -- Extract data using the path expression
  extractedData <- extractData (path rule) inputData
  case extractedData of
    Left err -> do
      logError logger $ "Data extraction failed: " <> err
      return $ Left $ DataEvaluationError err
      
    Right extractedValue -> do
      -- Evaluate conditions
      conditionResult <- evaluateConditions (conditions rule) extractedValue
      case conditionResult of
        Left err -> do
          logError logger $ "Condition evaluation failed: " <> err
          return $ Left $ DataEvaluationError err
          
        Right True -> do
          -- Conditions met, generate a fact
          logInfo logger $ "Rule " <> ruleId rule <> " matched, generating fact"
          
          -- Generate proof if enabled
          proofResult <- if configProofsEnabled (engineConfig engine)
                         then generateProof (proof rule) extractedValue inputData
                         else return $ Right Nothing
          
          case proofResult of
            Left err -> do
              logError logger $ "Proof generation failed: " <> err
              return $ Left $ ProofGenerationError err
              
            Right proofValue -> do
              -- Generate the fact
              factResult <- generateFactWithProof rule extractedValue proofValue
              
              -- Store the fact
              storeResult <- storeFact engine factResult
              case storeResult of
                Left err -> return $ Left err
                Right () -> return $ Right factResult
              
        Right False -> do
          -- Conditions not met
          logDebug logger $ "Rule " <> ruleId rule <> " did not match"
          return $ Left $ DataEvaluationError "Conditions not met"

-- | Evaluate data against a specific rule set
evaluateDataWithRuleSet :: RuleEngine -> RuleSet -> Value -> IO [Either EngineError FactResult]
evaluateDataWithRuleSet engine ruleSet inputData = do
  -- Filter for enabled rules
  let enabledRules = filter enabled (rules ruleSet)
  
  -- Evaluate against each rule
  forM enabledRules $ \rule -> do
    evaluateDataWithRule engine rule inputData

-- | Generate a fact from a rule and extracted data
generateFact :: FactObservationRule -> Value -> IO (Either EngineError FactResult)
generateFact rule extractedData = generateFactWithProof rule extractedData Nothing

-- | Generate a fact with an optional proof
generateFactWithProof :: FactObservationRule -> Value -> Maybe Value -> IO (Either EngineError FactResult)
generateFactWithProof rule extractedData proofData = do
  -- Get current time
  now <- getCurrentTime
  
  -- Create the fact result
  let result = FactResult
        { factRuleId = ruleId rule
        , factType = factType rule
        , factData = extractedData
        , factProof = proofData
        , factTimestamp = now
        , factSource = source (path rule)
        , factConfidence = calculateConfidence rule extractedData proofData
        }
  
  return $ Right result

-- | Store a fact in the configured directory
storeFact :: RuleEngine -> FactResult -> IO (Either EngineError ())
storeFact engine factResult = do
  let logger = engineLogger engine
  let factsDir = configFactsDirectory (engineConfig engine)
  
  -- Create a filename based on the fact type and timestamp
  timestamp <- formatTimeString (factTimestamp factResult)
  let filename = factsDir </> T.unpack (factType' <> "-" <> factRuleId factResult <> "-" <> timestamp <> ".json")
      factType' = case factType factResult of
        CustomFact name -> name
        other -> T.pack $ show other
  
  -- Convert the fact to JSON
  let factJson = Aeson.encode $ factToJson factResult
  
  -- Try to write the file
  writeResult <- try $ Aeson.encodeFile filename (factToJson factResult)
  case writeResult of
    Left (err :: SomeException) -> do
      let errMsg = "Failed to store fact: " <> T.pack (show err)
      logError logger errMsg
      return $ Left $ StorageError errMsg
      
    Right () -> do
      logInfo logger $ "Stored fact in " <> T.pack filename
      return $ Right ()

-- Helper functions

-- | Merge two rule sets
mergeRuleSets :: RuleSet -> RuleSet -> RuleSet
mergeRuleSets (RuleSet rules1 meta1) (RuleSet rules2 meta2) =
  RuleSet (rules1 ++ rules2) (Map.union meta1 meta2)

-- | Extract data using a path expression
extractData :: PathExpression -> Value -> IO (Either Text Value)
extractData PathExpression{..} inputData = do
  -- This is a simplified implementation
  -- In a real system, this would use the source and selector to extract data
  -- from various blockchain sources
  
  -- For now, we'll just try to find the data in the input
  case inputData of
    Object obj -> do
      -- Try to find the source in the object
      case Aeson.lookup source obj of
        Nothing -> return $ Left $ "Source '" <> source <> "' not found in input data"
        Just sourceData -> do
          -- Try to find the selector in the source data
          case extractBySelector selector sourceData of
            Nothing -> return $ Left $ "Selector '" <> selector <> "' not found in source data"
            Just extractedData -> return $ Right extractedData
            
    _ -> return $ Left "Input data is not an object"

-- | Extract data using a selector path
extractBySelector :: Text -> Value -> Maybe Value
extractBySelector selector value = 
  case T.splitOn "." selector of
    [] -> Just value
    (key:rest) -> case value of
      Object obj -> do
        nextValue <- Aeson.lookup key obj
        extractBySelector (T.intercalate "." rest) nextValue
      _ -> Nothing

-- | Evaluate conditions against extracted data
evaluateConditions :: [Condition] -> Value -> IO (Either Text Bool)
evaluateConditions [] _ = return $ Right True  -- No conditions means always true
evaluateConditions conditions value = do
  -- Evaluate each condition
  results <- forM conditions $ \condition -> evaluateCondition condition value
  
  -- Check if any evaluation failed
  case sequence results of
    Left err -> return $ Left err
    Right bools -> return $ Right (and bools)  -- All conditions must be true

-- | Evaluate a single condition
evaluateCondition :: Condition -> Value -> IO (Either Text Bool)
evaluateCondition (ComparisonCondition field operator condValue) inputValue = do
  -- Extract the field from the input
  case extractField field inputValue of
    Nothing -> return $ Left $ "Field '" <> field <> "' not found in data"
    Just fieldValue -> do
      -- Compare based on the operator
      case operator of
        "==" -> return $ Right $ fieldValue == condValue
        "!=" -> return $ Right $ fieldValue /= condValue
        ">"  -> return $ compareValues (>) fieldValue condValue
        ">=" -> return $ compareValues (>=) fieldValue condValue
        "<"  -> return $ compareValues (<) fieldValue condValue
        "<=" -> return $ compareValues (<=) fieldValue condValue
        _    -> return $ Left $ "Unknown operator: " <> operator

evaluateCondition (LogicalCondition op subConditions) inputValue = do
  -- Evaluate each sub-condition
  results <- forM subConditions $ \condition -> evaluateCondition condition inputValue
  
  -- Check if any evaluation failed
  case sequence results of
    Left err -> return $ Left err
    Right bools -> case op of
      "AND" -> return $ Right $ and bools
      "OR"  -> return $ Right $ or bools
      "NOT" -> case bools of
                 [b] -> return $ Right $ not b
                 _   -> return $ Left "NOT operator requires exactly one sub-condition"
      _     -> return $ Left $ "Unknown logical operator: " <> op

evaluateCondition (ExistsCondition field) inputValue = do
  -- Check if the field exists
  let exists = isJust $ extractField field inputValue
  return $ Right exists

-- | Extract a field from a value
extractField :: Text -> Value -> Maybe Value
extractField field = extractBySelector field

-- | Compare two JSON values
compareValues :: (Double -> Double -> Bool) -> Value -> Value -> Either Text Bool
compareValues op a b = do
  -- Try to convert both values to numbers
  case (toNumber a, toNumber b) of
    (Just numA, Just numB) -> Right $ op numA numB
    _ -> Left "Cannot compare non-numeric values"

-- | Convert a JSON value to a number
toNumber :: Value -> Maybe Double
toNumber (Number n) = Just $ fromRational $ toRational n
toNumber (String s) = case reads (T.unpack s) of
                        [(n, "")] -> Just n
                        _ -> Nothing
toNumber _ = Nothing

-- | Generate a proof for a fact
generateProof :: ProofType -> Value -> Value -> IO (Either Text (Maybe Value))
generateProof NoProof _ _ = return $ Right Nothing
generateProof proofType extractedData inputData = do
  -- This is a simplified implementation
  -- In a real system, this would generate actual cryptographic proofs
  
  -- For now, we'll just create a simple proof object
  let proofObj = object
        [ "type" .= show proofType
        , "timestamp" .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" =<< getCurrentTime
        , "data_hash" .= ("hash_placeholder" :: Text)
        ]
  
  return $ Right $ Just proofObj

-- | Calculate confidence for a fact
calculateConfidence :: FactObservationRule -> Value -> Maybe Value -> Double
calculateConfidence rule _ proofData =
  -- This is a simplified implementation
  -- In a real system, this would use various factors to determine confidence
  
  -- For now, we'll use a simple heuristic:
  -- - 1.0 if we have a proof
  -- - 0.8 if we don't have a proof but the rule has conditions
  -- - 0.5 if we don't have a proof and the rule has no conditions
  case proofData of
    Just _  -> 1.0
    Nothing -> if null (conditions rule) then 0.5 else 0.8

-- | Format a time as a string for filenames
formatTimeString :: UTCTime -> IO Text
formatTimeString time = do
  let formatted = formatTime defaultTimeLocale "%Y%m%d%H%M%S" time
  return $ T.pack formatted

-- | Convert a fact result to JSON
factToJson :: FactResult -> Value
factToJson FactResult{..} = object
  [ "rule_id" .= factRuleId
  , "type" .= case factType of
               CustomFact name -> name
               other -> T.pack $ show other
  , "data" .= factData
  , "proof" .= factProof
  , "timestamp" .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" factTimestamp
  , "source" .= factSource
  , "confidence" .= factConfidence
  ] 