{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : TimeBandits.Core.FactObservation.Engine
Description : Rule engine for fact observation
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module implements the rule engine for processing fact observation rules,
evaluating blockchain data against defined rules, and generating facts.
-}
module TimeBandits.Core.FactObservation.Engine
  ( -- * Engine Types
    RuleEngine(..)
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
  , getRules
  
    -- * Fact Generation
  , generateFact
  , generateFactWithProof
  
    -- * Compatibility Fields (used in tests)
  , ruleDirectories
  , schemaDirectory
  , proofEnabled
  , logVerbosity
  , factRuleId
  , factConfidence
  
    -- * Legacy Config Fields (for backward compatibility)
  , configRulesDirectory
  , configFactsDirectory
  , configProofsEnabled
  , configValidateRules
  , configLogLevel
  , configMaxConcurrent
  ) where

import Control.Exception (try, catch, SomeException)
import Control.Monad (forM, filterM, when, unless, void, foldM)
import Data.Aeson (Value(..), Object, (.=), object, fromJSON, Result(..), FromJSON, ToJSON, encode, decode, parseJSON, toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import Data.List (find, foldl, (!!))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension)
import System.IO.Error (isDoesNotExistError)
import Crypto.Hash (hash, SHA256(..), Digest)
import qualified Crypto.Hash as Hash
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import System.IO (hPutStrLn, stderr)
import System.Random (randomIO)
import GHC.Generics (Generic)

-- Import TimeBandits modules
import TimeBandits.Core.FactObservation.Rules as Rules
import TimeBandits.Core.FactObservation.Schema
import TimeBandits.Core.ResourceId (ResourceId)
import TimeBandits.Core.Types (Hash(..))
import qualified TimeBandits.Core.Types as Types

-- | Configuration for the rule engine
data EngineConfig = EngineConfig
  { ruleDirectories :: [FilePath]  -- ^ Directories containing rule files
  , schemaDirectory :: FilePath    -- ^ Directory containing schema files
  , proofEnabled :: Bool           -- ^ Whether to generate proofs
  , logVerbosity :: Int            -- ^ Log verbosity level (0-3)
  }

-- | Rule engine for fact observation
data RuleEngine = RuleEngine
  { engineConfig :: EngineConfig   -- ^ Engine configuration
  , ruleSets :: Map Text RuleSet   -- ^ Rule sets by name
  , schemas :: Map Text Schema     -- ^ Schemas by name
  }

-- | Errors that can occur during rule engine operations
data EngineError
  = RuleLoadError Text             -- ^ Error loading rules
  | SchemaLoadError Text           -- ^ Error loading schemas
  | ValidationError Text           -- ^ Rule validation error
  | EvaluationError Text           -- ^ Error during rule evaluation
  | ProofGenerationError Text      -- ^ Error generating proof
  | InternalEngineError Text       -- ^ Internal engine error
  deriving (Show, Eq)

-- | Result of fact evaluation
data FactResult = FactResult
  { factType :: FactType           -- ^ Type of the fact
  , factValue :: Value             -- ^ The fact value
  , factTimestamp :: UTCTime       -- ^ When the fact was generated
  , factSource :: Text             -- ^ Source of the fact
  , factProof :: Maybe Value       -- ^ Optional proof of the fact
  , factHash :: Hash               -- ^ Hash of the fact
  }

-- | Create a new rule engine with the given configuration
createEngine :: EngineConfig -> IO (Either EngineError RuleEngine)
createEngine config = do
  -- Create an empty engine
  let engine = RuleEngine
        { engineConfig = config
        , ruleSets = Map.empty
        , schemas = Map.empty
        }
  
  -- Load rules from the configured directories
  result <- loadRulesFromDirectory engine (ruleDirectories config)
  case result of
    Left err -> return $ Left err
    Right updatedEngine -> return $ Right updatedEngine

-- | Load rules from a list of directories
loadRulesFromDirectory :: RuleEngine -> [FilePath] -> IO (Either EngineError RuleEngine)
loadRulesFromDirectory engine [] = return $ Right engine
loadRulesFromDirectory engine (dir:dirs) = do
  -- Try to list the directory
  filesResult <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
  case filesResult of
    Left err -> 
      -- If there's an error, just continue with the next directory
      do
        hPutStrLn stderr $ "Warning: Error accessing directory " ++ dir ++ ": " ++ show err
        loadRulesFromDirectory engine dirs
    
    Right files -> do
      -- Filter for rule files (ending in .rule or .json)
      let ruleFiles = filter (\f -> takeExtension f == ".rule" || takeExtension f == ".json") files
      
      -- Load each rule file
      result <- foldM (\acc file -> do
          case acc of
            Left err -> return $ Left err
            Right eng -> loadRuleFile eng (dir </> file)
        ) (Right engine) ruleFiles
      
      -- Continue with the next directory
      case result of
        Left err -> return $ Left err
        Right updatedEngine -> loadRulesFromDirectory updatedEngine dirs

-- | Load a rule from a file
loadRuleFile :: RuleEngine -> FilePath -> IO (Either EngineError RuleEngine)
loadRuleFile engine filePath = do
  -- Check if the file exists
  fileExists <- doesFileExist filePath
  if not fileExists
    then return $ Left $ RuleLoadError $ "Rule file does not exist: " <> T.pack filePath
    else do
      -- Read the file content
      contentResult <- try (TIO.readFile filePath) :: IO (Either SomeException Text)
      case contentResult of
        Left err -> return $ Left $ RuleLoadError $ "Error reading rule file: " <> T.pack (show err)
        Right content -> do
          -- Parse the content as JSON
          case Aeson.eitherDecode (LBS.fromStrict $ BS.pack $ map (fromIntegral . fromEnum) $ T.unpack content) of
            Left err -> return $ Left $ RuleLoadError $ "Error parsing rule file: " <> T.pack err
            Right (rule :: FactObservationRule) -> do
              -- Validate the rule
              case validateRule rule of
                Left validationErr -> 
                  return $ Left $ ValidationError $ "Rule validation error: " <> T.pack (show validationErr)
                Right _ -> do
                  -- Add the rule to the appropriate rule set
                  let ruleSetName = getRuleSetName rule
                      currentRuleSets = ruleSets engine
                      updatedRuleSets = case Map.lookup ruleSetName currentRuleSets of
                        Nothing -> 
                          -- Create a new rule set
                          let newRuleSet = createRuleSet ruleSetName
                              ruleSetWithRule = addRule rule newRuleSet
                          in Map.insert ruleSetName ruleSetWithRule currentRuleSets
                        Just existingRuleSet ->
                          -- Add to existing rule set
                          let updatedRuleSet = addRule rule existingRuleSet
                          in Map.insert ruleSetName updatedRuleSet currentRuleSets
                  
                  -- Return the updated engine
                  return $ Right $ engine { ruleSets = updatedRuleSets }

-- | Get the rule set name for a rule
getRuleSetName :: FactObservationRule -> Text
getRuleSetName rule = 
  -- Use the rule set name if specified, otherwise use "default"
  fromMaybe "default" (ruleSetName rule)

-- | Load rules from a text string
loadRules :: RuleEngine -> Text -> IO (Either EngineError RuleEngine)
loadRules engine rulesText = do
  -- Parse the rules text as JSON
  case Aeson.eitherDecode (LBS.fromStrict $ BS.pack $ map (fromIntegral . fromEnum) $ T.unpack rulesText) of
    Left err -> return $ Left $ RuleLoadError $ "Error parsing rules: " <> T.pack err
    Right (rules :: [FactObservationRule]) -> do
      -- Add each rule to the engine
      foldM (\acc rule -> do
          case acc of
            Left err -> return $ Left err
            Right eng -> do
              -- Validate the rule
              case validateRule rule of
                Left validationErr -> 
                  return $ Left $ ValidationError $ "Rule validation error: " <> T.pack (show validationErr)
                Right _ -> do
                  -- Add the rule to the appropriate rule set
                  let ruleSetName = getRuleSetName rule
                      currentRuleSets = ruleSets eng
                      updatedRuleSets = case Map.lookup ruleSetName currentRuleSets of
                        Nothing -> 
                          -- Create a new rule set
                          let newRuleSet = createRuleSet ruleSetName
                              ruleSetWithRule = addRule rule newRuleSet
                          in Map.insert ruleSetName ruleSetWithRule currentRuleSets
                        Just existingRuleSet ->
                          -- Add to existing rule set
                          let updatedRuleSet = addRule rule existingRuleSet
                          in Map.insert ruleSetName updatedRuleSet currentRuleSets
                  
                  -- Return the updated engine
                  return $ Right $ eng { ruleSets = updatedRuleSets }
        ) (Right engine) rules

-- | Evaluate data against all rules in the engine
evaluateData :: RuleEngine -> Value -> IO [Either EngineError FactResult]
evaluateData engine inputData = do
  -- Get all rule sets
  let allRuleSets = Map.elems (ruleSets engine)
  
  -- Evaluate the data against each rule set
  results <- forM allRuleSets $ \ruleSet ->
    evaluateDataWithRuleSet engine ruleSet inputData
  
  -- Flatten the results
  return $ concat results

-- | Evaluate data against a specific rule
evaluateDataWithRule :: RuleEngine -> FactObservationRule -> Value -> IO (Either EngineError FactResult)
evaluateDataWithRule engine rule inputData = do
  -- Check if the rule conditions match the input data
  let conditionsMatch = all (evaluateCondition inputData) (conditions rule)
  
  if not conditionsMatch
    then return $ Left $ EvaluationError "Rule conditions do not match input data"
    else do
      -- Extract the fact value using the path expression
      let factValueResult = extractFactValue (factPath rule) inputData
      
      case factValueResult of
        Nothing -> 
          return $ Left $ EvaluationError "Failed to extract fact value from input data"
        Just factValue -> do
          -- Generate the fact
          timestamp <- getCurrentTime
          let factSource = ruleId rule
          
          -- Generate proof if enabled
          proofResult <- if proofEnabled (engineConfig engine)
                          then generateProof engine rule inputData
                          else return $ Right Nothing
          
          case proofResult of
            Left err -> return $ Left err
            Right proof -> do
              -- Compute the fact hash
              let factHash = computeFactHash factValue timestamp factSource proof
              
              -- Create the fact result
              return $ Right $ FactResult
                { factType = Rules.factType rule
                , factValue = factValue
                , factTimestamp = timestamp
                , factSource = factSource
                , factProof = proof
                , factHash = factHash
                }

-- | Evaluate data against a rule set
evaluateDataWithRuleSet :: RuleEngine -> RuleSet -> Value -> IO [Either EngineError FactResult]
evaluateDataWithRuleSet engine ruleSet inputData = do
  -- Get all rules in the rule set
  let allRules = getRules ruleSet
  
  -- Evaluate the data against each rule
  forM allRules $ \rule ->
    evaluateDataWithRule engine rule inputData

-- | Evaluate a condition against input data
evaluateCondition :: Value -> Condition -> Bool
evaluateCondition inputData condition =
  case condition of
    -- Implementation would depend on the condition types
    -- This is a placeholder
    _ -> True

-- | Extract a fact value using a path expression
extractFactValue :: PathExpression -> Value -> Maybe Value
extractFactValue pathExpr inputData =
  -- Implementation would depend on the path expression format
  -- This is a placeholder
  Just inputData

-- | Generate a proof for a fact
generateProof :: RuleEngine -> FactObservationRule -> Value -> IO (Either EngineError (Maybe Value))
generateProof engine rule inputData = do
  -- Implementation would depend on the proof type
  -- This is a placeholder
  return $ Right Nothing

-- | Compute the hash of a fact
computeFactHash :: Value -> UTCTime -> Text -> Maybe Value -> Hash
computeFactHash factValue timestamp source proof =
  -- Convert the fact components to a string
  let factString = T.pack $ show factValue ++ show timestamp ++ T.unpack source ++ maybe "" show proof
      -- Compute the SHA256 hash
      digest = hash (BS.pack $ map (fromIntegral . fromEnum) $ T.unpack factString) :: Digest SHA256
      -- Convert the digest to a ByteString
      hashBytes = BS.pack $ map (fromIntegral . fromEnum) $ show digest
  in Hash hashBytes

-- | Generate a fact from input data
generateFact :: RuleEngine -> FactType -> Value -> IO (Either EngineError FactResult)
generateFact engine factType inputData = do
  -- Find rules for the given fact type
  let matchingRules = concatMap (findRulesByFactType factType) (Map.elems (ruleSets engine))
  
  -- If no matching rules, return an error
  if null matchingRules
    then return $ Left $ EvaluationError $ "No rules found for fact type: " <> T.pack (show factType)
    else do
      -- Try each rule until one succeeds
      results <- forM matchingRules $ \rule ->
        evaluateDataWithRule engine rule inputData
      
      -- Return the first successful result, or the last error if there are results
      return $ case filter isRight results of
        (success:_) -> success
        [] -> if null results
              then Left $ EvaluationError "No results from rule evaluation"
              else results !! (length results - 1)  -- Safe because we know results is not empty
  where
    isRight (Right _) = True
    isRight _ = False

-- | Generate a fact with proof
generateFactWithProof :: RuleEngine -> FactType -> Value -> IO (Either EngineError FactResult)
generateFactWithProof engine factType inputData = do
  -- Ensure proof generation is enabled
  let config = engineConfig engine
      configWithProof = config { proofEnabled = True }
      engineWithProof = engine { engineConfig = configWithProof }
  
  -- Generate the fact with proof enabled
  generateFact engineWithProof factType inputData

-- | Get all rules from a rule set
getRules :: RuleSet -> [FactObservationRule]
getRules = rules

-- | Schema type (placeholder)
data Schema = Schema
  { schemaName :: Text
  , schemaDefinition :: Value
  }

-- Compatibility functions for FactResult
factRuleId :: FactResult -> Text
factRuleId = factSource

factConfidence :: FactResult -> Double
factConfidence _ = 0.8  -- Default confidence

-- Legacy config field accessors for backward compatibility
configRulesDirectory :: EngineConfig -> FilePath
configRulesDirectory cfg = 
  case ruleDirectories cfg of
    [] -> ""
    (dir:_) -> dir

configFactsDirectory :: EngineConfig -> FilePath
configFactsDirectory = schemaDirectory

configProofsEnabled :: EngineConfig -> Bool
configProofsEnabled = proofEnabled

configValidateRules :: EngineConfig -> Bool
configValidateRules _ = True

configLogLevel :: EngineConfig -> String
configLogLevel cfg = 
  case logVerbosity cfg of
    0 -> "error"
    1 -> "warn"
    2 -> "info"
    _ -> "debug"

configMaxConcurrent :: EngineConfig -> Int
configMaxConcurrent _ = 4 