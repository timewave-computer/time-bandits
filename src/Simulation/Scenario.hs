{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
This module defines the scenario framework for Time Bandits simulation.

A scenario is a predefined sequence of events, actor configurations, and
expected outcomes that can be used to:
  
1. Test the system's behavior in various conditions
2. Demonstrate specific features or capabilities
3. Validate correctness of implementations
4. Benchmark performance under specific conditions

The module provides tools for defining scenarios, loading them from files,
validating their structure, and executing them within a controlled environment.
-}
module Simulation.Scenario
  ( -- * Core Types
    Scenario(..)
  , ScenarioStep(..)
  , ScenarioConfig(..)
  , ScenarioResult(..)
  , StepResult(..)
  
  -- * Scenario Loading
  , loadScenarioFromFile
  , parseScenario
  
  -- * Scenario Execution
  , runScenario
  , executeStep
  , validateScenario
  
  -- * Result Handling
  , summarizeResults
  , formatScenarioReport
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (forM, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import System.FilePath (takeFileName)

import Core.Timeline (TimelineHash)
import Simulation.Messaging (ActorSpec)

-- | Configuration for a scenario
data ScenarioConfig = ScenarioConfig
  { scenarioName :: Text
  , scenarioDescription :: Text
  , scenarioActors :: [ActorSpec]  -- ^ Actors required for this scenario
  , scenarioTimelineConfig :: Map Text Text  -- ^ Timeline-specific settings
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | A single step in a scenario
data ScenarioStep = ScenarioStep
  { stepDescription :: Text
  , stepActor :: Text  -- ^ ID of actor performing this step
  , stepAction :: Text  -- ^ Action to perform (implementation-specific)
  , stepParams :: Map Text Text  -- ^ Parameters for the action
  , stepExpectedResult :: Text  -- ^ Expected outcome of this step
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | A complete scenario definition
data Scenario = Scenario
  { scenarioConfig :: ScenarioConfig
  , scenarioSteps :: [ScenarioStep]
  , scenarioTimeoutSecs :: Int  -- ^ Maximum allowed execution time
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Result of executing a single step
data StepResult = StepResult
  { stepResultSuccess :: Bool
  , stepResultMessage :: Text
  , stepResultDetails :: Map Text Text  -- ^ Additional details about execution
  , stepResultTimelineHash :: Maybe TimelineHash  -- ^ Timeline state after execution
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Complete result of executing a scenario
data ScenarioResult = ScenarioResult
  { scenarioResultName :: Text
  , scenarioResultSuccess :: Bool
  , scenarioResultSteps :: [(ScenarioStep, StepResult)]
  , scenarioResultTotalTime :: Double  -- ^ Total execution time in seconds
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Load a scenario from a file
loadScenarioFromFile :: FilePath -> IO Scenario
loadScenarioFromFile path = do
  contents <- LBS.readFile path
  case eitherDecode contents of
    Left err -> throwIO $ userError $ "Error parsing scenario file: " ++ err
    Right scenario -> pure scenario

-- | Parse a scenario from JSON text
parseScenario :: Text -> Either String Scenario
parseScenario = eitherDecode . LBS.fromStrict . encodeUtf8
  where
    encodeUtf8 = error "Not implemented: requires bytestring package"

-- | Run a complete scenario (placeholder implementation)
runScenario :: Scenario -> IO ScenarioResult
runScenario scenario = do
  -- This would set up the environment and execute the steps
  let config = scenarioConfig scenario
  let name = scenarioName config
  
  -- Placeholder implementation
  let results = map (\step -> (step, placeholderStepResult)) (scenarioSteps scenario)
  
  pure $ ScenarioResult
    { scenarioResultName = name
    , scenarioResultSuccess = all (stepResultSuccess . snd) results
    , scenarioResultSteps = results
    , scenarioResultTotalTime = 0.0  -- Placeholder
    }

-- | Execute a single step in a scenario (placeholder implementation)
executeStep :: ScenarioStep -> IO StepResult
executeStep _ = pure placeholderStepResult

-- | Placeholder step result for demonstration
placeholderStepResult :: StepResult
placeholderStepResult = StepResult
  { stepResultSuccess = True
  , stepResultMessage = "Placeholder execution (not implemented)"
  , stepResultDetails = Map.empty
  , stepResultTimelineHash = Nothing
  }

-- | Validate a scenario definition
validateScenario :: Scenario -> Either Text ()
validateScenario scenario = do
  -- Perform validation checks
  -- This is a placeholder that always succeeds
  Right ()

-- | Generate a summary of scenario execution results
summarizeResults :: ScenarioResult -> Text
summarizeResults result = T.unlines
  [ "Scenario: " <> scenarioResultName result
  , "Success: " <> if scenarioResultSuccess result then "Yes" else "No"
  , "Steps executed: " <> T.pack (show $ length $ scenarioResultSteps result)
  , "Total time: " <> T.pack (show $ scenarioResultTotalTime result) <> "s"
  ]

-- | Format a detailed report of a scenario execution
formatScenarioReport :: ScenarioResult -> Text
formatScenarioReport result = T.unlines
  [ "# Scenario Report: " <> scenarioResultName result
  , ""
  , "Status: " <> if scenarioResultSuccess result then "✅ Success" else "❌ Failure"
  , "Execution time: " <> T.pack (show $ scenarioResultTotalTime result) <> " seconds"
  , ""
  , "## Step Results:"
  , ""
  , stepsReport
  ]
  where
    stepsReport = T.unlines $ map formatStepResult (scenarioResultSteps result)
    
    formatStepResult (step, result) = T.unlines
      [ "### " <> stepDescription step
      , "Actor: " <> stepActor step
      , "Action: " <> stepAction step
      , "Status: " <> if stepResultSuccess result then "✅ Success" else "❌ Failure"
      , "Message: " <> stepResultMessage result
      , ""
      ] 