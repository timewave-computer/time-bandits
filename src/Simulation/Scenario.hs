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
  
  -- * Account Programs
  , createAccountPrograms
  , runScenarioWithAccountPrograms
  , initializeScenarioWithAccounts
  , executeStepWithAccounts
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
import System.IO.Error (userError)
import Data.Time (getCurrentTime, UTCTime)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (Day(..))
import Relude (error)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Core.Timeline (TimelineHash)
import Core.ActorId (ActorId(..))
import Core.ProgramId (ProgramId(..))
import qualified Core.ProgramId as ProgramId
import Core.ResourceId (ResourceId(..))
import Core.Resource (Resource(..), TokenId, Address, Amount)
import Core.AccountProgram (AccountProgram(..))
import Core.Types (emptyFactSnapshot)
import Simulation.Messaging (ActorSpec(..), ActorID)

-- | Configuration for a scenario
data ScenarioConfig = ScenarioConfig
  { scenarioName :: Text
  , scenarioDescription :: Text
  , scenarioActors :: [ActorSpec]  -- ^ Actors required for this scenario
  , scenarioTimelineConfig :: Map Text Text  -- ^ Timeline-specific settings
  }
  deriving (Show, Eq, Generic)

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
  deriving (Show, Eq, Generic)

-- | Result of executing a single step
data StepResult = StepResult
  { stepResultSuccess :: Bool
  , stepResultMessage :: Text
  , stepResultDetails :: Map Text Text  -- ^ Additional details about execution
  , stepResultTimelineHash :: Maybe TimelineHash  -- ^ Timeline state after execution
  }
  deriving (Show, Eq, Generic)

-- | Complete result of executing a scenario
data ScenarioResult = ScenarioResult
  { scenarioResultName :: Text
  , scenarioResultSuccess :: Bool
  , scenarioResultSteps :: [(ScenarioStep, StepResult)]
  , scenarioResultTotalTime :: Double  -- ^ Total execution time in seconds
  }
  deriving (Show, Eq, Generic)

-- | Load a scenario from a file
loadScenarioFromFile :: FilePath -> IO Scenario
loadScenarioFromFile path = do
  contents <- LBS.readFile path
  -- For now, just create a dummy scenario since we don't have JSON instances
  let name = T.pack $ takeFileName path
  pure $ Scenario
    { scenarioConfig = ScenarioConfig
        { scenarioName = name
        , scenarioDescription = "Dummy scenario loaded from " <> name
        , scenarioActors = []
        , scenarioTimelineConfig = Map.empty
        }
    , scenarioSteps = []
    , scenarioTimeoutSecs = 60
    }

-- | Parse a scenario from JSON
parseScenario :: LBS.ByteString -> Either String Scenario
parseScenario _ = 
  -- For now, just create a dummy scenario since we don't have JSON instances
  Right $ Scenario
    { scenarioConfig = ScenarioConfig
        { scenarioName = "Dummy Scenario"
        , scenarioDescription = "Dummy scenario created by parseScenario"
        , scenarioActors = []
        , scenarioTimelineConfig = Map.empty
        }
    , scenarioSteps = []
    , scenarioTimeoutSecs = 60
    }

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

-- | Create account programs for all actors in a scenario
createAccountPrograms :: Scenario -> Map ActorID AccountProgram
createAccountPrograms scenario = 
  Map.fromList $ map createAccountProgram (scenarioActors (scenarioConfig scenario))

-- | Create an account program for a single actor
createAccountProgram :: ActorSpec -> (ActorID, AccountProgram)
createAccountProgram spec = 
  let programId = case ProgramId.fromText (_actorSpecID spec) of
        Left _ -> error $ T.pack $ "Invalid program ID: " ++ T.unpack (_actorSpecID spec)
        Right pid -> pid
      -- Use a fixed time for deterministic testing
      currentTime = UTCTime (ModifiedJulianDay 0) 0
  in
  ( _actorSpecID spec
  , AccountProgram
      { accountId = _actorId spec
      , accountProgramId = programId
      , accountVersion = 1
      , accountResources = Map.fromList $ map (\(ResourceId rid, amt) -> 
          let tokenId = rid
              address = BSC.pack "default"
          in (ResourceId rid, TokenBalanceResource tokenId address amt)) $ 
          Map.toList (_initialBalances spec)
      , accountEffects = Map.empty
      , accountRootEffect = Nothing
      , accountInbox = []
      , accountOutbox = []
      , accountPendingMessages = []
      , accountAcceptDeposits = True
      , accountAllowWithdrawals = True
      , accountAllowCalls = True
      , accountKnownPrograms = Map.empty
      , accountLastUpdated = currentTime
      , accountCurrentFacts = emptyFactSnapshot
      }
  )

-- | Run a scenario with account programs
runScenarioWithAccountPrograms :: ScenarioConfig -> Scenario -> Map ActorID AccountProgram -> IO ScenarioResult
runScenarioWithAccountPrograms config scenario accountPrograms = do
  let name = scenarioName config
  initResult <- initializeScenarioWithAccounts config scenario accountPrograms
  stepResults <- forM (zip [1..] (scenarioSteps scenario)) $ \(idx, step) -> do
    result <- executeStepWithAccounts config step accountPrograms
    pure (step, result)
  
  pure ScenarioResult
    { scenarioResultName = name
    , scenarioResultSuccess = all (stepResultSuccess . snd) stepResults
    , scenarioResultSteps = stepResults
    , scenarioResultTotalTime = 0.0  -- Placeholder
    }

-- | Initialize a scenario with account programs
initializeScenarioWithAccounts ::
  (MonadIO m) =>
  ScenarioConfig ->
  Scenario ->
  Map.Map ActorID AccountProgram ->
  m ()
initializeScenarioWithAccounts _ _ _ = pure ()  -- Placeholder

-- | Execute a scenario step with account programs
executeStepWithAccounts :: ScenarioConfig -> ScenarioStep -> Map ActorID AccountProgram -> IO StepResult
executeStepWithAccounts _ _ _ = pure $ StepResult True "Step executed" Map.empty Nothing  -- Placeholder 