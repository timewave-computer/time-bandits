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
Module: Simulation.Controller
Description: Centralized simulation management

This module provides the simulation controller abstraction, responsible for:

- Setting up and configuring the simulation environment
- Loading and parsing scenario files
- Deploying and coordinating actors (Time Travelers, Time Keepers, Time Bandits)
- Managing the simulation lifecycle
- Collecting and reporting simulation results
- Coordinating account programs for all actors

The Controller is agnostic to the deployment mode (in-memory, local processes, or geo-distributed)
and provides a consistent interface for running simulations.
-}
module Simulation.Controller 
  ( -- * Core Types
    Controller(..)
  , ControllerConfig(..)
  , ControllerSpec(..)
  , SimulationMode(..)
  , ControllerError(..)
  , SimulationResult(..)
  
  -- * Controller Operations
  , runController
  , createController
  , loadScenario
  , runWithScenario
  , determineRequiredActors
  , setSimulationMode
  , getSimulationMode
  
  -- * Actor Management
  , deploySpecializedActors
  , createTimeTravelersForScenario
  , createTimeKeepersForScenario
  , createTimeBanditsForScenario
  
  -- * Account Program Management
  , createAccountProgramsForActors
  , getAccountProgramForActor
  , routeMessageThroughAccount
  ) where

import Control.Exception (Exception, throwIO, catch, throw)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Text (Text)
import Data.Text qualified as T
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, isJust)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)

-- Core imports
import Core.Common (EntityHash, LamportTime(..))
import Core.ActorId (ActorId)
import Core.Resource (ResourceId)
import Core.Timeline (TimelineHash, Timeline)
import Core.Types (AppError)

-- Programs imports
import Programs.Program (Program, ProgramId, ProgramState)
import Programs.AccountProgram (AccountProgram, createAccountProgram)

-- Simulation imports
import Simulation.Messaging (ActorSpec(..), ActorID, Message, ActorRole(..))
import Simulation.Scenario (Scenario(..), ScenarioConfig(..))

-- | Simulation mode determines how the simulation is executed
data SimulationMode
  = InMemoryMode      -- ^ Run everything in a single process
  | LocalProcessMode  -- ^ Run actors as separate local processes
  | DistributedMode   -- ^ Run actors as distributed processes
  deriving (Show, Eq, Generic)
  deriving anyclass (Serialize)

-- | Controller configuration
data ControllerConfig = ControllerConfig
  { configLogPath :: FilePath        -- ^ Path for simulation logs
  , configVerbose :: Bool            -- ^ Verbose logging
  , configMode :: SimulationMode     -- ^ Simulation execution mode
  , configTimeLimit :: Maybe Int     -- ^ Optional time limit in seconds
  , configSeed :: Maybe Int          -- ^ Optional random seed
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Serialize)

-- | Controller specification
data ControllerSpec = ControllerSpec
  { specName :: Text                 -- ^ Simulation name
  , specDescription :: Text          -- ^ Simulation description
  , specScenarioPath :: FilePath     -- ^ Path to scenario file
  , specConfig :: ControllerConfig   -- ^ Controller configuration
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Serialize)

-- | Controller errors
data ControllerError
  = ScenarioLoadError Text           -- ^ Error loading scenario
  | ActorDeploymentError Text        -- ^ Error deploying actors
  | SimulationExecutionError Text    -- ^ Error during simulation execution
  | TimeoutError Text                -- ^ Simulation timed out
  | ConfigurationError Text          -- ^ Invalid configuration
  deriving (Show, Eq, Generic, Exception)

-- | Simulation result
data SimulationResult = SimulationResult
  { resultSuccess :: Bool                    -- ^ Whether simulation completed successfully
  , resultErrors :: [Text]                   -- ^ Any errors encountered
  , resultMetrics :: Map Text Double         -- ^ Performance metrics
  , resultLogs :: [LogEntry]                 -- ^ Simulation logs
  , resultAccountStates :: Map ActorId ProgramState  -- ^ Final account program states
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Serialize)

-- | A simplified log entry for simulation purposes
data LogEntry = LogEntry
  { logTimestamp :: LamportTime
  , logMessage :: Text
  , logSource :: Text
  , logData :: Maybe ByteString
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Serialize)

-- | The simulation controller
data Controller = Controller
  { controllerSpec :: ControllerSpec
  , controllerScenario :: Maybe Scenario
  , controllerActors :: IORef (Map ActorID ActorSpec)
  , controllerAccountPrograms :: IORef (Map ActorId AccountProgram)
  , controllerLogs :: IORef [LogEntry]
  , controllerMetrics :: IORef (Map Text Double)
  , controllerErrors :: IORef [Text]
  , controllerRunning :: IORef Bool
  }

-- | Create a new controller
createController :: MonadIO m => ControllerSpec -> m Controller
createController spec = liftIO $ do
  -- Create the log directory
  createDirectoryIfMissing True (configLogPath $ specConfig spec)
  
  -- Create IORef fields
  actorsRef <- IORef.newIORef Map.empty
  accountsRef <- IORef.newIORef Map.empty
  logsRef <- IORef.newIORef []
  metricsRef <- IORef.newIORef Map.empty
  errorsRef <- IORef.newIORef []
  runningRef <- IORef.newIORef False
  
  pure Controller
    { controllerSpec = spec
    , controllerScenario = Nothing
    , controllerActors = actorsRef
    , controllerAccountPrograms = accountsRef
    , controllerLogs = logsRef
    , controllerMetrics = metricsRef
    , controllerErrors = errorsRef
    , controllerRunning = runningRef
    }

-- | Load a scenario into the controller
loadScenario :: MonadIO m => Controller -> Scenario -> m ()
loadScenario controller scenario = liftIO $ do
  -- Store the scenario
  let controller' = controller { controllerScenario = Just scenario }
  
  -- Determine required actors
  actors <- determineRequiredActors controller' scenario
  IORef.writeIORef (controllerActors controller') actors
  
  -- Log the scenario load
  let scenarioNameText = scenarioName (scenarioConfig scenario)
  let logEntry = LogEntry (LamportTime 0) ("Loaded scenario: " <> scenarioNameText) "Controller" Nothing
  IORef.modifyIORef (controllerLogs controller') (logEntry :)

-- | Run the controller with a scenario
runWithScenario :: MonadIO m => Controller -> Scenario -> m SimulationResult
runWithScenario controller scenario = liftIO $ do
  -- Load the scenario
  loadScenario controller scenario
  
  -- Set controller to running
  IORef.writeIORef (controllerRunning controller) True
  
  -- Deploy actors
  actors <- IORef.readIORef (controllerActors controller)
  deploySpecializedActors controller actors
  
  -- Create account programs for all actors
  createAccountProgramsForActors controller
  
  -- Execute the simulation
  runController controller `catch` \(e :: ControllerError) -> do
    -- Log the error
    let errorMsg = T.pack $ show e
    IORef.modifyIORef (controllerErrors controller) (errorMsg :)
    IORef.writeIORef (controllerRunning controller) False
  
  -- Collect results
  logs <- IORef.readIORef (controllerLogs controller)
  metrics <- IORef.readIORef (controllerMetrics controller)
  errors <- IORef.readIORef (controllerErrors controller)
  accounts <- IORef.readIORef (controllerAccountPrograms controller)
  
  -- Create account states map
  let accountStates = Map.empty  -- Placeholder, would extract states from programs
  
  -- Return the simulation result
  pure SimulationResult
    { resultSuccess = null errors
    , resultErrors = errors
    , resultMetrics = metrics
    , resultLogs = reverse logs
    , resultAccountStates = accountStates
    }

-- | Run the controller
runController :: MonadIO m => Controller -> m ()
runController controller = liftIO $ do
  -- Check if we have a scenario
  scenario <- case controllerScenario controller of
    Just s -> pure s
    Nothing -> throwIO $ ConfigurationError "No scenario loaded"
  
  -- Log simulation start
  let logEntry = LogEntry (LamportTime 0) "Starting simulation" "Controller" Nothing
  IORef.modifyIORef (controllerLogs controller) (logEntry :)
  
  -- Execute scenario steps
  executeScenarioSteps controller scenario
  
  -- Set controller to not running
  IORef.writeIORef (controllerRunning controller) False
  
  -- Log simulation end
  let logEntry' = LogEntry (LamportTime 0) "Simulation completed" "Controller" Nothing
  IORef.modifyIORef (controllerLogs controller) (logEntry' :)

-- | Execute scenario steps
executeScenarioSteps :: MonadIO m => Controller -> Scenario -> m ()
executeScenarioSteps controller scenario = liftIO $ do
  -- Placeholder for scenario execution
  -- In a real implementation, this would:
  -- 1. Initialize the scenario state
  -- 2. Execute each step in sequence
  -- 3. Route messages through account programs
  -- 4. Collect results
  pure ()

-- | Determine required actors for a scenario
determineRequiredActors :: MonadIO m => Controller -> Scenario -> m (Map ActorID ActorSpec)
determineRequiredActors _ scenario = liftIO $ do
  -- Extract actors from scenario
  let actors = scenarioActors (scenarioConfig scenario)
  
  -- Create a map of actor IDs to actor specs
  pure $ Map.fromList [(actorId actor, actor) | actor <- actors]
  where
    actorId :: ActorSpec -> ActorID
    actorId spec = _actorSpecID spec

-- | Set the simulation mode
setSimulationMode :: MonadIO m => Controller -> SimulationMode -> m ()
setSimulationMode controller mode = liftIO $ do
  let spec = controllerSpec controller
      config = specConfig spec
      newConfig = config { configMode = mode }
      newSpec = spec { specConfig = newConfig }
  
  -- Update the controller spec
  let controller' = controller { controllerSpec = newSpec }
  
  -- Log the mode change
  let logEntry = LogEntry (LamportTime 0) ("Set simulation mode to: " <> T.pack (show mode)) "Controller" Nothing
  IORef.modifyIORef (controllerLogs controller') (logEntry :)

-- | Get the current simulation mode
getSimulationMode :: MonadIO m => Controller -> m SimulationMode
getSimulationMode controller = liftIO $ do
  let config = specConfig $ controllerSpec controller
  pure $ configMode config

-- | Deploy specialized actors based on their roles
deploySpecializedActors :: MonadIO m => Controller -> Map ActorID ActorSpec -> m ()
deploySpecializedActors controller actors = liftIO $ do
  -- Group actors by role
  let travelers = Map.filter (\a -> _actorSpecRole a == TimeTravelerRole) actors
      keepers = Map.filter (\a -> _actorSpecRole a == TimeKeeperRole) actors
      bandits = Map.filter (\a -> _actorSpecRole a == TimeBanditRole) actors
  
  -- Deploy each type of actor
  createTimeTravelersForScenario controller travelers
  createTimeKeepersForScenario controller keepers
  createTimeBanditsForScenario controller bandits
  
  -- Log deployment
  let logEntry = LogEntry (LamportTime 0) 
        ("Deployed actors: " <> 
         T.pack (show (Map.size travelers)) <> " travelers, " <>
         T.pack (show (Map.size keepers)) <> " keepers, " <>
         T.pack (show (Map.size bandits)) <> " bandits")
        "Controller" Nothing
  IORef.modifyIORef (controllerLogs controller) (logEntry :)

-- | Create time travelers for the scenario
createTimeTravelersForScenario :: MonadIO m => Controller -> Map ActorID ActorSpec -> m ()
createTimeTravelersForScenario _ _ = liftIO $ do
  -- Placeholder for time traveler creation
  pure ()

-- | Create time keepers for the scenario
createTimeKeepersForScenario :: MonadIO m => Controller -> Map ActorID ActorSpec -> m ()
createTimeKeepersForScenario _ _ = liftIO $ do
  -- Placeholder for time keeper creation
  pure ()

-- | Create time bandits for the scenario
createTimeBanditsForScenario :: MonadIO m => Controller -> Map ActorID ActorSpec -> m ()
createTimeBanditsForScenario _ _ = liftIO $ do
  -- Placeholder for time bandit creation
  pure ()

-- | Create account programs for all actors
createAccountProgramsForActors :: MonadIO m => Controller -> m ()
createAccountProgramsForActors controller = liftIO $ do
  -- Get all actors
  actors <- IORef.readIORef (controllerActors controller)
  
  -- Create account programs for each actor
  accountPrograms <- forM (Map.elems actors) $ \actor -> do
    let actorId = _actorId actor
        initialBalances = _initialBalances actor
    program <- createAccountProgram actorId initialBalances
    pure (actorId, program)
  
  -- Store the account programs
  IORef.writeIORef (controllerAccountPrograms controller) (Map.fromList accountPrograms)
  
  -- Log creation
  let logEntry = LogEntry (LamportTime 0) 
        ("Created account programs for " <> T.pack (show (length accountPrograms)) <> " actors")
        "Controller" Nothing
  IORef.modifyIORef (controllerLogs controller) (logEntry :)

-- | Get the account program for an actor
getAccountProgramForActor :: MonadIO m => Controller -> ActorId -> m (Maybe AccountProgram)
getAccountProgramForActor controller actorId = liftIO $ do
  accounts <- IORef.readIORef (controllerAccountPrograms controller)
  pure $ Map.lookup actorId accounts

-- | Route a message through an actor's account program
routeMessageThroughAccount :: MonadIO m => Controller -> ActorId -> Message -> m (Maybe Message)
routeMessageThroughAccount controller actorId message = liftIO $ do
  -- Get the actor's account program
  accountProgram <- getAccountProgramForActor controller actorId
  
  case accountProgram of
    Just program -> do
      -- Route the message through the account program
      -- This is a placeholder - in a real implementation, this would:
      -- 1. Process the message through the account program
      -- 2. Apply any effects
      -- 3. Return any response message
      
      -- Log the message routing
      let logEntry = LogEntry (LamportTime 0) 
            ("Routed message through account program for actor " <> T.pack (show actorId))
            "Controller" (Just $ "message-data")
      IORef.modifyIORef (controllerLogs controller) (logEntry :)
      
      pure $ Just message  -- Placeholder response
      
    Nothing -> do
      -- Log the error
      let errorMsg = "No account program found for actor " <> T.pack (show actorId)
      IORef.modifyIORef (controllerErrors controller) (errorMsg :)
      pure Nothing
