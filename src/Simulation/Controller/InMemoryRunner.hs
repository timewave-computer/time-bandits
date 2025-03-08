{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Simulation.Controller.InMemoryRunner
Description : In-memory implementation of the controller
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module implements the in-memory version of the controller interface,
where all actors run in the same process and memory space.
-}
module Simulation.Controller.InMemoryRunner
  ( -- * In-Memory Controller
    InMemoryController
  , createInMemoryController
  , runInMemoryScenario
  
    -- * In-Memory Actor Management
  , createActorInMemory
  , addExistingActor
  ) where

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar, readMVar, modifyMVar_, forkIO, ThreadId)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, writeTVar)
import Control.Exception (try, SomeException)
import Control.Monad (void, forM, forM_, when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime, getCurrentTime)

import Types.Actor (ActorID(..), ActorType(..))
import Core.TimelineId (TimelineID(..))
import Types.Effect (Fact)
import Core.Effect (Effect)
import Simulation.Actors.Actor
import Simulation.Actors.SimpleActor
import Simulation.Scenario.Scenario
import Simulation.Controller.Controller

-- | In-memory controller implementation
data InMemoryController = InMemoryController
  { controller :: Controller  -- ^ Base controller interface
  , context :: ControllerContext  -- ^ Controller context
  }

-- | Create an in-memory controller for a scenario
createInMemoryController :: ControllerConfig -> IO InMemoryController
createInMemoryController config = do
  -- Create the controller context
  stateTVar <- newTVarIO ControllerInitializing
  logsMVar <- newMVar []
  actorsMVar <- newMVar Map.empty
  threadMVar <- newMVar Nothing
  
  let context = ControllerContext
        { controllerConfig = config
        , controllerState = stateTVar
        , controllerLogs = logsMVar
        , controllerActors = actorsMVar
        , controllerMainThread = threadMVar
        }
  
  -- Create the base controller interface
  let controller = Controller
        { queryActorState = \actorId -> getActorStateFromContext context actorId
        , injectFact = \fact -> injectFactToContext context fact
        , observeLog = \actorId -> getActorLogsFromContext context actorId
        , pauseActor = \actorId -> pauseActorInContext context actorId
        , resumeActor = \actorId -> resumeActorInContext context actorId
        , stopScenario = stopControllerContext context
        , getState = readTVarIO (controllerState context)
        , getLogs = readMVar (controllerLogs context)
        , getActorInfo = getActorInfoFromContext context
        }
  
  -- Return the in-memory controller
  return InMemoryController
    { controller = controller
    , context = context
    }

-- | Run a scenario with an in-memory controller
runInMemoryScenario :: Scenario -> IO (Either ControllerError Controller)
runInMemoryScenario scenario = do
  -- Create the controller configuration
  let config = ControllerConfig
        { configScenario = scenario
        , configLogFile = Nothing
        , configParams = Map.empty
        }
  
  -- Create the in-memory controller
  inMemController <- createInMemoryController config
  
  -- Initialize actors from the scenario
  initializeActorsFromScenario inMemController scenario
  
  -- Start the controller
  startInMemoryController inMemController
  
  -- Return the controller interface
  return $ Right $ controller inMemController

-- | Initialize actors from a scenario
initializeActorsFromScenario :: InMemoryController -> Scenario -> IO ()
initializeActorsFromScenario inMemController scenario = do
  -- Add a log entry
  addLogToContext (context inMemController) $ 
    LogInfo $ "Initializing actors from scenario: " <> scenarioName scenario
  
  -- Create actors for each actor specification
  forM_ (scenarioActors scenario) $ \actorSpec -> do
    -- Create actor configuration
    let actorConfig = ActorConfig
          { configActorId = actorId actorSpec
          , configActorType = actorType actorSpec
          , configTimeline = timeline actorSpec
          , configLogFile = Nothing
          , configParams = Map.empty
          }
    
    -- Create the actor
    createActorInMemory inMemController actorConfig
    
    -- Log the creation
    addLogToContext (context inMemController) $ 
      LogInfo $ "Created actor: " <> case actorId actorSpec of ActorID aid -> aid
  
  -- Log completion
  addLogToContext (context inMemController) $ 
    LogInfo $ "Initialized " <> T.pack (show $ length $ scenarioActors scenario) <> " actors"

-- | Start an in-memory controller
startInMemoryController :: InMemoryController -> IO (Either ControllerError ())
startInMemoryController inMemController = do
  -- Get the current state
  currentState <- readTVarIO (controllerState $ context inMemController)
  
  -- Only start if not already running
  case currentState of
    ControllerInitializing -> do
      -- Set state to running
      atomically $ writeTVar (controllerState $ context inMemController) ControllerRunning
      
      -- Add a log entry
      addLogToContext (context inMemController) $ LogInfo "Starting in-memory controller"
      
      -- Start all actors
      actorMap <- readMVar (controllerActors $ context inMemController)
      
      forM_ (Map.toList actorMap) $ \(actorId, AnyActor actor) -> do
        -- Start the actor
        void $ runActor actor
        
        -- Log the start
        addLogToContext (context inMemController) $ 
          LogInfo $ "Started actor: " <> case actorId of ActorID aid -> aid
      
      -- Log completion
      addLogToContext (context inMemController) $ 
        LogInfo $ "Started " <> T.pack (show $ Map.size actorMap) <> " actors"
      
      return $ Right ()
      
    ControllerRunning -> do
      -- Already running
      return $ Right ()
      
    _ -> do
      -- Cannot start in other states
      return $ Left $ ControllerInternalError $
        "Cannot start controller in state: " <> T.pack (show currentState)

-- | Create a new actor in the in-memory controller
createActorInMemory :: InMemoryController -> ActorConfig -> IO (Either ControllerError ActorID)
createActorInMemory inMemController config = do
  -- Create the actor
  actor <- createSimpleActor config
  
  -- Add the actor to the controller
  addExistingActor inMemController actor
  
  -- Return the actor ID
  return $ Right $ configActorId config

-- | Add an existing actor to the in-memory controller
addExistingActor :: Actor a => InMemoryController -> a -> IO ()
addExistingActor inMemController actor = do
  -- Get the actor ID
  let aid = actorId actor
  
  -- Add the actor to the map
  modifyMVar_ (controllerActors $ context inMemController) $ \actorMap ->
    return $ Map.insert aid (AnyActor actor) actorMap
  
  -- Add a log entry
  addLogToContext (context inMemController) $ 
    LogInfo $ "Added actor: " <> case aid of ActorID aid' -> aid' 