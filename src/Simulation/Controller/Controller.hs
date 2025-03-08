{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

{- |
Module      : Simulation.Controller.Controller
Description : Controller interface for simulation
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module defines the Controller interface for the Time Bandits simulation
system. The controller abstracts how actors are started, stopped, and
interacted with across different simulation modes.
-}
module Simulation.Controller.Controller
  ( -- * Controller Interface
    Controller(..)
  , ControllerConfig(..)
  , ControllerState(..)
  , ControllerError(..)
  , LogEntry(..)
  
    -- * Controller Creation
  , createController
  , runScenario
  
    -- * Controller Operations
  , startController
  , stopController
  , getControllerState
  
    -- * Actor Management
  , createActorInController
  , getActorState
  , injectFact
  , observeLog
  , pauseActor
  , resumeActor
  , stopActorInController
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

import Types.Actor (ActorID, ActorType)
import Core.TimelineId (TimelineID)
import Types.Effect (Fact)
import Core.Effect (Effect)
import Simulation.Actors.Actor
import Simulation.Scenario.Scenario

-- | Log entry for events in the simulation
data LogEntry
  = LogInfo Text                -- ^ Informational log message
  | LogError Text               -- ^ Error log message
  | LogActorState ActorID ActorState -- ^ Actor state change
  | LogActorEvent ActorID Text  -- ^ Event from an actor
  | LogFact Fact                -- ^ Fact observed or injected
  | LogEffect Effect            -- ^ Effect applied
  deriving (Show, Eq)

-- | Controller state
data ControllerState
  = ControllerInitializing      -- ^ Controller is initializing
  | ControllerRunning           -- ^ Controller is running
  | ControllerStopping          -- ^ Controller is stopping
  | ControllerStopped           -- ^ Controller has stopped
  | ControllerError Text        -- ^ Controller encountered an error
  deriving (Show, Eq)

-- | Controller configuration
data ControllerConfig = ControllerConfig
  { configScenario :: Scenario   -- ^ Scenario to run
  , configLogFile  :: Maybe FilePath -- ^ Where to write logs
  , configParams   :: Map Text Text  -- ^ Additional configuration parameters
  } deriving (Show, Eq)

-- | Controller error
data ControllerError
  = ActorNotFound ActorID       -- ^ Actor not found in the controller
  | InvalidActorState ActorID ActorState -- ^ Actor is in an invalid state
  | ControllerNotRunning        -- ^ Controller is not running
  | ControllerInternalError Text -- ^ Internal controller error
  deriving (Show, Eq)

-- | Abstract controller for managing actors
data Controller = Controller
  { -- | Query the state of an actor
    queryActorState :: ActorID -> IO (Either ControllerError ActorState)
    
    -- | Inject a fact into the simulation
  , injectFact :: Fact -> IO (Either ControllerError ())
    
    -- | Observe logs from an actor
  , observeLog :: ActorID -> IO (Either ControllerError [LogEntry])
    
    -- | Pause an actor
  , pauseActor :: ActorID -> IO (Either ControllerError ())
    
    -- | Resume a paused actor
  , resumeActor :: ActorID -> IO (Either ControllerError ())
    
    -- | Stop the entire scenario
  , stopScenario :: IO (Either ControllerError ())
    
    -- | Get the controller's state
  , getState :: IO ControllerState
    
    -- | Get all logs from the controller
  , getLogs :: IO [LogEntry]
    
    -- | Get information about all actors
  , getActorInfo :: IO (Map ActorID (Map Text Text))
  }

-- | An existential wrapper for actors of any type
data AnyActor = forall a. Actor a => AnyActor a

-- | Controller state container
data ControllerContext = ControllerContext
  { controllerConfig :: ControllerConfig  -- ^ Controller configuration
  , controllerState :: TVar ControllerState -- ^ Current controller state
  , controllerLogs :: MVar [LogEntry]    -- ^ Log entries
  , controllerActors :: MVar (Map ActorID AnyActor) -- ^ Managed actors
  , controllerMainThread :: MVar (Maybe ThreadId) -- ^ Main controller thread
  }

-- | Create a new controller for a scenario
createController :: ControllerConfig -> IO Controller
createController config = do
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
  
  -- Create the controller interface
  return Controller
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

-- | Run a scenario with a controller
runScenario :: ControllerConfig -> IO (Either ControllerError Controller)
runScenario config = do
  -- Create the controller
  controller <- createController config
  
  -- Start the controller
  result <- startController controller
  
  case result of
    Left err -> return $ Left err
    Right () -> return $ Right controller

-- | Start a controller
startController :: Controller -> IO (Either ControllerError ())
startController controller = do
  -- Get the current state
  currentState <- getState controller
  
  -- Only start if not already running
  case currentState of
    ControllerInitializing -> do
      -- Implementation will depend on controller type
      -- This is a simplified version
      return $ Right ()
      
    ControllerStopped -> do
      -- Restart a stopped controller
      return $ Right ()
      
    ControllerRunning -> do
      -- Already running
      return $ Right ()
      
    _ -> do
      -- Cannot start in other states
      return $ Left $ ControllerInternalError $
        "Cannot start controller in state: " <> T.pack (show currentState)

-- | Stop a controller
stopController :: Controller -> IO (Either ControllerError ())
stopController controller = stopScenario controller

-- | Get the state of a controller
getControllerState :: Controller -> IO ControllerState
getControllerState controller = getState controller

-- | Create an actor in a controller
createActorInController :: Controller -> ActorConfig -> IO (Either ControllerError ActorID)
createActorInController _ _ =
  -- Implementation depends on controller type
  return $ Left $ ControllerInternalError "Not implemented"

-- | Get the state of an actor
getActorState :: Controller -> ActorID -> IO (Either ControllerError ActorState)
getActorState controller actorId = queryActorState controller actorId

-- | Observe logs from an actor
observeActorLog :: Controller -> ActorID -> IO (Either ControllerError [LogEntry])
observeActorLog controller actorId = observeLog controller actorId

-- | Pause an actor in the controller
pauseActorInController :: Controller -> ActorID -> IO (Either ControllerError ())
pauseActorInController controller actorId = pauseActor controller actorId

-- | Resume an actor in the controller
resumeActorInController :: Controller -> ActorID -> IO (Either ControllerError ())
resumeActorInController controller actorId = resumeActor controller actorId

-- | Stop an actor in the controller
stopActorInController :: Controller -> ActorID -> IO (Either ControllerError ())
stopActorInController controller actorId = do
  -- First, pause the actor
  pauseResult <- pauseActor controller actorId
  
  case pauseResult of
    Left err -> return $ Left err
    Right () -> do
      -- Implementation depends on controller type
      return $ Left $ ControllerInternalError "Not implemented"

-- Helper functions for controller context

-- | Add a log entry to the controller's log
addLogToContext :: ControllerContext -> LogEntry -> IO ()
addLogToContext context entry = modifyMVar_ (controllerLogs context) $ \logs ->
  return (logs ++ [entry])

-- | Get the state of an actor from the context
getActorStateFromContext :: ControllerContext -> ActorID -> IO (Either ControllerError ActorState)
getActorStateFromContext context actorId = do
  -- Get the actor map
  actorMap <- readMVar (controllerActors context)
  
  -- Look up the actor
  case Map.lookup actorId actorMap of
    Nothing -> return $ Left $ ActorNotFound actorId
    Just (AnyActor actor) -> do
      -- Get the actor's state
      state <- getState actor
      return $ Right state

-- | Inject a fact to the context
injectFactToContext :: ControllerContext -> Fact -> IO (Either ControllerError ())
injectFactToContext context fact = do
  -- Log the fact
  addLogToContext context (LogFact fact)
  
  -- Get the actor map
  actorMap <- readMVar (controllerActors context)
  
  -- Inject the fact to all actors
  forM_ (Map.elems actorMap) $ \(AnyActor actor) -> do
    -- Send the InjectFact command to the actor
    void $ executeCommand actor (InjectFact fact)
  
  return $ Right ()

-- | Get logs from an actor
getActorLogsFromContext :: ControllerContext -> ActorID -> IO (Either ControllerError [LogEntry])
getActorLogsFromContext context actorId = do
  -- Get all logs
  allLogs <- readMVar (controllerLogs context)
  
  -- Filter logs for this actor
  let actorLogs = filter (isLogForActor actorId) allLogs
  
  return $ Right actorLogs
  where
    isLogForActor :: ActorID -> LogEntry -> Bool
    isLogForActor aid (LogActorState aid' _) = aid == aid'
    isLogForActor aid (LogActorEvent aid' _) = aid == aid'
    isLogForActor _ _ = False

-- | Pause an actor in the context
pauseActorInContext :: ControllerContext -> ActorID -> IO (Either ControllerError ())
pauseActorInContext context actorId = do
  -- Get the actor map
  actorMap <- readMVar (controllerActors context)
  
  -- Look up the actor
  case Map.lookup actorId actorMap of
    Nothing -> return $ Left $ ActorNotFound actorId
    Just (AnyActor actor) -> do
      -- Send the Pause command to the actor
      result <- executeCommand actor Pause
      
      -- Log the state change
      state <- getState actor
      addLogToContext context (LogActorState actorId state)
      
      case result of
        Success -> return $ Right ()
        NotSupported msg -> return $ Left $ ControllerInternalError msg
        Failed msg -> return $ Left $ ControllerInternalError msg

-- | Resume an actor in the context
resumeActorInContext :: ControllerContext -> ActorID -> IO (Either ControllerError ())
resumeActorInContext context actorId = do
  -- Get the actor map
  actorMap <- readMVar (controllerActors context)
  
  -- Look up the actor
  case Map.lookup actorId actorMap of
    Nothing -> return $ Left $ ActorNotFound actorId
    Just (AnyActor actor) -> do
      -- Send the Resume command to the actor
      result <- executeCommand actor Resume
      
      -- Log the state change
      state <- getState actor
      addLogToContext context (LogActorState actorId state)
      
      case result of
        Success -> return $ Right ()
        NotSupported msg -> return $ Left $ ControllerInternalError msg
        Failed msg -> return $ Left $ ControllerInternalError msg

-- | Stop the controller context
stopControllerContext :: ControllerContext -> IO (Either ControllerError ())
stopControllerContext context = do
  -- Set state to stopping
  atomically $ writeTVar (controllerState context) ControllerStopping
  
  -- Get the actor map
  actorMap <- readMVar (controllerActors context)
  
  -- Stop all actors
  forM_ (Map.toList actorMap) $ \(actorId, AnyActor actor) -> do
    -- Send the Stop command to the actor
    void $ executeCommand actor Stop
    
    -- Log the state change
    state <- getState actor
    addLogToContext context (LogActorState actorId state)
  
  -- Set state to stopped
  atomically $ writeTVar (controllerState context) ControllerStopped
  
  return $ Right ()

-- | Get information about all actors
getActorInfoFromContext :: ControllerContext -> IO (Map ActorID (Map Text Text))
getActorInfoFromContext context = do
  -- Get the actor map
  actorMap <- readMVar (controllerActors context)
  
  -- Get info for each actor
  actorInfos <- forM (Map.toList actorMap) $ \(actorId, AnyActor actor) -> do
    info <- getInfo actor
    return (actorId, info)
  
  return $ Map.fromList actorInfos 