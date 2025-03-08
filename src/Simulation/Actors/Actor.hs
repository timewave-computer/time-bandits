{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Simulation.Actors.Actor
Description : Unified actor interface for simulation
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module defines a unified interface for all actors (traders, keepers, bandits)
in the Time Bandits simulation system. It provides a common way to start, stop,
and interact with actors across different simulation modes.
-}
module Simulation.Actors.Actor
  ( -- * Actor Interface
    Actor(..)
  , ActorState(..)
  , ActorConfig(..)
  , ActorContext(..)
  , ActorCommand(..)
  , ActorResult(..)
  
    -- * Actor Creation
  , createActor
  , runActor
  , sendCommand
  
    -- * Actor State
  , getActorState
  , pauseActor
  , resumeActor
  , stopActor
  ) where

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar, readMVar, forkIO, ThreadId)
import Control.Monad (void, when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, getCurrentTime)

import Types.Actor (ActorID, ActorType)
import Core.TimelineId (TimelineID)
import Types.Effect (Fact)
import Core.Effect (Effect)

-- | Actor state data structure
data ActorState
  = Initializing       -- ^ Actor is initializing
  | Running            -- ^ Actor is running normally
  | Paused             -- ^ Actor is paused
  | ShuttingDown       -- ^ Actor is in the process of shutting down
  | Stopped            -- ^ Actor has stopped
  | Error Text         -- ^ Actor encountered an error
  deriving (Show, Eq)

-- | Actor configuration
data ActorConfig = ActorConfig
  { configActorId   :: ActorID        -- ^ Actor's unique identifier
  , configActorType :: ActorType      -- ^ Type of actor
  , configTimeline  :: Maybe TimelineID  -- ^ Associated timeline (for TimeKeepers)
  , configLogFile   :: Maybe FilePath -- ^ Where to write logs
  , configParams    :: Map Text Text   -- ^ Additional configuration parameters
  } deriving (Show, Eq)

-- | Actor execution context
data ActorContext = ActorContext
  { contextConfig :: ActorConfig      -- ^ Actor configuration
  , contextState  :: MVar ActorState  -- ^ Current actor state
  , contextCommands :: MVar [ActorCommand]  -- ^ Command queue
  , contextStartTime :: UTCTime       -- ^ When the actor started
  } deriving (Eq)

-- | Commands that can be sent to an actor
data ActorCommand
  = Pause              -- ^ Pause the actor
  | Resume             -- ^ Resume a paused actor
  | Stop               -- ^ Stop the actor
  | InjectFact Fact    -- ^ Inject a fact into the actor
  | CustomCommand Text -- ^ Custom command specific to actor type
  deriving (Show, Eq)

-- | Results returned from actor command execution
data ActorResult
  = Success            -- ^ Command executed successfully
  | NotSupported Text  -- ^ Command not supported by this actor
  | Failed Text        -- ^ Command failed to execute
  deriving (Show, Eq)

-- | The Actor typeclass defines the interface for all actors
class Actor a where
  -- | Get the actor's ID
  actorId :: a -> ActorID
  
  -- | Get the actor's type
  actorType :: a -> ActorType
  
  -- | Get the actor's current state
  getState :: a -> IO ActorState
  
  -- | Execute a command on the actor
  executeCommand :: a -> ActorCommand -> IO ActorResult
  
  -- | Start the actor
  start :: a -> IO ThreadId
  
  -- | Initialize the actor
  initialize :: a -> IO ()
  
  -- | Process incoming effects
  processEffect :: a -> Effect -> IO ()
  
  -- | Get information about the actor
  getInfo :: a -> IO (Map Text Text)

-- | Create a new actor context
createActorContext :: ActorConfig -> IO ActorContext
createActorContext config = do
  stateMVar <- newMVar Initializing
  commandsMVar <- newMVar []
  startTime <- getCurrentTime
  
  return ActorContext
    { contextConfig = config
    , contextState = stateMVar
    , contextCommands = commandsMVar
    , contextStartTime = startTime
    }

-- | Create an actor (to be implemented by specific actor types)
createActor :: ActorConfig -> IO a
createActor = error "createActor must be implemented by specific actor types"

-- | Run an actor in a separate thread
runActor :: Actor a => a -> IO ThreadId
runActor actor = start actor

-- | Send a command to an actor
sendCommand :: Actor a => a -> ActorCommand -> IO ActorResult
sendCommand actor command = executeCommand actor command

-- | Get the current state of an actor
getActorState :: Actor a => a -> IO ActorState
getActorState = getState

-- | Pause an actor
pauseActor :: Actor a => a -> IO ActorResult
pauseActor actor = executeCommand actor Pause

-- | Resume a paused actor
resumeActor :: Actor a => a -> IO ActorResult
resumeActor actor = executeCommand actor Resume

-- | Stop an actor
stopActor :: Actor a => a -> IO ActorResult
stopActor actor = executeCommand actor Stop 