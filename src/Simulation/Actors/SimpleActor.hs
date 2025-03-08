{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Simulation.Actors.SimpleActor
Description : Simple actor implementation for testing
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides a simple actor implementation that can be used for
testing the Actor interface. It implements the basic functionality required
by the Actor typeclass.
-}
module Simulation.Actors.SimpleActor
  ( SimpleActor
  , createSimpleActor
  ) where

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar, readMVar, modifyMVar_, forkIO, ThreadId)
import Control.Monad (void, forever, when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime, getCurrentTime)
import Control.Exception (try, SomeException)

import Types.Actor (ActorID, ActorType)
import Core.TimelineId (TimelineID)
import Types.Effect (Fact)
import Core.Effect (Effect)
import Simulation.Actors.Actor

-- | A simple actor implementation for testing
data SimpleActor = SimpleActor
  { simpleConfig   :: ActorConfig    -- ^ Actor configuration
  , simpleContext  :: ActorContext   -- ^ Actor execution context
  , simpleLog      :: MVar [Text]    -- ^ Log of actions taken
  , simpleThread   :: MVar (Maybe ThreadId) -- ^ Actor thread ID
  }

-- | Create a new simple actor
createSimpleActor :: ActorConfig -> IO SimpleActor
createSimpleActor config = do
  context <- createActorContext config
  logMVar <- newMVar []
  threadMVar <- newMVar Nothing
  
  return SimpleActor
    { simpleConfig = config
    , simpleContext = context
    , simpleLog = logMVar
    , simpleThread = threadMVar
    }

-- | Add a log message to the actor's log
addLog :: SimpleActor -> Text -> IO ()
addLog actor message = do
  time <- getCurrentTime
  let timeStr = T.pack $ show time
      logMessage = timeStr <> ": " <> message
  
  modifyMVar_ (simpleLog actor) $ \logs ->
    return (logs ++ [logMessage])

-- Instance of Actor typeclass for SimpleActor
instance Actor SimpleActor where
  -- | Get the actor's ID
  actorId actor = configActorId (simpleConfig actor)
  
  -- | Get the actor's type
  actorType actor = configActorType (simpleConfig actor)
  
  -- | Get the actor's current state
  getState actor = readMVar (contextState $ simpleContext actor)
  
  -- | Execute a command on the actor
  executeCommand actor cmd = do
    addLog actor $ "Executing command: " <> T.pack (show cmd)
    
    case cmd of
      Pause -> do
        modifyMVar_ (contextState $ simpleContext actor) $ \state ->
          if state == Running
            then return Paused
            else return state
        return Success
      
      Resume -> do
        modifyMVar_ (contextState $ simpleContext actor) $ \state ->
          if state == Paused
            then return Running
            else return state
        return Success
      
      Stop -> do
        modifyMVar_ (contextState $ simpleContext actor) $ \_ ->
          return ShuttingDown
        addLog actor "Shutting down..."
        
        -- In a real implementation, we'd wait for the thread to finish
        -- Here we'll just set the state to Stopped
        modifyMVar_ (contextState $ simpleContext actor) $ \_ ->
          return Stopped
        
        return Success
      
      InjectFact fact -> do
        addLog actor $ "Injected fact: " <> T.pack (show fact)
        return Success
      
      CustomCommand text -> do
        addLog actor $ "Custom command: " <> text
        return Success
  
  -- | Start the actor
  start actor = do
    -- Initialize the actor
    initialize actor
    
    -- Create a new thread for the actor
    threadId <- forkIO $ do
      -- Set state to Running
      modifyMVar_ (contextState $ simpleContext actor) $ \_ ->
        return Running
      
      -- Run the main loop
      result <- try $ forever $ do
        -- Check for commands
        cmds <- takeMVar (contextCommands $ simpleContext actor)
        putMVar (contextCommands $ simpleContext actor) []
        
        -- Process each command
        mapM_ (executeCommand actor) cmds
        
        -- Check if we should stop
        state <- readMVar (contextState $ simpleContext actor)
        when (state == ShuttingDown || state == Stopped) $ do
          addLog actor "Actor loop ending"
          modifyMVar_ (contextState $ simpleContext actor) $ \_ ->
            return Stopped
      
      -- Handle any exceptions
      case result of
        Left (e :: SomeException) -> do
          let errorMsg = "Actor error: " <> T.pack (show e)
          addLog actor errorMsg
          modifyMVar_ (contextState $ simpleContext actor) $ \_ ->
            return $ Error errorMsg
        
        Right _ ->
          addLog actor "Actor loop ended normally"
    
    -- Store the thread ID
    modifyMVar_ (simpleThread actor) $ \_ ->
      return $ Just threadId
    
    return threadId
  
  -- | Initialize the actor
  initialize actor = do
    addLog actor "Initializing actor"
    
    -- Initialize with empty command queue
    putMVar (contextCommands $ simpleContext actor) []
    
    -- Set state to Initializing
    modifyMVar_ (contextState $ simpleContext actor) $ \_ ->
      return Initializing
  
  -- | Process incoming effects
  processEffect actor effect = do
    addLog actor $ "Processing effect: " <> T.pack (show effect)
  
  -- | Get information about the actor
  getInfo actor = do
    state <- getState actor
    logs <- readMVar (simpleLog actor)
    
    return $ Map.fromList
      [ ("id", case actorId actor of
                 ActorID aid -> aid)
      , ("type", T.pack $ show $ actorType actor)
      , ("state", T.pack $ show state)
      , ("log_count", T.pack $ show $ length logs)
      , ("last_log", if null logs then "none" else last logs)
      ] 