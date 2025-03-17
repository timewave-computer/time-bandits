{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      : TimeBandits.Core.Log
Description : Logging system for Time Bandits
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides the primary interface to the Time Bandits logging system.
It re-exports the standard logging functionality and provides additional
convenience functions for integrating with other system components.

@since 0.1.0
-}
module TimeBandits.Core.Log
  ( -- * Re-export StandardLog
    module TimeBandits.Core.Log.StandardLog
    
    -- * System Integration
  , getSystemLogStore
  , initializeLogging
  , shutdownLogging
  , withSystemLogging
  
    -- * Context-based Logging
  , logWithContext
  , logComponentInfo
  , logComponentWarning
  , logComponentError
  
    -- * Specialized Logging
  , logNetworkEvent
  , logSecurityEvent
  , logPerformanceMetric
  ) where

import TimeBandits.Core.Log.StandardLog

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.IORef as IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (when)
import Data.Aeson (Value(..), object, (.=))
import Data.Time (getCurrentTime, diffUTCTime)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Control.Exception (bracket)

-- | Global log store for the application
{-# NOINLINE globalLogStore #-}
globalLogStore :: IORef.IORef (Maybe LogStore)
globalLogStore = unsafePerformIO $ IORef.newIORef Nothing

-- | Get the system log store, initializing it if needed
getSystemLogStore :: IO LogStore
getSystemLogStore = do
  mStore <- IORef.readIORef globalLogStore
  case mStore of
    Just store -> return store
    Nothing -> do
      store <- initializeLogging
      return store

-- | Initialize the logging system
initializeLogging :: IO LogStore
initializeLogging = do
  mStore <- IORef.readIORef globalLogStore
  case mStore of
    Just store -> return store
    Nothing -> do
      -- Create log directory in user's home directory
      homeDir <- getHomeDirectory
      let logDir = homeDir </> ".time-bandits" </> "logs"
      
      -- Create log store
      store <- createLogStore (Just logDir)
      
      -- Record creation in log
      _ <- logInfo store "Logging system initialized"
      
      -- Store in global ref
      IORef.writeIORef globalLogStore (Just store)
      return store

-- | Shutdown the logging system
shutdownLogging :: IO ()
shutdownLogging = do
  mStore <- IORef.readIORef globalLogStore
  case mStore of
    Just store -> do
      -- Log shutdown
      _ <- logInfo store "Logging system shutting down"
      
      -- Close store and remove from global ref
      IORef.writeIORef globalLogStore Nothing
    Nothing -> return ()

-- | Run an action with system logging, ensuring proper initialization and cleanup
withSystemLogging :: (LogStore -> IO a) -> IO a
withSystemLogging action = bracket
  initializeLogging
  (\_ -> shutdownLogging)
  action

-- | Log with additional context information
logWithContext :: LogStore -> LogLevel -> Text -> [(Text, Value)] -> IO LogID
logWithContext store level msg context = do
  -- This is just a placeholder for the migration
  logInfo store msg

-- | Log information about a component
logComponentInfo :: LogStore -> Text -> Text -> IO LogID
logComponentInfo store component msg = 
  logWithContext store Info msg [("component", String component)]

-- | Log a component warning
logComponentWarning :: LogStore -> Text -> Text -> IO LogID
logComponentWarning store component msg = 
  logWithContext store Warning msg [("component", String component)]

-- | Log a component error  
logComponentError :: LogStore -> Text -> Text -> IO LogID
logComponentError store component msg = 
  logWithContext store Error msg [("component", String component)]

-- | Log a network event
logNetworkEvent :: LogStore -> Text -> Text -> IO LogID
logNetworkEvent store eventType details = 
  logWithContext store Info details 
    [ ("type", String "network")
    , ("network_event", String eventType)
    ]

-- | Log a security event
logSecurityEvent :: LogStore -> Text -> Text -> IO LogID
logSecurityEvent store eventType details = 
  logWithContext store Warning details 
    [ ("type", String "security")
    , ("security_event", String eventType)
    ]

-- | Log a performance metric
logPerformanceMetric :: LogStore -> Text -> Double -> IO LogID
logPerformanceMetric store metricName value = 
  logWithContext store Info (metricName <> ": " <> T.pack (show value))
    [ ("type", String "performance")
    , ("metric", String metricName)
    , ("value", Number (fromRational (toRational value)))
    ] 