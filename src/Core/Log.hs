{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      : Core.Log
Description : Logging system for Time Bandits
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides the primary interface to the Time Bandits logging system.
It re-exports the standard logging functionality and provides additional
convenience functions for integrating with other system components.
-}
module Core.Log
  ( -- * Re-export StandardLog
    module Core.Log.StandardLog
    
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

import Core.Log.StandardLog

import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (when)
import Data.Aeson (Value(..), object, (.=))
import Data.Time (getCurrentTime, diffUTCTime)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Control.Exception (bracket)

-- | Global log store for the application
{-# NOINLINE globalLogStore #-}
globalLogStore :: IORef (Maybe LogStore)
globalLogStore = unsafePerformIO $ newIORef Nothing

-- | Get the system log store, initializing it if needed
getSystemLogStore :: IO LogStore
getSystemLogStore = do
  mStore <- readIORef globalLogStore
  case mStore of
    Just store -> return store
    Nothing -> do
      store <- initializeLogging
      return store

-- | Initialize the logging system
initializeLogging :: IO LogStore
initializeLogging = do
  mStore <- readIORef globalLogStore
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
      writeIORef globalLogStore (Just store)
      return store

-- | Shutdown the logging system
shutdownLogging :: IO ()
shutdownLogging = do
  mStore <- readIORef globalLogStore
  case mStore of
    Just store -> do
      -- Log shutdown
      _ <- logInfo store "Logging system shutting down"
      
      -- Close store
      closeLogStore store
      
      -- Clear global ref
      writeIORef globalLogStore Nothing
    Nothing -> 
      return ()

-- | Use the system logging within a scope
withSystemLogging :: (LogStore -> IO a) -> IO a
withSystemLogging action = bracket
  initializeLogging
  (\_ -> shutdownLogging)
  action

-- | Log with additional context
logWithContext :: LogStore -> Text -> Text -> [(Text, Value)] -> IO LogID
logWithContext store component message context = do
  -- Create context object
  let contextObj = object $ 
        [ "component" .= component
        , "message" .= message
        ] ++ context
  
  -- Log as event
  logEvent store "ContextLog" (Just contextObj)

-- | Log component info with context
logComponentInfo :: LogStore -> Text -> Text -> IO LogID
logComponentInfo store component message =
  logWithContext store component message [("level", String "info")]

-- | Log component warning with context
logComponentWarning :: LogStore -> Text -> Text -> IO LogID
logComponentWarning store component message =
  logWithContext store component message [("level", String "warning")]

-- | Log component error with context
logComponentError :: LogStore -> Text -> Text -> IO LogID
logComponentError store component message =
  logWithContext store component message [("level", String "error")]

-- | Log a network-related event
logNetworkEvent :: LogStore -> Text -> Text -> Maybe Text -> IO LogID
logNetworkEvent store eventType address details = do
  -- Create network event object
  let contextObj = object $ 
        [ "type" .= eventType
        , "address" .= address
        ] ++ maybe [] (\d -> [("details", String d)]) details
  
  -- Log as event
  logEvent store "NetworkEvent" (Just contextObj)

-- | Log a security-related event
logSecurityEvent :: LogStore -> Text -> Text -> IO LogID
logSecurityEvent store eventType details =
  logEvent store "SecurityEvent" (Just $ object 
    [ "type" .= eventType
    , "details" .= details
    ])

-- | Log a performance metric
logPerformanceMetric :: LogStore -> Text -> Double -> Text -> IO LogID
logPerformanceMetric store metricName value unit =
  logEvent store "PerformanceMetric" (Just $ object 
    [ "metric" .= metricName
    , "value" .= value
    , "unit" .= unit
    ]) 