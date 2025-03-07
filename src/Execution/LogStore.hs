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
Module: Execution.LogStore
Description: Centralized execution logging facility for the Time Bandits system.

This module provides a centralized logging facility that records all execution events
in the Time Bandits system, including:

- Program deployments
- Effect applications
- Timeline transitions
- Resource transfers
- Security verifications
- Actor interactions

The log store maintains these records in a structured format that can be queried
and analyzed to understand system behavior, debug issues, and verify correctness.
-}
module Execution.LogStore
  ( -- * Core Types
    LogStore
  , LogEntry(..)
  , LogLevel(..)
  , LogCategory(..)
  
  -- * Store Operations
  , createLogStore
  , appendLog
  , getLogEntries
  , filterLogsByLevel
  , filterLogsByCategory
  , filterLogsByTimeRange
  
  -- * Utility Functions
  , formatLogEntry
  , logToString
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Core.Timeline (TimelineHash)

-- | The severity level of a log entry
data LogLevel
  = Debug      -- ^ Detailed information for debugging
  | Info       -- ^ General information about system operation
  | Warning    -- ^ Potential issues or anomalies
  | Error      -- ^ Errors that don't stop execution
  | Critical   -- ^ Critical errors that may stop execution
  deriving (Show, Eq, Ord, Generic, Serialize, FromJSON, ToJSON)

-- | The category of a log entry
data LogCategory
  = Timeline    -- ^ Timeline-related events
  | Effect      -- ^ Effect application
  | Resource    -- ^ Resource management
  | Actor       -- ^ Actor operations
  | Security    -- ^ Security checks
  | System      -- ^ System-level events
  deriving (Show, Eq, Ord, Generic, Serialize, FromJSON, ToJSON)

-- | A single log entry
data LogEntry = LogEntry
  { logTimestamp :: UTCTime
  , logLevel :: LogLevel
  , logCategory :: LogCategory
  , logMessage :: Text
  , logSource :: Text
  , logTimelineHash :: Maybe TimelineHash
  , logData :: Map Text Text  -- ^ Additional structured data
  }
  deriving (Show, Eq, Generic)

-- | The log store holding all execution logs
data LogStore = LogStore
  { logEntries :: IORef [LogEntry]
  , logPath :: FilePath
  , logToConsole :: Bool
  , logToFile :: Bool
  }

-- | Create a new log store
createLogStore :: (MonadIO m) => FilePath -> m LogStore
createLogStore path = liftIO $ do
  -- Create the log directory if it doesn't exist
  createDirectoryIfMissing True path
  
  -- Initialize an empty log store
  entriesRef <- IORef.newIORef []
  
  return LogStore
    { logEntries = entriesRef
    , logPath = path
    , logToConsole = True
    , logToFile = True
    }

-- | Append a log entry to the store
appendLog :: (MonadIO m) => LogStore -> LogEntry -> m ()
appendLog store entry = liftIO $ do
  -- Add the entry to the in-memory store
  IORef.atomicModifyIORef' (logEntries store) $ \entries -> (entry : entries, ())
  
  -- Write to console if enabled
  when (logToConsole store) $
    TIO.putStrLn $ formatLogEntry entry
  
  -- Write to file if enabled
  when (logToFile store) $ do
    let filename = logPath store </> formatLogFilename entry
    TIO.appendFile filename (formatLogEntry entry <> "\n")

-- | Format a log entry for display
formatLogEntry :: LogEntry -> Text
formatLogEntry entry =
  T.pack (show (logTimestamp entry)) <> " [" <> T.pack (show (logLevel entry)) <> "] " <>
  "[" <> T.pack (show (logCategory entry)) <> "] " <>
  logMessage entry <>
  (case logTimelineHash entry of
     Nothing -> ""
     Just hash -> " (timeline: " <> T.pack (show hash) <> ")")

-- | Format a filename for a log entry
formatLogFilename :: LogEntry -> FilePath
formatLogFilename entry =
  let category = T.toLower $ T.pack $ show $ logCategory entry
      timestamp = T.pack $ formatTime defaultTimeLocale "%Y%m%d" $ logTimestamp entry
  in T.unpack $ category <> "-" <> timestamp <> ".log"

-- | Get all log entries
getLogEntries :: (MonadIO m) => LogStore -> m [LogEntry]
getLogEntries store = liftIO $ IORef.readIORef (logEntries store)

-- | Filter logs by level
filterLogsByLevel :: LogLevel -> [LogEntry] -> [LogEntry]
filterLogsByLevel level = filter (\entry -> logLevel entry >= level)

-- | Filter logs by category
filterLogsByCategory :: LogCategory -> [LogEntry] -> [LogEntry]
filterLogsByCategory category = filter (\entry -> logCategory entry == category)

-- | Filter logs by time range
filterLogsByTimeRange :: UTCTime -> UTCTime -> [LogEntry] -> [LogEntry]
filterLogsByTimeRange start end = filter inRange
  where
    inRange entry = 
      let timestamp = logTimestamp entry
      in timestamp >= start && timestamp <= end

-- | Convert a log entry to a string
logToString :: LogEntry -> String
logToString = T.unpack . formatLogEntry 