{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module: Core.Log
Description: Standardized logging system for Time Bandits

This module provides a unified, content-addressed logging system that:

1. Records all effects, facts, and events in a standard format
2. Ensures logs can be replayed deterministically
3. Maintains causal relationships between log entries
4. Works consistently across all simulation modes

All actors in the Time Bandits system use this logging system to maintain
a consistent record of their actions and observations.
-}
module Core.Log
  ( -- * Core Log Types
    LogEntry(..)
  , LogEntryType(..)
  , LogID
  , LogMetadata(..)
  
  -- * Log Operations
  , writeLog
  , readLog
  , appendLogEntry
  , getLogEntries
  , getLatestLogEntry
  
  -- * Query Functions
  , findLogEntriesByType
  , findLogEntriesByActor
  , findLogEntriesBetween
  
  -- * Replay Functions
  , replayLogEntries
  ) where

import Control.Exception (catch, throwIO, Exception)
import Control.Monad (forM, unless, when, void)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Serialize (Serialize)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO (IOMode(..), withFile, hPutStrLn)

import Core.Common (Hash, EntityHash, TimelineHash, hashEntity)
import Core.Types (LamportTime(..), ActorId(..), ProgramId(..))
import Core.Effect (Effect, EffectID)

-- | Unique identifier for a log entry
type LogID = Hash

-- | Types of log entries
data LogEntryType
  = EffectEntry    -- ^ An effect was applied
  | FactEntry      -- ^ A fact was observed
  | EventEntry     -- ^ An event occurred
  | ErrorEntry     -- ^ An error was recorded
  | SystemEntry    -- ^ A system message (startup, shutdown, etc.)
  deriving (Show, Eq, Generic, FromJSON, ToJSON, Serialize)

-- | Metadata for log entries
data LogMetadata = LogMetadata
  { logTimestamp :: UTCTime        -- ^ Wall clock time
  , logLamportTime :: LamportTime  -- ^ Logical clock time
  , logActorId :: ActorId          -- ^ Actor that created the log
  , logProgram :: Maybe ProgramId  -- ^ Program associated with the log (if any)
  , logTimeline :: Maybe TimelineHash -- ^ Timeline associated with the log (if any)
  , logParentEntries :: [LogID]    -- ^ Parent log entries (causal history)
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, Serialize)

-- | A single log entry
data LogEntry = LogEntry
  { logId :: LogID                 -- ^ Content-addressed ID
  , logType :: LogEntryType        -- ^ Type of log entry
  , logMetadata :: LogMetadata     -- ^ Metadata
  , logContent :: ByteString       -- ^ Content (serialized effect, fact, etc.)
  , logTags :: [Text]              -- ^ Optional tags for filtering
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, Serialize)

-- | Write a log entry to the specified log file
writeLog :: FilePath -> LogEntry -> IO ()
writeLog logPath entry = do
  -- Create log directory if it doesn't exist
  createDirectoryIfMissing True (takeDirectory logPath)
  
  -- Append log entry to file
  withFile logPath AppendMode $ \h -> do
    BL.hPut h (encode entry)
    BL.hPut h "\n"  -- Add newline for readability
  where
    takeDirectory :: FilePath -> FilePath
    takeDirectory p = case break (== '/') (reverse p) of
      (_, "")   -> "."
      (_, _:ds) -> reverse ds

-- | Read all log entries from the specified log file
readLog :: FilePath -> IO [LogEntry]
readLog logPath = do
  exists <- doesFileExist logPath
  if exists
    then do
      contents <- BL.readFile logPath
      let entries = map parseLine (BL.split 10 contents)  -- 10 is ASCII for newline
      return $ concatMap (either (const []) pure) entries
    else return []
  where
    parseLine :: ByteString -> Either String LogEntry
    parseLine line 
      | BL.null line = Left "Empty line"
      | otherwise    = eitherDecode line

-- | Append a new log entry
appendLogEntry :: FilePath -> LogEntryType -> ActorId -> ByteString -> IO LogEntry
appendLogEntry logPath entryType actorId content = do
  -- Generate metadata
  timestamp <- getCurrentTime
  let lamportTime = LamportTime 0  -- In a real implementation, this would be tracked
  
  -- Create log entry
  let metadata = LogMetadata
        { logTimestamp = timestamp
        , logLamportTime = lamportTime
        , logActorId = actorId
        , logProgram = Nothing
        , logTimeline = Nothing
        , logParentEntries = []
        }
  
  let entry = LogEntry
        { logId = computeLogId entryType metadata content
        , logType = entryType
        , logMetadata = metadata
        , logContent = content
        , logTags = []
        }
  
  -- Write to log
  writeLog logPath entry
  
  return entry
  where
    computeLogId :: LogEntryType -> LogMetadata -> ByteString -> LogID
    computeLogId entryType metadata content =
      hashEntity (show entryType, show metadata, BL.toStrict content)

-- | Get all log entries from a log file
getLogEntries :: FilePath -> IO [LogEntry]
getLogEntries = readLog

-- | Get the latest log entry from a log file
getLatestLogEntry :: FilePath -> IO (Maybe LogEntry)
getLatestLogEntry logPath = do
  entries <- readLog logPath
  return $ if null entries
    then Nothing
    else Just (last entries)

-- | Find all log entries of a specific type
findLogEntriesByType :: LogEntryType -> [LogEntry] -> [LogEntry]
findLogEntriesByType targetType = filter (\entry -> logType entry == targetType)

-- | Find all log entries from a specific actor
findLogEntriesByActor :: ActorId -> [LogEntry] -> [LogEntry]
findLogEntriesByActor actorId = filter (\entry -> logActorId (logMetadata entry) == actorId)

-- | Find all log entries between two logical times
findLogEntriesBetween :: LamportTime -> LamportTime -> [LogEntry] -> [LogEntry]
findLogEntriesBetween start end = filter inRange
  where
    inRange entry = 
      let time = logLamportTime (logMetadata entry)
      in time >= start && time <= end

-- | Replay a sequence of log entries (implementation depends on entry type)
replayLogEntries :: [LogEntry] -> IO ()
replayLogEntries entries = do
  -- Sort by Lamport time to ensure causal order
  let sortedEntries = sortByLamportTime entries
  
  -- In a real implementation, this would apply each entry to reconstruct state
  traverse_ processEntry sortedEntries
  where
    sortByLamportTime :: [LogEntry] -> [LogEntry]
    sortByLamportTime = sortOn (logLamportTime . logMetadata)
    
    sortOn :: Ord b => (a -> b) -> [a] -> [a]
    sortOn f = map snd . sortBy (comparing fst) . map (\x -> (f x, x))
    
    comparing :: Ord b => (a -> b) -> a -> a -> Ordering
    comparing f x y = compare (f x) (f y)
    
    sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    sortBy _ [] = []
    sortBy cmp (x:xs) = sortBy cmp smaller ++ [x] ++ sortBy cmp larger
      where
        smaller = [y | y <- xs, cmp y x /= GT]
        larger  = [y | y <- xs, cmp y x == GT]
    
    processEntry :: LogEntry -> IO ()
    processEntry entry = case logType entry of
      EffectEntry -> putStrLn $ "Replaying effect: " ++ show (logId entry)
      FactEntry   -> putStrLn $ "Observing fact: " ++ show (logId entry)
      EventEntry  -> putStrLn $ "Processing event: " ++ show (logId entry)
      ErrorEntry  -> putStrLn $ "Handling error: " ++ show (logId entry)
      SystemEntry -> putStrLn $ "System message: " ++ show (logId entry) 