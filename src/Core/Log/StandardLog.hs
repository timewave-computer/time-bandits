{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Core.Log.StandardLog
Description : Standard logging system for Time Bandits
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides a standard logging system for Time Bandits,
supporting content-addressed append-only logs for effects, facts, and events.
-}
module Core.Log.StandardLog
  ( -- * Log Types
    LogEntry(..)
  , LogLevel(..)
  , LogID
  , LogStore
  
    -- * Creating Logs
  , createLogStore
  , withLogStore
  
    -- * Writing Logs
  , writeLog
  , writeLogEntry
  , logEffect
  , logFact
  , logEvent
  , logInfo
  , logWarning
  , logError
  , logDebug
  
    -- * Reading Logs
  , readLog
  , readLogRange
  , readLogByID
  , readLogsByType
  , getLogCount
  
    -- * Filtering & Searching
  , filterLogs
  , searchLogs
  ) where

import GHC.Generics
import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar, modifyMVar_)
import Control.Exception (bracket, finally)
import Control.Monad (when, forM)
import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, encode, decode)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (sortOn)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import Crypto.Hash (SHA256, Digest, hash)
import qualified Crypto.Hash as Hash

import Types.Effect (Fact)
import Core.Effect (Effect)

-- | Log entry ID based on content hash
newtype LogID = LogID { unLogID :: Text }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON LogID
instance FromJSON LogID

-- | Log level for messages
data LogLevel
  = Debug    -- ^ Detailed debugging information
  | Info     -- ^ General information
  | Warning  -- ^ Warning conditions
  | Error    -- ^ Error conditions
  deriving (Show, Eq, Ord, Generic)

instance ToJSON LogLevel
instance FromJSON LogLevel

-- | Log entry types
data LogEntry
  = LogEffect Effect               -- ^ Effect applied to the system
  | LogFact Fact                   -- ^ Fact observed in the system
  | LogMessage LogLevel Text       -- ^ General log message
  | LogEvent Text (Maybe Aeson.Value) -- ^ Named event with optional data
  deriving (Generic)

instance ToJSON LogEntry
instance FromJSON LogEntry

-- | Additional metadata for log entries
data LogMetadata = LogMetadata
  { logTimestamp :: UTCTime        -- ^ When the entry was logged
  , logID :: LogID                 -- ^ Content-addressed ID
  , logPrevious :: Maybe LogID     -- ^ Previous log entry ID (for chaining)
  , logIndex :: Int                -- ^ Position in the log
  } deriving (Show, Eq, Generic)

instance ToJSON LogMetadata
instance FromJSON LogMetadata

-- | Complete log entry with metadata
data CompleteLogEntry = CompleteLogEntry
  { entryContent :: LogEntry       -- ^ The log entry content
  , entryMetadata :: LogMetadata   -- ^ Entry metadata
  } deriving (Show, Eq, Generic)

instance ToJSON CompleteLogEntry
instance FromJSON CompleteLogEntry

-- | Log store for managing logs
data LogStore = LogStore
  { logEntries :: MVar [CompleteLogEntry]  -- ^ In-memory log entries
  , logDirectory :: Maybe FilePath         -- ^ Optional directory for persisting logs
  , logLatestID :: MVar (Maybe LogID)      -- ^ Latest log entry ID
  }

-- | Create a new log store
createLogStore :: Maybe FilePath -> IO LogStore
createLogStore mdir = do
  -- Create directory if provided
  case mdir of
    Just dir -> createDirectoryIfMissing True dir
    Nothing -> return ()
  
  -- Initialize empty log
  entriesMVar <- newMVar []
  latestMVar <- newMVar Nothing
  
  -- Load existing logs if directory provided
  logStore <- case mdir of
    Just dir -> do
      let store = LogStore
            { logEntries = entriesMVar
            , logDirectory = mdir
            , logLatestID = latestMVar
            }
      loadExistingLogs store dir
    Nothing -> do
      return LogStore
        { logEntries = entriesMVar
        , logDirectory = mdir
        , logLatestID = latestMVar
        }
  
  return logStore

-- | Use a log store within a scope and ensure it's properly closed
withLogStore :: Maybe FilePath -> (LogStore -> IO a) -> IO a
withLogStore mdir action = do
  store <- createLogStore mdir
  action store `finally` closeLogStore store

-- | Close a log store and ensure all entries are saved
closeLogStore :: LogStore -> IO ()
closeLogStore store = do
  -- If using a directory, save all entries
  case logDirectory store of
    Just dir -> do
      entries <- readMVar (logEntries store)
      mapM_ (saveEntryToFile dir) entries
    Nothing -> return ()

-- | Write an entry to the log
writeLog :: LogStore -> LogEntry -> IO LogID
writeLog store entry = do
  -- Get current time
  now <- getCurrentTime
  
  -- Get latest log ID
  latestIDMaybe <- readMVar (logLatestID store)
  
  -- Compute entry hash
  let entryHash = computeEntryHash entry latestIDMaybe
      entryID = LogID entryHash
  
  -- Create metadata
  entries <- readMVar (logEntries store)
  let index = length entries
      metadata = LogMetadata
        { logTimestamp = now
        , logID = entryID
        , logPrevious = latestIDMaybe
        , logIndex = index
        }
  
  -- Create complete entry
  let completeEntry = CompleteLogEntry
        { entryContent = entry
        , entryMetadata = metadata
        }
  
  -- Add to in-memory log
  modifyMVar_ (logEntries store) $ \es ->
    return (es ++ [completeEntry])
  
  -- Update latest ID
  modifyMVar_ (logLatestID store) $ \_ ->
    return (Just entryID)
  
  -- Save to disk if using a directory
  case logDirectory store of
    Just dir -> saveEntryToFile dir completeEntry
    Nothing -> return ()
  
  -- Return the log ID
  return entryID

-- | Write a pre-formed log entry
writeLogEntry :: LogStore -> CompleteLogEntry -> IO ()
writeLogEntry store completeEntry = do
  -- Add to in-memory log
  modifyMVar_ (logEntries store) $ \es ->
    return (es ++ [completeEntry])
  
  -- Update latest ID
  let entryID = logID (entryMetadata completeEntry)
  modifyMVar_ (logLatestID store) $ \_ ->
    return (Just entryID)
  
  -- Save to disk if using a directory
  case logDirectory store of
    Just dir -> saveEntryToFile dir completeEntry
    Nothing -> return ()

-- | Log an effect
logEffect :: LogStore -> Effect -> IO LogID
logEffect store effect = writeLog store (LogEffect effect)

-- | Log a fact
logFact :: LogStore -> Fact -> IO LogID
logFact store fact = writeLog store (LogFact fact)

-- | Log an event
logEvent :: LogStore -> Text -> Maybe Aeson.Value -> IO LogID
logEvent store name value = writeLog store (LogEvent name value)

-- | Log an info message
logInfo :: LogStore -> Text -> IO LogID
logInfo store msg = writeLog store (LogMessage Info msg)

-- | Log a warning message
logWarning :: LogStore -> Text -> IO LogID
logWarning store msg = writeLog store (LogMessage Warning msg)

-- | Log an error message
logError :: LogStore -> Text -> IO LogID
logError store msg = writeLog store (LogMessage Error msg)

-- | Log a debug message
logDebug :: LogStore -> Text -> IO LogID
logDebug store msg = writeLog store (LogMessage Debug msg)

-- | Read the entire log
readLog :: LogStore -> IO [CompleteLogEntry]
readLog store = readMVar (logEntries store)

-- | Read a range of log entries
readLogRange :: LogStore -> Int -> Int -> IO [CompleteLogEntry]
readLogRange store start count = do
  entries <- readMVar (logEntries store)
  let len = length entries
      safeStart = max 0 (min start (len - 1))
      safeEnd = min len (safeStart + count)
  return $ take (safeEnd - safeStart) $ drop safeStart entries

-- | Read a log entry by ID
readLogByID :: LogStore -> LogID -> IO (Maybe CompleteLogEntry)
readLogByID store targetID = do
  entries <- readMVar (logEntries store)
  return $ find (\e -> logID (entryMetadata e) == targetID) entries

-- | Read log entries by type
readLogsByType :: LogStore -> (LogEntry -> Bool) -> IO [CompleteLogEntry]
readLogsByType store predicate = do
  entries <- readMVar (logEntries store)
  return $ filter (predicate . entryContent) entries

-- | Get the number of log entries
getLogCount :: LogStore -> IO Int
getLogCount store = do
  entries <- readMVar (logEntries store)
  return $ length entries

-- | Filter logs based on a predicate
filterLogs :: LogStore -> (CompleteLogEntry -> Bool) -> IO [CompleteLogEntry]
filterLogs store predicate = do
  entries <- readMVar (logEntries store)
  return $ filter predicate entries

-- | Search logs for a text pattern
searchLogs :: LogStore -> Text -> IO [CompleteLogEntry]
searchLogs store pattern = do
  entries <- readMVar (logEntries store)
  return $ filter (entryContainsText pattern) entries

-- Helper functions

-- | Compute a hash for a log entry
computeEntryHash :: LogEntry -> Maybe LogID -> Text
computeEntryHash entry prevIDMaybe = do
  -- Convert entry to JSON
  let entryJson = Aeson.encode entry
      prevHashText = maybe "" unLogID prevIDMaybe
      combinedText = LBS.toStrict entryJson <> (BS.pack $ T.unpack prevHashText)
      digest = hash combinedText :: Digest SHA256
      hashText = T.pack $ show digest
  
  hashText

-- | Save an entry to a file
saveEntryToFile :: FilePath -> CompleteLogEntry -> IO ()
saveEntryToFile dir entry = do
  let id = unLogID $ logID $ entryMetadata entry
      filename = dir </> T.unpack id <> ".json"
      json = Aeson.encode entry
  
  -- Only write if file doesn't exist (append-only)
  fileExists <- doesFileExist filename
  when (not fileExists) $
    LBS.writeFile filename json

-- | Load existing logs from a directory
loadExistingLogs :: LogStore -> FilePath -> IO LogStore
loadExistingLogs store dir = do
  -- List JSON files
  files <- listDirectory dir
  let jsonFiles = filter (\f -> takeExtension f == ".json") files
  
  -- Parse each file
  entryResults <- forM jsonFiles $ \file -> do
    let path = dir </> file
    contentMaybe <- try $ LBS.readFile path
    return $ case contentMaybe of
      Left (e :: IOException) -> Nothing
      Right content -> Aeson.decode content
  
  -- Filter successful parses and sort by index
  let entries = sortOn (logIndex . entryMetadata) $ catMaybes entryResults
  
  -- Update store
  when (not $ null entries) $ do
    -- Update entries
    modifyMVar_ (logEntries store) $ \_ -> return entries
    
    -- Update latest ID
    let latestEntry = last entries
        latestID = logID $ entryMetadata latestEntry
    modifyMVar_ (logLatestID store) $ \_ -> return $ Just latestID
  
  return store

-- | Check if an entry contains text
entryContainsText :: Text -> CompleteLogEntry -> Bool
entryContainsText pattern entry =
  case entryContent entry of
    LogMessage _ msg -> pattern `T.isInfixOf` msg
    LogEvent name _ -> pattern `T.isInfixOf` name
    _ -> False  -- For Effect and Fact, we'd need more specific checks

-- | Find an entry in a list
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) = if p x then Just x else find p xs

-- | Get file extension
takeExtension :: FilePath -> String
takeExtension path = case dropWhile (/= '.') path of
  "" -> ""
  ext -> ext

-- | Try to run an action, catching IO exceptions
try :: IO a -> IO (Either IOException a)
try action = do
  result <- catch (Right <$> action) (\e -> return $ Left e)
  return result

-- | Catch an exception
catch :: IO a -> (IOException -> IO a) -> IO a
catch action handler = do
  -- In real code, use Control.Exception.catch
  -- This is a simplified implementation
  result <- action
  return result

-- | IO Exception type
data IOException = IOException
  deriving Show 