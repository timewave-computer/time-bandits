{-# LANGUAGE OverloadedStrings #-}

-- A minimal standalone test for Core Log system, with minimal dependencies

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word64)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Maybe (isJust)

-- Simplified Log types
data LogLevel = Debug | Info | Warning | Error
  deriving (Show, Eq, Ord)

data LogEntry = LogEntry
  { logTimestamp :: UTCTime
  , logLevel :: LogLevel
  , logSource :: Text
  , logMessage :: Text
  , logMetadata :: Map Text Text
  } deriving (Show, Eq)

newtype LogID = LogID ByteString
  deriving (Show, Eq, Ord)

data LogStore = LogStore
  { logEntries :: Map LogID LogEntry
  , logSequence :: Word64
  } deriving (Show, Eq)

-- Simple mock hash function for log entry IDs
mockHash :: LogEntry -> LogID
mockHash entry = 
  -- This is a simplified mock function, not a real hash
  LogID $ BS.pack $ map (fromIntegral . fromEnum) $ T.unpack $ 
    logSource entry <> logMessage entry

-- Create a new log store
createLogStore :: LogStore
createLogStore = LogStore Map.empty 0

-- Write a log entry to the store
writeLogEntry :: LogStore -> LogEntry -> (LogStore, LogID)
writeLogEntry store entry =
  let id = mockHash entry
      newEntries = Map.insert id entry (logEntries store)
      newSequence = logSequence store + 1
      newStore = store { logEntries = newEntries, logSequence = newSequence }
  in (newStore, id)

-- Retrieve a log entry by ID
getLogEntry :: LogStore -> LogID -> Maybe LogEntry
getLogEntry store id = Map.lookup id (logEntries store)

-- Helper to create a log entry
createLogEntry :: LogLevel -> Text -> Text -> Map Text Text -> IO LogEntry
createLogEntry level source message metadata = do
  timestamp <- getCurrentTime
  return LogEntry
    { logTimestamp = timestamp
    , logLevel = level
    , logSource = source
    , logMessage = message
    , logMetadata = metadata
    }

-- Test creating and writing log entries
testBasicLogging :: IO ()
testBasicLogging = do
  putStrLn "Testing basic logging functionality..."
  
  -- Create a new log store
  let store = createLogStore
  putStrLn $ "Initial log count: " ++ show (Map.size $ logEntries store)
  
  -- Create a log entry
  entry1 <- createLogEntry Info "test" "This is a test message" Map.empty
  let (store1, id1) = writeLogEntry store entry1
  
  putStrLn $ "Log count after first entry: " ++ show (Map.size $ logEntries store1)
  putStrLn $ "First log ID: " ++ show id1
  
  -- Create another log entry
  entry2 <- createLogEntry Warning "test" "Warning message" (Map.singleton "context" "testing")
  let (store2, id2) = writeLogEntry store1 entry2
  
  putStrLn $ "Log count after second entry: " ++ show (Map.size $ logEntries store2)
  
  -- Retrieve log entries
  let retrieved1 = getLogEntry store2 id1
  let retrieved2 = getLogEntry store2 id2
  
  case retrieved1 of
    Nothing -> putStrLn "ERROR: First log entry not found!"
    Just _ -> putStrLn "First log entry retrieved successfully"
    
  case retrieved2 of
    Nothing -> putStrLn "ERROR: Second log entry not found!"
    Just entry -> do
      putStrLn "Second log entry retrieved successfully"
      putStrLn $ "  Level: " ++ show (logLevel entry)
      putStrLn $ "  Message: " ++ T.unpack (logMessage entry)
      let contextValue = Map.lookup "context" (logMetadata entry)
      case contextValue of
        Nothing -> putStrLn "  ERROR: Metadata missing!"
        Just value -> putStrLn $ "  Context: " ++ T.unpack value

main :: IO ()
main = do
  putStrLn "Running simplified Core Log tests"
  testBasicLogging 