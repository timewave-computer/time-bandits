{-# LANGUAGE OverloadedStrings #-}

module Core.Log.StandardLogTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value(..), object, (.=))
import Data.Time (getCurrentTime, diffUTCTime)
import System.Directory (removeDirectoryRecursive, createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Control.Monad (forM_)
import Control.Exception (bracket, finally)
import qualified Data.ByteString.Lazy as LBS

import Core.Log.StandardLog
import Core.Effect (Effect(..))
import Types.Effect (Fact(..))

-- | Main test group
tests :: TestTree
tests = testGroup "StandardLog Tests"
  [ testCase "Create empty log store" testCreateLogStore
  , testCase "Write and read logs" testWriteReadLogs
  , testCase "Log different types" testLogDifferentTypes
  , testCase "Read log range" testReadLogRange
  , testCase "Filter logs" testFilterLogs
  , testCase "Search logs" testSearchLogs
  , testCase "Log chaining" testLogChaining
  , testCase "Persistent storage" testPersistentStorage
  , testCase "withLogStore bracket" testWithLogStore
  ]

-- | Test creating an empty log store
testCreateLogStore :: Assertion
testCreateLogStore = do
  store <- createLogStore Nothing
  count <- getLogCount store
  assertEqual "Empty log store should have 0 entries" 0 count

-- | Test writing and reading logs
testWriteReadLogs :: Assertion
testWriteReadLogs = do
  store <- createLogStore Nothing
  
  -- Write logs
  _ <- logInfo store "Test log 1"
  _ <- logInfo store "Test log 2"
  _ <- logInfo store "Test log 3"
  
  -- Read logs
  logs <- readLog store
  assertEqual "Should have 3 log entries" 3 (length logs)
  
  -- Check content of logs
  let msgs = map (\entry -> case entryContent entry of
                             LogMessage _ msg -> Just msg
                             _ -> Nothing) logs
  assertEqual "Messages should match" 
              [Just "Test log 1", Just "Test log 2", Just "Test log 3"] 
              msgs

-- | Test logging different types of entries
testLogDifferentTypes :: Assertion
testLogDifferentTypes = do
  store <- createLogStore Nothing
  
  -- Log different types
  _ <- logInfo store "Info message"
  _ <- logWarning store "Warning message"
  _ <- logError store "Error message"
  _ <- logDebug store "Debug message"
  
  -- Test event logging
  _ <- logEvent store "UserLogin" (Just $ object ["userId" .= ("user123" :: Text)])
  
  -- Test effect logging (mock effect for testing)
  let effect = EffectDummy "test-effect"
  _ <- logEffect store effect
  
  -- Test fact logging (mock fact for testing)
  let fact = FactDummy "test-fact"
  _ <- logFact store fact
  
  -- Read all logs
  logs <- readLog store
  assertEqual "Should have 7 log entries" 7 (length logs)
  
  -- Filter by type
  let isInfo entry = case entryContent entry of
                      LogMessage Info _ -> True
                      _ -> False
  infoLogs <- filterLogs store isInfo
  assertEqual "Should have 1 info log" 1 (length infoLogs)
  
  let isEvent entry = case entryContent entry of
                       LogEvent _ _ -> True
                       _ -> False
  eventLogs <- filterLogs store isEvent
  assertEqual "Should have 1 event log" 1 (length eventLogs)

-- | Test reading log ranges
testReadLogRange :: Assertion
testReadLogRange = do
  store <- createLogStore Nothing
  
  -- Write 10 logs
  forM_ [1..10] $ \i -> 
    logInfo store (T.pack $ "Log entry " ++ show i)
  
  -- Read a range
  middleLogs <- readLogRange store 3 4
  assertEqual "Should have 4 log entries" 4 (length middleLogs)
  
  -- Check indexes
  let indexes = map (logIndex . entryMetadata) middleLogs
  assertEqual "Indexes should be sequential" [3, 4, 5, 6] indexes

-- | Test filtering logs
testFilterLogs :: Assertion
testFilterLogs = do
  store <- createLogStore Nothing
  
  -- Write logs with different levels
  _ <- logInfo store "Info message 1"
  _ <- logWarning store "Warning message"
  _ <- logError store "Error message"
  _ <- logInfo store "Info message 2"
  _ <- logDebug store "Debug message"
  
  -- Filter by level
  let isWarningOrError entry = case entryContent entry of
                                LogMessage Warning _ -> True
                                LogMessage Error _ -> True
                                _ -> False
  
  filteredLogs <- filterLogs store isWarningOrError
  assertEqual "Should have 2 warning/error logs" 2 (length filteredLogs)

-- | Test searching logs
testSearchLogs :: Assertion
testSearchLogs = do
  store <- createLogStore Nothing
  
  -- Write logs with various content
  _ <- logInfo store "User login successful"
  _ <- logInfo store "Database connection established"
  _ <- logWarning store "Low memory warning"
  _ <- logError store "Database query failed"
  _ <- logInfo store "User logout successful"
  
  -- Search for "database" keyword
  dbLogs <- searchLogs store "database"
  assertEqual "Should find 2 logs with 'database'" 2 (length dbLogs)
  
  -- Search for "user" keyword
  userLogs <- searchLogs store "user"
  assertEqual "Should find 2 logs with 'user'" 2 (length userLogs)
  
  -- Search for non-existent keyword
  emptyLogs <- searchLogs store "nonexistent"
  assertEqual "Should find 0 logs with 'nonexistent'" 0 (length emptyLogs)

-- | Test log chaining via previous IDs
testLogChaining :: Assertion
testLogChaining = do
  store <- createLogStore Nothing
  
  -- Create a chain of logs
  id1 <- logInfo store "First log"
  id2 <- logInfo store "Second log"
  id3 <- logInfo store "Third log"
  
  -- Get logs
  logs <- readLog store
  
  -- Check chain
  let extractPrevID entry = logPrevious (entryMetadata entry)
      prevIDs = map extractPrevID logs
  
  -- First log should have no previous
  assertEqual "First log should have no previous" Nothing (prevIDs !! 0)
  
  -- Second log should link to first
  assertEqual "Second log should link to first" (Just id1) (prevIDs !! 1)
  
  -- Third log should link to second
  assertEqual "Third log should link to second" (Just id2) (prevIDs !! 2)

-- | Test persistent storage
testPersistentStorage :: Assertion
testPersistentStorage = withTempLogDir $ \logDir -> do
  -- Create log store with persistence
  store1 <- createLogStore (Just logDir)
  
  -- Write some logs
  _ <- logInfo store1 "Persistent log 1"
  _ <- logWarning store1 "Persistent log 2"
  _ <- logError store1 "Persistent log 3"
  
  -- Close first store
  closeLogStore store1
  
  -- Create a new store that should load the logs
  store2 <- createLogStore (Just logDir)
  
  -- Read logs from new store
  logs <- readLog store2
  assertEqual "Should have loaded 3 logs from disk" 3 (length logs)
  
  -- Check content
  let levels = map (\entry -> case entryContent entry of
                               LogMessage level _ -> Just level
                               _ -> Nothing) logs
  assertEqual "Log levels should match" 
              [Just Info, Just Warning, Just Error] 
              levels

-- | Test withLogStore bracket
testWithLogStore :: Assertion
testWithLogStore = withTempLogDir $ \logDir -> do
  -- Use withLogStore which should auto-close
  result <- withLogStore (Just logDir) $ \store -> do
    -- Write logs
    _ <- logInfo store "Test withLogStore"
    -- Return something
    return "success"
  
  -- Check the result
  assertEqual "withLogStore should return the action result" "success" result
  
  -- Create a store to verify logs were saved
  store <- createLogStore (Just logDir)
  logs <- readLog store
  assertEqual "Should have 1 log saved" 1 (length logs)

-- Helper functions

-- | Create a temporary directory for logs
withTempLogDir :: (FilePath -> IO a) -> IO a
withTempLogDir action = do
  let baseTempDir = "test-logs"
  createDirectoryIfMissing True baseTempDir
  let tempDir = baseTempDir </> "temp-log-test"
  
  -- Create directory and ensure cleanup
  bracket
    (createTempLogDir tempDir)
    (\_ -> removeTempLogDir tempDir)
    action

-- | Create a temporary log directory
createTempLogDir :: FilePath -> IO FilePath
createTempLogDir dir = do
  exists <- doesDirectoryExist dir
  when exists $ removeDirectoryRecursive dir
  createDirectoryIfMissing True dir
  return dir

-- | Remove a temporary log directory
removeTempLogDir :: FilePath -> IO ()
removeTempLogDir dir = do
  exists <- doesDirectoryExist dir
  when exists $ removeDirectoryRecursive dir

-- | Helper for Bool assertions in HUnit
when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return () 