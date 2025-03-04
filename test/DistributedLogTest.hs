{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module DistributedLogTest (tests) where

import Prelude
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import qualified Data.ByteString.Char8 as BS
import Polysemy
import Polysemy.Error
import Polysemy.Trace
import Data.Time.Clock (getCurrentTime, UTCTime)
import qualified Data.Map as Map
import Data.IORef (newIORef)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import Control.Exception (try, SomeException)
import Control.Monad (void)

-- Import from TimeBandits modules
import TimeBandits.Core
import TimeBandits.Types
import TimeBandits.Crypto
import TimeBandits.DistributedLog
import qualified TimeBandits.Network as Network

-- Define our test suite
tests :: Tasty.TestTree
tests = Tasty.testGroup "Distributed Log Tests" 
  [ testCase "Create and append to log" testCreateAndAppend
  , testCase "Persist and load log" testPersistAndLoad
  , testCase "Verify log integrity" testVerifyIntegrity
  , testCase "Read from log with time range" testReadWithTimeRange
  ]

-- | Test creating a log and appending entries
testCreateAndAppend :: IO ()
testCreateAndAppend = do
  -- Create a test directory
  let testDir = "test-logs"
  createDirectoryIfMissing True testDir
  
  -- Run the test
  result <- runM $ runError @AppError $ traceToStdout $ do
    -- Create a log config
    let config = LogConfig
          { lcStorageType = MemoryStorage
          , lcStoragePath = testDir
          , lcReplicationMode = NoReplication
          , lcReplicationFactor = 1
          , lcConsistencyLevel = CausalConsistency
          , lcCompactionInterval = 3600
          , lcMaxLogSize = 1024 * 1024
          , lcEncrypted = False
          }
    
    -- Create a log
    let owner = "test-owner"
    log <- createLog config owner
    
    -- Generate a key pair
    (privKey, pubKey) <- generateSecureEd25519KeyPair
    
    -- Append entries to the log
    (log1, entry1) <- appendToLog log "Entry 1" privKey
    (log2, entry2) <- appendToLog log1 "Entry 2" privKey
    (log3, entry3) <- appendToLog log2 "Entry 3" privKey
    
    -- Verify the log has 3 entries
    pure $ length (dlEntries log3)
  
  -- Clean up
  removeDirectoryRecursive testDir
  
  -- Check the result
  case result of
    Left err -> assertFailure $ "Test failed: " ++ show err
    Right count -> count @?= 3

-- | Test persisting a log to disk and loading it back
testPersistAndLoad :: IO ()
testPersistAndLoad = do
  -- Create a test directory
  let testDir = "test-logs"
  createDirectoryIfMissing True testDir
  
  -- Run the test
  result <- runM $ runError @AppError $ traceToStdout $ do
    -- Create a log config
    let config = LogConfig
          { lcStorageType = FileStorage
          , lcStoragePath = testDir
          , lcReplicationMode = NoReplication
          , lcReplicationFactor = 1
          , lcConsistencyLevel = CausalConsistency
          , lcCompactionInterval = 3600
          , lcMaxLogSize = 1024 * 1024
          , lcEncrypted = False
          }
    
    -- Create a log
    let owner = "test-owner"
    log <- createLog config owner
    
    -- Generate a key pair
    (privKey, pubKey) <- generateSecureEd25519KeyPair
    
    -- Append entries to the log
    (log1, _) <- appendToLog log "Entry 1" privKey
    (log2, _) <- appendToLog log1 "Entry 2" privKey
    
    -- Persist the log to disk
    persistLogToDisk log2
    
    -- Load the log from disk
    maybeLog <- loadLogFromDisk config owner
    case maybeLog of
      Nothing -> throw $ AppError "Failed to load log from disk"
      Just loadedLog -> do
        -- Verify the loaded log has the same entries
        pure $ length (dlEntries loadedLog) == length (dlEntries log2)
  
  -- Clean up
  removeDirectoryRecursive testDir
  
  -- Check the result
  case result of
    Left err -> assertFailure $ "Test failed: " ++ show err
    Right success -> assertBool "Log should be loaded successfully" success

-- | Test verifying the integrity of a log
testVerifyIntegrity :: IO ()
testVerifyIntegrity = do
  -- Run the test
  result <- runM $ runError @AppError $ traceToStdout $ do
    -- Create a log config
    let config = LogConfig
          { lcStorageType = MemoryStorage
          , lcStoragePath = "test-logs"
          , lcReplicationMode = NoReplication
          , lcReplicationFactor = 1
          , lcConsistencyLevel = CausalConsistency
          , lcCompactionInterval = 3600
          , lcMaxLogSize = 1024 * 1024
          , lcEncrypted = False
          }
    
    -- Create a log
    let owner = "test-owner"
    log <- createLog config owner
    
    -- Generate a key pair
    (privKey, pubKey) <- generateSecureEd25519KeyPair
    
    -- Append entries to the log
    (log1, _) <- appendToLog log "Entry 1" privKey
    (log2, _) <- appendToLog log1 "Entry 2" privKey
    (log3, _) <- appendToLog log2 "Entry 3" privKey
    
    -- Verify the log integrity
    verifyLogIntegrity log3
  
  -- Check the result
  case result of
    Left err -> assertFailure $ "Test failed: " ++ show err
    Right integrity -> assertBool "Log integrity should be valid" integrity

-- | Test reading from a log with a time range
testReadWithTimeRange :: IO ()
testReadWithTimeRange = do
  -- Run the test
  result <- runM $ runError @AppError $ traceToStdout $ do
    -- Create a log config
    let config = LogConfig
          { lcStorageType = MemoryStorage
          , lcStoragePath = "test-logs"
          , lcReplicationMode = NoReplication
          , lcReplicationFactor = 1
          , lcConsistencyLevel = CausalConsistency
          , lcCompactionInterval = 3600
          , lcMaxLogSize = 1024 * 1024
          , lcEncrypted = False
          }
    
    -- Create a log
    let owner = "test-owner"
    log <- createLog config owner
    
    -- Generate a key pair
    (privKey, pubKey) <- generateSecureEd25519KeyPair
    
    -- Append entries to the log
    (log1, _) <- appendToLog log "Entry 1" privKey
    (log2, _) <- appendToLog log1 "Entry 2" privKey
    (log3, _) <- appendToLog log2 "Entry 3" privKey
    (log4, _) <- appendToLog log3 "Entry 4" privKey
    (log5, _) <- appendToLog log4 "Entry 5" privKey
    
    -- Read entries with time range
    entries1 <- readFromLog log5 (Just (LamportTime 2)) (Just (LamportTime 4))
    entries2 <- readFromLog log5 Nothing (Just (LamportTime 3))
    entries3 <- readFromLog log5 (Just (LamportTime 3)) Nothing
    
    -- Verify the number of entries in each range
    pure (length entries1, length entries2, length entries3)
  
  -- Check the result
  case result of
    Left err -> assertFailure $ "Test failed: " ++ show err
    Right (count1, count2, count3) -> do
      count1 @?= 3  -- Entries 2, 3, 4
      count2 @?= 3  -- Entries 1, 2, 3
      count3 @?= 3  -- Entries 3, 4, 5 