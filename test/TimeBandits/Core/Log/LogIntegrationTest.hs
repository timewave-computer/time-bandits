{-# LANGUAGE OverloadedStrings #-}

module TimeBandits.Core.Log.LogIntegrationTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value(..), object, (.=))
import Control.Monad (forM_)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))

import TimeBandits.Core.Log
import Types.Effect (Fact(..))
import TimeBandits.Core.Effect (Effect(..))
import TimeBandits.Core.Concurrency.ResourceLock (ResourceLock(..), withResourceLock)
import TimeBandits.Core.Concurrency.LockManager (LockManager, createLockManager, acquireLock, releaseLock)

-- | Main test group
tests :: TestTree
tests = testGroup "Log Integration Tests"
  [ testCase "Integration with Lock Manager" testLockManagerIntegration
  , testCase "Multi-component logging" testMultiComponentLogging
  , testCase "Concurrent logging" testConcurrentLogging
  , testCase "System log context propagation" testContextPropagation
  ]

-- | Test integration with the Lock Manager
testLockManagerIntegration :: Assertion
testLockManagerIntegration = do
  withSystemTempDirectory "lock-log-test" $ \tempDir -> do
    let logDir = tempDir </> "logs"
    
    -- Create a log store
    store <- createLogStore (Just logDir)
    
    -- Create a lock manager that uses the log store
    lockManager <- createLoggingLockManager store
    
    -- Perform lock operations
    let resource = ResourceLock "test-resource" "test"
    
    -- Acquire lock
    acquired <- acquireLock lockManager resource 5000 -- 5 second timeout
    assertBool "Lock should be acquired" acquired
    
    -- Read logs to verify lock acquisition was logged
    logs <- readLog store
    let lockEvents = filter (isLockEvent "acquire") logs
    assertBool "Lock acquisition should be logged" (not $ null lockEvents)
    
    -- Release lock
    released <- releaseLock lockManager resource
    assertBool "Lock should be released" released
    
    -- Read logs to verify lock release was logged
    logs' <- readLog store
    let releaseEvents = filter (isLockEvent "release") logs'
    assertBool "Lock release should be logged" (not $ null releaseEvents)
  where
    isLockEvent :: Text -> CompleteLogEntry -> Bool
    isLockEvent eventType entry =
      case entryContent entry of
        LogEvent name _ -> name == "LockEvent" && containsEventType eventType entry
        _ -> False
      
    containsEventType :: Text -> CompleteLogEntry -> Bool
    containsEventType eventType entry =
      case entryContent entry of
        LogEvent _ (Just obj) -> case obj of
          Object o -> case lookup "type" o of
            Just (String t) -> t == eventType
            _ -> False
          _ -> False
        _ -> False
        
    -- Create a lock manager with logging
    createLoggingLockManager :: LogStore -> IO LockManager
    createLoggingLockManager store = do
      manager <- createLockManager
      
      -- Log creation
      _ <- logComponentInfo store "LockManager" "Lock manager created"
      
      return $ wrapLockManagerWithLogging store manager
    
    -- Wrap a lock manager with logging
    wrapLockManagerWithLogging :: LogStore -> LockManager -> LockManager
    wrapLockManagerWithLogging store manager = 
      manager { 
        acquireLock = \resource timeout -> do
          -- Log before attempt
          _ <- logEvent store "LockEvent" (Just $ object
            [ "type" .= ("attempt" :: Text)
            , "resource" .= resourceName resource
            , "namespace" .= resourceNamespace resource
            , "timeout" .= timeout
            ])
          
          -- Call original function
          result <- acquireLock manager resource timeout
          
          -- Log result
          _ <- logEvent store "LockEvent" (Just $ object
            [ "type" .= ("acquire" :: Text)
            , "resource" .= resourceName resource
            , "namespace" .= resourceNamespace resource
            , "success" .= result
            ])
          
          return result,
          
        releaseLock = \resource -> do
          -- Log before attempt
          _ <- logEvent store "LockEvent" (Just $ object
            [ "type" .= ("release_attempt" :: Text)
            , "resource" .= resourceName resource
            , "namespace" .= resourceNamespace resource
            ])
          
          -- Call original function
          result <- releaseLock manager resource
          
          -- Log result
          _ <- logEvent store "LockEvent" (Just $ object
            [ "type" .= ("release" :: Text)
            , "resource" .= resourceName resource
            , "namespace" .= resourceNamespace resource
            , "success" .= result
            ])
          
          return result
      }

-- | Test logging from multiple components
testMultiComponentLogging :: Assertion
testMultiComponentLogging = do
  withSystemLogging $ \store -> do
    -- Log from "Network" component
    _ <- logComponentInfo store "Network" "Connection established to peer"
    _ <- logNetworkEvent store "connect" "192.168.1.100" Nothing
    
    -- Log from "Consensus" component
    _ <- logComponentInfo store "Consensus" "Starting consensus round"
    _ <- logComponentWarning store "Consensus" "Insufficient peers for optimal consensus"
    
    -- Log from "Storage" component
    _ <- logComponentInfo store "Storage" "Initializing blockchain storage"
    _ <- logPerformanceMetric store "storage_init_time" 235.5 "ms"
    
    -- Read all logs
    logs <- readLog store
    
    -- Verify we have the expected number of logs
    assertEqual "Should have 6 log entries" 6 (length logs)
    
    -- Verify component distribution
    let componentLogs comp = 
          filter (isComponentLog comp) logs
    
    assertEqual "Should have 2 Network logs" 2 (length $ componentLogs "Network")
    assertEqual "Should have 2 Consensus logs" 2 (length $ componentLogs "Consensus")
    assertEqual "Should have 2 Storage logs" 2 (length $ componentLogs "Storage")
  where
    isComponentLog :: Text -> CompleteLogEntry -> Bool
    isComponentLog compName entry =
      case entryContent entry of
        LogEvent "ContextLog" (Just obj) -> 
          case obj of
            Object o -> case lookup "component" o of
              Just (String c) -> c == compName
              _ -> False
            _ -> False
        LogEvent name _ -> name == "NetworkEvent" && compName == "Network" ||
                           name == "PerformanceMetric" && compName == "Storage"
        _ -> False

-- | Test concurrent logging from multiple threads
testConcurrentLogging :: Assertion
testConcurrentLogging = do
  withSystemTempDirectory "concurrent-log-test" $ \tempDir -> do
    let logDir = tempDir </> "logs"
    
    -- Create a log store
    store <- createLogStore (Just logDir)
    
    -- Create sync points
    allThreadsFinished <- newEmptyMVar
    let threadCount = 5
    let messagesPerThread = 100
    
    -- Spawn threads
    forM_ [1..threadCount] $ \threadId -> do
      _ <- forkIO $ do
        forM_ [1..messagesPerThread] $ \msgId -> do
          _ <- logComponentInfo store 
                (T.pack $ "Thread" ++ show threadId)
                (T.pack $ "Message " ++ show msgId)
          -- Small delay to increase chance of interleaving
          threadDelay 1 -- 1 microsecond
        
        -- If this is the last thread, signal completion
        if threadId == threadCount
          then putMVar allThreadsFinished ()
          else return ()
    
    -- Wait for all threads to finish
    takeMVar allThreadsFinished
    
    -- Give a little time for all logs to be written
    threadDelay 100000 -- 100 ms
    
    -- Read all logs
    logs <- readLog store
    
    -- Verify we have the expected number of logs
    let expectedLogs = threadCount * messagesPerThread
    assertEqual ("Should have " ++ show expectedLogs ++ " log entries") 
                expectedLogs 
                (length logs)
    
    -- Verify each thread's logs
    forM_ [1..threadCount] $ \threadId -> do
      let threadName = T.pack $ "Thread" ++ show threadId
      let threadLogs = filter (isThreadLog threadName) logs
      assertEqual ("Thread " ++ show threadId ++ " should have " ++ show messagesPerThread ++ " logs")
                  messagesPerThread
                  (length threadLogs)
  where
    isThreadLog :: Text -> CompleteLogEntry -> Bool
    isThreadLog threadName entry =
      case entryContent entry of
        LogEvent "ContextLog" (Just obj) -> 
          case obj of
            Object o -> case lookup "component" o of
              Just (String c) -> c == threadName
              _ -> False
            _ -> False
        _ -> False

-- | Test context propagation across log entries
testContextPropagation :: Assertion
testContextPropagation = do
  withSystemTempDirectory "context-log-test" $ \tempDir -> do
    let logDir = tempDir </> "logs"
    
    -- Create a log store
    store <- createLogStore (Just logDir)
    
    -- Create a transaction context and log multiple entries in it
    let transactionId = "tx-12345"
    let userId = "user-789"
    
    -- Start transaction
    _ <- logWithContext store "Transaction" "Transaction started" 
          [ ("transaction_id", String transactionId)
          , ("user_id", String userId)
          ]
    
    -- Log steps within the transaction
    _ <- logWithContext store "Transaction" "Validating inputs" 
          [ ("transaction_id", String transactionId)
          , ("step", String "validation")
          ]
    
    _ <- logWithContext store "Transaction" "Processing payment" 
          [ ("transaction_id", String transactionId)
          , ("step", String "payment")
          ]
    
    _ <- logWithContext store "Transaction" "Transaction completed" 
          [ ("transaction_id", String transactionId)
          , ("status", String "success")
          ]
    
    -- Read the logs
    logs <- readLog store
    
    -- Verify transaction logs
    let transactionLogs = 
          filter (hasTransactionId transactionId) logs
    
    assertEqual "Should have 4 logs for the transaction" 4 (length transactionLogs)
    
    -- Check chain of log entries
    let sortedLogs = transactionLogs
        firstEntry = head sortedLogs
        
    -- Verify first entry has transaction_id and user_id
    assertBool "First log should have transaction context" $ 
      hasField "transaction_id" (String transactionId) firstEntry &&
      hasField "user_id" (String userId) firstEntry
    
    -- Verify last entry has transaction_id and status
    let lastEntry = last sortedLogs
    assertBool "Last log should have transaction context and status" $ 
      hasField "transaction_id" (String transactionId) lastEntry &&
      hasField "status" (String "success") lastEntry
  where
    hasTransactionId :: Text -> CompleteLogEntry -> Bool
    hasTransactionId txId entry =
      hasField "transaction_id" (String txId) entry
    
    hasField :: Text -> Value -> CompleteLogEntry -> Bool
    hasField fieldName expectedValue entry =
      case entryContent entry of
        LogEvent "ContextLog" (Just obj) -> 
          case obj of
            Object o -> case lookup fieldName o of
              Just v -> v == expectedValue
              _ -> False
            _ -> False
        _ -> False 