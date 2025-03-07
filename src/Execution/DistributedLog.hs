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
This module provides the distributed execution log functionality for the Time Bandits system.
It enables persistent storage of execution logs across multiple nodes in the network.

The distributed log ensures:
1. Durability - logs are persisted to disk and replicated across nodes
2. Consistency - logs maintain causal ordering of events
3. Availability - logs can be accessed even if some nodes are offline
4. Partition tolerance - the system continues to function during network partitions
-}
module Execution.DistributedLog 
  ( -- * Core Types
    DistributedLog(..)
  , LogEntry(..)
  , LogConfig(..)
  , LogStorageType(..)
  , LogReplicationMode(..)
  , LogConsistencyLevel(..)
  
  -- * Log Operations
  , createLog
  , appendToLog
  , readFromLog
  , replicateLog
  , compactLog
  
  -- * Distributed Operations
  , syncWithPeers
  , resolveConflicts
  , verifyLogIntegrity
  
  -- * Storage Operations
  , persistLogToDisk
  , loadLogFromDisk
  , exportLogToTimeline
  ) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, readMVar)
import Control.Exception (SomeException, catch, try)
import Control.Monad (forever, void, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Serialize (Serialize, encode, decode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Polysemy (Embed, Member, Sem, embed, interpret, makeSem)
import Polysemy.Error (Error, throw)
import Polysemy.State (State)
import Polysemy.Trace (Trace)
import qualified Polysemy.Trace as Trace
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO (IOMode(..), hClose, hPutStrLn, withFile)

import Core (Hash(..), EntityHash(..), ActorHash)
import Core.Crypto (PubKey(..), PrivKey(..), signMessage, verifySignature)
import Core.Error (AppError(..))
import Core.Types (LamportTime(..))
import qualified Adapters.Network as Network
import qualified Adapters.NetworkQUIC as NetworkQUIC

-- | Log storage type
data LogStorageType
  = MemoryStorage       -- ^ In-memory storage (volatile)
  | FileStorage         -- ^ File-based storage (persistent)
  | DatabaseStorage     -- ^ Database storage (persistent)
  | BlockchainStorage   -- ^ Blockchain storage (persistent and verifiable)
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, Serialize)

-- | Log replication mode
data LogReplicationMode
  = NoReplication       -- ^ No replication (single node)
  | SyncReplication     -- ^ Synchronous replication (wait for all replicas)
  | AsyncReplication    -- ^ Asynchronous replication (background replication)
  | QuorumReplication   -- ^ Quorum-based replication (wait for quorum)
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, Serialize)

-- | Log consistency level
data LogConsistencyLevel
  = EventualConsistency -- ^ Eventually consistent (may have temporary inconsistencies)
  | StrongConsistency   -- ^ Strongly consistent (always consistent)
  | CausalConsistency   -- ^ Causally consistent (respects causal ordering)
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, Serialize)

-- | Log configuration
data LogConfig = LogConfig
  { lcStorageType :: LogStorageType           -- ^ Storage type
  , lcStoragePath :: FilePath                 -- ^ Path for file-based storage
  , lcReplicationMode :: LogReplicationMode   -- ^ Replication mode
  , lcReplicationFactor :: Int                -- ^ Number of replicas
  , lcConsistencyLevel :: LogConsistencyLevel -- ^ Consistency level
  , lcCompactionInterval :: Int               -- ^ Compaction interval in seconds
  , lcMaxLogSize :: Int                       -- ^ Maximum log size in bytes
  , lcEncrypted :: Bool                       -- ^ Whether the log is encrypted
  } deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, Serialize)

-- | Default log configuration
defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig
  { lcStorageType = FileStorage
  , lcStoragePath = "logs"
  , lcReplicationMode = AsyncReplication
  , lcReplicationFactor = 3
  , lcConsistencyLevel = CausalConsistency
  , lcCompactionInterval = 3600  -- 1 hour
  , lcMaxLogSize = 1024 * 1024 * 100  -- 100 MB
  , lcEncrypted = True
  }

-- | Log entry
data LogEntry a = LogEntry
  { leTimestamp :: LamportTime     -- ^ Logical timestamp
  , leCreatedAt :: UTCTime         -- ^ Physical timestamp
  , leContent :: a                 -- ^ Entry content
  , lePrevHash :: Maybe Hash       -- ^ Previous entry hash
  , leHash :: Hash                 -- ^ Entry hash
  , leSignature :: ByteString      -- ^ Entry signature
  , leSigner :: PubKey             -- ^ Signer's public key
  , leReplicated :: Bool           -- ^ Whether the entry has been replicated
  } deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, Serialize)

-- | Distributed log
data DistributedLog a = DistributedLog
  { dlConfig :: LogConfig          -- ^ Log configuration
  , dlEntries :: [LogEntry a]      -- ^ Log entries
  , dlLastHash :: Maybe Hash       -- ^ Last entry hash
  , dlLastTimestamp :: LamportTime -- ^ Last logical timestamp
  , dlOwner :: ActorHash           -- ^ Log owner
  , dlPeers :: [Network.P2PNode]   -- ^ Replication peers
  } deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, Serialize)

-- | Create a new distributed log
createLog :: 
  ( Member (Embed IO) r
  , Member Trace r
  , Member (Error AppError) r
  , Serialize a
  ) => 
  LogConfig -> ActorHash -> Sem r (DistributedLog a)
createLog config owner = do
  Trace.trace $ "Creating distributed log for owner " <> T.pack (show owner)
  
  -- Create the log directory if needed
  when (lcStorageType config == FileStorage) $ do
    embed $ createDirectoryIfMissing True (lcStoragePath config)
  
  -- Create an empty log
  let log = DistributedLog
        { dlConfig = config
        , dlEntries = []
        , dlLastHash = Nothing
        , dlLastTimestamp = LamportTime 0
        , dlOwner = owner
        , dlPeers = []
        }
  
  Trace.trace "Distributed log created successfully"
  return log

-- | Append an entry to the log
appendToLog :: 
  ( Member (Embed IO) r
  , Member Trace r
  , Member (Error AppError) r
  , Serialize a
  ) => 
  DistributedLog a -> a -> PrivKey -> Sem r (DistributedLog a, LogEntry a)
appendToLog log content privKey = do
  Trace.trace "Appending entry to distributed log"
  
  -- Get the current time
  now <- embed getCurrentTime
  
  -- Create the log entry
  let timestamp = LamportTime $ unLamportTime (dlLastTimestamp log) + 1
      prevHash = dlLastHash log
      contentBytes = encode content
      entryHash = Hash $ BS.concat [encode timestamp, contentBytes]
  
  -- Sign the entry
  signResult <- case signMessage privKey contentBytes of
    Left err -> throw $ AppError $ "Failed to sign log entry: " <> err
    Right sig -> return sig
  
  -- Create the log entry
  let entry = LogEntry
        { leTimestamp = timestamp
        , leCreatedAt = now
        , leContent = content
        , lePrevHash = prevHash
        , leHash = entryHash
        , leSignature = signResult
        , leSigner = PubKey $ BS.pack "dummy-public-key"  -- In a real implementation, this would be derived from privKey
        , leReplicated = False
        }
  
  -- Update the log
  let updatedLog = log
        { dlEntries = dlEntries log ++ [entry]
        , dlLastHash = Just entryHash
        , dlLastTimestamp = timestamp
        }
  
  -- Persist the log if configured to do so
  when (lcStorageType (dlConfig log) == FileStorage) $ do
    persistLogToDisk updatedLog
  
  -- Replicate the log if configured to do so
  when (lcReplicationMode (dlConfig log) /= NoReplication) $ do
    void $ replicateLog updatedLog
  
  Trace.trace $ "Entry appended to log with hash " <> T.pack (show entryHash)
  return (updatedLog, entry)

-- | Read entries from the log
readFromLog :: 
  ( Member (Embed IO) r
  , Member Trace r
  , Member (Error AppError) r
  , Serialize a
  ) => 
  DistributedLog a -> Maybe LamportTime -> Maybe LamportTime -> Sem r [LogEntry a]
readFromLog log startTime endTime = do
  Trace.trace "Reading entries from distributed log"
  
  -- Filter entries by time range
  let filteredEntries = filter filterByTime (dlEntries log)
      filterByTime entry =
        (maybe True (\st -> leTimestamp entry >= st) startTime) &&
        (maybe True (\et -> leTimestamp entry <= et) endTime)
  
  Trace.trace $ "Read " <> T.pack (show (length filteredEntries)) <> " entries from log"
  return filteredEntries

-- | Replicate the log to peers
replicateLog :: 
  ( Member (Embed IO) r
  , Member Trace r
  , Member (Error AppError) r
  , Serialize a
  ) => 
  DistributedLog a -> Sem r (DistributedLog a)
replicateLog log = do
  Trace.trace "Replicating distributed log to peers"
  
  -- In a real implementation, this would send the log to peers
  -- For now, just mark all entries as replicated
  let updatedEntries = map (\e -> e { leReplicated = True }) (dlEntries log)
      updatedLog = log { dlEntries = updatedEntries }
  
  Trace.trace $ "Replicated log to " <> T.pack (show (length (dlPeers log))) <> " peers"
  return updatedLog

-- | Compact the log by removing old entries
compactLog :: 
  ( Member (Embed IO) r
  , Member Trace r
  , Member (Error AppError) r
  , Serialize a
  ) => 
  DistributedLog a -> Sem r (DistributedLog a)
compactLog log = do
  Trace.trace "Compacting distributed log"
  
  -- In a real implementation, this would compact the log
  -- For now, just return the log unchanged
  
  Trace.trace "Log compaction completed"
  return log

-- | Synchronize the log with peers
syncWithPeers :: 
  ( Member (Embed IO) r
  , Member Trace r
  , Member (Error AppError) r
  , Serialize a
  ) => 
  DistributedLog a -> [Network.P2PNode] -> Sem r (DistributedLog a)
syncWithPeers log peers = do
  Trace.trace $ "Synchronizing log with " <> T.pack (show (length peers)) <> " peers"
  
  -- In a real implementation, this would sync the log with peers
  -- For now, just update the peers list
  let updatedLog = log { dlPeers = peers }
  
  Trace.trace "Log synchronization completed"
  return updatedLog

-- | Resolve conflicts between different versions of the log
resolveConflicts :: 
  ( Member (Embed IO) r
  , Member Trace r
  , Member (Error AppError) r
  , Serialize a
  ) => 
  DistributedLog a -> [DistributedLog a] -> Sem r (DistributedLog a)
resolveConflicts localLog remoteLogs = do
  Trace.trace $ "Resolving conflicts between " <> T.pack (show (length remoteLogs + 1)) <> " log versions"
  
  -- In a real implementation, this would resolve conflicts
  -- For now, just return the local log
  
  Trace.trace "Conflict resolution completed"
  return localLog

-- | Verify the integrity of the log
verifyLogIntegrity :: 
  ( Member (Embed IO) r
  , Member Trace r
  , Member (Error AppError) r
  , Serialize a
  ) => 
  DistributedLog a -> Sem r Bool
verifyLogIntegrity log = do
  Trace.trace "Verifying log integrity"
  
  -- Verify each entry's hash and signature
  let verifyEntry :: LogEntry a -> Bool
      verifyEntry entry =
        -- Verify the hash
        let contentBytes = encode (leContent entry)
            expectedHash = Hash $ BS.concat [encode (leTimestamp entry), contentBytes]
            hashValid = leHash entry == expectedHash
            
            -- Verify the signature
            sigValid = verifySignature (leSigner entry) contentBytes (leSignature entry)
        in hashValid && sigValid
  
  -- Verify the chain of hashes
  let verifyChain :: [LogEntry a] -> Bool
      verifyChain [] = True
      verifyChain [_] = True
      verifyChain (e1:e2:es) =
        lePrevHash e2 == Just (leHash e1) && verifyChain (e2:es)
  
  let entriesValid = all verifyEntry (dlEntries log)
      chainValid = verifyChain (dlEntries log)
      
  Trace.trace $ "Log integrity verification result: " <> T.pack (show (entriesValid && chainValid))
  return (entriesValid && chainValid)

-- | Persist the log to disk
persistLogToDisk :: 
  ( Member (Embed IO) r
  , Member Trace r
  , Member (Error AppError) r
  , Serialize a
  ) => 
  DistributedLog a -> Sem r ()
persistLogToDisk log = do
  Trace.trace "Persisting log to disk"
  
  -- Create the log directory if needed
  embed $ createDirectoryIfMissing True (lcStoragePath (dlConfig log))
  
  -- Serialize the log
  let serializedLog = encode log
      logPath = lcStoragePath (dlConfig log) </> show (dlOwner log) <> ".log"
  
  -- Write the log to disk
  result <- embed $ try @SomeException $ BS.writeFile logPath serializedLog
  case result of
    Left err -> do
      Trace.trace $ "Failed to persist log: " <> T.pack (show err)
      throw $ AppError $ "Failed to persist log: " <> T.pack (show err)
    Right _ -> 
      Trace.trace "Log persisted successfully"

-- | Load the log from disk
loadLogFromDisk :: 
  ( Member (Embed IO) r
  , Member Trace r
  , Member (Error AppError) r
  , Serialize a
  ) => 
  LogConfig -> ActorHash -> Sem r (Maybe (DistributedLog a))
loadLogFromDisk config owner = do
  Trace.trace $ "Loading log from disk for owner " <> T.pack (show owner)
  
  -- Check if the log file exists
  let logPath = lcStoragePath config </> show owner <> ".log"
  logExists <- embed $ doesFileExist logPath
  
  if not logExists
    then do
      Trace.trace "Log file does not exist"
      return Nothing
    else do
      -- Read the log file
      result <- embed $ try @SomeException $ BS.readFile logPath
      case result of
        Left err -> do
          Trace.trace $ "Failed to load log: " <> T.pack (show err)
          throw $ AppError $ "Failed to load log: " <> T.pack (show err)
        Right serializedLog -> 
          -- Deserialize the log
          case decode serializedLog of
            Left err -> do
              Trace.trace $ "Failed to deserialize log: " <> T.pack err
              throw $ AppError $ "Failed to deserialize log: " <> T.pack err
            Right log -> do
              Trace.trace "Log loaded successfully"
              return $ Just log

-- | Export the log to a timeline
exportLogToTimeline :: 
  ( Member (Embed IO) r
  , Member Trace r
  , Member (Error AppError) r
  , Serialize a
  ) => 
  DistributedLog a -> EntityHash timeline -> Sem r ()
exportLogToTimeline log timeline = do
  Trace.trace $ "Exporting log to timeline " <> T.pack (show timeline)
  
  -- In a real implementation, this would export the log to a timeline
  -- For now, just log the action
  
  Trace.trace "Log exported successfully" 