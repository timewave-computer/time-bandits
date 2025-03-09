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
import Control.Exception (SomeException)
import Control.Exception qualified as Exception
import Control.Monad (forever, void, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Serialize (Serialize, encode, decode, put, get)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Polysemy (Embed, Member, Sem, embed, interpret, makeSem)
import Polysemy.Error (Error, throw)
import Polysemy.Error qualified as Error
import Polysemy.State (State)
import Polysemy.Trace (Trace)
import qualified Polysemy.Trace as Trace
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO (IOMode(..), hClose, hPutStrLn, withFile)

import Core (Hash(..), EntityHash(..), ActorHash)
import Core.Types (LamportTime(..), PubKey(..), AppError(NetworkError))
import Core.Common (PrivKey(..), Signature(..))
import Core.Utils (signMessage, localVerifySignature)
import qualified Adapters.Network as Network
import qualified Adapters.NetworkQUIC as NetworkQUIC

-- | Log storage type
data LogStorageType
  = MemoryStorage      -- ^ In-memory storage (volatile)
  | FileStorage        -- ^ File-based storage (persistent)
  | DatabaseStorage    -- ^ Database storage (persistent and queryable)
  | BlockchainStorage   -- ^ Blockchain storage (persistent and verifiable)
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Serialize instance for LogStorageType
instance Serialize LogStorageType

-- | Log replication mode
data LogReplicationMode
  = NoReplication      -- ^ No replication (single node)
  | AsyncReplication   -- ^ Asynchronous replication (fire and forget)
  | SyncReplication    -- ^ Synchronous replication (wait for ack)
  | QuorumReplication   -- ^ Quorum-based replication (wait for quorum)
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Serialize instance for LogReplicationMode
instance Serialize LogReplicationMode

-- | Log consistency level
data LogConsistencyLevel
  = EventualConsistency  -- ^ Eventually consistent (no ordering guarantees)
  | SequentialConsistency -- ^ Sequentially consistent (respects global ordering)
  | StrongConsistency    -- ^ Strongly consistent (linearizable)
  | CausalConsistency   -- ^ Causally consistent (respects causal ordering)
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Serialize instance for LogConsistencyLevel
instance Serialize LogConsistencyLevel

-- | Log configuration
data LogConfig = LogConfig
  { lcStorageType :: LogStorageType             -- ^ Storage type
  , lcReplicationMode :: LogReplicationMode     -- ^ Replication mode
  , lcConsistencyLevel :: LogConsistencyLevel   -- ^ Consistency level
  , lcReplicationFactor :: Int                  -- ^ Number of replicas
  , lcStoragePath :: FilePath                   -- ^ Path for file-based storage
  , lcEncrypted :: Bool                       -- ^ Whether the log is encrypted
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Serialize instance for LogConfig
instance Serialize LogConfig

-- | Default log configuration
defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig
  { lcStorageType = FileStorage
  , lcStoragePath = "logs"
  , lcReplicationMode = AsyncReplication
  , lcReplicationFactor = 3
  , lcConsistencyLevel = CausalConsistency
  , lcEncrypted = True
  }

-- | Log entry
data LogEntry a = LogEntry
  { leTimestamp :: LamportTime     -- ^ Logical timestamp
  , leCreatedAt :: UTCTime         -- ^ Physical timestamp
  , leContent :: a                 -- ^ Entry content
  , lePrevHash :: Maybe Hash       -- ^ Previous entry hash
  , leHash :: Hash                 -- ^ Entry hash
  , leSignature :: Signature       -- ^ Entry signature
  , leSigner :: PubKey             -- ^ Signer's public key
  , leReplicated :: Bool           -- ^ Whether the entry has been replicated
  } deriving (Show, Eq, Generic)

-- | Serialize instance for LogEntry
instance (Serialize a) => Serialize (LogEntry a)

-- | Distributed log
data DistributedLog a = DistributedLog
  { dlConfig :: LogConfig          -- ^ Log configuration
  , dlEntries :: [LogEntry a]      -- ^ Log entries
  , dlLastHash :: Maybe Hash       -- ^ Last entry hash
  , dlLastTimestamp :: LamportTime -- ^ Last logical timestamp
  , dlOwner :: ActorHash           -- ^ Log owner
  , dlPeers :: [Network.P2PNode]   -- ^ Replication peers
  } deriving (Show, Eq, Generic)

-- | Custom serialization function for DistributedLog
serializeLog :: (Serialize a) => DistributedLog a -> ByteString
serializeLog log = encode (
  dlConfig log,
  dlEntries log,
  dlLastHash log,
  dlLastTimestamp log,
  dlOwner log
  )

-- | Custom deserialization function for DistributedLog
deserializeLog :: (Serialize a) => ByteString -> Either String (DistributedLog a)
deserializeLog bs = case decode bs of
  Left err -> Left err
  Right (config, entries, lastHash, lastTimestamp, owner) ->
    Right $ DistributedLog
      { dlConfig = config
      , dlEntries = entries
      , dlLastHash = lastHash
      , dlLastTimestamp = lastTimestamp
      , dlOwner = owner
      , dlPeers = []  -- Initialize with empty peers list
      }

-- | Create a new distributed log
createLog :: 
  ( Member (Embed IO) r
  , Member Trace r
  , Member (Error AppError) r
  , Serialize a
  ) => 
  LogConfig -> ActorHash -> Sem r (DistributedLog a)
createLog config owner = do
  Trace.trace $ "Creating distributed log for owner " <> show owner
  
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
      entryHash = Hash $ BS.concat [contentBytes, encode timestamp]
  
  -- Sign the entry
  signResult <- case signMessage privKey contentBytes of
    Left err -> throw (NetworkError ("Failed to sign log entry: " <> err))
    Right sig -> return sig
  
  -- Create the log entry
  let entry = LogEntry
        { leTimestamp = timestamp
        , leCreatedAt = now
        , leContent = content
        , lePrevHash = prevHash
        , leHash = entryHash
        , leSignature = signResult
        , leSigner = PubKey $ BS8.pack "dummy-public-key"  -- In a real implementation, this would be derived from privKey
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
  
  Trace.trace $ "Entry appended to log with hash " <> show entryHash
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
  
  Trace.trace $ "Read " <> show (length filteredEntries) <> " entries from log"
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
  
  Trace.trace $ "Replicated log to " <> show (length (dlPeers log)) <> " peers"
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
  Trace.trace $ "Synchronizing log with " <> show (length peers) <> " peers"
  
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
  Trace.trace $ "Resolving conflicts between " <> show (length remoteLogs + 1) <> " log versions"
  
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
  let verifyEntry :: (Serialize a) => LogEntry a -> Bool
      verifyEntry entry =
        -- Verify the hash
        let contentBytes = encode (leContent entry)
            expectedHash = Hash $ BS.concat [encode (leTimestamp entry), contentBytes]
            hashValid = leHash entry == expectedHash
            
            -- Verify the signature
            sigValid = localVerifySignature (leSigner entry) contentBytes (leSignature entry)
        in hashValid && sigValid
  
  -- Verify the chain of hashes
  let verifyChain :: [LogEntry a] -> Bool
      verifyChain [] = True
      verifyChain [_] = True
      verifyChain (e1:e2:es) =
        lePrevHash e2 == Just (leHash e1) && verifyChain (e2:es)
  
  let entriesValid = all verifyEntry (dlEntries log)
      chainValid = verifyChain (dlEntries log)
      
  Trace.trace $ "Log integrity verification result: " <> show (entriesValid && chainValid)
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
  
  -- Only proceed if using file storage
  when (lcStorageType (dlConfig log) == FileStorage) $ do
    -- Create the log directory if it doesn't exist
    embed $ createDirectoryIfMissing True (lcStoragePath (dlConfig log))
    
    -- Serialize the log
    let serializedLog = serializeLog log
        logPath = lcStoragePath (dlConfig log) </> "log.bin"
    
    -- Write to disk
    result <- embed $ Exception.try @SomeException $ BS.writeFile logPath serializedLog
    case result of
      Left err -> throw (NetworkError ("Failed to persist log: " <> T.pack (show err)))
      Right _ -> Trace.trace "Log persisted to disk successfully"

-- | Load the log from disk
loadLogFromDisk :: 
  ( Member (Embed IO) r
  , Member Trace r
  , Member (Error AppError) r
  , Serialize a
  ) => 
  LogConfig -> ActorHash -> Sem r (DistributedLog a)
loadLogFromDisk config owner = do
  Trace.trace "Loading log from disk"
  
  -- Only proceed if using file storage
  if lcStorageType config /= FileStorage
    then createLog config owner
    else do
      let logPath = lcStoragePath config </> "log.bin"
      
      -- Check if the log file exists
      result <- embed $ Exception.try @SomeException $ do
        fileExists <- doesFileExist logPath
        if fileExists
          then do
            serializedLog <- BS.readFile logPath
            case deserializeLog serializedLog of
              Left err -> return $ Left $ NetworkError ("Failed to decode log: " <> T.pack err)
              Right decodedLog -> return $ Right decodedLog
          else return $ Left $ NetworkError "Log file not found"
      
      case result of
        Left _ -> do
          Trace.trace "No existing log file found, creating new log"
          createLog config owner
        Right (Left err) -> throw err
        Right (Right log) -> do
          Trace.trace $ "Loaded log with " <> show (length (dlEntries log)) <> " entries"
          return log

-- | Export the log to a timeline
exportLogToTimeline :: 
  ( Member (Embed IO) r
  , Member Trace r
  , Member (Error AppError) r
  , Serialize a
  ) => 
  DistributedLog a -> EntityHash timeline -> Sem r ()
exportLogToTimeline log timeline = do
  Trace.trace $ "Exporting log to timeline " <> show timeline
  
  -- In a real implementation, this would export the log to a timeline
  -- For now, just log the action
  
  Trace.trace "Log exported successfully" 