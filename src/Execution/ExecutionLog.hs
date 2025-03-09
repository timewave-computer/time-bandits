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
This module provides the ExecutionLog abstraction and related functionality.
ExecutionLogs are content-addressed, causally linked records of all applied effects.

Execution Logs:
- Maintain a complete audit trail of all program transitions
- Are content-addressed for tamper evidence
- Are causally linked to ensure proper execution order
- Support verification of execution histories
-}
module Execution.ExecutionLog 
  ( -- * Core Types
    ExecutionLog
  , LogStore
  , LogIndex
  , LogQuery(..)
  , LogError(..)
  
  -- * Log Operations
  , createLogStore
  , appendLogEntry
  , getLogEntry
  , queryLog
  , verifyLogChain
  , exportLogToFile
  
  -- * Re-exports from TransitionMessage
  , LogEntry(..)
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw, catch)
import System.IO (IOMode(..), withFile)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (foldl)

-- Import from TimeBandits modules
import Core (Hash(..), EntityHash(..))
import Core.Types
  ( AppError(..)
  , LamportTime(..)
  , TimelineErrorType(..)
  )
import Core.Resource 
  ( Resource
  , Address
  )
import Programs.Program 
  ( ProgramId
  )
import Programs.ProgramEffect 
  ( Effect
  )
import Actors.TransitionMessage
  ( LogEntry(..)
  , TransitionMessage
  , verifyLogEntryChain
  )

-- | Index for efficient log queries
type LogIndex = Map ProgramId [EntityHash "LogEntry"]

-- | Log query criteria
data LogQuery
  = ByProgramId ProgramId
  | ByTimeRange LamportTime LamportTime
  | ByEffect Effect
  | ByCausalChain (EntityHash "LogEntry")
  | Combined [LogQuery]
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Error types for log operations
data LogError
  = EntryNotFound (EntityHash "LogEntry")
  | InvalidEntry (EntityHash "LogEntry")
  | ChainBroken (EntityHash "LogEntry") (EntityHash "LogEntry")
  | StorageError Text
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | The execution log data structure
data ExecutionLog = ExecutionLog
  { entries :: Map (EntityHash "LogEntry") LogEntry
  , indexes :: LogIndex
  , rootEntry :: Maybe (EntityHash "LogEntry")
  , latestEntry :: Maybe (EntityHash "LogEntry")
  }
  deriving (Eq, Show)

-- | LogStore is just an alias for ExecutionLog
type LogStore = ExecutionLog

-- | Create a new log store
createLogStore :: 
  (Member (Error AppError) r) => 
  Sem r LogStore
createLogStore = do
  pure $ ExecutionLog
    { entries = Map.empty
    , indexes = Map.empty
    , rootEntry = Nothing
    , latestEntry = Nothing
    }

-- | Append a log entry to the log store
appendLogEntry ::
  (Member (Error AppError) r) =>
  LogStore ->
  LogEntry ->
  Sem r LogStore
appendLogEntry store entry = do
  -- Verify the entry can be appended
  let entryHash = entryId entry
  
  -- Check for duplicate entry
  case Map.lookup entryHash (entries store) of
    Just _ -> throw $ TimelineError $ TimelineGenericError $ "Duplicate log entry: " <> T.pack (show entryHash)
    Nothing -> pure ()
  
  -- Verify causal link if not the first entry
  case latestEntry store of
    Nothing -> do
      -- This is the first entry
      let updatedStore = store
            { entries = Map.insert entryHash entry (entries store)
            , rootEntry = Just entryHash
            , latestEntry = Just entryHash
            }
      
      -- Update indexes
      updatedStore' <- updateLogIndexes updatedStore entry
      pure updatedStore'
    
    Just latest -> do
      -- Get the latest entry
      latestEntry <- case Map.lookup latest (entries store) of
        Just e -> pure e
        Nothing -> throw $ TimelineError $ TimelineGenericError "Latest entry not found in store"
      
      -- Verify causal link
      valid <- verifyLogEntryChain entry latestEntry
      if not valid
        then throw $ TimelineError $ TimelineGenericError "Invalid causal link in log entry"
        else do
          -- Append the entry
          let updatedStore = store
                { entries = Map.insert entryHash entry (entries store)
                , latestEntry = Just entryHash
                }
          
          -- Update indexes
          updatedStore' <- updateLogIndexes updatedStore entry
          pure updatedStore'

-- | Update log indexes when adding a new entry
updateLogIndexes ::
  (Member (Error AppError) r) =>
  LogStore ->
  LogEntry ->
  Sem r LogStore
updateLogIndexes store entry = do
  -- In a real implementation, this would:
  -- 1. Update indexes by program ID
  -- 2. Update indexes by time
  -- 3. Update indexes by effect type
  
  -- Placeholder implementation
  pure store

-- | Get a log entry by its ID
getLogEntry ::
  (Member (Error AppError) r) =>
  LogStore ->
  EntityHash "LogEntry" ->
  Sem r LogEntry
getLogEntry store entryId = do
  -- Look up the entry in the store
  case Map.lookup entryId (entries store) of
    Just entry -> pure entry
    Nothing -> throw $ TimelineError $ TimelineGenericError $ "Log entry not found: " <> T.pack (show entryId)

-- | Query the log using search criteria
queryLog ::
  (Member (Error AppError) r) =>
  LogStore ->
  LogQuery ->
  Sem r [LogEntry]
queryLog store query = do
  -- In a real implementation, this would use the indexes to efficiently query
  -- For now, implement a naive linear search
  case query of
    ByProgramId progId -> do
      -- Get all entries for the program
      let entryIds = Map.findWithDefault [] progId (indexes store)
      -- Resolve the entries
      resolveEntries store entryIds
    
    ByTimeRange start end -> do
      -- Filter entries by time range
      let allEntries = Map.elems (entries store)
      pure [e | e <- allEntries, appliedAt e >= start && appliedAt e <= end]
    
    ByEffect targetEffect -> do
      -- Filter entries by effect
      let allEntries = Map.elems (entries store)
      pure [e | e <- allEntries, show (appliedEffect e) == show targetEffect]
    
    ByCausalChain startId -> do
      -- Follow the causal chain starting from an entry
      followCausalChain store startId
    
    Combined queries -> do
      -- Apply each query and intersect the results
      results <- mapM (queryLog store) queries
      -- Naive implementation of intersection
      case results of
        [] -> pure []
        (first:rest) -> pure $ foldl intersectEntries first rest

-- | Resolve a list of entry IDs to entries
resolveEntries ::
  (Member (Error AppError) r) =>
  LogStore ->
  [EntityHash "LogEntry"] ->
  Sem r [LogEntry]
resolveEntries store ids = do
  -- Look up each entry
  mapM (getLogEntry store) ids

-- | Follow the causal chain starting from an entry
followCausalChain ::
  (Member (Error AppError) r) =>
  LogStore ->
  EntityHash "LogEntry" ->
  Sem r [LogEntry]
followCausalChain store startId = do
  -- Get the starting entry
  startEntry <- getLogEntry store startId
  
  -- Follow the chain recursively
  go [startEntry] (causalParent startEntry)
  where
    go acc parentId
      -- If we've reached the root (genesis entry)
      | parentId == EntityHash (Hash "genesis") = pure acc
      | otherwise = do
          -- Try to get the parent entry
          parentEntryResult <- 
            (Just <$> getLogEntry store parentId)
            `catch` (\(_ :: AppError) -> pure Nothing)
          
          case parentEntryResult of
            -- Chain is broken or we've reached the end
            Nothing -> pure acc
            -- Continue following the chain
            Just parentEntry -> go (parentEntry : acc) (causalParent parentEntry)

-- | Intersect two lists of entries
intersectEntries :: [LogEntry] -> [LogEntry] -> [LogEntry]
intersectEntries as bs = [a | a <- as, a `elem` bs]

-- | Verify the integrity of the log chain
verifyLogChain ::
  (Member (Error AppError) r) =>
  LogStore ->
  Sem r Bool
verifyLogChain store = do
  -- Start from the latest entry and verify the chain back to the root
  case latestEntry store of
    Nothing -> pure True  -- Empty log is valid
    Just latestId -> do
      -- Follow the chain and verify each link
      chain <- followCausalChain store latestId
      
      -- Verify each adjacent pair in the chain
      valid <- verifyChainLinks chain
      pure valid

-- | Verify the links between adjacent entries in a chain
verifyChainLinks ::
  (Member (Error AppError) r) =>
  [LogEntry] ->
  Sem r Bool
verifyChainLinks [] = pure True
verifyChainLinks [_] = pure True  -- Single entry is valid
verifyChainLinks (a:b:rest) = do
  -- Verify the link between a and b
  valid <- verifyLogEntryChain a b
  if valid
    then verifyChainLinks (b:rest)
    else pure False

-- | Export the log to a file for external analysis
exportLogToFile ::
  (Member (Error AppError) r) =>
  LogStore ->
  FilePath ->
  Sem r ()
exportLogToFile store filePath = do
  -- Get all entries in causal order
  entries <- case rootEntry store of
    Nothing -> pure []
    Just root -> followCausalChain store root
  
  -- Format each entry as text
  let formattedEntries = map formatLogEntry entries
  let content = T.unlines formattedEntries
  
  -- Write to file
  liftIO $ TIO.writeFile filePath content
  where
    -- Format a log entry as text
    formatLogEntry entry = T.pack $ 
      "Entry: " ++ show (entryId entry) ++ "\n" ++
      "Effect: " ++ show (appliedEffect entry) ++ "\n" ++
      "Time: " ++ show (appliedAt entry) ++ "\n" ++
      "Parent: " ++ show (causalParent entry) ++ "\n" ++
      "Result: " ++ BS.unpack (resultState entry) ++ "\n"

    -- Lift IO to Sem
    liftIO :: IO a -> Sem r a
    liftIO = undefined  -- In a real implementation, this would use the IO effect 

getLatestLogEntry :: (Member (Error AppError) r) => LogStore -> Sem r LogEntry
getLatestLogEntry store = do
  case latestEntry store of
    Just entryId -> getLogEntry store entryId
    Nothing -> throw $ TimelineError $ TimelineGenericError "Latest entry not found in store" 