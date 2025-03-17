{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TimeBandits.Core.ContentAddress.Repository
Description : Repository operations for content-addressable code
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides operations for storing and retrieving content-addressable code
in a repository, including name registration and lookups.
-}
module TimeBandits.Core.ContentAddress.Repository
  ( -- * Repository Operations
    newCodeRepository
  , storeDefinition
  , lookupByHash
  , lookupByName
  , registerName
  ) where

import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Concurrent.STM as STM

import TimeBandits.Core.ContentAddress.Types
  ( CodeHash
  , CodeDefinition(..)
  , CodeRepository(..)
  )

-- | Create a new empty code repository
newCodeRepository :: MonadIO m => m CodeRepository
newCodeRepository = do
  defMap <- liftIO $ STM.newTVarIO Map.empty
  nameMap <- liftIO $ STM.newTVarIO Map.empty
  return $ CodeRepository defMap nameMap

-- | Store a code definition in the repository
storeDefinition :: MonadIO m => CodeRepository -> CodeDefinition -> m CodeHash
storeDefinition repo def = liftIO $ STM.atomically $ do
  let hash = cdHash def
  defs <- STM.readTVar (crHashStore repo)
  STM.writeTVar (crHashStore repo) (Map.insert hash def defs)
  return hash

-- | Look up a definition by its hash
lookupByHash :: MonadIO m => CodeRepository -> CodeHash -> m (Maybe CodeDefinition)
lookupByHash repo hash = liftIO $ STM.atomically $ do
  defs <- STM.readTVar (crHashStore repo)
  return $ Map.lookup hash defs

-- | Look up a definition by its name
lookupByName :: MonadIO m => CodeRepository -> Text -> m (Maybe CodeDefinition)
lookupByName repo name = liftIO $ do
  mHash <- STM.atomically $ do
    nameMap <- STM.readTVar (crNameRegistry repo)
    return $ Map.lookup name nameMap
  case mHash of
    Nothing -> return Nothing
    Just hash -> lookupByHash repo hash

-- | Register a name for a hash in the repository
registerName :: MonadIO m => CodeRepository -> Text -> CodeHash -> m ()
registerName repo name hash = liftIO $ STM.atomically $ do
  nameMap <- STM.readTVar (crNameRegistry repo)
  STM.writeTVar (crNameRegistry repo) (Map.insert name hash nameMap) 