{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Core.CodeAddress
Description : Content-addressable code functionality for Time Bandits
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides functionality for content-addressable code storage,
allowing code to be identified by its content hash rather than by name.
This enables immutable code definitions, eliminates dependency conflicts,
and simplifies refactoring.
-}
module Core.CodeAddress
  ( -- * Types
    CodeHash
  , CodeDefinition(..)
  , DefType(..)
  , CodeRepository
  
    -- * Hash Generation
  , generateCodeHash
  , hashFunction
  , hashModule
  
    -- * Repository Operations
  , newCodeRepository
  , storeDefinition
  , lookupByHash
  , lookupByName
  , registerName
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Concurrent.STM as STM

import Crypto.Hash (hash, SHA256, Digest)

-- | Type alias for code hash values
type CodeHash = ByteString

-- | Represents a code definition with its hash and source
data CodeDefinition = CodeDefinition
  { cdHash   :: CodeHash   -- ^ Content hash of the code
  , cdSource :: Text       -- ^ Source code
  , cdType   :: DefType    -- ^ Type of definition (function or module)
  } deriving (Show, Eq, Generic)

-- | Type of code definition
data DefType = FunctionDef | ModuleDef
  deriving (Show, Eq, Generic)

-- | Repository for code definitions with name mapping
data CodeRepository = CodeRepository
  { crDefinitions :: STM.TVar (Map.Map CodeHash CodeDefinition)  -- ^ Map from hash to definition
  , crNameMap     :: STM.TVar (Map.Map Text CodeHash)           -- ^ Map from name to hash
  }

-- | Create a new empty code repository
newCodeRepository :: MonadIO m => m CodeRepository
newCodeRepository = do
  defMap <- liftIO $ STM.newTVarIO Map.empty
  nameMap <- liftIO $ STM.newTVarIO Map.empty
  return $ CodeRepository defMap nameMap

-- | Generate a hash for a code definition based on its content
generateCodeHash :: Text -> CodeHash
generateCodeHash content =
  let digest = hash (TE.encodeUtf8 content) :: Digest SHA256
      hashBytes = C8.pack $ show digest
  in hashBytes

-- | Hash a function definition
hashFunction :: Text -> Text -> CodeHash
hashFunction functionName functionBody =
  let content = T.concat ["function:", functionName, ":", functionBody]
  in generateCodeHash content

-- | Hash a module definition
hashModule :: Text -> [Text] -> Text -> CodeHash
hashModule moduleName imports moduleBody =
  let importsText = T.intercalate ";" imports
      content = T.concat ["module:", moduleName, ":", importsText, ":", moduleBody]
  in generateCodeHash content

-- | Store a code definition in the repository
storeDefinition :: MonadIO m => CodeRepository -> CodeDefinition -> m CodeHash
storeDefinition repo def = liftIO $ STM.atomically $ do
  let hash = cdHash def
  defs <- STM.readTVar (crDefinitions repo)
  STM.writeTVar (crDefinitions repo) (Map.insert hash def defs)
  return hash

-- | Look up a definition by its hash
lookupByHash :: MonadIO m => CodeRepository -> CodeHash -> m (Maybe CodeDefinition)
lookupByHash repo hash = liftIO $ STM.atomically $ do
  defs <- STM.readTVar (crDefinitions repo)
  return $ Map.lookup hash defs

-- | Look up a definition by its name
lookupByName :: MonadIO m => CodeRepository -> Text -> m (Maybe CodeDefinition)
lookupByName repo name = liftIO $ do
  mHash <- STM.atomically $ do
    nameMap <- STM.readTVar (crNameMap repo)
    return $ Map.lookup name nameMap
  case mHash of
    Nothing -> return Nothing
    Just hash -> lookupByHash repo hash

-- | Register a name for a hash in the repository
registerName :: MonadIO m => CodeRepository -> Text -> CodeHash -> m ()
registerName repo name hash = liftIO $ STM.atomically $ do
  nameMap <- STM.readTVar (crNameMap repo)
  STM.writeTVar (crNameMap repo) (Map.insert name hash nameMap) 