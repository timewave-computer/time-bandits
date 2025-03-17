{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : TimeBandits.Core.ContentAddress.Types
Description : Type definitions for content-addressable code
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides type definitions for content-addressable code,
including hashes, code definitions, and repositories.
-}
module TimeBandits.Core.ContentAddress.Types
  ( -- * Core Types
    CodeHash
  , mkCodeHash
  , unCodeHash  -- Export the unCodeHash accessor
  , HashAlgorithm(..)
    
  -- * Code Definitions
  , CodeDefinition(..)
  , DefType(..)
  
  -- * Repository Types
  , CodeRepository(..)
  , HashStore
  , NameRegistry
  , Name
  ) where

import Control.Concurrent.STM (TVar)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.ByteString as BS

import TimeBandits.Core.Common.Types (Hash(..))

-- | Hash algorithm used for content addressing
data HashAlgorithm = 
    SHA256     -- ^ SHA-256 algorithm
  | Blake2b    -- ^ Blake2b algorithm
  | SHA3_256   -- ^ SHA3-256 algorithm
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

-- | A hash that uniquely identifies a piece of code
newtype CodeHash = CodeHash { unCodeHash :: Hash }
  deriving stock (Show, Eq, Ord, Generic)

-- Manual instance for Hashable since we can't derive it (Hash doesn't have Hashable)
instance Hashable CodeHash where
  hashWithSalt salt (CodeHash (Hash bs)) = hashWithSalt salt (BS.unpack bs)

-- | Create a code hash from a ByteString
mkCodeHash :: ByteString -> CodeHash
mkCodeHash = CodeHash . Hash

-- | Type of code definition
data DefType = 
    ModuleDef  -- ^ Module definition
  | FunctionDef -- ^ Function definition
  | TypeDef    -- ^ Type definition
  | ClassDef   -- ^ Type class definition
  | InstanceDef -- ^ Instance definition
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

-- | A definition in the content-addressable store
data CodeDefinition = CodeDefinition
  { cdHash   :: !CodeHash    -- ^ Hash of the code
  , cdSource :: !Text        -- ^ Source code
  , cdType   :: !DefType     -- ^ Type of definition
  } 
  deriving stock (Show, Eq)

-- | Name type for named code entities
type Name = Text

-- | Store of code definitions indexed by hash
type HashStore = Map CodeHash CodeDefinition

-- | Registry mapping names to hashes
type NameRegistry = Map Name CodeHash

-- | Repository for content-addressable code
data CodeRepository = CodeRepository
  { crHashStore     :: !(TVar HashStore)      -- ^ Store of code definitions by hash
  , crNameRegistry  :: !(TVar NameRegistry)   -- ^ Registry mapping names to hashes
  } 

-- | Since we can't easily derive Show for CodeRepository (because TVar doesn't have Show)
-- we'll just keep it simple and not define a Show instance at all.
-- Anywhere that needs to display a CodeRepository should implement its own
-- custom display logic.

-- Eq can be derived using standalone deriving
deriving stock instance Eq CodeRepository 