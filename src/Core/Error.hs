{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Core.Error
Description: Error types for the Time-Bandits system

This module defines the error types used throughout the Time-Bandits system,
providing a consistent way to represent and handle errors across all components.
-}
module Core.Error
  ( -- * Result type
    Result
    
    -- * Error types
  , SystemError(..)
  , DefinitionError(..)
  , ExecutionError(..)
  , ResourceError(..)
  , TimelineError(..)
  , NetworkError(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | A Result is either an error or a successful value
type Result a = Either DefinitionError a

-- | System-level errors 
data SystemError
  = ConfigError Text           -- ^ Configuration error
  | InitializationError Text   -- ^ System initialization error
  | ShutdownError Text         -- ^ System shutdown error
  | InternalError Text         -- ^ Unexpected internal error
  deriving (Show, Eq, Generic)

-- | Definition errors for program and memory contracts
data DefinitionError
  = InvalidDefinition Text     -- ^ Invalid program definition
  | ValidationError Text       -- ^ Validation failure
  | ParseError Text            -- ^ Error parsing definition
  | InvalidReference Text      -- ^ Invalid reference to a resource
  deriving (Show, Eq, Generic)

-- | Execution errors during program execution
data ExecutionError
  = LockConflict Text          -- ^ Resource lock conflict
  | TimeMapConflict Text       -- ^ Inconsistent time map
  | PreconditionFailure Text   -- ^ Guard precondition failure
  | ResourceNotFound Text      -- ^ Resource not available
  | TimelineNotFound Text      -- ^ Timeline not available
  | MemorySlotError Text       -- ^ Error with memory slot
  | PermissionDenied Text      -- ^ Insufficient permissions
  deriving (Show, Eq, Generic)

-- | Resource-related errors
data ResourceError
  = ResourceNotAvailable Text  -- ^ Resource not available
  | ResourceLockFailed Text    -- ^ Failed to lock resource
  | InvalidResourceType Text   -- ^ Wrong resource type
  | EscrowError Text           -- ^ Error with escrow
  | OwnershipError Text        -- ^ Error with resource ownership
  deriving (Show, Eq, Generic)

-- | Timeline-related errors
data TimelineError
  = InvalidBlock Text          -- ^ Invalid block
  | InvalidChain Text          -- ^ Invalid chain structure
  | InvalidProof Text          -- ^ Invalid cryptographic proof
  | ForkDetected Text          -- ^ Timeline fork detected
  | ConsensusFailure Text      -- ^ Consensus failure
  deriving (Show, Eq, Generic)

-- | Network-related errors
data NetworkError
  = ConnectionError Text       -- ^ Connection error
  | TimeoutError Text          -- ^ Request timeout
  | ProtocolError Text         -- ^ Protocol violation
  | AuthenticationError Text   -- ^ Authentication failure
  | SerializationError Text    -- ^ Error serializing/deserializing data
  deriving (Show, Eq, Generic) 