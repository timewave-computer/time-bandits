{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module: Types.Core
Description: Core shared types used throughout the Time Bandits system

This module provides essential shared types to break circular dependencies between
Core modules, Program modules, and Actor modules. It defines fundamental types needed
across multiple components of the system.
-}
module Types.Core (
  -- * Error Types
  AppError(..),
  TimelineErrorType(..),
  ResourceErrorType(..),
  ActorErrorType(..),
  CryptoErrorType(..),
  StorageErrorType(..),
  ProgramErrorType(..),
  
  -- * Time and Identifier Types
  TimeMapId(..),
  FactId(..),
  EffectId(..),
  
  -- * Fact Observation Types
  FactValue(..),
  ObservationProof(..),
  ObservedFact(..),
  FactSnapshot(..),
  
  -- * Functions
  emptyFactSnapshot
) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Data.Time.Clock (UTCTime)
import Core.Common (Hash(..), EntityHash(..), LamportTime(..))

-- | Error types for the application
data AppError
  = TimelineError TimelineErrorType
  | ResourceError ResourceErrorType
  | ActorError ActorErrorType
  | CryptoError CryptoErrorType
  | StorageError StorageErrorType
  | ProgramError ProgramErrorType
  | NetworkError Text
  | ConfigError Text
  | SystemError Text
  | UnknownError Text
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Specific error types for timeline operations
data TimelineErrorType
  = TimelineNotFound (EntityHash Timeline)
  | InvalidTimelineState (EntityHash Timeline)
  | TimelineSyncFailed (EntityHash Timeline)
  | TimelineAlreadyExists (EntityHash Timeline)
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Specific error types for resource operations
data ResourceErrorType
  = ResourceNotFound (EntityHash Resource)
  | InvalidResourceState (EntityHash Resource)
  | ResourceConflict (EntityHash Resource)
  | ResourceAlreadyExists (EntityHash Resource)
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Specific error types for actor operations
data ActorErrorType
  = ActorNotFound (EntityHash Actor)
  | InvalidActorState (EntityHash Actor)
  | ActorConflict (EntityHash Actor)
  | ActorAlreadyExists (EntityHash Actor)
  | InsufficientPermissions (EntityHash Actor)
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Specific error types for cryptographic operations
data CryptoErrorType
  = SignatureVerificationFailed
  | InvalidKey
  | EncryptionFailed
  | DecryptionFailed
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Specific error types for storage operations
data StorageErrorType
  = StorageReadFailed Text
  | StorageWriteFailed Text
  | StorageCorruption Text
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Specific error types for program operations
data ProgramErrorType
  = ProgramNotFound Text
  | InvalidProgramState Text
  | ProgramExecutionFailed Text
  | ProgramCompilationFailed Text
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Phantom types for type-safe entity references

-- | Phantom type for Actor entity
data Actor

-- | Phantom type for Resource entity
data Resource

-- | Phantom type for Timeline entity
data Timeline

-- | TimeMapId type
newtype TimeMapId = TimeMapId { getTimeMapId :: Hash }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Fact ID type
newtype FactId = FactId { getFactId :: Hash }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Effect ID type
newtype EffectId = EffectId { getEffectId :: Hash }
  deriving (Eq, Show, Generic, Ord)
  deriving anyclass (Serialize)

-- | Fact value
data FactValue
  = TextFact Text
  | BytesFact ByteString
  | IntFact Integer
  | BoolFact Bool
  | TimeFact UTCTime
  | NullFact
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Proof of observation
data ObservationProof
  = SignatureProof ByteString
  | MerkleProof [Hash]
  | ConsensusProof [Hash]
  | NoProof
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Observed fact with proof
data ObservedFact = ObservedFact
  { factId :: FactId
  , factValue :: FactValue
  , observationProof :: ObservationProof
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Snapshot of facts observed during effect execution
newtype FactSnapshot = FactSnapshot { facts :: [ObservedFact] }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Empty fact snapshot function
emptyFactSnapshot :: FactSnapshot
emptyFactSnapshot = FactSnapshot [] 