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
  
  -- * Re-exports from Types.EffectBase
  EffectId(..),
  FactId(..),
  FactValue(..),
  TimeMapId(..),
  ObservedFact(..),
  ObservationProof(..),
  FactSnapshot,
  emptyFactSnapshot,
  
  -- * Phantom Types
  ActorType,
  ResourceType,
  TimelineType
) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

import Core.Common (Hash(..), EntityHash(..))

-- Re-export types from EffectBase
import Types.EffectBase
  ( EffectId(..)
  , FactId(..)
  , FactValue(..)
  , TimeMapId(..)
  , ObservedFact(..)
  , ObservationProof(..)
  , FactSnapshot
  , emptyFactSnapshot
  )

-- | Phantom types for type-safe entity references
data ActorType
data ResourceType  
data TimelineType

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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Specific error types for timeline operations
data TimelineErrorType
  = TimelineNotFound (EntityHash "Timeline")
  | InvalidTimelineState (EntityHash "Timeline")
  | TimelineSyncFailed (EntityHash "Timeline")
  | TimelineAlreadyExists (EntityHash "Timeline")
  | DescriptorError Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Specific error types for resource operations
data ResourceErrorType
  = ResourceNotFound (EntityHash "Resource")
  | ResourceInUse (EntityHash "Resource")
  | ResourcePermissionDenied (EntityHash "Resource")
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Specific error types for actor operations
data ActorErrorType
  = ActorNotFound (EntityHash "Actor")
  | InvalidActorState (EntityHash "Actor")
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Specific error types for cryptographic operations
data CryptoErrorType
  = SignatureInvalid
  | VerificationFailed
  | EncryptionFailed Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Specific error types for storage operations
data StorageErrorType
  = StorageFull
  | StorageCorrupted
  | StorageInaccessible Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Specific error types for program operations
data ProgramErrorType
  = CompilationError Text
  | RuntimeError Text
  | ValidationError Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

