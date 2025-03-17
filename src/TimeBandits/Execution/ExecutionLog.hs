{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module: TimeBandits.Execution.ExecutionLog
Description: Defines the ExecutionLog type for tracking applied effects

This module defines the ExecutionLog type, which tracks the history of applied
effects per resource. Each resource maintains its own causal log of effects.

@since 0.1.0
-}
module TimeBandits.Execution.ExecutionLog
  ( -- * Core Types
    ExecutionLog
  , LogEntry(..)
  , LogError(..)
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import Data.Hashable (Hashable)

import TimeBandits.Core.ResourceId (ResourceId)
import TimeBandits.Core.Effect (Effect)

-- | Hash type for content addressing log entries
type Hash = BS.ByteString

-- | Type representing a proof of correct execution
type Proof = BS.ByteString

-- | ExecutionLog maps ResourceId to a list of LogEntries
type ExecutionLog = Map.Map ResourceId [LogEntry]

-- | LogEntry represents a single applied effect in the execution log
data LogEntry = LogEntry
  { effect :: Effect
  , parentHash :: Maybe Hash
  , timeMapHash :: Hash
  , resultingStateHash :: Hash
  , zkProof :: Maybe Proof
  }
  deriving (Eq, Show, Generic)

-- | LogError represents errors that can occur during log operations
data LogError =
    EntryNotFound Hash
  | InvalidParentHash Hash
  | InvalidTimeMapHash Hash
  | InvalidProof Proof
  | LogCorrupted ResourceId
  deriving (Eq, Show, Generic) 