{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : TimeBandits.Core.TimeMap.Types
Description : Core types for the TimeMap
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides the core types for the TimeMap, which is the
central data structure that tracks all timelines in the system and
their relationships to each other.
-}
module TimeBandits.Core.TimeMap.Types
  ( -- * Core Types
    TimeMap(..)
  , TimeBranch(..)
  , TimeNode(..)
  , TimeEdge(..)
  , EdgeType(..)
  , LamportClock(..)
  ) where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)

import TimeBandits.Core.Timeline (Timeline, TimelineId)

-- | A branch in the time map, representing a chain of connected timelines
newtype TimeBranch = TimeBranch
  { getBranchTimelines :: [TimelineId]
  }
  deriving (Show, Eq, Generic)

-- | A node in the time map, representing a single timeline
data TimeNode = TimeNode
  { nodeHash :: TimelineId
  , nodeTimeline :: Timeline
  , nodeInEdges :: Set TimeEdge  -- ^ Incoming edges from ancestor timelines
  , nodeOutEdges :: Set TimeEdge  -- ^ Outgoing edges to descendant timelines
  }
  deriving (Show, Eq, Generic)

-- | An edge in the time map, representing a transition between timelines
data TimeEdge = TimeEdge
  { edgeFrom :: TimelineId  -- ^ Source timeline
  , edgeTo :: TimelineId    -- ^ Destination timeline
  , edgeType :: EdgeType    -- ^ Type of transition
  }
  deriving (Show, Eq, Generic, Ord)

-- | The type of transition between timelines
data EdgeType
  = ForkEdge     -- ^ A timeline split/fork
  | ContinueEdge -- ^ A linear continuation
  | MergeEdge    -- ^ A timeline merge
  deriving (Show, Eq, Generic, Ord)

-- | The core time map data structure
data TimeMap = TimeMap
  { timeNodes :: Map TimelineId TimeNode
  , timeBranches :: [TimeBranch]
  }
  deriving (Show, Eq, Generic)

-- | Lamport clock for tracking logical time
data LamportClock = LamportClock
  { clockValue :: Integer
  , clockOwner :: Text
  }
  deriving (Show, Eq, Generic) 