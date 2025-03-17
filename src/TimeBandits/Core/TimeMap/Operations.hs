{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : TimeBandits.Core.TimeMap.Operations
Description : Operations for managing the TimeMap
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides the core operations for the TimeMap, enabling the
creation, modification, and querying of timelines and their relationships.
-}
module TimeBandits.Core.TimeMap.Operations
  ( -- * Map Creation
    empty
  , singleton
  , fromList
  
  -- * Map Operations
  , addTimeline
  , mergeTimelines
  , linkTimelines
  , validateTimeline
  , timelineExists
  , getCurrentTimeMap
  
  -- * Query Operations
  , getTimeline
  , getAncestors
  , getDescendants
  , getRootTimelines
  , getLatestTimelines
  , findCommonAncestor
  
  -- * Utility Functions
  , renderTimeMap
  , timeMapToGraph
  , validateTimeMapIntegrity
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as Set
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO, liftIO)
import Prelude hiding (empty, fromList)

import TimeBandits.Core.Timeline (Timeline, timelineId, TimelineId)

import TimeBandits.Core.TimeMap.Types
  ( TimeMap(..)
  , TimeBranch(..)
  , TimeNode(..)
  , TimeEdge(..)
  , EdgeType(..)
  )

-- | Create an empty time map
empty :: TimeMap
empty = TimeMap
  { timeNodes = Map.empty
  , timeBranches = []
  }

-- | Create a time map with a single timeline
singleton :: Timeline -> TimeMap
singleton timeline = 
  let id = timelineId timeline
      node = TimeNode
        { nodeHash = id
        , nodeTimeline = timeline
        , nodeInEdges = Set.empty
        , nodeOutEdges = Set.empty
        }
  in TimeMap
    { timeNodes = Map.singleton id node
    , timeBranches = [TimeBranch [id]]
    }

-- | Create a time map from a list of timelines
fromList :: [Timeline] -> TimeMap
fromList timelines = foldr addTimeline empty timelines

-- | Add a timeline to the time map
addTimeline :: Timeline -> TimeMap -> TimeMap
addTimeline timeline timeMap =
  let id = timelineId timeline
      node = TimeNode
        { nodeHash = id
        , nodeTimeline = timeline
        , nodeInEdges = Set.empty
        , nodeOutEdges = Set.empty
        }
  in timeMap
    { timeNodes = Map.insert id node (timeNodes timeMap)
    , timeBranches = TimeBranch [id] : timeBranches timeMap
    }

-- | Merge two timelines, creating a new timeline
mergeTimelines :: TimelineId -> TimelineId -> TimeMap -> Maybe TimeMap
mergeTimelines src1 src2 timeMap =
  -- Implementation would find the two timelines, create a new merged timeline,
  -- and add it to the time map with appropriate edges
  Nothing -- Placeholder implementation

-- | Link two existing timelines with an edge
linkTimelines :: TimelineId -> TimelineId -> EdgeType -> TimeMap -> Maybe TimeMap
linkTimelines src dest edgeType timeMap = do
  -- Check that both timelines exist
  srcNode <- Map.lookup src (timeNodes timeMap)
  destNode <- Map.lookup dest (timeNodes timeMap)
  
  -- Create the edge
  let edge = TimeEdge
        { edgeFrom = src
        , edgeTo = dest
        , edgeType = edgeType
        }
  
  -- Update the nodes with the new edge
  let updatedSrcNode = srcNode { nodeOutEdges = Set.insert edge (nodeOutEdges srcNode) }
      updatedDestNode = destNode { nodeInEdges = Set.insert edge (nodeInEdges destNode) }
      
  -- Update the time map
  let updatedNodes = Map.insert dest updatedDestNode $
                    Map.insert src updatedSrcNode (timeNodes timeMap)
  
  -- Return the updated time map
  Just timeMap { timeNodes = updatedNodes }

-- | Validate whether a timeline conforms to the rules of the time map
validateTimeline :: TimelineId -> TimeMap -> Bool
validateTimeline hash timeMap =
  -- Implementation would check if the timeline's state is consistent with
  -- its ancestors and the system's temporal rules
  maybe False (const True) (Map.lookup hash (timeNodes timeMap))

-- | Check if a timeline exists in the time map
timelineExists :: TimelineId -> TimeMap -> Bool
timelineExists hash timeMap = Map.member hash (timeNodes timeMap)

-- | Get the current TimeMap (placeholder implementation)
getCurrentTimeMap :: MonadIO m => m TimeMap
getCurrentTimeMap = liftIO $ return empty  -- Placeholder: in a real implementation, this would get the current TimeMap from a repository

-- | Get a timeline from the time map
getTimeline :: TimelineId -> TimeMap -> Maybe Timeline
getTimeline hash timeMap = do
  node <- Map.lookup hash (timeNodes timeMap)
  return (nodeTimeline node)

-- | Get all ancestors of a timeline
getAncestors :: TimelineId -> TimeMap -> Set.Set TimelineId
getAncestors hash timeMap = case Map.lookup hash (timeNodes timeMap) of
  Nothing -> Set.empty
  Just node -> 
    let directAncestors = Set.map edgeFrom (nodeInEdges node)
        recursiveAncestors = Set.unions (Set.map (\h -> getAncestors h timeMap) directAncestors)
    in directAncestors `Set.union` recursiveAncestors

-- | Get all descendants of a timeline
getDescendants :: TimelineId -> TimeMap -> Set.Set TimelineId
getDescendants hash timeMap = case Map.lookup hash (timeNodes timeMap) of
  Nothing -> Set.empty
  Just node -> 
    let directDescendants = Set.map edgeTo (nodeOutEdges node)
        recursiveDescendants = Set.unions (Set.map (\h -> getDescendants h timeMap) directDescendants)
    in directDescendants `Set.union` recursiveDescendants

-- | Get all root timelines (those with no ancestors)
getRootTimelines :: TimeMap -> [TimelineId]
getRootTimelines timeMap = 
  let isRoot node = Set.null (nodeInEdges node)
  in Map.keys $ Map.filter isRoot (timeNodes timeMap)

-- | Get all leaf timelines (those with no descendants)
getLatestTimelines :: TimeMap -> [TimelineId]
getLatestTimelines timeMap = 
  let isLeaf node = Set.null (nodeOutEdges node)
  in Map.keys $ Map.filter isLeaf (timeNodes timeMap)

-- | Find the nearest common ancestor of two timelines
findCommonAncestor :: TimelineId -> TimelineId -> TimeMap -> Maybe TimelineId
findCommonAncestor hash1 hash2 timeMap =
  let ancestors1 = getAncestors hash1 timeMap
      ancestors2 = getAncestors hash2 timeMap
      common = ancestors1 `Set.intersection` ancestors2
  in if Set.null common
     then Nothing
     else Just (Set.findMin common) -- Return the "earliest" common ancestor

-- | Render a text representation of the time map for debugging
renderTimeMap :: TimeMap -> T.Text
renderTimeMap _ = "TimeMap representation" -- Placeholder implementation

-- | Convert a TimeMap to a graph representation
timeMapToGraph :: TimeMap -> T.Text
timeMapToGraph _ = "Graph representation" -- Placeholder implementation

-- | Validate the integrity of a TimeMap
validateTimeMapIntegrity :: TimeMap -> Bool
validateTimeMapIntegrity _ = True -- Placeholder implementation

-- | Helper for working with Maybe
timeMapMaybe :: (a -> Maybe b) -> [a] -> [b]
timeMapMaybe _ [] = []
timeMapMaybe f (x:xs) = case f x of
  Nothing -> timeMapMaybe f xs
  Just y  -> y : timeMapMaybe f xs 