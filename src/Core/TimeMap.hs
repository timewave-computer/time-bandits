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
Module: Core.TimeMap
Description: Core data structure for managing timeline relationships.

The TimeMap is the central data structure that tracks all timelines in the system
and their relationships to each other. It provides operations for:

1. Adding new timelines
2. Querying timeline relationships (ancestors, descendants)
3. Validating timeline consistency
4. Computing timeline hash relationships

This structure is essential for maintaining the integrity of the time-travel mechanics
and ensuring that timeline modifications adhere to the system's temporal rules.
-}
module Core.TimeMap
  ( -- * Core Types
    TimeMap
  , TimeBranch
  , TimeNode
  , TimeEdge
  
  -- * Map Creation
  , empty
  , singleton
  , fromList
  
  -- * Map Operations
  , addTimeline
  , mergeTimelines
  , linkTimelines
  , validateTimeline
  , timelineExists
  
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

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Core.Timeline (TimelineHash, Timeline, timelineHash)

-- | A branch in the time map, representing a chain of connected timelines
newtype TimeBranch = TimeBranch
  { getBranchTimelines :: [TimelineHash]
  }
  deriving (Show, Eq, Generic)

-- | A node in the time map, representing a single timeline
data TimeNode = TimeNode
  { nodeHash :: TimelineHash
  , nodeTimeline :: Timeline
  , nodeInEdges :: Set TimeEdge  -- ^ Incoming edges from ancestor timelines
  , nodeOutEdges :: Set TimeEdge  -- ^ Outgoing edges to descendant timelines
  }
  deriving (Show, Eq, Generic)

-- | An edge in the time map, representing a transition between timelines
data TimeEdge = TimeEdge
  { edgeFrom :: TimelineHash  -- ^ Source timeline
  , edgeTo :: TimelineHash    -- ^ Destination timeline
  , edgeType :: EdgeType      -- ^ Type of transition
  }
  deriving (Show, Eq, Generic)

-- | The type of transition between timelines
data EdgeType
  = ForkEdge     -- ^ A timeline split/fork
  | ContinueEdge -- ^ A linear continuation
  | MergeEdge    -- ^ A timeline merge
  deriving (Show, Eq, Generic)

-- | The core time map data structure
data TimeMap = TimeMap
  { timeNodes :: Map TimelineHash TimeNode
  , timeBranches :: [TimeBranch]
  }
  deriving (Show, Eq, Generic)

-- | Create an empty time map
empty :: TimeMap
empty = TimeMap
  { timeNodes = Map.empty
  , timeBranches = []
  }

-- | Create a time map with a single timeline
singleton :: Timeline -> TimeMap
singleton timeline = 
  let hash = timelineHash timeline
      node = TimeNode
        { nodeHash = hash
        , nodeTimeline = timeline
        , nodeInEdges = Set.empty
        , nodeOutEdges = Set.empty
        }
  in TimeMap
    { timeNodes = Map.singleton hash node
    , timeBranches = [TimeBranch [hash]]
    }

-- | Create a time map from a list of timelines
fromList :: [Timeline] -> TimeMap
fromList timelines = foldr addTimeline empty timelines

-- | Add a timeline to the time map
addTimeline :: Timeline -> TimeMap -> TimeMap
addTimeline timeline timeMap =
  let hash = timelineHash timeline
      node = TimeNode
        { nodeHash = hash
        , nodeTimeline = timeline
        , nodeInEdges = Set.empty
        , nodeOutEdges = Set.empty
        }
  in timeMap
    { timeNodes = Map.insert hash node (timeNodes timeMap)
    , timeBranches = TimeBranch [hash] : timeBranches timeMap
    }

-- | Merge two timelines, creating a new timeline
mergeTimelines :: TimelineHash -> TimelineHash -> TimeMap -> Maybe TimeMap
mergeTimelines src1 src2 timeMap =
  -- Implementation would find the two timelines, create a new merged timeline,
  -- and add it to the time map with appropriate edges
  Nothing -- Placeholder implementation

-- | Link two existing timelines with an edge
linkTimelines :: TimelineHash -> TimelineHash -> EdgeType -> TimeMap -> Maybe TimeMap
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
validateTimeline :: TimelineHash -> TimeMap -> Bool
validateTimeline hash timeMap =
  -- Implementation would check if the timeline's state is consistent with
  -- its ancestors and the system's temporal rules
  maybe False (const True) (Map.lookup hash (timeNodes timeMap))

-- | Check if a timeline exists in the time map
timelineExists :: TimelineHash -> TimeMap -> Bool
timelineExists hash timeMap = Map.member hash (timeNodes timeMap)

-- | Get a timeline from the time map
getTimeline :: TimelineHash -> TimeMap -> Maybe Timeline
getTimeline hash timeMap = do
  node <- Map.lookup hash (timeNodes timeMap)
  return (nodeTimeline node)

-- | Get all ancestors of a timeline
getAncestors :: TimelineHash -> TimeMap -> Set TimelineHash
getAncestors hash timeMap = case Map.lookup hash (timeNodes timeMap) of
  Nothing -> Set.empty
  Just node -> 
    let directAncestors = Set.map edgeFrom (nodeInEdges node)
        recursiveAncestors = Set.unions (Set.map (\h -> getAncestors h timeMap) directAncestors)
    in directAncestors `Set.union` recursiveAncestors

-- | Get all descendants of a timeline
getDescendants :: TimelineHash -> TimeMap -> Set TimelineHash
getDescendants hash timeMap = case Map.lookup hash (timeNodes timeMap) of
  Nothing -> Set.empty
  Just node -> 
    let directDescendants = Set.map edgeTo (nodeOutEdges node)
        recursiveDescendants = Set.unions (Set.map (\h -> getDescendants h timeMap) directDescendants)
    in directDescendants `Set.union` recursiveDescendants

-- | Get all root timelines (those with no ancestors)
getRootTimelines :: TimeMap -> [TimelineHash]
getRootTimelines timeMap = 
  let isRoot node = Set.null (nodeInEdges node)
  in Map.keys $ Map.filter isRoot (timeNodes timeMap)

-- | Get all leaf timelines (those with no descendants)
getLatestTimelines :: TimeMap -> [TimelineHash]
getLatestTimelines timeMap = 
  let isLeaf node = Set.null (nodeOutEdges node)
  in Map.keys $ Map.filter isLeaf (timeNodes timeMap)

-- | Find the nearest common ancestor of two timelines
findCommonAncestor :: TimelineHash -> TimelineHash -> TimeMap -> Maybe TimelineHash
findCommonAncestor hash1 hash2 timeMap =
  let ancestors1 = getAncestors hash1 timeMap
      ancestors2 = getAncestors hash2 timeMap
      common = ancestors1 `Set.intersection` ancestors2
  in if Set.null common
     then Nothing
     else Just (Set.findMin common) -- Return the "earliest" common ancestor

-- | Render a text representation of the time map for debugging
renderTimeMap :: TimeMap -> Text
renderTimeMap timeMap =
  let nodeStrings = Map.foldrWithKey (\hash node acc ->
        let inEdges = T.intercalate ", " $ map (T.pack . show . edgeFrom) $ Set.toList $ nodeInEdges node
            outEdges = T.intercalate ", " $ map (T.pack . show . edgeTo) $ Set.toList $ nodeOutEdges node
        in T.pack (show hash) <> " [in: " <> inEdges <> ", out: " <> outEdges <> "]" : acc
        ) [] (timeNodes timeMap)
  in T.unlines nodeStrings

-- | Convert a time map to a graph representation for visualization
timeMapToGraph :: TimeMap -> [(TimelineHash, TimelineHash, EdgeType)]
timeMapToGraph timeMap =
  let makeEdges node = map (\edge -> (edgeFrom edge, edgeTo edge, edgeType edge)) $ 
                         Set.toList $ nodeOutEdges node
  in concatMap makeEdges $ Map.elems $ timeNodes timeMap

-- | Validate the integrity of the entire time map
validateTimeMapIntegrity :: TimeMap -> Bool
validateTimeMapIntegrity timeMap =
  let -- Check that every edge referenced in a node actually connects to a valid node
      validateEdge (TimeEdge from to _) = Map.member from (timeNodes timeMap) && Map.member to (timeNodes timeMap)
      
      -- Check that for every out-edge from node A to B, there's a corresponding in-edge on node B from A
      validateEdgeSymmetry (TimeEdge from to edgeType) =
        case Map.lookup to (timeNodes timeMap) of
          Nothing -> False
          Just destNode -> 
            let matchingInEdge = Set.member (TimeEdge from to edgeType) (nodeInEdges destNode)
            in matchingInEdge
      
      -- All edges in the time map
      allEdges = concatMap (Set.toList . nodeOutEdges) $ Map.elems $ timeNodes timeMap
      
  in all validateEdge allEdges && all validateEdgeSymmetry allEdges 