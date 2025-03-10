{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module TimeMap
  ( -- * Core Types
    TimeMap(..)
  , TimeBranch(..)
  , TimeNode(..)
  , TimeEdge(..)
  , EdgeType(..)
  
  -- * Map Creation
  , empty
  , singleton
  , fromList
  
  -- * Map Operations
  , addTimeline
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
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Timeline (Timeline, timelineId)
import Types (TimelineId)

-- | A branch in the time map, representing a chain of connected timelines
newtype TimeBranch = TimeBranch
  { getBranchTimelines :: [TimelineId]
  }
  deriving stock (Show, Eq, Generic)

-- | A node in the time map, representing a single timeline
data TimeNode = TimeNode
  { nodeHash :: TimelineId
  , nodeTimeline :: Timeline
  , nodeInEdges :: Set TimeEdge  -- ^ Incoming edges from ancestor timelines
  , nodeOutEdges :: Set TimeEdge  -- ^ Outgoing edges to descendant timelines
  }
  deriving stock (Show, Eq, Generic)

-- | An edge in the time map, representing a transition between timelines
data TimeEdge = TimeEdge
  { edgeFrom :: TimelineId  -- ^ Source timeline
  , edgeTo :: TimelineId    -- ^ Destination timeline
  , edgeType :: EdgeType    -- ^ Type of transition
  }
  deriving stock (Show, Eq, Generic, Ord)

-- | The type of transition between timelines
data EdgeType
  = ForkEdge     -- ^ A timeline split/fork
  | ContinueEdge -- ^ A linear continuation
  | MergeEdge    -- ^ A timeline merge
  deriving stock (Show, Eq, Generic, Ord)

-- | The core time map data structure
data TimeMap = TimeMap
  { timeNodes :: Map TimelineId TimeNode
  , timeBranches :: [TimeBranch]
  }
  deriving stock (Show, Eq, Generic)

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
  
  -- Update branches
  let updatedBranches = updateBranches src dest edgeType (timeBranches timeMap)
  
  -- Return the updated time map
  Just timeMap { timeNodes = updatedNodes, timeBranches = updatedBranches }

-- | Update branches after adding a new edge
updateBranches :: TimelineId -> TimelineId -> EdgeType -> [TimeBranch] -> [TimeBranch]
updateBranches src dest edgeType branches =
  case edgeType of
    ContinueEdge -> extendBranch src dest branches
    ForkEdge -> forkBranch src dest branches
    MergeEdge -> mergeBranches src dest branches

-- | Extend a branch with a new timeline
extendBranch :: TimelineId -> TimelineId -> [TimeBranch] -> [TimeBranch]
extendBranch src dest branches =
  let extendSingleBranch (TimeBranch ids) =
        if last ids == src
          then TimeBranch (ids ++ [dest])
          else TimeBranch ids
  in map extendSingleBranch branches

-- | Fork a branch with a new timeline
forkBranch :: TimelineId -> TimelineId -> [TimeBranch] -> [TimeBranch]
forkBranch src dest branches =
  let findBranch = find (\(TimeBranch ids) -> src `elem` ids) branches
  in case findBranch of
       Just (TimeBranch ids) ->
         let prefix = takeWhile (/= src) ids ++ [src]
             newBranch = TimeBranch (prefix ++ [dest])
         in newBranch : branches
       Nothing -> TimeBranch [src, dest] : branches
  where
    find p = foldr (\x -> \acc -> if p x then Just x else acc) Nothing

-- | Merge two branches into one
mergeBranches :: TimelineId -> TimelineId -> [TimeBranch] -> [TimeBranch]
mergeBranches src1 src2 branches =
  let branch1 = find (\(TimeBranch ids) -> last ids == src1) branches
      branch2 = find (\(TimeBranch ids) -> last ids == src2) branches
  in case (branch1, branch2) of
       (Just (TimeBranch ids1), Just (TimeBranch ids2)) ->
         let mergedBranch = TimeBranch (ids1 ++ [src2])
             otherBranches = filter (\b -> b /= TimeBranch ids1 && b /= TimeBranch ids2) branches
         in mergedBranch : otherBranches
       _ -> branches
  where
    find p = foldr (\x -> \acc -> if p x then Just x else acc) Nothing

-- | Validate whether a timeline conforms to the rules of the time map
validateTimeline :: TimelineId -> TimeMap -> Bool
validateTimeline hash timeMap =
  -- Implementation would check if the timeline's state is consistent with
  -- its ancestors and the system's temporal rules
  Map.member hash (timeNodes timeMap)

-- | Check if a timeline exists in the time map
timelineExists :: TimelineId -> TimeMap -> Bool
timelineExists id timeMap = Map.member id (timeNodes timeMap)

-- | Get a timeline from the time map
getTimeline :: TimelineId -> TimeMap -> Maybe Timeline
getTimeline id timeMap = nodeTimeline <$> Map.lookup id (timeNodes timeMap)

-- | Get all ancestor timelines of a timeline
getAncestors :: TimelineId -> TimeMap -> [TimelineId]
getAncestors id timeMap =
  case Map.lookup id (timeNodes timeMap) of
    Nothing -> []
    Just node ->
      let directAncestors = Set.map edgeFrom (nodeInEdges node)
          recursiveAncestors = concatMap (\a -> getAncestors a timeMap) (Set.toList directAncestors)
      in Set.toList directAncestors ++ recursiveAncestors

-- | Get all descendant timelines of a timeline
getDescendants :: TimelineId -> TimeMap -> [TimelineId]
getDescendants id timeMap =
  case Map.lookup id (timeNodes timeMap) of
    Nothing -> []
    Just node ->
      let directDescendants = Set.map edgeTo (nodeOutEdges node)
          recursiveDescendants = concatMap (\d -> getDescendants d timeMap) (Set.toList directDescendants)
      in Set.toList directDescendants ++ recursiveDescendants

-- | Get all root timelines (those with no ancestors)
getRootTimelines :: TimeMap -> [TimelineId]
getRootTimelines timeMap =
  Map.keys $ Map.filter (Set.null . nodeInEdges) (timeNodes timeMap)

-- | Get all leaf timelines (those with no descendants)
getLatestTimelines :: TimeMap -> [TimelineId]
getLatestTimelines timeMap =
  Map.keys $ Map.filter (Set.null . nodeOutEdges) (timeNodes timeMap)

-- | Find the common ancestor of two timelines, if any
findCommonAncestor :: TimelineId -> TimelineId -> TimeMap -> Maybe TimelineId
findCommonAncestor id1 id2 timeMap =
  let ancestors1 = Set.fromList (id1 : getAncestors id1 timeMap)
      ancestors2 = Set.fromList (id2 : getAncestors id2 timeMap)
      common = Set.intersection ancestors1 ancestors2
  in if Set.null common
       then Nothing
       else Just (head $ Set.toList common)

-- | Render a text representation of the time map
renderTimeMap :: TimeMap -> Text
renderTimeMap timeMap =
  let edges = concatMap (Set.toList . nodeOutEdges) (Map.elems (timeNodes timeMap))
      renderEdge (TimeEdge from to edgeTy) =
        T.pack (show from) <> " --(" <> T.pack (show edgeTy) <> ")--> " <> T.pack (show to)
      renderedEdges = map renderEdge edges
  in T.unlines renderedEdges 