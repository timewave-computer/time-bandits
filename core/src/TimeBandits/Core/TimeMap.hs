{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
This module provides the TimeMap abstraction and related functionality.
It encapsulates a snapshot of multiple timelines and their logical clocks,
allowing programs to execute against a consistent view of time across timelines.

Time Maps are:
- Snapshots of timeline states at specific logical times
- Used to enforce causal ordering across timelines
- The basis for cross-timeline consistency guarantees
- Used to prevent time-based attacks and race conditions
-}
module TimeBandits.Core.TimeMap 
  ( -- * Core Types
    TimeMap(..)
  , TimeMapId
  , TimeMapEntry(..)
  , TimeMapError(..)
  , TimelineDependency(..)
  , DependencyType(..)
  
  -- * TimeMap Operations
  , createTimeMap
  , updateTimeMap
  , getTimelineState
  , verifyTimeMapConsistency
  , ensureCausalOrder
  
  -- * Advanced TimeMap Operations
  , advanceLamportClock
  , mergeTimeMaps
  , isValidAdvancement
  , extractTimelineView
  , getTimelineDependencies
  , registerTimelineDependency
  , validateTimeMapTransition
  
  -- * Re-exports from Types and Timeline
  , TimelineHash
  , LamportTime(..)
  , BlockHeader(..)
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Serialize (Serialize)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Polysemy (Member, Sem, embed)
import Polysemy.Embed (Embed)
import Polysemy.Error (Error, throw, fromEither)
import Prelude hiding (lookup, filter, map, fold, find, maximum)
import qualified Prelude as P
import Data.Maybe (fromMaybe)
import Control.Monad (when, forM_, foldM)
import Data.Foldable (mapM_, maximum)

-- Import from TimeBandits modules
import TimeBandits.Core.Core (Hash(..), EntityHash(..))
import TimeBandits.Core.Types
  ( TimelineHash
  , LamportTime(..)
  , AppError(..)
  , TimelineErrorType(..)
  )
import TimeBandits.Core.Timeline (BlockHeader(..))

-- | Unique identifier for a TimeMap
type TimeMapId = EntityHash TimeMap

-- | The type of dependency between timelines
data DependencyType = 
    CausalDependency        -- ^ A strictly happens-before relationship between timelines
  | ReadDependency          -- ^ Timeline A reads from timeline B
  | WriteDependency         -- ^ Timeline A writes to timeline B
  | BidirectionalDependency -- ^ Both timelines interact with each other
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A dependency relationship between two timelines
data TimelineDependency = TimelineDependency
  { sourceTimeline :: TimelineHash
  , targetTimeline :: TimelineHash 
  , dependencyType :: DependencyType
  , minLamportDelta :: LamportTime -- ^ Minimum Lamport clock difference required between timelines
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Entry in a TimeMap for a specific timeline
data TimeMapEntry = TimeMapEntry
  { timelineHash :: TimelineHash
  , observedHead :: BlockHeader
  , lamportTime :: LamportTime
  , lastUpdated :: Maybe UTCTime  -- ^ When this entry was last updated (real time)
  , dependencies :: Set TimelineHash -- ^ Other timelines this one depends on
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | TimeMap is a snapshot of multiple timelines at specific logical times
data TimeMap = TimeMap
  { timeMapId :: TimeMapId
  , entries :: Map TimelineHash TimeMapEntry
  , createdAt :: LamportTime
  , timelineDependencies :: [TimelineDependency] -- ^ Known dependencies between timelines
  , globalClock :: LamportTime -- ^ Global logical clock, always advances
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Errors specific to TimeMap operations
data TimeMapError
  = TimelineNotInMap TimelineHash
  | CausalOrderViolation TimelineHash LamportTime LamportTime
  | InconsistentTimeMap TimeMapId
  | DependencyViolation TimelineHash TimelineHash LamportTime
  | NonMonotonicAdvancement TimelineHash LamportTime LamportTime
  | StaleTimeMapError TimeMapId UTCTime
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a new TimeMap with initial timeline entries
createTimeMap :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  [TimeMapEntry] -> 
  LamportTime ->
  Sem r TimeMap
createTimeMap initialEntries creationTime = do
  -- Create a map from the initial entries
  let entriesMap = Map.fromList [(timelineHash entry, entry) | entry <- initialEntries]
  
  -- Create a TimeMap ID from a hash of the entries
  let timeMapId = EntityHash $ Hash "time-map-id" -- In a real implementation, this would be a hash of the entries
  
  -- Add current time to each entry that doesn't have it
  currentTime <- embed getCurrentTime
  let updatedEntries = Map.map (\entry -> 
                           if lastUpdated entry == Nothing 
                           then entry { lastUpdated = Just currentTime }
                           else entry) entriesMap
  
  pure $ TimeMap
    { timeMapId = timeMapId
    , entries = updatedEntries
    , createdAt = creationTime
    , timelineDependencies = []
    , globalClock = creationTime
    }

-- | Update a TimeMap with a new timeline entry
updateTimeMap ::
  (Member (Error AppError) r, Member (Embed IO) r) =>
  TimeMap ->
  TimeMapEntry ->
  Sem r TimeMap
updateTimeMap timeMap entry = do
  -- Check if the timeline already exists in the map
  let tlHash = timelineHash entry
  let existingEntry = Map.lookup tlHash (entries timeMap)
  
  -- Verify causal ordering if the timeline already exists
  case existingEntry of
    Just existing -> do
      if lamportTime entry > lamportTime existing
        then pure ()
        else throw $ TimelineError $ TimelineGenericError $ 
              T.pack $ "Causal order violation: new time " ++ show (lamportTime entry) ++ 
              " is not greater than existing time " ++ show (lamportTime existing)
    Nothing -> pure ()
  
  -- Ensure all dependencies are satisfied
  validateDependencies timeMap tlHash (lamportTime entry)
  
  -- Get current time for the entry
  currentTime <- embed getCurrentTime
  let entryWithTime = entry { lastUpdated = Just currentTime }
  
  -- Update the TimeMap with the new entry
  let updatedEntries = Map.insert tlHash entryWithTime (entries timeMap)
  
  -- Advance the global clock
  let newGlobalClock = max (globalClock timeMap) (LamportTime $ getLamportTime (lamportTime entry) + 1)
  
  pure $ timeMap 
    { entries = updatedEntries
    , globalClock = newGlobalClock
    }

-- | Get the integer value from a LamportTime
getLamportTime :: LamportTime -> Int
getLamportTime (LamportTime t) = t

-- | Validate that all dependencies for a timeline are satisfied
validateDependencies ::
  (Member (Error AppError) r) =>
  TimeMap ->
  TimelineHash ->
  LamportTime ->
  Sem r ()
validateDependencies timeMap tlHash newTime = do
  -- Get all dependencies where this timeline is the target
  let timelineDeps = P.filter (\dep -> targetTimeline dep == tlHash) (timelineDependencies timeMap)
  
  -- Check each dependency
  forM_ timelineDeps $ \dep -> do
    -- Get the source timeline's current time
    sourceEntry <- getTimelineState timeMap (sourceTimeline dep)
    let sourceTime = lamportTime sourceEntry
    
    -- Check if the dependency is satisfied based on type
    case dependencyType dep of
      CausalDependency -> 
        -- For causal dependencies, target must be strictly after source
        when (newTime <= sourceTime) $
          throw $ TimelineError $ TimelineGenericError $
            T.pack $ "Causal dependency violation: " ++ show tlHash ++ " must be after " ++ show (sourceTimeline dep)
      
      ReadDependency ->
        -- For read dependencies, target must be at least at source time
        when (newTime < sourceTime) $
          throw $ TimelineError $ TimelineGenericError $
            T.pack $ "Read dependency violation: " ++ show tlHash ++ " must be at least at " ++ show (sourceTimeline dep)
      
      WriteDependency ->
        -- For write dependencies, target must be at least minDelta after source 
        when (newTime < advanceLamportBy sourceTime (minLamportDelta dep)) $
          throw $ TimelineError $ TimelineGenericError $
            T.pack $ "Write dependency violation: " ++ show tlHash ++ " must be at least " 
            ++ show (minLamportDelta dep) ++ " after " ++ show (sourceTimeline dep)
      
      BidirectionalDependency ->
        -- For bidirectional, just make sure they don't go backwards
        when (newTime < sourceTime) $
          throw $ TimelineError $ TimelineGenericError $
            T.pack $ "Bidirectional dependency violation: " ++ show tlHash ++ " cannot go backwards from " 
            ++ show (sourceTimeline dep)

-- | Get the state of a specific timeline from the TimeMap
getTimelineState ::
  (Member (Error AppError) r) =>
  TimeMap ->
  TimelineHash ->
  Sem r TimeMapEntry
getTimelineState timeMap tlHash = do
  -- Look up the timeline in the TimeMap
  case Map.lookup tlHash (entries timeMap) of
    Just entry -> pure entry
    Nothing -> throw $ TimelineError $ TimelineGenericError $ 
                T.pack $ "Timeline " ++ show tlHash ++ " not found in TimeMap"

-- | Verify the consistency of a TimeMap
verifyTimeMapConsistency ::
  (Member (Error AppError) r) =>
  TimeMap ->
  Sem r ()
verifyTimeMapConsistency timeMap = do
  -- Check that all timelines have valid Lamport clocks
  forM_ (Map.toList $ entries timeMap) $ \(tlHash, entry) ->
    when (lamportTime entry <= LamportTime 0) $
      throw $ TimelineError $ TimelineGenericError $
        T.pack $ "Invalid Lamport clock for timeline " ++ show tlHash ++ ": " ++ show (lamportTime entry)
  
  -- Check that the global clock is at least as large as all timeline clocks
  let allClocks = P.map lamportTime $ Map.elems $ entries timeMap
  let maxClock = if null allClocks then LamportTime 0 else maximum allClocks
  
  when (globalClock timeMap < maxClock) $
    throw $ TimelineError $ TimelineGenericError $
      T.pack $ "Global clock (" ++ show (globalClock timeMap) ++ 
      ") is less than max timeline clock (" ++ show maxClock ++ ")"

-- | Ensure causal ordering between two timeline states
ensureCausalOrder ::
  (Member (Error AppError) r) =>
  TimeMap ->
  TimelineHash ->  -- Source timeline
  TimelineHash ->  -- Target timeline
  Sem r Bool
ensureCausalOrder timeMap sourceTl targetTl = do
  -- Get the entries for both timelines
  sourceEntry <- getTimelineState timeMap sourceTl
  targetEntry <- getTimelineState timeMap targetTl
  
  -- Check if the source happens before the target
  let sourceTime = lamportTime sourceEntry
  let targetTime = lamportTime targetEntry
  
  -- Look for a dependency
  let maybeDep = find (\dep -> sourceTimeline dep == sourceTl && targetTimeline dep == targetTl) (timelineDependencies timeMap)
  
  case maybeDep of
    -- If there's a dependency, check its specific constraints
    Just dep -> case dependencyType dep of
      CausalDependency -> pure (targetTime > sourceTime)
      ReadDependency -> pure (targetTime >= sourceTime)
      WriteDependency -> pure (targetTime >= advanceLamportBy sourceTime (minLamportDelta dep))
      BidirectionalDependency -> pure True -- Special case - no specific ordering required
    
    -- Otherwise just check if target is at least not before source  
    Nothing -> pure (targetTime >= sourceTime)

-- | Advance a Lamport clock by a certain increment
advanceLamportBy :: LamportTime -> LamportTime -> LamportTime
advanceLamportBy (LamportTime t1) (LamportTime t2) = LamportTime (t1 + t2)

-- | Advance the Lamport clock for a specific timeline
advanceLamportClock ::
  (Member (Error AppError) r, Member (Embed IO) r) =>
  TimeMap ->
  TimelineHash ->
  LamportTime ->
  Sem r TimeMap
advanceLamportClock timeMap tlHash newTime = do
  -- Get the current timeline entry
  let timelineEntry = Map.lookup tlHash (entries timeMap)
  
  -- Verify the timeline exists and the new time is valid
  case timelineEntry of
    Just entry -> do
      -- Ensure the new time is greater than the current time
      let currentTime = lamportTime entry
      if newTime > currentTime
        then do
          -- Update the entry with the new time
          let updatedEntry = entry { lamportTime = newTime }
          let updatedEntries = Map.insert tlHash updatedEntry (entries timeMap)
          
          -- Update the global clock if needed
          let newGlobalClock = max (globalClock timeMap) (LamportTime $ getLamportTime newTime + 1)
          
          -- Return the updated TimeMap
          pure $ timeMap 
            { entries = updatedEntries
            , globalClock = newGlobalClock
            }
        else throw $ TimelineError $ TimelineGenericError $ 
              T.pack $ "Cannot advance clock backwards: current=" ++ show currentTime ++ 
              ", new=" ++ show newTime
    
    Nothing -> throw $ TimelineError $ TimelineGenericError $ 
              T.pack $ "Timeline not found: " ++ show tlHash

-- | Merge two time maps, taking the latest state for each timeline
mergeTimeMaps ::
  (Member (Error AppError) r, Member (Embed IO) r) =>
  TimeMap ->
  TimeMap ->
  Sem r TimeMap
mergeTimeMaps map1 map2 = do
  -- Get a list of all timeline hashes from both maps
  let allHashes = Set.union 
                    (Set.fromList $ Map.keys $ entries map1) 
                    (Set.fromList $ Map.keys $ entries map2)
  
  -- Create a new base time map
  let baseMap = TimeMap 
        { timeMapId = EntityHash $ Hash "merged-time-map"
        , entries = Map.empty
        , createdAt = max (createdAt map1) (createdAt map2)
        , timelineDependencies = unionDependencies (timelineDependencies map1) (timelineDependencies map2)
        , globalClock = max (globalClock map1) (globalClock map2)
        }
  
  -- Fold over all timeline hashes, adding the latest entry for each
  foldM addLatestEntry baseMap (Set.toList allHashes)
  where
    -- Combine dependencies, removing duplicates
    unionDependencies deps1 deps2 =
      let allDeps = deps1 ++ deps2
          depKey dep = (sourceTimeline dep, targetTimeline dep)
      in nubBy depKey allDeps
    
    -- Add the latest entry for a timeline to the merged map
    addLatestEntry accMap tlHash = do
      let entry1 = Map.lookup tlHash (entries map1)
      let entry2 = Map.lookup tlHash (entries map2)
      
      case (entry1, entry2) of
        (Just e1, Just e2) -> 
          if lamportTime e1 >= lamportTime e2
            then updateTimeMap accMap e1
            else updateTimeMap accMap e2
        (Just e, Nothing) -> updateTimeMap accMap e
        (Nothing, Just e) -> updateTimeMap accMap e
        (Nothing, Nothing) -> pure accMap -- Should never happen given how we collect allHashes

-- | Verify if a new time map is a valid advancement of an old time map
isValidAdvancement ::
  (Member (Error AppError) r) =>
  TimeMap ->  -- Old time map
  TimeMap ->  -- New time map
  Sem r Bool
isValidAdvancement oldMap newMap = do
  -- Check that all timelines in the old map are present in the new map
  let oldHashes = Map.keysSet (entries oldMap)
  let newHashes = Map.keysSet (entries newMap)
  
  when (not $ oldHashes `Set.isSubsetOf` newHashes) $
    throw $ TimelineError $ TimelineGenericError $
      T.pack $ "New time map is missing timelines from old time map"
  
  -- Check that all timelines have advanced (or at least not gone backwards)
  forM_ (Map.toList $ entries oldMap) $ \(tlHash, oldEntry) -> do
    case Map.lookup tlHash (entries newMap) of
      Nothing -> throw $ TimelineError $ TimelineGenericError $
                   T.pack $ "Timeline " ++ show tlHash ++ " missing from new time map"
      Just newEntry ->
        when (lamportTime newEntry < lamportTime oldEntry) $
          throw $ TimelineError $ TimelineGenericError $
            T.pack $ "Timeline " ++ show tlHash ++ " went backwards: " ++
            show (lamportTime oldEntry) ++ " -> " ++ show (lamportTime newEntry)
  
  -- Check that the global clock has advanced
  when (globalClock newMap < globalClock oldMap) $
    throw $ TimelineError $ TimelineGenericError $
      T.pack $ "Global clock went backwards: " ++
      show (globalClock oldMap) ++ " -> " ++ show (globalClock newMap)
  
  -- Verify all dependencies are satisfied in the new time map
  verifyTimeMapConsistency newMap
  
  -- If we got here, the advancement is valid
  pure True

-- | Extract a view of a specific timeline from a TimeMap
extractTimelineView :: 
  (Member (Error AppError) r) => 
  TimeMap -> 
  [TimelineHash] -> 
  Sem r TimeMap
extractTimelineView timeMap tlHashes = do
  -- Filter the entries to only include the requested timelines
  let filteredEntries = Map.filterWithKey (\k _ -> k `elem` tlHashes) (entries timeMap)
  
  -- Check if any requested timelines are missing
  let missingTimelines = P.filter (\h -> Map.notMember h filteredEntries) tlHashes
  
  when (not $ null missingTimelines) $
    throw $ TimelineError $ TimelineGenericError $
      T.pack $ "Missing timelines: " ++ show missingTimelines
  
  -- Filter dependencies to only include those relevant to the requested timelines
  let filteredDeps = P.filter 
        (\dep -> sourceTimeline dep `elem` tlHashes && targetTimeline dep `elem` tlHashes) 
        (timelineDependencies timeMap)
  
  -- Create a new TimeMap with just the requested timelines
  pure $ timeMap
    { entries = filteredEntries
    , timelineDependencies = filteredDeps
    }

-- | Get all dependencies for a specific timeline
getTimelineDependencies :: 
  TimeMap -> 
  TimelineHash -> 
  [TimelineDependency]
getTimelineDependencies timeMap tlHash = 
  P.filter (\dep -> sourceTimeline dep == tlHash || targetTimeline dep == tlHash) 
    (timelineDependencies timeMap)

-- | Register a new dependency between timelines
registerTimelineDependency ::
  (Member (Error AppError) r) =>
  TimeMap ->
  TimelineDependency ->
  Sem r TimeMap
registerTimelineDependency timeMap newDep = do
  -- Check that both timelines exist
  _ <- getTimelineState timeMap (sourceTimeline newDep)
  _ <- getTimelineState timeMap (targetTimeline newDep)
  
  -- Check for duplicates (same source and target)
  let existingDep = find (\dep -> 
                      sourceTimeline dep == sourceTimeline newDep && 
                      targetTimeline dep == targetTimeline newDep)
                    (timelineDependencies timeMap)
  
  -- Update or add the dependency
  let updatedDeps = case existingDep of
        Just _ -> P.map (\dep -> 
                    if sourceTimeline dep == sourceTimeline newDep && 
                       targetTimeline dep == targetTimeline newDep
                    then newDep
                    else dep) 
                  (timelineDependencies timeMap)
        Nothing -> newDep : timelineDependencies timeMap
  
  -- Return the updated time map
  pure $ timeMap { timelineDependencies = updatedDeps }

-- | Validate a transition from one time map to another
validateTimeMapTransition ::
  (Member (Error AppError) r) =>
  TimeMap ->  -- Old time map
  TimeMap ->  -- New time map
  Sem r ()
validateTimeMapTransition oldMap newMap = do
  -- Check if the new map is a valid advancement of the old map
  valid <- isValidAdvancement oldMap newMap
  
  unless valid $
    throw $ TimelineError $ TimelineGenericError $
      "Invalid time map transition"
      
  -- Additional validations could be added here
  pure ()

-- Helper functions

-- | Find the first element in a list that satisfies a predicate
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) = if p x then Just x else find p xs

-- | Remove duplicate elements from a list based on a key function
nubBy :: Eq b => (a -> b) -> [a] -> [a]
nubBy f = go []
  where
    go seen [] = []
    go seen (x:xs)
      | f x `elem` seen = go seen xs
      | otherwise = x : go (f x : seen) xs

-- | Find a timeline in the map by its hash
findTimeline :: TimelineHash -> TimeMap -> Maybe TimeMapEntry
findTimeline hash (TimeMap _ entries _ _ _) = Map.lookup hash entries

-- | Validate that a timeline dependency is valid
validateDependency :: TimelineHash -> TimelineDependency -> Bool
validateDependency selfHash (TimelineDependency depHash _ _ _) = depHash /= selfHash

-- | Filter out invalid dependencies
filterDependencies :: TimelineHash -> [TimelineDependency] -> [TimelineDependency]
filterDependencies selfHash = P.filter (validateDependency selfHash) 