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
module TimeBandits.TimeMap 
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
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Polysemy (Member, Sem, embed)
import Polysemy.Embed (Embed)
import Polysemy.Error (Error, throw, fromEither)

-- Import from TimeBandits modules
import TimeBandits.Core (Hash(..), EntityHash(..))
import TimeBandits.Types
  ( TimelineHash
  , LamportTime(..)
  , AppError(..)
  , TimelineErrorType(..)
  )
import TimeBandits.Timeline (BlockHeader(..))

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
  , dependencies :: [TimelineDependency] -- ^ Known dependencies between timelines
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
    , dependencies = []
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
              "Causal order violation: new time " ++ show (lamportTime entry) ++ 
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
  let newGlobalClock = max (globalClock timeMap) (LamportTime $ unLamportTime (lamportTime entry) + 1)
  
  pure $ timeMap 
    { entries = updatedEntries
    , globalClock = newGlobalClock
    }

-- | Validate that all dependencies for a timeline are satisfied
validateDependencies ::
  (Member (Error AppError) r) =>
  TimeMap ->
  TimelineHash ->
  LamportTime ->
  Sem r ()
validateDependencies timeMap tlHash newTime = do
  -- Get all dependencies where this timeline is the target
  let timelineDeps = filter (\dep -> targetTimeline dep == tlHash) (dependencies timeMap)
  
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
            "Causal dependency violation: " ++ show tlHash ++ " must be after " ++ show (sourceTimeline dep)
      
      ReadDependency ->
        -- For read dependencies, target must be at least at source time
        when (newTime < sourceTime) $
          throw $ TimelineError $ TimelineGenericError $
            "Read dependency violation: " ++ show tlHash ++ " must be at least at " ++ show (sourceTimeline dep)
      
      WriteDependency ->
        -- For write dependencies, target must be at least minDelta after source 
        when (newTime < advanceLamportBy sourceTime (minLamportDelta dep)) $
          throw $ TimelineError $ TimelineGenericError $
            "Write dependency violation: " ++ show tlHash ++ " must be at least " 
            ++ show (minLamportDelta dep) ++ " after " ++ show (sourceTimeline dep)
      
      BidirectionalDependency ->
        -- For bidirectional, just make sure they don't go backwards
        when (newTime < sourceTime) $
          throw $ TimelineError $ TimelineGenericError $
            "Bidirectional dependency violation: " ++ show tlHash ++ " cannot go backwards from " 
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
                "Timeline " ++ show tlHash ++ " not found in TimeMap"

-- | Verify the consistency of a TimeMap
verifyTimeMapConsistency ::
  (Member (Error AppError) r) =>
  TimeMap ->
  Sem r Bool
verifyTimeMapConsistency timeMap = do
  -- Check that all dependencies are satisfied
  forM_ (Map.toList $ entries timeMap) $ \(tlHash, entry) ->
    validateDependencies timeMap tlHash (lamportTime entry)
  
  -- Check global clock is at least as high as all timeline clocks
  let allClocks = map lamportTime $ Map.elems $ entries timeMap
  let maxClock = if null allClocks then LamportTime 0 else maximum allClocks
  when (globalClock timeMap < maxClock) $
    throw $ TimelineError $ TimelineGenericError $
      "Global clock " ++ show (globalClock timeMap) ++ " is less than max timeline clock " ++ show maxClock
      
  -- If we get here, the TimeMap is consistent
  pure True

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
  let maybeDep = find (\dep -> sourceTimeline dep == sourceTl && targetTimeline dep == targetTl) (dependencies timeMap)
  
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
  Sem r TimeMap
advanceLamportClock timeMap tlHash = do
  -- Get the current entry
  entry <- getTimelineState timeMap tlHash
  
  -- Advance the Lamport clock by 1
  let newTime = advanceLamportBy (lamportTime entry) (LamportTime 1)
  
  -- Create a new entry with the advanced clock
  let newEntry = entry { lamportTime = newTime }
  
  -- Update the time map
  updateTimeMap timeMap newEntry

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
        , dependencies = unionDependencies (dependencies map1) (dependencies map2)
        , globalClock = max (globalClock map1) (globalClock map2)
        }
  
  -- Fold over all timeline hashes, adding the latest entry for each
  foldM addLatestEntry baseMap (Set.toList allHashes)
  where
    -- Combine dependencies, removing duplicates
    unionDependencies deps1 deps2 =
      let allDeps = deps1 ++ deps2
          depKey dep = (sourceTimeline dep, targetTimeline dep)
      in nubBy (\a b -> depKey a == depKey b) allDeps
    
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
      "New time map is missing timelines from old time map"
  
  -- Check that all timelines have advanced (or at least not gone backwards)
  forM_ (Map.toList $ entries oldMap) $ \(tlHash, oldEntry) -> do
    case Map.lookup tlHash (entries newMap) of
      Nothing -> throw $ TimelineError $ TimelineGenericError $
                   "Timeline " ++ show tlHash ++ " missing from new time map"
      Just newEntry ->
        when (lamportTime newEntry < lamportTime oldEntry) $
          throw $ TimelineError $ TimelineGenericError $
            "Timeline " ++ show tlHash ++ " went backwards: " ++
            show (lamportTime oldEntry) ++ " -> " ++ show (lamportTime newEntry)
  
  -- Check that the global clock has advanced
  when (globalClock newMap < globalClock oldMap) $
    throw $ TimelineError $ TimelineGenericError $
      "Global clock went backwards: " ++
      show (globalClock oldMap) ++ " -> " ++ show (globalClock newMap)
  
  -- Verify all dependencies are satisfied in the new time map
  verifyTimeMapConsistency newMap

-- | Extract a view of the time map containing only specified timelines
extractTimelineView ::
  (Member (Error AppError) r) =>
  TimeMap ->
  [TimelineHash] ->
  Sem r TimeMap
extractTimelineView timeMap tlHashes = do
  -- Create a new map with just the specified timelines
  let filteredEntries = Map.filterWithKey (\k _ -> k `elem` tlHashes) (entries timeMap)
  
  -- Check that all requested timelines exist
  let missingTimelines = filter (\h -> Map.notMember h filteredEntries) tlHashes
  unless (null missingTimelines) $
    throw $ TimelineError $ TimelineGenericError $
      "Missing timelines in extraction: " ++ show missingTimelines
  
  -- Filter dependencies to only include those between the specified timelines
  let filteredDeps = filter (\dep -> 
                      sourceTimeline dep `elem` tlHashes && 
                      targetTimeline dep `elem` tlHashes) 
                    (dependencies timeMap)
  
  -- Return the filtered time map
  pure $ timeMap 
    { entries = filteredEntries
    , dependencies = filteredDeps
    }

-- | Get all dependencies for a specific timeline
getTimelineDependencies ::
  (Member (Error AppError) r) =>
  TimeMap ->
  TimelineHash ->
  Sem r [TimelineDependency]
getTimelineDependencies timeMap tlHash = do
  -- Find dependencies where this timeline is either source or target
  let relatedDeps = filter (\dep -> 
                      sourceTimeline dep == tlHash || 
                      targetTimeline dep == tlHash)
                    (dependencies timeMap)
  pure relatedDeps

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
                    (dependencies timeMap)
  
  -- Update or add the dependency
  let updatedDeps = case existingDep of
        Just _ -> map (\dep -> 
                    if sourceTimeline dep == sourceTimeline newDep && 
                       targetTimeline dep == targetTimeline newDep
                    then newDep
                    else dep) 
                  (dependencies timeMap)
        Nothing -> newDep : dependencies timeMap
  
  -- Return the updated time map
  pure $ timeMap { dependencies = updatedDeps }

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

-- | Apply an action to each element of a list
forM_ :: Monad m => [a] -> (a -> m b) -> m ()
forM_ xs f = mapM_ f xs

-- | Apply a function to each element in a list and return the results in a list
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f = foldr ((>>) . f) (return ())

-- | Fold over a list with a monadic function
foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM _ z [] = return z
foldM f z (x:xs) = do
  z' <- f z x
  foldM f z' xs

-- | Return a value if a condition is true, otherwise fail
when :: Monad m => Bool -> m () -> m ()
when p s = if p then s else return () 