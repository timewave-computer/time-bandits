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
{-# LANGUAGE TypeFamilies #-}

{- |
This module implements coordination between different actor roles:
- Time Travelers (program creation and usage)
- Time Keepers (timeline integrity)
- Time Bandits (program execution and p2p infrastructure)

It defines the protocols and workflows for how these roles interact to 
accomplish common tasks in the Time Bandits system.
-}
module Actors.ActorCoordination
  ( -- * Core Types
    CoordinationError(..)
  , WorkflowResult(..)
  , ActorNetwork(..)
  , SimulationMode(..)
  , DeploymentMode
  
  -- * Workflow Functions
  , runProgramTransition
  , deployNewProgram
  , transferResource
  , queryTimelineState
  
  -- * Actor Network Management
  , createActorNetwork
  , registerActorInNetwork
  , discoverPeers
  
  -- * Synchronization Functions
  , syncTimeline
  , syncTimeMap
  , syncExecutionLog
  ) where

import Control.Monad (when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Serialize (Serialize)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)

-- Import from TimeBandits modules
import Core (Hash(..), EntityHash(..))
import Core.Types (AppError(..), PeerId(..))
import Core.Resource (Resource, Address, ResourceId)
import Programs.Program (ProgramId, ProgramDefinition, ProgramState, Effect)
import Core.Timeline (TimelineId)
import Core.TimeMap (TimeMap)
import Actors.TransitionMessage (TransitionMessage)
import CLI.Controller (Controller)
import Actors.ActorTypes (SimulationMode(..))

-- Actor role imports
import Actors.TimeTraveler (TimeTraveler, TransitionResult)
import qualified Actors.TimeTraveler as TimeTraveler
import Actors.TimeKeeper (TimeKeeper, ValidationResult, ApplyResult)
import qualified Actors.TimeKeeper as TimeKeeper
import Actors.TimeBandit (TimeBandit, ExecutionResult, Proof)
import qualified Actors.TimeBandit as TimeBandit

-- | Type alias for deployment mode
type DeploymentMode = SimulationMode

-- | Simulation mode for actors
-- data SimulationMode removed
  = InMemory      -- ^ All actors run in the same process 
  | MultiProcess  -- ^ Actors run in separate processes
  | Distributed   -- ^ Actors run on separate machines
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Network of actors in a simulation
data ActorNetwork = ActorNetwork
  { travelers :: Map Address TimeTraveler
  , keepers :: Map TimelineId TimeKeeper
  , bandits :: Map PeerId TimeBandit
  , controller :: Controller
  , mode :: SimulationMode
  }
  deriving (Show, Generic)
  deriving anyclass (Serialize)

-- | Errors that can occur during coordination
data CoordinationError
  = MissingActor Text Address
  | UnauthorizedOperation Text
  | TimelineAccessError TimelineId Text
  | MessageValidationError [Text]
  | ExecutionError Text
  | SynchronizationError Text
  | NetworkError Text
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Result of a workflow operation
data WorkflowResult = WorkflowResult
  { success :: Bool
  , resultHash :: Hash
  , updatedTimeMap :: TimeMap
  , logs :: [Text]
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a new actor network for a simulation
createActorNetwork :: 
  Controller -> 
  SimulationMode -> 
  ActorNetwork
createActorNetwork ctrl mode = ActorNetwork
  { travelers = Map.empty
  , keepers = Map.empty
  , bandits = Map.empty
  , controller = ctrl
  , mode = mode
  }

-- | Register an actor in the network
registerActorInNetwork :: 
  (Member (Error CoordinationError) r) =>
  ActorNetwork ->
  Either TimeTraveler (Either TimeKeeper TimeBandit) ->
  Sem r ActorNetwork
registerActorInNetwork network actorEither = do
  case actorEither of
    Left traveler ->
      -- Register a Time Traveler
      let tid = TimeTraveler.travelerId traveler
          updatedTravelers = Map.insert tid traveler (travelers network)
      in return $ network { travelers = updatedTravelers }
    
    Right (Left keeper) ->
      -- Register a Time Keeper
      let kid = TimeKeeper.keeperId keeper
          managedTimelines = TimeKeeper.managedTimelines keeper
          -- Create mapping from each timeline to this keeper
          updatedKeepers = foldr (\tid acc -> Map.insert tid keeper acc) 
                                (keepers network) 
                                (Map.keys managedTimelines)
      in return $ network { keepers = updatedKeepers }
    
    Right (Right bandit) ->
      -- Register a Time Bandit
      let bid = PeerId $ T.pack $ show $ TimeBandit.banditId bandit
          updatedBandits = Map.insert bid bandit (bandits network)
      in return $ network { bandits = updatedBandits }

-- | Run a program transition using all three actor roles
-- 
-- This function orchestrates the workflow between:
-- 1. Time Traveler submitting a transition
-- 2. Time Keeper validating the transition
-- 3. Time Bandit executing the program and generating proof
-- 4. Time Traveler submitting the finalized transition with proof
-- 5. Time Keeper applying the transition to the timeline
runProgramTransition :: 
  (Member (Error CoordinationError) r) =>
  ActorNetwork ->
  Address ->            -- ^ Time Traveler ID
  TimelineId ->         -- ^ Timeline ID
  ProgramId ->          -- ^ Program ID
  Effect ->             -- ^ Program effect to execute
  Sem r (ActorNetwork, WorkflowResult)
runProgramTransition network travelerAddr tid pid effect = do
  -- 1. Get the Time Traveler
  traveler <- case Map.lookup travelerAddr (travelers network) of
    Just t -> return t
    Nothing -> throw $ MissingActor "Time Traveler" travelerAddr
  
  -- 2. Get the TimeKeeper for this timeline
  keeper <- case Map.lookup tid (keepers network) of
    Just k -> return k
    Nothing -> throw $ TimelineAccessError tid "No Time Keeper found"
  
  -- 3. Get a Time Bandit (any available one for now)
  (banditId, bandit) <- case Map.lookupMin (bandits network) of
    Just pair -> return pair
    Nothing -> throw $ MissingActor "Time Bandit" undefined
  
  -- 4. Time Traveler creates initial transition (without proof)
  (updatedTraveler1, initialTransMsg) <- TimeTraveler.submitTransition traveler 
                                                                (controller network) 
                                                                pid 
                                                                effect 
                                                                Nothing
                                                                
  -- 5. Time Keeper validates transition
  validationResult <- TimeKeeper.validateMessage keeper initialTransMsg tid
  
  when (not $ TimeKeeper.validationSuccess validationResult) $
    let errorMsgs = map show $ TimeKeeper.validationErrors validationResult
    in throw $ MessageValidationError $ map T.pack errorMsgs
  
  -- 6. Time Bandit executes program and generates proof
  execResult <- TimeBandit.executeProgram bandit pid []
  proof <- TimeBandit.generateProof bandit (TimeBandit.execResultHash execResult) "zkp"
  
  -- 7. Time Traveler submits finalized transition with proof
  (updatedTraveler2, finalTransResult) <- TimeTraveler.submitTransition updatedTraveler1
                                                                   (controller network)
                                                                   pid
                                                                   effect
                                                                   (Just (TimeBandit.proofTarget proof, TimeBandit.proofData proof))
  
  -- 8. Time Keeper applies the transition to the timeline
  (updatedKeeper, applyResult) <- TimeKeeper.applyToTimeline keeper initialTransMsg tid validationResult
  
  -- 9. Update the actor network
  let updatedTravelers = Map.insert travelerAddr updatedTraveler2 (travelers network)
      updatedKeepers = Map.insert tid updatedKeeper (keepers network)
      updatedNetwork = network { travelers = updatedTravelers, keepers = updatedKeepers }
  
  -- 10. Create workflow result
  let result = WorkflowResult
        { success = TimeKeeper.applySuccess applyResult
        , resultHash = TimeKeeper.validationMessageHash validationResult
        , updatedTimeMap = TimeTraveler.updatedTimeMap finalTransResult
        , logs = ["Program transition executed successfully"]
        }
  
  return (updatedNetwork, result)

-- | Deploy a new program
deployNewProgram :: 
  (Member (Error CoordinationError) r) =>
  ActorNetwork ->
  Address ->            -- ^ Time Traveler ID
  ProgramDefinition ->  -- ^ Program definition
  Map Text Text ->      -- ^ Initial state
  Sem r (ActorNetwork, ProgramId)
deployNewProgram network travelerAddr def initialState = do
  -- 1. Get the Time Traveler
  traveler <- case Map.lookup travelerAddr (travelers network) of
    Just t -> return t
    Nothing -> throw $ MissingActor "Time Traveler" travelerAddr
  
  -- 2. Deploy the program
  (updatedTraveler, pid) <- TimeTraveler.deployProgram traveler (controller network) def initialState
  
  -- 3. Update the actor network
  let updatedTravelers = Map.insert travelerAddr updatedTraveler (travelers network)
      updatedNetwork = network { travelers = updatedTravelers }
  
  return (updatedNetwork, pid)

-- | Transfer a resource between actors
transferResource :: 
  (Member (Error CoordinationError) r) =>
  ActorNetwork ->
  Address ->            -- ^ Sender Time Traveler ID
  Address ->            -- ^ Recipient Time Traveler ID
  ResourceId ->         -- ^ Resource ID
  Sem r ActorNetwork
transferResource network fromAddr toAddr rid = do
  -- 1. Get the sender Time Traveler
  sender <- case Map.lookup fromAddr (travelers network) of
    Just t -> return t
    Nothing -> throw $ MissingActor "Sender Time Traveler" fromAddr
  
  -- 2. Check if recipient exists
  recipient <- case Map.lookup toAddr (travelers network) of
    Just t -> return t
    Nothing -> throw $ MissingActor "Recipient Time Traveler" toAddr
  
  -- 3. Transfer the resource
  updatedSender <- TimeTraveler.transferResource sender (controller network) rid toAddr
  
  -- 4. Update the actor network
  let updatedTravelers = Map.insert fromAddr updatedSender (travelers network)
      updatedNetwork = network { travelers = updatedTravelers }
  
  return updatedNetwork

-- | Query timeline state
queryTimelineState :: 
  (Member (Error CoordinationError) r) =>
  ActorNetwork ->
  Address ->            -- ^ Time Traveler ID
  TimelineId ->         -- ^ Timeline ID
  Sem r TimeMap
queryTimelineState network travelerAddr tid = do
  -- 1. Get the Time Traveler
  traveler <- case Map.lookup travelerAddr (travelers network) of
    Just t -> return t
    Nothing -> throw $ MissingActor "Time Traveler" travelerAddr
  
  -- 2. Get the TimeKeeper for this timeline
  keeper <- case Map.lookup tid (keepers network) of
    Just k -> return k
    Nothing -> throw $ TimelineAccessError tid "No Time Keeper found"
  
  -- 3. Query the timeline state
  timeMap <- TimeTraveler.queryTimeline traveler (controller network) tid
  
  return timeMap

-- | Discover peers in the network
discoverPeers :: 
  ActorNetwork ->
  IO ActorNetwork
discoverPeers network = do
  -- In a real implementation, this would query the network and update all bandits
  -- For now, just return the unchanged network
  return network

-- | Synchronize a timeline across actors
syncTimeline :: 
  (Member (Error CoordinationError) r) =>
  ActorNetwork ->
  TimelineId ->         -- ^ Timeline ID
  Sem r ActorNetwork
syncTimeline network tid = do
  -- In a real implementation, this would synchronize the timeline state across all actors
  -- For now, just return the unchanged network
  return network

-- | Synchronize time maps across actors
syncTimeMap :: 
  (Member (Error CoordinationError) r) =>
  ActorNetwork ->
  Sem r ActorNetwork
syncTimeMap network = do
  -- In a real implementation, this would synchronize time maps across all actors
  -- For now, just return the unchanged network
  return network

-- | Synchronize execution logs across actors
syncExecutionLog :: 
  (Member (Error CoordinationError) r) =>
  ActorNetwork ->
  Sem r ActorNetwork
syncExecutionLog network = do
  -- In a real implementation, this would synchronize execution logs across all actors
  -- For now, just return the unchanged network
  return network 