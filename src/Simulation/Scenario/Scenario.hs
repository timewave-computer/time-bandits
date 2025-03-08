{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Simulation.Scenario.Scenario
Description : Data structures for scenario-based simulation
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module defines the core data structures for scenario-based simulation
in the Time Bandits system. A scenario is the source of truth for any simulation
run, defining actors, timelines, initial facts, and invariants.
-}
module Simulation.Scenario.Scenario
  ( -- * Core Types
    Scenario(..)
  , SimulationMode(..)
  , ActorSpec(..)
  , FactSpec(..)
  , InvariantSpec(..)
  
    -- * Helper Functions
  , createScenario
  , addActor
  , addFact
  , addInvariant
  ) where

import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import Types.Actor (ActorID, ActorType(..))
import Core.TimelineId (TimelineID)
import Types.Effect (Fact)

-- | Simulation mode determines how actors are executed
data SimulationMode
  = InMemory       -- ^ All actors run in the same process, in memory
  | LocalProcesses -- ^ Actors run as separate processes on the same machine
  | GeoDistributed -- ^ Actors run as separate processes on different machines
  deriving (Show, Eq, Generic)

-- | Actor specification within a scenario
data ActorSpec = ActorSpec
  { actorId   :: ActorID      -- ^ Unique identifier for the actor
  , actorType :: ActorType    -- ^ Type of actor (Trader, TimeKeeper, etc.)
  , timeline  :: Maybe TimelineID  -- ^ Associated timeline (for TimeKeepers)
  } deriving (Show, Eq, Generic)

-- | Fact specification for initializing timelines
data FactSpec = FactSpec
  { factTimeline :: TimelineID  -- ^ Timeline to which the fact belongs
  , fact        :: Fact        -- ^ The actual fact data
  } deriving (Show, Eq, Generic)

-- | Invariant specification for validation
data InvariantSpec = InvariantSpec
  { invariantName :: Text             -- ^ Name of the invariant
  , invariantType :: Text             -- ^ Type of invariant
  , invariantParams :: Map Text Text  -- ^ Parameters for the invariant
  } deriving (Show, Eq, Generic)

-- | The main scenario data structure
data Scenario = Scenario
  { scenarioName     :: Text             -- ^ Name of the scenario
  , scenarioMode     :: SimulationMode   -- ^ Mode of simulation
  , scenarioActors   :: [ActorSpec]      -- ^ Actors involved in the scenario
  , scenarioFacts    :: [FactSpec]       -- ^ Initial facts for the scenario
  , scenarioInvariants :: [InvariantSpec] -- ^ Invariants to check during simulation
  } deriving (Show, Eq, Generic)

-- | Create a new scenario with the given name and mode
createScenario :: Text -> SimulationMode -> Scenario
createScenario name mode = Scenario
  { scenarioName = name
  , scenarioMode = mode
  , scenarioActors = []
  , scenarioFacts = []
  , scenarioInvariants = []
  }

-- | Add an actor to a scenario
addActor :: Scenario -> ActorID -> ActorType -> Maybe TimelineID -> Scenario
addActor scenario id actorType timelineId =
  scenario { scenarioActors = scenarioActors scenario ++ [newActor] }
  where
    newActor = ActorSpec
      { actorId = id
      , actorType = actorType
      , timeline = timelineId
      }

-- | Add a fact to a scenario
addFact :: Scenario -> TimelineID -> Fact -> Scenario
addFact scenario timelineId fact =
  scenario { scenarioFacts = scenarioFacts scenario ++ [newFact] }
  where
    newFact = FactSpec
      { factTimeline = timelineId
      , fact = fact
      }

-- | Add an invariant to a scenario
addInvariant :: Scenario -> Text -> Text -> Map Text Text -> Scenario
addInvariant scenario name invType params =
  scenario { scenarioInvariants = scenarioInvariants scenario ++ [newInvariant] }
  where
    newInvariant = InvariantSpec
      { invariantName = name
      , invariantType = invType
      , invariantParams = params
      } 