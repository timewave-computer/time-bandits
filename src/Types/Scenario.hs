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
Module: Types.Scenario
Description: Shared scenario types for the Time Bandits system

This module provides shared types for scenario configuration and errors,
which are used by both CLI.Controller and Programs.Scenario modules.
This helps break circular dependencies between these modules.
-}
module Types.Scenario
  ( -- * Scenario Configuration
    ScenarioConfig(..)
  , ScenarioError(..)
  , ScenarioResult(..)
  , ExecutionMode(..)
  , ScenarioEntity(..)
  , ScenarioTimeline(..)
  ) where

import Data.Text (Text)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import qualified Data.Text.Encoding as TE
import qualified Data.Serialize as S
import Core.Common (SimulationMode(..), EntityHash(..))
import Types.Actor (ActorSpec)

-- | Execution mode for scenarios
data ExecutionMode
  = SingleNode                             -- ^ Run on a single node
  | MultiNode                              -- ^ Run on multiple nodes
  | DistributedNodes                       -- ^ Run on distributed nodes
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Entity in a scenario
data ScenarioEntity = ScenarioEntity
  { entityId :: Text                       -- ^ Entity ID
  , entityName :: Text                     -- ^ Entity name
  , entityDescription :: Text              -- ^ Entity description
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Timeline in a scenario
data ScenarioTimeline = ScenarioTimeline
  { timelineId :: Text                     -- ^ Timeline ID
  , timelineName :: Text                   -- ^ Timeline name
  , timelineDescription :: Text            -- ^ Timeline description
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Scenario configuration
data ScenarioConfig = ScenarioConfig
  { scenarioName :: Text                   -- ^ Name of the scenario
  , scenarioDescription :: Text            -- ^ Description of the scenario
  , scenarioMode :: SimulationMode         -- ^ Simulation mode for the scenario
  , scenarioActors :: [ActorSpec]          -- ^ Actors in the scenario
  , scenarioInitialPrograms :: [Text]      -- ^ Initial programs to deploy
  , scenarioLogPath :: FilePath            -- ^ Path for scenario logs
  , scenarioMaxSteps :: Int                -- ^ Maximum number of steps
  , scenarioTimeout :: Int                 -- ^ Timeout in seconds
  , scenarioExecutionMode :: ExecutionMode -- ^ Execution mode
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Scenario errors
data ScenarioError
  = ParseError Text                        -- ^ Error parsing scenario
  | ValidationError Text                   -- ^ Error validating scenario
  | ExecutionError Text                    -- ^ Error executing scenario
  | TimeoutError                           -- ^ Scenario timed out
  | ResourceError Text                     -- ^ Error with resources
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Scenario execution results
data ScenarioResult = ScenarioResult
  { resultSuccess :: Bool                  -- ^ Whether the scenario was successful
  , resultSteps :: Int                     -- ^ Number of steps executed
  , resultErrors :: [Text]                 -- ^ Errors encountered during execution
  , resultFinalState :: Maybe Text         -- ^ Final state (serialized)
  , resultExecutionLog :: [Text]           -- ^ Execution log
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize) 