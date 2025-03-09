{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
This module provides deployment functionality for different simulation modes.
It handles the deployment of actors, programs, and controllers in:

- In-memory mode: All components run in the same process
- Local multi-process mode: Components run in separate local processes
- Geo-distributed mode: Components run on different machines

The deployment module ensures that the system behaves consistently
regardless of the deployment mode.
-}
module CLI.Deployment 
  ( -- * Core Types
    ScenarioConfig(..)
  , Scenario(..)
  , displayScenario
  ) where

import Control.Exception (try, SomeException)
import Data.Map.Strict (Map)
import Data.Serialize (Serialize)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Polysemy (Member, Sem, Embed, embed)
import Polysemy.Error (Error, throw)
import System.FilePath ((</>))
import qualified Data.ByteString.Char8 as BS
import qualified Toml

-- Simplified for testing
data SimulationMode = InMemory | LocalProcesses | GeoDistributed
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Scenario configuration from TOML
data ScenarioConfig = ScenarioConfig
  { scenarioName :: String
  , scenarioMode :: SimulationMode
  , scenarioActors :: [String]  -- Actor identifiers
  , scenarioPrograms :: [String]  -- Program identifiers
  , scenarioLogPath :: Maybe FilePath
  } deriving stock (Eq, Generic, Show)
    deriving anyclass (Serialize)

-- | A scenario is a specific configuration of actors and programs
data Scenario = Scenario
  { scenarioConfig :: ScenarioConfig
  , scenarioDeployment :: Maybe ()  -- Simplified for now
  }
  deriving stock (Eq)

-- | Custom display function for Scenario
displayScenario :: Scenario -> String
displayScenario s = "Scenario { config = " ++ show (scenarioConfig s) ++ 
  ", deployment = " ++ maybe "Nothing" (const "Just ()") (scenarioDeployment s) ++ " }" 