{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.Deployment
  ( SimulationMode(..)
  , ScenarioConfig(..)
  , Scenario(..)
  , displayScenario
  ) where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)

-- | Mode for running a simulation
data SimulationMode = Interactive | Batch
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Configuration for a scenario
data ScenarioConfig = ScenarioConfig
  { scenarioName :: String
  , scenarioMode :: SimulationMode
  , scenarioActors :: [String]
  , scenarioPrograms :: [String]
  , scenarioLogPath :: String
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A scenario for simulation
data Scenario = Scenario
  { scenarioConfig :: ScenarioConfig
  , scenarioDeployment :: Maybe String  -- Simplified for now
  }
  deriving stock (Eq, Generic)

-- | Display a scenario as a string
displayScenario :: Scenario -> String
displayScenario scenario = 
  "Scenario { config: " ++ show (scenarioConfig scenario) ++ 
  ", deployment: " ++ maybe "None" id (scenarioDeployment scenario) ++ " }" 