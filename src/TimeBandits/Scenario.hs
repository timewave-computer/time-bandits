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
This module provides functionality for defining and running scenarios using TOML.
Scenarios define:

- Actors, their roles, and capabilities
- Initial program deployments
- Simulation mode (in-memory, local processes, geo-distributed)
- Communication channels

The scenario module allows for reproducible simulations and tests.
-}
module TimeBandits.Scenario 
  ( -- * Core Types
    Scenario(..)
  , ScenarioConfig(..)
  , ScenarioError(..)
  , ScenarioResult(..)
  
  -- * Scenario Operations
  , loadScenario
  , runScenario
  , validateScenario
  , exportScenarioResults
  
  -- * TOML Operations
  , parseScenarioToml
  , generateScenarioToml
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (forM, forM_, void, when, unless)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Polysemy (Member, Sem, Embed, embed)
import Polysemy.Error (Error, throw, runError, fromEither)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import qualified Data.ByteString.Char8 as BS
import qualified Toml

-- Import from TimeBandits modules
import TimeBandits.Core (Hash(..), EntityHash(..))
import TimeBandits.Types
  ( AppError(..)
  , LamportTime(..)
  )
import TimeBandits.Resource 
  ( Resource
  , Address
  )
import TimeBandits.Program 
  ( ProgramId
  , ProgramState
  )
import TimeBandits.Controller
  ( Controller
  , ControllerConfig(..)
  , SimulationMode(..)
  , initController
  )
import TimeBandits.Actor
  ( Actor
  , ActorSpec(..)
  , ActorRole(..)
  , ActorCapability(..)
  )
import TimeBandits.Deployment
  ( Deployment
  , DeploymentConfig(..)
  , DeploymentError
  , createDeployment
  , startDeployment
  )

-- | Scenario configuration
data ScenarioConfig = ScenarioConfig
  { scenarioName :: Text
  , scenarioDescription :: Text
  , scenarioMode :: SimulationMode
  , scenarioActors :: [ActorSpec]
  , scenarioPrograms :: [ProgramId]
  , scenarioInitialResources :: [EntityHash Resource]
  , scenarioLogPath :: FilePath
  , scenarioMaxSteps :: Int
  , scenarioTimeout :: Int  -- in seconds
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Scenario errors
data ScenarioError
  = ParseError Text
  | ValidationError Text
  | ExecutionError Text
  | TimeoutError
  | ResourceError Text
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Scenario execution results
data ScenarioResult = ScenarioResult
  { resultSuccess :: Bool
  , resultSteps :: Int
  , resultErrors :: [Text]
  , resultFinalState :: Maybe Controller
  , resultExecutionLog :: [Text]
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A scenario is a specific configuration of actors and programs
data Scenario = Scenario
  { scenarioConfig :: ScenarioConfig
  , scenarioDeployment :: Maybe Deployment
  , scenarioResults :: Maybe ScenarioResult
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Load a scenario from a TOML file
loadScenario :: 
  (Member (Error ScenarioError) r, Member (Embed IO) r) => 
  FilePath -> 
  Sem r Scenario
loadScenario path = do
  -- Read the TOML file
  tomlResult <- liftIO $ try $ TIO.readFile path
  tomlContent <- case tomlResult of
    Left (e :: SomeException) -> throw $ ParseError $ "Failed to read file: " <> T.pack (show e)
    Right content -> return content
  
  -- Parse the TOML content
  config <- parseScenarioToml tomlContent
  
  -- Return the scenario
  return Scenario
    { scenarioConfig = config
    , scenarioDeployment = Nothing
    , scenarioResults = Nothing
    }

-- | Parse a scenario from TOML content
parseScenarioToml :: 
  (Member (Error ScenarioError) r) => 
  Text -> 
  Sem r ScenarioConfig
parseScenarioToml content = do
  -- In a real implementation, this would use the Toml library to parse the content
  -- For now, we'll create a dummy scenario config
  return ScenarioConfig
    { scenarioName = "Dummy Scenario"
    , scenarioDescription = "A dummy scenario for testing"
    , scenarioMode = InMemory
    , scenarioActors = []
    , scenarioPrograms = []
    , scenarioInitialResources = []
    , scenarioLogPath = "logs"
    , scenarioMaxSteps = 100
    , scenarioTimeout = 60
    }

-- | Generate TOML content from a scenario
generateScenarioToml :: ScenarioConfig -> Text
generateScenarioToml config =
  -- In a real implementation, this would use the Toml library to generate the content
  -- For now, we'll create a dummy TOML string
  T.unlines
    [ "[scenario]"
    , "name = \"" <> scenarioName config <> "\""
    , "description = \"" <> scenarioDescription config <> "\""
    , "mode = \"" <> T.pack (show (scenarioMode config)) <> "\""
    , "log_path = \"" <> T.pack (scenarioLogPath config) <> "\""
    , "max_steps = " <> T.pack (show (scenarioMaxSteps config))
    , "timeout = " <> T.pack (show (scenarioTimeout config))
    ]

-- | Validate a scenario
validateScenario :: 
  (Member (Error ScenarioError) r) => 
  Scenario -> 
  Sem r ()
validateScenario scenario = do
  -- Check that the scenario name is not empty
  when (T.null (scenarioName (scenarioConfig scenario))) $
    throw $ ValidationError "Scenario name cannot be empty"
  
  -- Check that there is at least one actor
  when (null (scenarioActors (scenarioConfig scenario))) $
    throw $ ValidationError "Scenario must have at least one actor"
  
  -- Check that the max steps is positive
  when (scenarioMaxSteps (scenarioConfig scenario) <= 0) $
    throw $ ValidationError "Max steps must be positive"
  
  -- Check that the timeout is positive
  when (scenarioTimeout (scenarioConfig scenario) <= 0) $
    throw $ ValidationError "Timeout must be positive"

-- | Run a scenario
runScenario :: 
  (Member (Error AppError) r, Member (Error ScenarioError) r, Member (Error DeploymentError) r, Member (Embed IO) r) => 
  Scenario -> 
  Sem r Scenario
runScenario scenario = do
  -- Validate the scenario
  validateScenario scenario
  
  -- Create a deployment config from the scenario
  let deploymentConfig = DeploymentConfig
        { deploymentMode = scenarioMode (scenarioConfig scenario)
        , deploymentLogPath = scenarioLogPath (scenarioConfig scenario)
        , deploymentVerbose = True
        , deploymentActors = scenarioActors (scenarioConfig scenario)
        , deploymentInitialPrograms = []
        }
  
  -- Create and start the deployment
  deploymentResult <- runError $ do
    deployment <- createDeployment deploymentConfig
    startDeployment deployment
  
  deployment <- case deploymentResult of
    Left err -> throw $ ExecutionError $ T.pack $ show err
    Right d -> return d
  
  -- Create a successful result
  let result = ScenarioResult
        { resultSuccess = True
        , resultSteps = 0
        , resultErrors = []
        , resultFinalState = Nothing
        , resultExecutionLog = []
        }
  
  -- Return the updated scenario
  return scenario
    { scenarioDeployment = Just deployment
    , scenarioResults = Just result
    }

-- | Export scenario results to a file
exportScenarioResults :: 
  (Member (Error ScenarioError) r, Member (Embed IO) r) => 
  Scenario -> 
  FilePath -> 
  Sem r ()
exportScenarioResults scenario path = do
  -- Check that the scenario has results
  case scenarioResults scenario of
    Nothing -> throw $ ValidationError "Scenario has no results to export"
    Just results -> do
      -- Create the output directory if it doesn't exist
      let dir = takeDirectory path
      liftIO $ createDirectoryIfMissing True dir
      
      -- Generate the results as text
      let resultText = T.unlines
            [ "# Scenario Results"
            , "Name: " <> scenarioName (scenarioConfig scenario)
            , "Success: " <> T.pack (show (resultSuccess results))
            , "Steps: " <> T.pack (show (resultSteps results))
            , ""
            , "## Errors"
            , T.unlines (map ("- " <>) (resultErrors results))
            , ""
            , "## Execution Log"
            , T.unlines (map ("- " <>) (resultExecutionLog results))
            ]
      
      -- Write the results to the file
      liftIO $ TIO.writeFile path resultText

-- | Helper function to lift IO actions into Sem
liftIO :: Member (Embed IO) r => IO a -> Sem r a
liftIO = embed 