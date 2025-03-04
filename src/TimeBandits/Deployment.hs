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
This module provides deployment functionality for different simulation modes.
It handles the deployment of actors, programs, and controllers in:

- In-memory mode: All components run in the same process
- Local multi-process mode: Components run in separate local processes
- Geo-distributed mode: Components run on different machines

The deployment module ensures that the system behaves consistently
regardless of the deployment mode.
-}
module TimeBandits.Deployment 
  ( -- * Core Types
    Deployment(..)
  , DeploymentConfig(..)
  , DeploymentError(..)
  , DeploymentStatus(..)
  
  -- * Deployment Operations
  , createDeployment
  , startDeployment
  , stopDeployment
  , getDeploymentStatus
  , getDeployedActors
  , getDeployedPrograms
  
  -- * Scenario Operations
  , Scenario(..)
  , ScenarioConfig(..)
  , loadScenario
  , runScenario
  ) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Exception (try, SomeException)
import Control.Monad (forM, forM_, void, when)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Network.Socket (Socket)
import Polysemy (Member, Sem, runM, interpret, Embed, embed)
import Polysemy.Error (Error, throw, runError, fromEither)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Process (ProcessHandle, createProcess, proc, terminateProcess)
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
  , ActorHandle
  , deployActor
  )

-- | Deployment status
data DeploymentStatus
  = Initializing
  | Running
  | Stopping
  | Stopped
  | Failed Text
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Deployment errors
data DeploymentError
  = ConfigurationError Text
  | ActorDeploymentError Text
  | ControllerDeploymentError Text
  | ScenarioLoadError Text
  | CommunicationError Text
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Deployment configuration
data DeploymentConfig = DeploymentConfig
  { deploymentMode :: SimulationMode
  , deploymentLogPath :: FilePath
  , deploymentVerbose :: Bool
  , deploymentActors :: [ActorSpec]
  , deploymentInitialPrograms :: [ProgramState]
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A deployment represents a running instance of the Time Bandits system
data Deployment = Deployment
  { deploymentConfig :: DeploymentConfig
  , deploymentController :: Controller
  , deploymentActors :: Map Address ActorHandle
  , deploymentStatus :: DeploymentStatus
  , deploymentThreads :: [ThreadId]
  , deploymentProcesses :: [ProcessHandle]
  }

-- | Scenario configuration from TOML
data ScenarioConfig = ScenarioConfig
  { scenarioName :: Text
  , scenarioMode :: SimulationMode
  , scenarioActors :: [ActorSpec]
  , scenarioPrograms :: [ProgramId]
  , scenarioLogPath :: FilePath
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A scenario is a specific configuration of actors and programs
data Scenario = Scenario
  { scenarioConfig :: ScenarioConfig
  , scenarioDeployment :: Maybe Deployment
  }
  deriving (Eq, Show)

-- | Create a new deployment with the given configuration
createDeployment :: 
  (Member (Error AppError) r, Member (Error DeploymentError) r) => 
  DeploymentConfig -> 
  Sem r Deployment
createDeployment config = do
  -- Create the controller
  let controllerConfig = ControllerConfig
        { configMode = deploymentMode config
        , configLogPath = deploymentLogPath config
        , configVerbose = deploymentVerbose config
        }
  
  controllerResult <- runError $ initController controllerConfig
  controller <- case controllerResult of
    Left err -> throw $ ControllerDeploymentError $ T.pack $ show err
    Right c -> return c
  
  -- Return the initialized deployment
  return Deployment
    { deploymentConfig = config
    , deploymentController = controller
    , deploymentActors = Map.empty
    , deploymentStatus = Initializing
    , deploymentThreads = []
    , deploymentProcesses = []
    }

-- | Start a deployment
startDeployment :: 
  (Member (Error AppError) r, Member (Error DeploymentError) r, Member (Embed IO) r) => 
  Deployment -> 
  Sem r Deployment
startDeployment deployment = do
  -- Deploy the actors
  actorResults <- liftIO $ forM (deploymentActors (deploymentConfig deployment)) $ \spec -> do
    result <- deployActor (deploymentMode (deploymentConfig deployment)) spec
    return (actorSpecId spec, result)
  
  -- Check for deployment errors
  let errors = [err | (_, Left err) <- actorResults]
  unless (null errors) $
    throw $ ActorDeploymentError $ T.pack $ show errors
  
  -- Extract the successful actor handles
  let actorHandles = Map.fromList [(id, handle) | (id, Right handle) <- actorResults]
  
  -- Deploy the initial programs
  let controller = deploymentController deployment
  
  -- Start the controller thread
  controllerThread <- liftIO $ forkIO $ do
    -- In a real implementation, this would run the controller
    pure ()
  
  -- Return the updated deployment
  return deployment
    { deploymentActors = actorHandles
    , deploymentStatus = Running
    , deploymentThreads = controllerThread : deploymentThreads deployment
    }

-- | Stop a deployment
stopDeployment :: 
  (Member (Error AppError) r, Member (Error DeploymentError) r, Member (Embed IO) r) => 
  Deployment -> 
  Sem r Deployment
stopDeployment deployment = do
  -- Set the status to stopping
  let deployment' = deployment { deploymentStatus = Stopping }
  
  -- Terminate all processes
  liftIO $ forM_ (deploymentProcesses deployment') terminateProcess
  
  -- Return the updated deployment
  return deployment' { deploymentStatus = Stopped, deploymentProcesses = [] }

-- | Get the deployment status
getDeploymentStatus :: Deployment -> DeploymentStatus
getDeploymentStatus = deploymentStatus

-- | Get the deployed actors
getDeployedActors :: Deployment -> Map Address ActorHandle
getDeployedActors = deploymentActors

-- | Get the deployed programs
getDeployedPrograms :: Deployment -> [ProgramId]
getDeployedPrograms deployment =
  -- In a real implementation, this would query the controller
  []

-- | Load a scenario from a TOML file
loadScenario :: 
  (Member (Error DeploymentError) r, Member (Embed IO) r) => 
  FilePath -> 
  Sem r Scenario
loadScenario path = do
  -- Read the TOML file
  tomlResult <- liftIO $ try $ TIO.readFile path
  tomlContent <- case tomlResult of
    Left (e :: SomeException) -> throw $ ScenarioLoadError $ "Failed to read file: " <> T.pack (show e)
    Right content -> return content
  
  -- Parse the TOML content
  -- In a real implementation, this would use the Toml library to parse the content
  -- For now, we'll create a dummy scenario config
  let config = ScenarioConfig
        { scenarioName = "Dummy Scenario"
        , scenarioMode = InMemory
        , scenarioActors = []
        , scenarioPrograms = []
        , scenarioLogPath = "logs"
        }
  
  -- Return the scenario
  return Scenario
    { scenarioConfig = config
    , scenarioDeployment = Nothing
    }

-- | Run a scenario
runScenario :: 
  (Member (Error AppError) r, Member (Error DeploymentError) r, Member (Embed IO) r) => 
  Scenario -> 
  Sem r Scenario
runScenario scenario = do
  -- Create a deployment config from the scenario
  let deploymentConfig = DeploymentConfig
        { deploymentMode = scenarioMode (scenarioConfig scenario)
        , deploymentLogPath = scenarioLogPath (scenarioConfig scenario)
        , deploymentVerbose = True
        , deploymentActors = scenarioActors (scenarioConfig scenario)
        , deploymentInitialPrograms = []
        }
  
  -- Create and start the deployment
  deployment <- createDeployment deploymentConfig
  deployment' <- startDeployment deployment
  
  -- Return the updated scenario
  return scenario { scenarioDeployment = Just deployment' }

-- | Helper function to lift IO actions into Sem
liftIO :: Member (Embed IO) r => IO a -> Sem r a
liftIO = embed 