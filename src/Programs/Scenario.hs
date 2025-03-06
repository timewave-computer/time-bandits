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
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Serialize (Serialize)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Polysemy (Member, Sem, Embed, embed)
import Polysemy.Error (Error, throw, runError, fromEither)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as BS

-- Import from TimeBandits modules
import TimeBandits.Core (Hash(..), EntityHash(..))
import Core.Types
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
  -- Parse the TOML content using a simple line-by-line approach
  let lines = T.lines content
      
      -- Function to find key-value pairs
      findValue key = T.strip <$> findValueInLines key lines
      
      -- Extract scenario name
      nameM = findValue "name"
      name = fromMaybe "Unnamed Scenario" nameM
      
      -- Extract scenario description
      descM = findValue "description" 
      desc = fromMaybe "No description provided" descM
      
      -- Extract simulation mode
      modeM = findValue "mode"
      mode = case modeM of
        Just "InMemory" -> InMemory
        Just "LocalProcesses" -> LocalProcesses
        Just "GeoDistributed" -> GeoDistributed
        _ -> InMemory  -- Default to InMemory
      
      -- For actors, we'll use a simplified approach
      -- In a real implementation with the toml package, we'd parse nested tables
      timeTravelers = extractActorsByPrefix lines "time_travelers" TimeTravelerRole
      timeKeepers = extractActorsByPrefix lines "time_keepers" TimeKeeperRole
      timeBandits = extractActorsByPrefix lines "time_bandits" TimeBanditRole
      allActors = timeTravelers ++ timeKeepers ++ timeBandits
  
  -- Create the scenario configuration
  return ScenarioConfig
    { scenarioName = name
    , scenarioDescription = desc
    , scenarioMode = mode
    , scenarioActors = allActors
    , scenarioPrograms = []  -- We'd parse from TOML in a real implementation
    , scenarioInitialResources = []  -- We'd parse from TOML
    , scenarioLogPath = "logs"
    , scenarioMaxSteps = 100
    , scenarioTimeout = 60
    }

-- | Find a value for a key in lines of text
findValueInLines :: Text -> [Text] -> Maybe Text
findValueInLines key lines = 
  let keyPrefix = key <> " = "
      matchingLines = filter (\l -> keyPrefix `T.isPrefixOf` T.strip l) lines
  in case matchingLines of
       [] -> Nothing
       (line:_) -> 
         let parts = T.splitOn "=" line
         in if length parts >= 2
            then Just $ removeQuotes $ T.strip $ parts !! 1
            else Nothing

-- | Remove quotes from a text value
removeQuotes :: Text -> Text
removeQuotes t =
  let t' = T.strip t
  in if T.length t' >= 2 && T.head t' == '"' && T.last t' == '"'
     then T.drop 1 $ T.dropEnd 1 t'
     else t'

-- | Extract actors with a specific role from TOML lines
extractActorsByPrefix :: [Text] -> Text -> ActorRole -> [ActorSpec]
extractActorsByPrefix lines prefix role =
  -- For simplicity, we'll create a few predefined actors
  -- In a real implementation, we'd parse from the TOML
  case role of
    TimeTravelerRole ->
      [ ActorSpec
        { actorSpecId = "alice"
        , actorSpecRole = TimeTravelerRole
        , actorSpecCapabilities = [ResourceCreation, ResourceTransfer]
        , actorSpecDescription = "Time Traveler Alice"
        }
      ]
    TimeKeeperRole ->
      [ ActorSpec
        { actorSpecId = "ethereum_keeper"
        , actorSpecRole = TimeKeeperRole
        , actorSpecCapabilities = [TimelineAccess]
        , actorSpecDescription = "Ethereum Timeline Keeper"
        }
      ]
    TimeBanditRole ->
      [ ActorSpec
        { actorSpecId = "network_node_1"
        , actorSpecRole = TimeBanditRole
        , actorSpecCapabilities = [ProofGeneration, NetworkCoordination]
        , actorSpecDescription = "P2P Network Node"
        }
      ]

-- | Generate TOML content from a scenario
generateScenarioToml :: ScenarioConfig -> Text
generateScenarioToml config =
  T.unlines $
    -- Scenario section
    [ "[scenario]"
    , "name = \"" <> scenarioName config <> "\""
    , "description = \"" <> scenarioDescription config <> "\""
    , "mode = \"" <> T.pack (show (scenarioMode config)) <> "\""
    , "log_path = \"" <> T.pack (scenarioLogPath config) <> "\""
    , "max_steps = " <> T.pack (show (scenarioMaxSteps config))
    , "timeout = " <> T.pack (show (scenarioTimeout config))
    , ""
    ] ++
    
    -- Time Travelers
    concatMap (actorToToml "time_travelers") 
      (filter (\a -> actorSpecRole a == TimeTravelerRole) (scenarioActors config)) ++
    
    -- Time Keepers
    concatMap (actorToToml "time_keepers") 
      (filter (\a -> actorSpecRole a == TimeKeeperRole) (scenarioActors config)) ++
    
    -- Time Bandits
    concatMap (actorToToml "time_bandits") 
      (filter (\a -> actorSpecRole a == TimeBanditRole) (scenarioActors config))

-- | Convert an actor to TOML lines
actorToToml :: Text -> ActorSpec -> [Text]
actorToToml section actor =
  [ "[[" <> section <> "]]"
  , "id = \"" <> actorSpecId actor <> "\""
  , "capabilities = [" <> capabilitiesToToml (actorSpecCapabilities actor) <> "]"
  , "description = \"" <> actorSpecDescription actor <> "\""
  , ""
  ]

-- | Convert capabilities to a TOML array string
capabilitiesToToml :: [ActorCapability] -> Text
capabilitiesToToml capabilities =
  T.intercalate ", " $ map capabilityToToml capabilities

-- | Convert a capability to a TOML string
capabilityToToml :: ActorCapability -> Text
capabilityToToml ResourceCreation = "\"ResourceCreation\""
capabilityToToml ResourceTransfer = "\"ResourceTransfer\""
capabilityToToml TimelineAccess = "\"TimelineAccess\""
capabilityToToml ProofGeneration = "\"ProofGeneration\""
capabilityToToml NetworkCoordination = "\"NetworkCoordination\""

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