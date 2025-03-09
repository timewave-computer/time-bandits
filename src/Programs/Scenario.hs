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
module Programs.Scenario 
  ( -- * Core Types
    Scenario(..)
  , ScenarioConfig(..)
  , ScenarioError(..)
  , ScenarioResult(..)
  , ExecutionMode(..)
  , ScenarioEntity(..)
  , ScenarioTimeline(..)
  
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
import Control.Monad.IO.Class (liftIO)
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
import Core (Hash(..), EntityHash(..))
import Core.Types
  ( AppError(..)
  , LamportTime(..)
  )
import Core.Resource 
  ( Resource
  , Address
  )
import Programs.Program 
  ( ProgramId
  , ProgramState
  )
import Core.Common (SimulationMode(..))
import Types.Actor
  ( ActorRole(..)
  , ActorCapability(..)
  , ActorSpec(..)
  )
import Actors.ActorTypes
  ( ActorHandle
  )
import Types.Deployment
  ( DeploymentConfig(..)
  , DeploymentError(..)
  , DeploymentResult(..)
  , Deployment(..)
  )
import Types.Scenario
  ( ScenarioConfig(..)
  , ScenarioError(..)
  , ScenarioResult(..)
  , ExecutionMode(..)
  , ScenarioEntity(..)
  , ScenarioTimeline(..)
  )

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
  -- Try to read the file
  contentE <- embed $ try @SomeException $ readFile path
  case contentE of
    Right content -> do
      -- Parse the TOML content
      config <- parseScenarioToml (T.pack content)
      
      -- Return the scenario
      return $ Scenario
        { scenarioConfig = config
        , scenarioDeployment = Nothing
        , scenarioResults = Nothing
        }
    Left e -> throw $ ParseError $ "Failed to read file: " <> T.pack (show e)

-- | Parse a scenario from TOML content
parseScenarioToml :: 
  (Member (Error ScenarioError) r) => 
  Text -> 
  Sem r ScenarioConfig
parseScenarioToml content = do
  -- Parse the TOML content using a simple line-by-line approach
  let contentLines = T.lines content
      
      -- Function to find key-value pairs
      findValue key = T.strip <$> findValueInLines key contentLines
      
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
        Just "LocalProcesses" -> MultiProcess
        Just "GeoDistributed" -> Distributed
        _ -> InMemory  -- Default to InMemory
      
      -- For actors, we'll use a simplified approach
      -- In a real implementation with the toml package, we'd parse nested tables
      timeTravelers = extractActorsByPrefix contentLines "time_travelers" TimeTravelerRole
      timeKeepers = extractActorsByPrefix contentLines "time_keepers" TimeKeeperRole
      timeBandits = extractActorsByPrefix contentLines "time_bandits" TimeBanditRole
      allActors = timeTravelers ++ timeKeepers ++ timeBandits
  
  -- Create the scenario configuration
  return ScenarioConfig
    { scenarioName = name
    , scenarioDescription = desc
    , scenarioMode = mode
    , scenarioActors = allActors
    , scenarioInitialPrograms = []  -- We'd parse from TOML in a real implementation
    , scenarioLogPath = "logs"
    , scenarioMaxSteps = 100
    , scenarioTimeout = 60
    , scenarioExecutionMode = SingleNode  -- Default to SingleNode
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
            then Just $ removeQuotes $ T.strip $ case parts of
                                                   (_:value:_) -> value
                                                   _ -> ""
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
        , actorSpecCapabilities = [CanCreateProgram, CanTransferResource]
        , actorSpecInitialPrograms = []
        }
      ]
    TimeKeeperRole ->
      [ ActorSpec
        { actorSpecId = "ethereum_keeper"
        , actorSpecRole = TimeKeeperRole
        , actorSpecCapabilities = [CanServeTimelineState]
        , actorSpecInitialPrograms = []
        }
      ]
    TimeBanditRole ->
      [ ActorSpec
        { actorSpecId = "network_node_1"
        , actorSpecRole = TimeBanditRole
        , actorSpecCapabilities = [CanGenerateProof, CanValidateTransition]
        , actorSpecInitialPrograms = []
        }
      ]

-- | Generate TOML content from a scenario
generateScenarioToml :: ScenarioConfig -> Text
generateScenarioToml config =
  unlines $
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
capabilityToToml CanCreateProgram = "\"CanCreateProgram\""
capabilityToToml CanExecuteProgram = "\"CanExecuteProgram\""
capabilityToToml CanTransferResource = "\"CanTransferResource\""
capabilityToToml CanValidateTransition = "\"CanValidateTransition\""
capabilityToToml CanGenerateProof = "\"CanGenerateProof\""
capabilityToToml CanServeTimelineState = "\"CanServeTimelineState\""

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
  
  -- Create a deployment configuration
  let deploymentConfig = createDeploymentConfig scenario
  
  -- Run the deployment
  deploymentResult <- runError @DeploymentError $ do
    deployment <- createDeployment deploymentConfig
    startDeployment deployment
  
  -- Handle the deployment result
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
      let resultText = unlines
            [ "# Scenario Results"
            , "Name: " <> scenarioName (scenarioConfig scenario)
            , "Success: " <> T.pack (show (resultSuccess results))
            , "Steps: " <> T.pack (show (resultSteps results))
            , ""
            , "## Errors"
            , unlines (map ("- " <>) (resultErrors results))
            , ""
            , "## Execution Log"
            , unlines (map ("- " <>) (resultExecutionLog results))
            ]
      
      -- Write the results to the file
      liftIO $ writeFileText path resultText

-- | Create a deployment from a deployment config
-- This is a placeholder for the actual implementation
createDeployment :: 
  (Member (Error DeploymentError) r, Member (Embed IO) r) => 
  DeploymentConfig -> 
  Sem r Deployment
createDeployment config = do
  -- In a real implementation, this would create a deployment
  return Deployment
    { deploymentConfig = config
    , deploymentName = "test-deployment"
    , deploymentId = "deployment-123"
    , deploymentStartTime = "2023-01-01T00:00:00Z"
    , deploymentActorIds = []
    , deploymentProgramIds = []
    }

-- | Start a deployment
-- This is a placeholder for the actual implementation
startDeployment :: 
  (Member (Error DeploymentError) r, Member (Embed IO) r) => 
  Deployment -> 
  Sem r Deployment
startDeployment deployment = do
  -- In a real implementation, this would start the deployment
  return deployment 

-- | Get a description for an actor spec
actorSpecDescription :: ActorSpec -> Text
actorSpecDescription actor = 
  "Actor with role: " <> T.pack (show (actorSpecRole actor)) 

-- | Create a deployment configuration from a scenario
createDeploymentConfig :: Scenario -> DeploymentConfig
createDeploymentConfig scenario = 
  DeploymentConfig
    { deploymentMode = scenarioMode (scenarioConfig scenario)
    , deploymentLogPath = scenarioLogPath (scenarioConfig scenario)
    , deploymentVerbose = True
    , deploymentActors = scenarioActors (scenarioConfig scenario)
    , deploymentInitialPrograms = scenarioInitialPrograms (scenarioConfig scenario)
    } 