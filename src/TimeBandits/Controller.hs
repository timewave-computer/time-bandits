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
This module provides the Controller abstraction, which enforces the system contract
regardless of deployment mode. The Controller is responsible for:

- Validating transition messages
- Applying effects and updating program state
- Maintaining the execution log
- Ensuring time map consistency
- Enforcing security properties

The Controller is the central component that ensures the system behaves consistently
across different deployment modes.
-}
module TimeBandits.Controller 
  ( -- * Core Types
    Controller(..)
  , ControllerConfig(..)
  , SimulationMode(..)
  , DeploymentMode(..)
  , ControllerError(..)
  
  -- * Controller Operations
  , initController
  , runController
  , processTransition
  , deployProgram
  , getExecutionLog
  , getTimeMap
  , getProgramState
  
  -- * Simulation Mode Operations
  , setSimulationMode
  , getSimulationMode

  -- * New functions
  , deploySpecializedActors
  , runWithScenario
  , determineRequiredActors
  , createTimeTravelersForScenario
  , createTimeKeepersForScenario
  , createTimeBanditsForScenario
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw, fromEither, catch)
import qualified Data.ByteString.Char8 as BS

-- Import from TimeBandits modules
import TimeBandits.Core (Hash(..), EntityHash(..))
import TimeBandits.Types
  ( AppError(..)
  , LamportTime(..)
  )
import TimeBandits.Resource 
  ( Resource(..)
  , Address
  , verifyResourceOwnership
  )
import TimeBandits.Program 
  ( ProgramId
  , ProgramState(..)
  , isAuthorizedCaller
  )
import TimeBandits.ProgramEffect 
  ( Effect
  , GuardedEffect(..)
  )
import TimeBandits.TimeMap
  ( TimeMap
  , TimeMapId
  , updateTimeMap
  , verifyTimeMapConsistency
  )
import TimeBandits.TransitionMessage
  ( TransitionMessage(..)
  , LogEntry(..)
  , validateTransitionMessage
  , applyTransitionMessage
  , hashTransitionMessage
  , createLogEntry
  )
import TimeBandits.ExecutionLog
  ( ExecutionLog(..)
  , LogStore
  , createLogStore
  , appendLogEntry
  , verifyLogChain
  )
import TimeBandits.EffectExecutor
  ( applyEffect
  , verifyTransitionProof
  )
import TimeBandits.Scenario
  ( Scenario(..)
  , ScenarioEntity(..)
  , ScenarioTimeline(..)
  , ExecutionMode(..)
  , scenarioEntities
  , scenarioTimelines
  , scenarioExecutionMode
  )
import TimeBandits.Actor
  ( Actor(..)
  , ActorSpec(..)
  , ActorRole(..)
  , ActorCapability(..)
  , ActorHandle
  , ActorError(..)
  , deployActor
  )

-- Import specialized actor role modules
import qualified TimeBandits.TimeTraveler
import qualified TimeBandits.TimeKeeper
import qualified TimeBandits.TimeBandit

-- | Simulation mode determines how actors are deployed and communicate
data SimulationMode
  = InMemory        -- ^ All actors run in the same process
  | LocalProcesses  -- ^ Actors run in separate local processes
  | GeoDistributed  -- ^ Actors run on different machines
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Deployment mode (alias for SimulationMode for consistency)
type DeploymentMode = SimulationMode

-- | Controller configuration
data ControllerConfig = ControllerConfig
  { configMode :: SimulationMode
  , configLogPath :: FilePath
  , configVerbose :: Bool
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Controller errors
data ControllerError
  = InvalidTransition Text
  | ProgramNotFound ProgramId
  | ResourceNotAvailable (EntityHash Resource)
  | TimeMapInconsistency Text
  | LogVerificationFailed Text
  | DeploymentError Text
  | UnauthorizedAccess Address ProgramId
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | The Controller enforces the system contract
data Controller = Controller
  { controllerMode :: SimulationMode
  , controllerTimeMap :: TimeMap
  , controllerLogStore :: LogStore
  , controllerPrograms :: Map ProgramId ProgramState
  , controllerResources :: Map (EntityHash Resource) Resource
  , controllerActors :: Map Address (ActorHandle r)
  , controllerIsInitialized :: Bool
  , controllerDeploymentMode :: SimulationMode
  }
  deriving (Generic)
  deriving anyclass (Serialize)

-- | Initialize a new controller with the given configuration
initController :: (Member (Error AppError) r) => ControllerConfig -> Sem r Controller
initController config = do
  -- Create an empty time map
  let timeMap = mempty
  
  -- Initialize the log store
  logStore <- createLogStore (configLogPath config)
  
  -- Return the initialized controller
  return Controller
    { controllerMode = configMode config
    , controllerTimeMap = timeMap
    , controllerLogStore = logStore
    , controllerPrograms = Map.empty
    , controllerResources = Map.empty
    , controllerActors = Map.empty
    , controllerIsInitialized = False
    , controllerDeploymentMode = configMode config
    }

-- | Run the controller to process a transition message
runController :: 
  (Member (Error AppError) r, Member (Error ControllerError) r) => 
  Controller -> 
  TransitionMessage -> 
  Sem r (Controller, ProgramState)
runController controller msg = do
  -- Process the transition
  (newController, programState) <- processTransition controller msg
  
  -- Return the updated controller and program state
  return (newController, programState)

-- | Process a transition message
processTransition ::
  (Member (Error AppError) r, Member (Error ControllerError) r) =>
  Controller ->
  TransitionMessage ->
  Sem r (Controller, ProgramState)
processTransition controller msg = do
  -- 1. Validate the transition message
  validationResult <- fromEither $ validateTransitionMessage msg
  case validationResult of
    Left err -> throw $ InvalidTransition (T.pack $ show err)
    Right _ -> pure ()
  
  -- 2. Verify resource ownership
  let resources = transitionResources msg
  forM_ resources $ \resource -> do
    let resourceHash = resourceId resource
    unless (verifyResourceOwnership (transitionActor msg) resourceHash) $
      throw $ ResourceNotAvailable resourceHash
  
  -- 3. Verify the actor is authorized to call the program
  let progId = transitionProgram msg
  programState <- case Map.lookup progId (controllerPrograms controller) of
    Nothing -> throw $ ProgramNotFound progId
    Just state -> pure state
  
  unless (isAuthorizedCaller programState (transitionActor msg)) $
    throw $ UnauthorizedAccess (transitionActor msg) progId
  
  -- 4. Verify the transition proof
  unless (verifyTransitionProof msg) $
    throw $ InvalidTransition "Invalid transition proof"
  
  -- 5. Apply the transition message
  (effect, newProgramState) <- fromEither $ applyTransitionMessage msg programState
  
  -- 6. Update the time map
  let timeMapId = programTimeMap programState
  newTimeMap <- updateTimeMap (controllerTimeMap controller) timeMapId effect
  
  -- 7. Verify time map consistency
  unless (verifyTimeMapConsistency newTimeMap) $
    throw $ TimeMapInconsistency "Time map inconsistency detected"
  
  -- 8. Create a log entry and append to the log
  let transitionHash = hashTransitionMessage msg
  timestamp <- embed getCurrentTime
  logEntry <- createLogEntry effect timestamp transitionHash
  newLogStore <- appendLogEntry (controllerLogStore controller) logEntry
  
  -- 9. Verify the log chain
  unless (verifyLogChain newLogStore) $
    throw $ LogVerificationFailed "Log chain verification failed"
  
  -- 10. Update the controller state
  let newPrograms = Map.insert progId newProgramState (controllerPrograms controller)
      newController = controller
        { controllerTimeMap = newTimeMap
        , controllerLogStore = newLogStore
        , controllerPrograms = newPrograms
        }
  
  -- Return the updated controller and program state
  return (newController, newProgramState)

-- | Deploy a new program
deployProgram ::
  (Member (Error AppError) r, Member (Error ControllerError) r) =>
  Controller ->
  ProgramState ->
  Sem r Controller
deployProgram controller programState = do
  let progId = programId programState
  
  -- Check if the program already exists
  when (Map.member progId (controllerPrograms controller)) $
    throw $ DeploymentError "Program already exists"
  
  -- Add the program to the controller
  let newPrograms = Map.insert progId programState (controllerPrograms controller)
      newController = controller { controllerPrograms = newPrograms }
  
  return newController

-- | Get the execution log
getExecutionLog :: Controller -> ExecutionLog
getExecutionLog controller = 
  ExecutionLog { logEntries = logEntries (controllerLogStore controller) }

-- | Get the time map
getTimeMap :: Controller -> TimeMap
getTimeMap = controllerTimeMap

-- | Get a program state
getProgramState :: 
  (Member (Error ControllerError) r) => 
  Controller -> 
  ProgramId -> 
  Sem r ProgramState
getProgramState controller progId =
  case Map.lookup progId (controllerPrograms controller) of
    Nothing -> throw $ ProgramNotFound progId
    Just state -> return state

-- | Set the simulation mode
setSimulationMode :: Controller -> SimulationMode -> Controller
setSimulationMode controller mode =
  controller { controllerMode = mode }

-- | Get the current simulation mode
getSimulationMode :: Controller -> SimulationMode
getSimulationMode = controllerMode 

-- | Deploy specialized actors based on their roles
deploySpecializedActors :: (Member (Error ControllerError) r)
                       => DeploymentMode
                       -> [ActorSpec]
                       -> Sem r (Map.Map Address (ActorHandle r))
deploySpecializedActors mode specs = do
  -- Deploy each specialized actor
  actors <- forM specs $ \spec -> do
    -- Deploy the actor with the specialized implementation
    actorHandle <- catch
      (deployActor spec mode)
      (\e -> throw $ DeploymentError $ "Actor initialization failed: " <> T.pack (show e))
    
    -- Return the actor handle with its ID
    return (actorSpecId spec, actorHandle)
  
  -- Return a map of actor handles by address
  return $ Map.fromList actors

-- | Controller specification for initialization
data ControllerSpec = ControllerSpec
  { controllerSpecMode :: DeploymentMode
  , controllerSpecLogPath :: FilePath
  , controllerSpecVerbose :: Bool
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a controller from a specification
createController :: (Member (Error ControllerError) r)
                 => ControllerSpec
                 -> Sem r Controller
createController spec = do
  -- Create an empty time map
  let timeMap = mempty
  
  -- Initialize the log store
  logStore <- createLogStore (controllerSpecLogPath spec)
  
  -- Return the initialized controller
  return Controller
    { controllerMode = controllerSpecMode spec
    , controllerTimeMap = timeMap
    , controllerLogStore = logStore
    , controllerPrograms = Map.empty
    , controllerResources = Map.empty
    , controllerActors = Map.empty
    , controllerIsInitialized = False
    , controllerDeploymentMode = controllerSpecMode spec
    }

-- | Run the controller with a scenario
runWithScenario :: (Member (Error ControllerError) r)
                => ControllerSpec
                -> Scenario
                -> Sem r Controller
runWithScenario spec scenario = do
  -- Create the controller from the specification
  controller <- createController spec
  
  -- Deploy actors based on the scenario requirements
  let requiredActors = determineRequiredActors scenario
  
  -- Group actors by role for specialized deployment
  let travelers = filter (\s -> actorSpecRole s == TimeTravelerRole) requiredActors
      keepers = filter (\s -> actorSpecRole s == TimeKeeperRole) requiredActors
      bandits = filter (\s -> actorSpecRole s == TimeBanditRole) requiredActors
  
  -- Deploy specialized actors
  travelerActors <- deploySpecializedActors (controllerDeploymentMode controller) travelers
  keeperActors <- deploySpecializedActors (controllerDeploymentMode controller) keepers
  banditActors <- deploySpecializedActors (controllerDeploymentMode controller) bandits
  
  -- Combine all actor maps
  let allActors = travelerActors `Map.union` keeperActors `Map.union` banditActors
  
  -- Update the controller with the deployed actors
  let updatedController = controller 
        { controllerActors = allActors
        , controllerIsInitialized = True
        }
  
  -- Return the updated controller
  return updatedController

-- | Determine the required actors for a scenario
determineRequiredActors :: Scenario -> [ActorSpec]
determineRequiredActors scenario = 
  -- Create actor specs for each role required by the scenario
  concat
    [ createTimeTravelersForScenario scenario
    , createTimeKeepersForScenario scenario
    , createTimeBanditsForScenario scenario
    ]

-- | Create Time Traveler actor specs for a scenario
createTimeTravelersForScenario :: Scenario -> [ActorSpec]
createTimeTravelersForScenario scenario =
  -- For each entity in the scenario that needs a Time Traveler role
  map (\(idx, entity) -> ActorSpec
    { actorSpecId = "traveler-" <> T.pack (show idx)
    , actorSpecRole = TimeTravelerRole
    , actorSpecCapabilities = 
        [ CanCreateProgram
        , CanTransferResource 
        ]
    , actorSpecInitialPrograms = []
    }) (zip [1..] (scenarioEntities scenario))

-- | Create Time Keeper actor specs for a scenario
createTimeKeepersForScenario :: Scenario -> [ActorSpec]
createTimeKeepersForScenario scenario =
  -- For each timeline in the scenario that needs a Time Keeper role
  map (\(idx, timeline) -> ActorSpec
    { actorSpecId = "keeper-" <> T.pack (show idx)
    , actorSpecRole = TimeKeeperRole
    , actorSpecCapabilities = 
        [ CanValidateTransition
        , CanServeTimelineState
        ]
    , actorSpecInitialPrograms = []
    }) (zip [1..] (scenarioTimelines scenario))

-- | Create Time Bandit actor specs for a scenario
createTimeBanditsForScenario :: Scenario -> [ActorSpec]
createTimeBanditsForScenario scenario =
  -- Create a fixed number of Time Bandits based on the scenario's execution needs
  let numBandits = case scenarioExecutionMode scenario of
        SingleNode -> 1
        MultiNode -> 3
        DistributedNodes -> 5
  in
  map (\idx -> ActorSpec
    { actorSpecId = "bandit-" <> T.pack (show idx)
    , actorSpecRole = TimeBanditRole
    , actorSpecCapabilities = 
        [ CanExecuteProgram
        , CanGenerateProof
        ]
    , actorSpecInitialPrograms = []
    }) [1..numBandits] 