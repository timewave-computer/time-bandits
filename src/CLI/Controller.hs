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
module CLI.Controller 
  ( -- * Core Types
    Controller(..)
  , ControllerConfig(..)
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
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, replicateM)
import System.Directory (createDirectoryIfMissing)
import Network.Socket (SockAddr(..), PortNumber)
import qualified Polysemy.Trace as PT
import Adapters.NetworkQUIC (startQuicServer, p2pConfigToQuicConfig)
import Adapters.Network (P2PConfig(..), P2PCapability(..))
import Polysemy (Embed, embed, runM)
import Polysemy.Error (runError)

-- Import from TimeBandits modules
import Core (Hash(..), EntityHash(..))
import Core.Types
  ( AppError(..)
  , LamportTime(..)
  )
import Core.Resource 
  ( Resource(..)
  , Address
  , verifyResourceOwnership
  )
import Programs.Program 
  ( ProgramId
  , ProgramState(..)
  , isAuthorizedCaller
  )
import Programs.ProgramEffect 
  ( Effect
  , GuardedEffect(..)
  )
import Core.TimeMap
  ( TimeMap
  , TimeMapId
  , updateTimeMap
  , verifyTimeMapConsistency
  )
import Actors.TransitionMessage
  ( TransitionMessage(..)
  , LogEntry(..)
  , validateTransitionMessage
  , applyTransitionMessage
  , hashTransitionMessage
  , createLogEntry
  )
import Execution.ExecutionLog
  ( ExecutionLog(..)
  , LogStore
  , createLogStore
  , appendLogEntry
  , verifyLogChain
  )
import Execution.EffectExecutor
  ( applyEffect
  , verifyTransitionProof
  )

-- Import from Core.Common for shared types
import Core.Common (SimulationMode(..))

-- Import shared actor types
import Types.Actor
  ( ActorRole(..)
  , ActorCapability(..)
  , ActorSpec(..)
  )

-- Import shared scenario types
import Types.Scenario
  ( ScenarioConfig(..)
  , ScenarioError(..)
  , ScenarioResult(..)
  , ExecutionMode(..)
  , ScenarioEntity(..)
  , ScenarioTimeline(..)
  )

-- Import from Actors modules
import Actors.ActorTypes (ActorHandle(..))

-- Import specialized actor role modules
import qualified Actors.TimeTraveler
import qualified Actors.TimeKeeper
import qualified Actors.TimeBandit

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
  | ActorDeploymentError Text
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

-- | Run the controller with the specified simulation mode
runController :: (Member (Error AppError) r, Member (Embed IO) r) => 
                Controller -> 
                SimulationMode -> 
                Sem r Controller
runController controller mode = do
  -- Update the controller's mode
  let updatedController = controller { controllerMode = mode }
  
  -- Initialize the appropriate network infrastructure based on mode
  case mode of
    InMemory -> do
      -- For in-memory mode, no special network setup is needed
      return updatedController
      
    LocalProcesses -> do
      -- For local processes mode, set up local IPC
      -- This would typically use Unix sockets or similar
      return updatedController
      
    GeoDistributed -> do
      -- For geo-distributed mode, set up QUIC-based networking
      setupGeoDistributedNetworking updatedController
      
-- | Set up QUIC-based networking for geo-distributed mode
setupGeoDistributedNetworking :: (Member (Error AppError) r, Member (Embed IO) r) => 
                               Controller -> 
                               Sem r Controller
setupGeoDistributedNetworking controller = do
  -- Create a default P2P configuration
  let p2pConfig = P2PConfig
        { pcBindAddress = SockAddrInet 8443 0  -- Default QUIC port
        , pcBootstrapNodes = []  -- No bootstrap nodes initially
        , pcMaxConnections = 50  -- Default value
        , pcHeartbeatInterval = 30  -- 30 seconds
        , pcDiscoveryInterval = 300  -- 5 minutes
        , pcNodeCapabilities = [CanRoute, CanStore, CanCreateTimelines]  -- Default capabilities
        }
  
  -- Convert to QUIC configuration
  let quicConfig = p2pConfigToQuicConfig p2pConfig
  
  -- Create a local actor for the controller
  now <- embed getCurrentTime
  controllerPubKey <- embed $ getRandomBytes 32
  let controllerActor = Actor
        { actorId = EntityHash $ Hash "controller"
        , actorType = Controller
        }
  
  -- Start the QUIC server for the controller
  embed $ do
    -- Create the certificate directory if it doesn't exist
    createDirectoryIfMissing True "certs"
    
    -- In a real implementation, we would properly manage this thread
    _ <- forkIO $ runM $ runError @AppError $ PT.traceToStdout $ do
      -- Start the QUIC server
      startQuicServer quicConfig controllerActor (PubKey controllerPubKey)
      
      -- Log that the server started
      PT.trace "Started QUIC server for controller"
      
      -- Keep the server running
      embed $ forever $ threadDelay 1000000  -- 1 second
    
    -- Give the server time to start
    threadDelay 100000  -- 100ms
  
  -- Return the updated controller
  return controller

-- | Helper function to get random bytes
getRandomBytes :: (Member (Embed IO) r) => Int -> Sem r ByteString
getRandomBytes n = embed $ BS.pack <$> replicateM n (randomRIO (0, 255))

-- | Helper function for random number generation
randomRIO :: (Integral a, Member (Embed IO) r) => (a, a) -> Sem r a
randomRIO (lo, hi) = embed $ do
  r <- randomIO
  return $ lo + fromIntegral (abs r `mod` fromIntegral (hi - lo + 1))

-- | Random number generator (would be properly imported in real implementation)
randomIO :: IO Int
randomIO = return 0  -- Placeholder, replace with actual implementation

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
                -> ScenarioConfig
                -> Sem r Controller
runWithScenario spec scenarioConfig = do
  -- Create the controller from the specification
  controller <- createController spec
  
  -- Deploy actors based on the scenario requirements
  let requiredActors = scenarioActors scenarioConfig
  
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
determineRequiredActors :: ScenarioConfig -> [ActorSpec]
determineRequiredActors = scenarioActors

-- | Create Time Traveler actor specs for a scenario
createTimeTravelersForScenario :: ScenarioConfig -> [ActorSpec]
createTimeTravelersForScenario scenarioConfig =
  -- Filter actors from the scenario config that have TimeTravelerRole
  filter (\spec -> actorSpecRole spec == TimeTravelerRole) 
         (scenarioActors scenarioConfig)

-- | Create Time Keeper actor specs for a scenario
createTimeKeepersForScenario :: ScenarioConfig -> [ActorSpec]
createTimeKeepersForScenario scenarioConfig =
  -- Filter actors from the scenario config that have TimeKeeperRole
  filter (\spec -> actorSpecRole spec == TimeKeeperRole) 
         (scenarioActors scenarioConfig)

-- | Create Time Bandit actor specs for a scenario
createTimeBanditsForScenario :: ScenarioConfig -> [ActorSpec]
createTimeBanditsForScenario scenarioConfig =
  -- Filter actors from the scenario config that have TimeBanditRole
  filter (\spec -> actorSpecRole spec == TimeBanditRole) 
         (scenarioActors scenarioConfig)

-- | Deploy an actor based on the spec
deployActor :: (Member (Error ControllerError) r)
            => ActorSpec
            -> SimulationMode
            -> Sem r (ActorHandle r)
deployActor spec mode = do
  -- TODO: Implement actual deployment logic
  throw $ ActorDeploymentError "Actor deployment not yet implemented"
  
  -- This part never executes due to the throw above
  -- but is needed for type checking
  undefined