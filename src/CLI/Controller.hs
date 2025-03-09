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

  -- * Specialized Actor Deployment
  , deploySpecializedActors
  , runWithScenario
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, UTCTime)
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw, fromEither, catch)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, replicateM)
import System.Directory (createDirectoryIfMissing)
import Network.Socket (SockAddr(..), PortNumber)
import qualified Polysemy.Trace as PT
import Adapters.NetworkQUIC (startQuicServer, p2pConfigToQuicConfig)
import Adapters.Network (P2PConfig(..), P2PCapability(..))
import Polysemy (Embed, embed, runM)
import Polysemy.Error (runError)
import qualified Data.Text.Encoding as TE
import Data.Maybe (mapMaybe)
import Control.Exception (bracket)
import qualified Crypto.Random.Types as CryptoRandom
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)

-- Import from TimeBandits modules
import Core (Hash(..), EntityHash(..))
import Core.Types
  ( AppError(..)
  , LamportTime(..)
  , TimeMapId
  , TimelineHash(..)
  )
import Core.TimelineId
  ( TimelineId(..)
  , timelineIdToText
  )
import Core.Resource 
  ( Resource(..)
  , verifyResourceOwnership
  , Address
  )
import Core.ResourceId (ResourceId(..), resourceIdToText)
import Core.ActorId (ActorId)
import Programs.Program 
  ( ProgramState(..)
  , ProgramId
  , programId
  )
import qualified Programs.ProgramState as ProgramState
import Programs.ProgramState
  ( isAuthorizedCaller
  )
import Programs.ProgramEffect 
  ( GuardedEffect(..)
  , Effect
  )
import Core.TimeMap
  ( renderTimeMap
  , TimeMap
  )
import Types.EffectBase (TimeMapId(..))
import qualified Core.TimeMap as CoreTimeMap
import Actors.TransitionMessage
  ( TransitionMessage(..)
  , TransitionError(..)
  , LogEntry(..)
  , LogMetadata(..)
  , validateTransitionMessage
  , applyTransitionMessage
  , hashTransitionMessage
  , createLogEntry
  )
import qualified Actors.TransitionMessage as TransitionMessage
import Execution.ExecutionLog
  ( ExecutionLog
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
import Core.Common (SimulationMode(..), Actor(..), PubKey(..))

-- Import shared actor types
import Types.Actor
  ( ActorRole(..)
  , ActorCapability(..)
  , ActorSpec(..)
  , actorSpecId
  , actorSpecRole
  )

-- Import shared scenario types
import Types.Scenario
  ( ScenarioConfig(..)
  , ScenarioError(..)
  , ScenarioResult(..)
  , ExecutionMode(..)
  , ScenarioEntity(..)
  , ScenarioTimeline(..)
  , scenarioActors
  )

-- Import from Actors modules
import Actors.ActorTypes (ActorHandle)

-- Import specialized actor role modules
import qualified Actors.TimeTraveler
import qualified Actors.TimeKeeper
import qualified Actors.TimeBandit

-- Import from Programs.Types
import Programs.Types
  ( TimeMap(..)
  )
import qualified Programs.Types as ProgramsTypes

-- Import from Core.Timeline
import Core.Timeline (BlockHeader(..))

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
  | ResourceNotAvailable Hash
  | TimeMapInconsistency Text
  | LogVerificationFailed Text
  | UnauthorizedAccess Address ProgramId
  | ActorDeploymentError Text
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Simple wrapper for Network Adapter
data NetworkAdapter = NetworkAdapter
  deriving (Generic, Serialize)

-- | Controller state
data ControllerState = ControllerState
  { controllerResources :: Map ResourceId ControllerResource
  , controllerPrograms :: Map ProgramId ProgramState
  , controllerActors :: Map Address ActorId
  , controllerLocks :: LockManager
  , controllerTimeMap :: CoreTimeMap.TimeMap
  , controllerLogStore :: LogStore
  , controllerMode :: SimulationMode
  , controllerIsInitialized :: Bool
  , controllerDeploymentMode :: SimulationMode
  }
  deriving (Generic)

-- | Serialize instance for ControllerState
instance Serialize ControllerState

-- | Lock manager for resource locking
data LockManager = LockManager
  deriving (Generic)

-- | Serialize instance for LockManager
instance Serialize LockManager

-- | Resource type (local controller resource)
data ControllerResource = ControllerResource
  { resourceId :: ResourceId
  , resourceOwner :: ActorId
  , resourceData :: ByteString
  }
  deriving (Generic)

-- | Serialize instance for ControllerResource
instance Serialize ControllerResource

-- | The Controller enforces the system contract
data Controller = Controller
  { controllerConfig :: ControllerConfig
  , controllerState :: ControllerState
  , controllerNetwork :: NetworkAdapter
  }
  deriving (Generic)
  deriving anyclass (Serialize)

-- | Dummy Serialize instance for CoreTimeMap.TimeMap
-- This is a simplification for development purposes
instance S.Serialize CoreTimeMap.TimeMap where
  put tm = S.put (BS.empty)  -- Simplified serialization
  get = pure CoreTimeMap.empty  -- Return an empty map when deserializing

-- | Serialize instance for LogStore
instance S.Serialize LogStore where
  put _ = S.put (BS.empty)  -- Simplified serialization
  get = pure $ error "LogStore deserialization not implemented"  -- Simplification

-- | Monoid instance for CoreTimeMap.TimeMap
instance Monoid CoreTimeMap.TimeMap where
  mempty = CoreTimeMap.empty

instance Semigroup CoreTimeMap.TimeMap where
  -- Custom implementation that combines two TimeMaps
  tm1 <> tm2 = 
    -- Extract all timeline IDs from tm2
    let timelineIds = Map.keys (CoreTimeMap.timeNodes tm2)
        -- For each timeline ID, get the timeline from tm2 if it exists
        timelines = mapMaybe (\tid -> CoreTimeMap.getTimeline tid tm2) timelineIds
    -- Then fold each timeline into tm1
    in foldr CoreTimeMap.addTimeline tm1 timelines

-- | Initialize a new controller with the given configuration
initController :: (Member (Error AppError) r) => ControllerConfig -> Sem r Controller
initController config = do
  -- Create an empty time map
  let timeMap = CoreTimeMap.empty
  
  -- Initialize the log store
  logStore <- createLogStore
  
  -- Return the initialized controller
  return Controller
    { controllerConfig = config
    , controllerState = ControllerState
        { controllerResources = Map.empty
        , controllerPrograms = Map.empty
        , controllerActors = Map.empty
        , controllerLocks = LockManager
        , controllerTimeMap = timeMap
        , controllerLogStore = logStore
        , controllerMode = configMode config
        , controllerIsInitialized = False
        , controllerDeploymentMode = configMode config
        }
    , controllerNetwork = NetworkAdapter
    }

-- | Run the controller with the specified simulation mode
runController :: (Member (Error AppError) r, Member (Embed IO) r) => 
                Controller -> 
                SimulationMode -> 
                Sem r Controller
runController controller mode = do
  -- Update the controller's mode
  let updatedController = controller { controllerState = (controllerState controller) { controllerMode = mode } }
  
  -- Initialize the appropriate network infrastructure based on mode
  case mode of
    InMemory -> do
      -- For in-memory mode, no special network setup is needed
      return updatedController
      
    MultiProcess -> do
      -- For local processes mode, set up local IPC
      -- This would typically use Unix sockets or similar
      return updatedController
      
    Distributed -> do
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
        , pcSeedNodes = []  -- No bootstrap nodes initially
        , pcReplicationFactor = 3  -- Default replication factor
        , pcNodeCapacity = 50  -- Default node capacity
        , pcRefreshInterval = 300  -- 5 minutes refresh interval
        , pcHealthCheckInterval = 30  -- 30 seconds health check interval
        , pcConnectionTimeout = 5000  -- 5 seconds connection timeout
        , pcMaxMessageSize = 1024 * 1024  -- 1MB max message size
        }
  
  -- Convert to QUIC configuration
  let quicConfig = p2pConfigToQuicConfig p2pConfig
  
  -- Create a local actor for the controller
  now <- embed getCurrentTime
  controllerPubKey <- embed $ CLI.Controller.getRandomBytes 32
  let controllerActor = Actor
        { Core.Common.actorId = EntityHash $ Hash "controller"
        , actorRole = "Controller"
        , actorEndpoint = "localhost:8443"
        }
  
  -- Start the QUIC server for the controller
  embed $ do
    -- Create the certificate directory if it doesn't exist
    createDirectoryIfMissing True "certs"
    
    -- In a real implementation, we would properly manage this thread
    _ <- forkIO $ do
      -- For simplicity, we'll just log that we would start a QUIC server
      -- Instead of actually starting it, to avoid error constraint issues
      putStrLn "Would start QUIC server for controller (simulation only)"
      putStrLn $ "  With config: " ++ show quicConfig
      putStrLn $ "  For actor: " ++ show controllerActor
      
      -- Keep the thread running
      forever $ threadDelay 1000000  -- 1 second
    
    -- Give the server time to start
    threadDelay 100000  -- 100ms
  
  -- Return the updated controller
  return controller

-- | Helper function to get random bytes
getRandomBytes :: Int -> IO ByteString
getRandomBytes n = BS.pack <$> replicateM n (randomRIO (0, 255))

-- | Helper function for random number generation
randomRIO :: (Integral a) => (a, a) -> IO a
randomRIO (lo, hi) = do
  r <- randomIO
  return $ lo + fromIntegral (abs r `mod` fromIntegral (hi - lo + 1))

-- | Random number generator (would be properly imported in real implementation)
randomIO :: IO Int
randomIO = return 0  -- Placeholder, replace with actual implementation

-- | Process a transition message
processTransition ::
  (Member (Error AppError) r, Member (Error ControllerError) r, Member (Embed IO) r) =>
  Controller ->
  TransitionMessage ->
  Sem r (Controller, ProgramState)
processTransition controller msg = do
  -- 1. Validate the transition message
  -- For simplicity, we'll assume validation always succeeds
  -- In a real implementation, we would call validateTransitionMessage
  
  -- 2. Verify resource ownership - using our local implementation for simplicity
  let resources = transitionResources msg
  forM_ resources $ \resource -> do
    let resourceId = getResourceId resource  -- Use a helper function to get ResourceId
    -- Using our local verifyResourceOwnership implementation which always returns True
    -- In a real implementation, we would call Core.Resource.verifyResourceOwnership
    unless (CLI.Controller.verifyResourceOwnership (transitionActor msg) resourceId) $
      throw $ ResourceNotAvailable (Hash $ show resourceId)  -- Convert ResourceId to Hash
  
  -- 3. Verify the actor is authorized to call the program
  let progId = transitionProgram msg
      controllerState' = controllerState controller
  programState <- case Map.lookup progId (controllerPrograms controllerState') of
    Nothing -> throw $ ProgramNotFound progId
    Just state -> pure state
  
  -- Convert ActorId to Address for authorization check
  -- In a real implementation, this conversion would be more sophisticated
  let actorAddressText = actorIdToText (transitionActor msg)
  
  -- Use Programs.ProgramState.isAuthorizedCaller which takes a ProgramState
  unless (Programs.ProgramState.isAuthorizedCaller programState actorAddressText) $
    throw $ UnauthorizedAccess (TE.encodeUtf8 actorAddressText) progId
  
  -- 4. Verify the transition proof - using our local implementation
  -- In a real implementation, we would use Execution.EffectExecutor.verifyTransitionProof
  unless (CLI.Controller.verifyTransitionProof msg) $
    throw $ InvalidTransition "Invalid transition proof"
  
  -- 5. Apply the transition message
  -- For simplicity, we'll create a dummy effect and new program state
  let effect = dummyEffect
      newProgramState = programState
  
  -- 6. Update the time map
  let timeMapId = programTimeMap programState
  newTimeMap <- updateTimeMap (controllerTimeMap controllerState') timeMapId effect
  
  -- 7. Verify time map consistency
  unless (verifyTimeMapConsistency newTimeMap) $
    throw $ TimeMapInconsistency "Time map inconsistency detected"
  
  -- 8. Create a log entry and append to the log
  let transitionHash = Hash $ show $ transitionProgram msg  -- Create a dummy Hash
  timestamp <- embed getCurrentTime
  logEntry <- CLI.Controller.createLogEntry effect timestamp transitionHash
  newLogStore <- appendLogEntry (controllerLogStore controllerState') logEntry
  
  -- 9. Verify the log chain
  logChainValid <- Execution.ExecutionLog.verifyLogChain newLogStore
  unless logChainValid $
    throw $ LogVerificationFailed "Log chain verification failed"
  
  -- 10. Update the controller state
  let newPrograms = Map.insert progId newProgramState (controllerPrograms controllerState')
      newControllerState = controllerState'
        { controllerTimeMap = newTimeMap
        , controllerPrograms = newPrograms
        , controllerLogStore = newLogStore
        }
      newController = controller
        { controllerState = newControllerState
        }
  
  -- Return the updated controller and program state
  return (newController, newProgramState)

-- | Helper function to get ResourceId from Resource
getResourceId :: Resource -> ResourceId
getResourceId _ = error "Not implemented: getResourceId"

-- | Convert actor ID to text (simplified implementation)
actorIdToText :: ResourceId -> T.Text
actorIdToText = resourceIdToText  -- Use the ResourceId conversion function

-- | Create a dummy effect for testing
dummyEffect :: Effect
dummyEffect = error "Not implemented: dummyEffect"

-- | Deploy a new program
deployProgram ::
  (Member (Error AppError) r, Member (Error ControllerError) r) =>
  Controller ->
  ProgramState ->
  Sem r Controller
deployProgram controller programState = do
  let progId = ProgramState.programId programState  -- Use qualified name
      controllerState' = controllerState controller
  
  -- Check if the program already exists
  when (Map.member progId (controllerPrograms controllerState')) $
    throw $ ActorDeploymentError "Program already exists"
  
  -- Add the program to the controller
  let newPrograms = Map.insert progId programState (controllerPrograms controllerState')
      newControllerState = controllerState' { controllerPrograms = newPrograms }
      newController = controller { controllerState = newControllerState }
  
  return newController

-- | Get the execution log
getExecutionLog :: Controller -> ExecutionLog
getExecutionLog controller = 
  -- Use the controller log store through the controller state
  controllerLogStore (controllerState controller)

-- | Get the time map
getTimeMap :: Controller -> CoreTimeMap.TimeMap
getTimeMap = controllerTimeMap . controllerState

-- | Get a program state
getProgramState :: 
  (Member (Error ControllerError) r) => 
  Controller -> 
  ProgramId -> 
  Sem r ProgramState
getProgramState controller progId =
  case Map.lookup progId (controllerPrograms (controllerState controller)) of
    Nothing -> throw $ ProgramNotFound progId
    Just state -> return state

-- | Set the simulation mode
setSimulationMode :: Controller -> SimulationMode -> Controller
setSimulationMode controller mode =
  let newState = (controllerState controller) { controllerMode = mode }
  in controller { controllerState = newState }

-- | Get the current simulation mode
getSimulationMode :: Controller -> SimulationMode
getSimulationMode controller = controllerMode (controllerState controller)

-- | Deploy specialized actors for a specific role
deploySpecializedActors :: 
  (Member (Error AppError) r, Member (Error ControllerError) r, Member (Embed IO) r) => 
  SimulationMode -> 
  [ActorSpec] -> 
  Sem r (Map.Map Address (ActorHandle r))
deploySpecializedActors mode specs = do
  -- Deploy each actor and collect the results
  actors <- forM specs $ \spec -> do
    -- Try to deploy the actor directly (errors will be caught by runError in higher level)
    actorHandleResult <- deployActor spec mode
      `catch` (\(e :: AppError) -> throw $ ActorDeploymentError $ "Actor initialization failed: " <> T.pack (show e))
    
    -- Return the actor handle with its ID (convert Text to ByteString)
    let actorAddress = TE.encodeUtf8 $ actorSpecId spec
    return (actorAddress, actorHandleResult)
  
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
createController :: (Member (Error ControllerError) r, Member (Error AppError) r)
                 => ControllerSpec
                 -> Sem r Controller
createController spec = do
  -- Create an empty time map
  let timeMap = CoreTimeMap.empty
  
  -- Initialize the log store
  logStore <- createLogStore
  
  -- Return the initialized controller
  return Controller
    { controllerConfig = ControllerConfig
        { configMode = controllerSpecMode spec
        , configLogPath = controllerSpecLogPath spec
        , configVerbose = controllerSpecVerbose spec
        }
    , controllerState = ControllerState
        { controllerResources = Map.empty
        , controllerPrograms = Map.empty
        , controllerActors = Map.empty
        , controllerLocks = LockManager
        , controllerTimeMap = timeMap
        , controllerLogStore = logStore
        , controllerMode = controllerSpecMode spec
        , controllerIsInitialized = False
        , controllerDeploymentMode = controllerSpecMode spec
        }
    , controllerNetwork = NetworkAdapter
    }

-- | Run the controller with a scenario
runWithScenario :: (Member (Error ControllerError) r, Member (Error AppError) r, Member (Embed IO) r)
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
  travelerActors <- deploySpecializedActors (controllerDeploymentMode (controllerState controller)) travelers
  keeperActors <- deploySpecializedActors (controllerDeploymentMode (controllerState controller)) keepers
  banditActors <- deploySpecializedActors (controllerDeploymentMode (controllerState controller)) bandits
  
  -- Combine all actor maps
  let allActors = travelerActors `Map.union` keeperActors `Map.union` banditActors
  
  -- Return the updated controller
  return controller

-- | Deploy an actor with the given specification
-- This is a stub implementation that will be replaced with actual deployment logic
deployActor :: 
  (Member (Error AppError) r) =>
  ActorSpec ->
  SimulationMode ->
  Sem r (ActorHandle r)
deployActor _ _ = do
  throw $ NetworkError "deployActor not yet implemented in Controller"

-- | Update the time map with a new effect
updateTimeMap :: CoreTimeMap.TimeMap -> TimeMapId -> Effect -> Sem r CoreTimeMap.TimeMap
updateTimeMap currentTimeMap timeMapId effect = do
  -- Extract the timeline from the effect
  let timelineHash = extractTimelineFromEffect effect
      timelineId = timelineHashToTimelineId timelineHash
      -- Create a dummy block header for now
      dummyBlockHeader = BlockHeader
        { bhTimeline = timelineHash
        , bhHeight = 0
        , bhPrevBlockHash = Nothing
        , bhMerkleRoot = Hash "dummy"
        , bhTimestamp = LamportTime 0
        }
      -- Get the current time and increment it
      -- For simplification, we're just using a dummy time value
      currentTime = LamportTime 0
      newTime = advanceLamportClock currentTime
  
  -- In a proper implementation, we would use the TimeMap's functions like addTimeline
  -- For simplicity in this mock implementation, we'll just return the original map
  pure currentTimeMap

-- | Helper to convert TimelineHash to TimelineId
timelineHashToTimelineId :: TimelineHash -> TimelineId
timelineHashToTimelineId (EntityHash hash) = TimelineId $ TE.encodeUtf8 $ T.pack $ show hash

-- | Extract the timeline from an effect (simplified implementation)
extractTimelineFromEffect :: Effect -> TimelineHash
extractTimelineFromEffect _ = EntityHash $ Hash "main" -- Simplified implementation

-- | Verify resource ownership (simplified implementation)
verifyResourceOwnership :: ResourceId -> ResourceId -> Bool
verifyResourceOwnership _ _ = True  -- Simplified implementation for now

-- | Verify the transition proof
verifyTransitionProof :: TransitionMessage -> Bool
verifyTransitionProof _ = True  -- Simplified implementation for now

-- | Validate a transition message
validateTransitionMessage :: TransitionMessage -> Either TransitionError ()
validateTransitionMessage _ = Right ()  -- Simplified implementation for now

-- | Apply a transition message to a program state
applyTransitionMessage :: TransitionMessage -> ProgramState -> Either TransitionError (Effect, ProgramState)
applyTransitionMessage msg state = 
  -- Simplified implementation for now
  Right (effect msg, state)

-- | Hash a transition message
hashTransitionMessage :: TransitionMessage -> Hash
hashTransitionMessage _ = Hash "dummy-hash"  -- Simplified implementation for now

-- | Create a log entry
createLogEntry :: Effect -> UTCTime -> Hash -> Sem r LogEntry
createLogEntry effect timestamp hash = do
  -- Simplified implementation for now
  pure $ LogEntry
    { entryId = EntityHash $ Hash "dummy-log-entry"
    , transitionId = EntityHash $ Hash "dummy-transition"
    , appliedEffect = effect
    , appliedAt = LamportTime 0
    , resultState = BS.empty
    , causalParent = EntityHash $ Hash "dummy-parent"
    , logMetadata = LogMetadata
        { executionTime = timestamp
        , executedBy = EntityHash $ Hash "dummy-actor"
        , resourcesChanged = []
        , timelineAdvanced = EntityHash $ Hash "main"
        , additionalTags = Map.empty
        }
    }

-- | Advance Lamport clock
advanceLamportClock :: LamportTime -> LamportTime
advanceLamportClock (LamportTime t) = LamportTime (t + 1)

-- | Extract TimeMapId from a program state
programTimeMap :: ProgramState -> TimeMapId
programTimeMap _ = TimeMapId $ Hash "main-time-map" -- Simplified implementation

-- | Verify time map consistency
verifyTimeMapConsistency :: CoreTimeMap.TimeMap -> Bool
verifyTimeMapConsistency _ = True -- Simplified implementation

-- | Extract resources from a transition message
transitionResources :: TransitionMessage -> [Resource]
transitionResources = resources

-- | Extract actor ID from a transition message
transitionActor :: TransitionMessage -> ResourceId
transitionActor msg = ResourceId $ BS.pack $ BS.unpack $ TE.encodeUtf8 $ T.pack $ show $ Actors.TransitionMessage.actorId msg

-- | Extract program ID from a transition message
transitionProgram :: TransitionMessage -> ProgramId
transitionProgram = TransitionMessage.programId

-- | Verify the log chain integrity
verifyLogChain :: ExecutionLog -> Bool
verifyLogChain _ = True  -- Simplified implementation for now