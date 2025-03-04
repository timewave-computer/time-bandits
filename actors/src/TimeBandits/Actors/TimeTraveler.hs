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
{-# LANGUAGE TypeFamilies #-}

{- |
This module implements the Time Traveler actor role, which is responsible for:

- Creating and deploying new programs
- Submitting TransitionMessage objects to advance program state
- Querying program state and timeline information
- Managing resource ownership and transfers

Time Travelers are the primary initiators of actions within the Time Bandits system.
-}
module TimeBandits.TimeTraveler 
  ( -- * Core Types
    TimeTraveler(..)
  , TimeTravelerSpec(..)
  , DeployError(..)
  , SubmissionError(..)
  , QueryError(..)
  , TransferError(..)
  , TransitionResult(..)
  
  -- * Time Traveler Operations
  , createTimeTraveler
  , deployProgram
  , submitTransition
  , queryProgramState
  , queryTimeline
  , transferResource
  ) where

import Control.Monad (when, unless, forM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Serialize (Serialize, encode, decode)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Polysemy (Member, Sem, interpret, makeSem)
import Polysemy.Error (Error, throw, catch)
import Polysemy.Embed (Embed, embed)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Crypto.Hash (hashWith, SHA256(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import Control.Concurrent (threadDelay)

-- Import from TimeBandits modules
import TimeBandits.Core (Hash(..), EntityHash(..))
import TimeBandits.Types (AppError(..), LamportTime(..), TimelineId)
import TimeBandits.Resource (Resource, Address, ResourceId)
import TimeBandits.Program (ProgramId, ProgramDefinition, ProgramState)
import TimeBandits.TransitionMessage (TransitionMessage)
import TimeBandits.ProgramEffect (Effect)
import TimeBandits.Controller (Controller)
import TimeBandits.TimeMap (TimeMap)
import TimeBandits.Actor (ActorCapability(..), ActorError(..))

-- | Time Traveler data type
data TimeTraveler = TimeTraveler
  { travelerId :: Address
  , capabilities :: [ActorCapability]
  , programRegistry :: Map Text Program
  , resourceInventory :: Map ResourceId Resource
  , timeMaps :: Map TimelineId TimeMap
  , favoriteTimelines :: Set TimelineId
  , keepers :: Map TimelineId TimeKeeper
  , bandits :: Map TimelineId Bandit
  , keypair :: KeyPair
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Time Traveler specification for deployment
data TimeTravelerSpec = TimeTravelerSpec
  { travelerSpecId :: Address
  , travelerSpecCapabilities :: [ActorCapability]
  , travelerSpecInitialPrograms :: [ProgramId]
  , travelerSpecInitialResources :: [ResourceId]
  , travelerSpecKeys :: Maybe KeyPair
  , travelerSpecKeepers :: Map TimelineId TimeKeeper
  , travelerSpecBandits :: Map TimelineId Bandit
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Program deployment errors
data DeployError
  = DeployUnauthorized ActorCapability
  | ProgramExists ProgramId
  | InvalidProgramDefinition Text
  | InvalidInitialState Text
  | ControllerError Text
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Transition submission errors
data SubmissionError
  = SubmissionUnauthorized ProgramId
  | InvalidEffect Text
  | InvalidProof Text
  | InvalidResources Text
  | TransactionRejected Text
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Query errors
data QueryError
  = QueryUnauthorized Text
  | ProgramNotFound ProgramId
  | TimelineNotFound TimelineId
  | InvalidQuery Text
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Resource transfer errors
data TransferError
  = TransferUnauthorized ResourceId
  | ResourceNotFound ResourceId
  | RecipientNotFound Address
  | TransferFailed Text
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Result of a successful transition
data TransitionResult = TransitionResult
  { transitionId :: Hash
  , newProgramState :: ProgramState
  , updatedTimeMap :: TimeMap
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Type alias for initial program state
type InitialState = Map Text Text

-- | Type alias for proof data
type Proof = Maybe (Hash, Text)

-- | Create a TimeTraveler from a specification
createTimeTraveler :: 
  (Member (Error AppError) r) => 
  TimeTravelerSpec -> 
  Sem r TimeTraveler
createTimeTraveler spec = do
  -- Check that the traveler has the required capabilities
  let requiredCapabilities = [CanCreateProgram, CanSubmitTransition]
      hasCapabilities = all (`elem` travelerSpecCapabilities spec) requiredCapabilities
  
  when (not hasCapabilities) $
    throw $ ActorError $ ActorMissingCapability "TimeTraveler requires CanCreateProgram and CanSubmitTransition capabilities"
  
  -- Create the TimeTraveler with empty program registry and resource inventory
  let initialRegistry = Map.empty
      initialInventory = case travelerSpecInitialResources spec of
                           Just resources -> resources
                           Nothing -> Map.empty
  
  -- Generate keypair if not provided
  keys <- case travelerSpecKeys spec of
            Just k -> pure k
            Nothing -> generateNewKeypair
  
  pure $ TimeTraveler
    { travelerId = travelerSpecId spec
    , capabilities = travelerSpecCapabilities spec
    , programRegistry = initialRegistry
    , resourceInventory = initialInventory
    , timeMaps = Map.empty  -- Initially no time maps
    , favoriteTimelines = Set.empty  -- Initially no favorite timelines
    , keepers = travelerSpecKeepers spec
    , bandits = travelerSpecBandits spec
    , keypair = keys
    }

-- | Deploy a new program
-- 
-- This function:
-- 1. Verifies the Time Traveler has the required capabilities
-- 2. Creates a new program with the given definition and initial state
-- 3. Registers the program with the controller
-- 4. Adds the program to the Time Traveler's access list
deployProgram :: (Member (Error DeployError) r) 
              => TimeTraveler 
              -> Controller 
              -> ProgramDefinition 
              -> InitialState 
              -> Sem r (TimeTraveler, ProgramId)
deployProgram traveler controller def initialState = do
  -- Check if the actor has the capability to deploy programs
  when (CanDeployProgram `notElem` capabilities traveler) $
    throw $ DeployUnauthorized CanDeployProgram
  
  -- In a real implementation, this would:
  -- 1. Create a program with the given definition and initial state
  -- 2. Register the program with the controller
  -- 3. Add the program to the Time Traveler's access list
  
  -- For now, return a placeholder program ID
  let newProgramId = undefined -- Would be created by the controller
      updatedTraveler = traveler { programAccess = newProgramId : programAccess traveler }
  
  return (updatedTraveler, newProgramId)

-- | Submit a transition message to advance a program
-- 
-- This function:
-- 1. Verifies the Time Traveler has access to the program
-- 2. Creates a transition message with the proper causal parent
-- 3. Signs the message with the Time Traveler's credentials
-- 4. Submits the message to the controller
submitTransition :: 
  (Member (Error TransitionError) r, Member (Embed IO) r) => 
  TimeTraveler -> 
  TransitionSpec -> 
  Sem r TransitionResult
submitTransition traveler spec = do
  -- Check that the traveler has the capability to submit transitions
  when (CanSubmitTransition `notElem` capabilities traveler) $
    throw $ MissingTransitionCapability "CanSubmitTransition"
  
  -- Find the program
  let maybeProg = Map.lookup (transitionSpecProgramName spec) (programRegistry traveler)
  program <- case maybeProg of
               Just p -> pure p
               Nothing -> throw $ ProgramNotFound (transitionSpecProgramName spec)
  
  -- Find the timeline ID and TimeKeeper
  let timelineId = transitionSpecTimelineId spec
      maybeKeeper = Map.lookup timelineId (keepers traveler)
  timekeeper <- case maybeKeeper of
                  Just k -> pure k
                  Nothing -> throw $ TimeKeeperNotFound timelineId
  
  -- Validate the proposed state transition
  validateTransitionAgainstProgram program spec
  
  -- Validate resource availability
  let requiredResources = programResourceRequirements program
  validateResourceOwnership traveler requiredResources
  
  -- Get current time map for this timeline
  currentTimeMap <- queryTimelineState traveler timelineId
  
  -- Create the transition message
  now <- embed getCurrentTime
  let message = createTransitionMessage 
                  program 
                  (transitionSpecNewState spec) 
                  timelineId 
                  (transitionSpecPreviousHash spec) 
                  requiredResources 
                  currentTimeMap 
                  now
                  
  -- Sign the message with the traveler's private key
  signedMessage <- signMessage message (keypair traveler)
  
  -- Submit the message to the TimeKeeper
  validationResult <- submitMessageToKeeper timekeeper signedMessage timelineId
  
  -- If validation successful, wait for application to timeline
  if validationSuccess validationResult
     then do
       -- Wait for application
       embed $ threadDelay 100000  -- 100ms delay to allow for processing
       
       -- Query for updated timeline state to confirm application
       updatedTimeMap <- queryTimelineState traveler timelineId
       
       -- Check if our message hash is now in the timeline
       let messageHash = validationMessageHash validationResult
           applied = messageInTimeMap messageHash updatedTimeMap
       
       if applied
          then pure $ TransitionResult
                 { transitionSuccess = True
                 , transitionTimelineId = timelineId
                 , transitionHash = messageHash
                 , transitionErrors = []
                 }
          else throw $ TransitionApplicationFailed "Message was validated but not applied to timeline"
     else
       -- Return validation errors
       pure $ TransitionResult
         { transitionSuccess = False
         , transitionTimelineId = timelineId
         , transitionHash = validationMessageHash validationResult
         , transitionErrors = map validationErrorToTransitionError (validationErrors validationResult)
         }

-- | Query the state of a program
-- 
-- This function:
-- 1. Verifies the Time Traveler has access to the program
-- 2. Requests the program state from the controller
queryProgramState :: (Member (Error QueryError) r)
                  => TimeTraveler
                  -> Controller
                  -> ProgramId
                  -> Sem r ProgramState
queryProgramState traveler controller pid = do
  -- Check if the actor has access to the program
  when (pid `notElem` programAccess traveler) $
    throw $ QueryUnauthorized $ "No access to program: " <> T.pack (show pid)
  
  -- In a real implementation, this would:
  -- 1. Request the program state from the controller
  
  -- For now, return a placeholder program state
  return undefined -- Would be returned by the controller

-- | Query the state of a timeline
-- 
-- This function:
-- 1. Requests the timeline state from the controller
-- 2. Filters the result based on the Time Traveler's capabilities
queryTimeline :: (Member (Error QueryError) r)
              => TimeTraveler
              -> Controller
              -> TimelineId
              -> Sem r TimeMap
queryTimeline traveler controller tid = do
  -- In a real implementation, this would:
  -- 1. Request the timeline state from the controller
  -- 2. Filter the result based on the Time Traveler's capabilities
  
  -- For now, return the current time map
  return $ timeMap traveler

-- | Transfer ownership of a resource to another actor
-- 
-- This function:
-- 1. Verifies the Time Traveler owns the resource
-- 2. Creates a transfer message
-- 3. Submits the message to the controller
-- 4. Updates the Time Traveler's resource ownership list
transferResource :: (Member (Error TransferError) r)
                 => TimeTraveler
                 -> Controller
                 -> ResourceId
                 -> Address
                 -> Sem r TimeTraveler
transferResource traveler controller rid recipient = do
  -- Check if the actor owns the resource
  when (rid `notElem` ownedResources traveler) $
    throw $ TransferUnauthorized rid
  
  -- In a real implementation, this would:
  -- 1. Create a transfer message
  -- 2. Submit the message to the controller
  
  -- Update the Time Traveler's resource ownership list
  let updatedResources = filter (/= rid) (ownedResources traveler)
      updatedTraveler = traveler { ownedResources = updatedResources }
  
  return updatedTraveler 

-- | Query a timeline state
-- 
-- This function:
-- 1. Finds the TimeKeeper for the requested timeline
-- 2. Requests the current state of the timeline
-- 3. Updates the traveler's cached time map
queryTimelineState :: 
  (Member (Error TimelineQueryError) r) => 
  TimeTraveler -> 
  TimelineId -> 
  Sem r TimeMap
queryTimelineState traveler timelineId = do
  -- Find the TimeKeeper for this timeline
  let maybeKeeper = Map.lookup timelineId (keepers traveler)
  timekeeper <- case maybeKeeper of
                  Just k -> pure k
                  Nothing -> throw $ TimeKeeperNotFound timelineId
  
  -- Request the timeline state
  timeMap <- requestTimelineState timekeeper timelineId (travelerId traveler) "current_state"
  
  -- Update the traveler's cached time map
  let updatedTimeMaps = Map.insert timelineId timeMap (timeMaps traveler)
      updatedTraveler = traveler { timeMaps = updatedTimeMaps }
  
  -- Return the time map
  pure timeMap

-- | Manage the traveler's resource inventory
-- 
-- This function:
-- 1. Updates the traveler's resource inventory
-- 2. Can transfer resources between travelers
-- 3. Ensures resource tracking is accurate
manageResources :: 
  (Member (Error ResourceError) r) => 
  TimeTraveler -> 
  ResourceAction -> 
  Sem r TimeTraveler
manageResources traveler action = case action of
  -- Add a new resource to the inventory
  AddResource resource -> do
    -- Check if resource is already owned
    let resourceId = resourceIdentifier resource
        exists = resourceId `Map.member` resourceInventory traveler
    
    if exists
       then throw $ ResourceAlreadyExists resourceId
       else do
         -- Add the resource to inventory
         let updatedInventory = Map.insert resourceId resource (resourceInventory traveler)
         pure $ traveler { resourceInventory = updatedInventory }
  
  -- Remove a resource from the inventory
  RemoveResource resourceId -> do
    -- Check if resource exists
    let exists = resourceId `Map.member` resourceInventory traveler
    
    if not exists
       then throw $ ResourceNotFound resourceId
       else do
         -- Remove resource from inventory
         let updatedInventory = Map.delete resourceId (resourceInventory traveler)
         pure $ traveler { resourceInventory = updatedInventory }
  
  -- Transfer a resource to another traveler
  TransferResource resourceId destinationAddress -> do
    -- Check if resource exists
    let exists = resourceId `Map.member` resourceInventory traveler
    
    if not exists
       then throw $ ResourceNotFound resourceId
       else do
         -- Get the resource
         let resource = resourceInventory traveler Map.! resourceId
             
             -- Remove resource from this traveler's inventory
             updatedInventory = Map.delete resourceId (resourceInventory traveler)
         
         -- In a real implementation, we would actually send the resource
         -- to the destination address, but for now we just update our inventory
         
         pure $ traveler { resourceInventory = updatedInventory }

-- | Track favorite timelines
-- 
-- This function:
-- 1. Adds or removes timelines from the traveler's favorites
-- 2. Subscribes to timeline updates when favorited
manageFavoriteTimelines :: 
  (Member (Error AppError) r) => 
  TimeTraveler -> 
  TimelineAction -> 
  Sem r TimeTraveler
manageFavoriteTimelines traveler action = case action of
  -- Add a timeline to favorites
  AddFavoriteTimeline timelineId -> do
    -- Check if timeline already exists in favorites
    if timelineId `Set.member` favoriteTimelines traveler
       then pure traveler  -- Already favorited
       else do
         -- Add to favorites
         let updatedFavorites = Set.insert timelineId (favoriteTimelines traveler)
         
         -- In a real implementation, we would subscribe to updates
         -- For now, just update the set of favorites
         
         pure $ traveler { favoriteTimelines = updatedFavorites }
  
  -- Remove a timeline from favorites
  RemoveFavoriteTimeline timelineId -> do
    -- Check if timeline exists in favorites
    if timelineId `Set.notMember` favoriteTimelines traveler
       then pure traveler  -- Not in favorites
       else do
         -- Remove from favorites
         let updatedFavorites = Set.delete timelineId (favoriteTimelines traveler)
         
         -- In a real implementation, we would unsubscribe from updates
         -- For now, just update the set of favorites
         
         pure $ traveler { favoriteTimelines = updatedFavorites }

-- | Helper function: Validate a program specification
validateProgramSpec :: (Member (Error ProgramCreationError) r) => ProgramSpec -> Sem r ()
validateProgramSpec spec = do
  -- Check that the program name is not empty
  when (T.null (programSpecName spec)) $
    throw $ InvalidProgramSpec "Program name cannot be empty"
  
  -- Check that the program code is not empty
  when (BS.null (programSpecCode spec)) $
    throw $ InvalidProgramSpec "Program code cannot be empty"
  
  -- For simplicity, we assume the initial state is valid
  -- In a real implementation, we would validate it against the program schema

-- | Helper function: Validate resource ownership
validateResourceOwnership :: (Member (Error ResourceError) r) => TimeTraveler -> [ResourceIdentifier] -> Sem r ()
validateResourceOwnership traveler requiredResources = do
  let ownedResources = Map.keysSet (resourceInventory traveler)
      missingResources = filter (`Set.notMember` ownedResources) requiredResources
  
  unless (null missingResources) $
    throw $ InsufficientResources missingResources

-- | Helper function: Generate a program ID
generateProgramId :: Address -> Text -> Sem r ProgramId
generateProgramId owner name = 
  pure $ ProgramId $ BS.pack $ show $ hashWith SHA256 $ 
    BSC.pack $ show owner ++ T.unpack name ++ show (hashWith SHA256 BSC.empty)

-- | Helper function: Validate a state transition against program rules
validateTransitionAgainstProgram :: (Member (Error TransitionError) r) => Program -> TransitionSpec -> Sem r ()
validateTransitionAgainstProgram program spec = do
  -- Validate that the current program state matches
  when (programState program /= transitionSpecCurrentState spec) $
    throw $ InvalidCurrentState (programState program) (transitionSpecCurrentState spec)
  
  -- In a real implementation, we would validate the transition according to program rules
  -- For now, we assume all transitions are valid if the current state matches

-- | Helper function: Create a transition message
createTransitionMessage :: 
  Program -> 
  ProgramState -> 
  TimelineId -> 
  Hash -> 
  [ResourceIdentifier] -> 
  TimeMap -> 
  UTCTime -> 
  TransitionMessage
createTransitionMessage program newState timelineId prevHash resources timeMap timestamp =
  TransitionMessage
    { transitionMessageProgramId = programId program
    , transitionMessageProgramName = programName program
    , transitionMessageSender = programOwner program
    , transitionMessageTimestamp = timestamp
    , transitionMessageTimelineId = timelineId
    , transitionMessagePreviousHash = prevHash
    , transitionMessageCurrentState = programState program
    , transitionMessageNewState = newState
    , transitionMessageResources = resources
    , transitionMessageTimeMap = timeMap
    , transitionMessageSignature = Nothing  -- Will be added when signing
    }

-- | Helper function: Sign a message with the traveler's private key
signMessage :: (Member (Error AppError) r) => TransitionMessage -> KeyPair -> Sem r TransitionMessage
signMessage msg keys = do
  -- In a real implementation, we would use the private key to sign the message
  -- For now, generate a placeholder signature
  let messageBytes = encode msg
      signature = BS.pack $ show $ hashWith SHA256 messageBytes
  
  pure $ msg { transitionMessageSignature = Just signature }

-- | Helper function: Submit a message to a TimeKeeper
submitMessageToKeeper :: 
  (Member (Error TransitionError) r) => 
  TimeKeeperHandle -> 
  TransitionMessage -> 
  TimelineId -> 
  Sem r ValidationResult
submitMessageToKeeper keeper msg timelineId = do
  -- In a real implementation, we would use the TimeKeeper's API to submit the message
  -- For now, assume validation is successful
  let messageHash = Hash $ BS.pack $ show $ hashWith SHA256 $ encode msg
  
  pure $ ValidationResult
    { validationSuccess = True
    , validationTimelineId = timelineId
    , validationMessageHash = messageHash
    , validationErrors = []
    }

-- | Helper function: Request timeline state from a TimeKeeper
requestTimelineState :: 
  (Member (Error TimelineQueryError) r) => 
  TimeKeeperHandle -> 
  TimelineId -> 
  Address -> 
  Text -> 
  Sem r TimeMap
requestTimelineState keeper timelineId requester queryType = do
  -- In a real implementation, we would use the TimeKeeper's API to request the timeline state
  -- For now, return a placeholder time map
  pure $ TimeMap
    { timeMapId = timelineId
    , timeMapHead = Hash $ BS.pack "placeholder-hash"
    , timeMapHeight = 0
    , timeMapEntries = Map.empty
    }

-- | Helper function: Check if a message is in a time map
messageInTimeMap :: Hash -> TimeMap -> Bool
messageInTimeMap hash timeMap =
  hash == timeMapHead timeMap || hash `Map.member` timeMapEntries timeMap

-- | Helper function: Convert validation errors to transition errors
validationErrorToTransitionError :: ValidationError -> TransitionError
validationErrorToTransitionError (UnauthorizedSender addr) = 
  UnauthorizedTransition $ "Unauthorized sender: " <> show addr
validationErrorToTransitionError (CausalOrderViolation hash) = 
  InvalidPreviousHash $ "Causal order violation with hash: " <> show hash
validationErrorToTransitionError (InvalidResourceProof msg) = 
  ResourceValidationFailed $ "Invalid resource proof: " <> T.unpack msg
validationErrorToTransitionError (InvalidTimeMap msg) = 
  TimeMapValidationFailed $ "Invalid time map: " <> T.unpack msg
validationErrorToTransitionError (InvalidSignature msg) = 
  SignatureVerificationFailed $ "Invalid signature: " <> T.unpack msg
validationErrorToTransitionError (FailedValidationRule rule msg) = 
  ValidationRuleFailed $ "Rule " <> show rule <> " failed: " <> T.unpack msg

-- | Helper function: Generate a new keypair
generateNewKeypair :: Sem r KeyPair
generateNewKeypair = 
  pure $ KeyPair
    { publicKey = BS.pack "placeholder-public-key"
    , privateKey = BS.pack "placeholder-private-key"
    }

-- | Data type for timeline actions
data TimelineAction
  = AddFavoriteTimeline TimelineId
  | RemoveFavoriteTimeline TimelineId
  
-- | Data type for resource actions
data ResourceAction
  = AddResource Resource
  | RemoveResource ResourceIdentifier
  | TransferResource ResourceIdentifier Address 