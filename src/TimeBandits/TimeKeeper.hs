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
This module implements the Time Keeper actor role, which is responsible for:

- Maintaining the integrity of timelines
- Validating transition messages from Time Travelers
- Processing messages and applying them to timelines
- Providing timeline state queries
- Ensuring proper timeline behavior across simulation modes

Time Keepers are the gatekeepers of the system, ensuring all timeline
operations follow the rules defined in the system contract.
-}
module TimeBandits.TimeKeeper
  ( -- * Core Types
    TimeKeeper(..)
  , TimeKeeperSpec(..)
  , ValidationError(..)
  , MessageApplyError(..)
  , TimelineQueryError(..)
  , ValidationResult(..)
  , ApplyResult(..)

  -- * Time Keeper Operations
  , createTimeKeeper
  , validateMessage
  , applyToTimeline
  , serveTimelineQuery
  , registerTimeline
  , verifyTimelineState
  ) where

import Control.Monad (when, forM, unless)
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
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

-- Import from TimeBandits modules
import TimeBandits.Core (Hash(..), EntityHash(..))
import TimeBandits.Types (AppError(..), LamportTime(..))
import TimeBandits.Resource (Resource, Address)
import TimeBandits.Program (ProgramId, ProgramState)
import TimeBandits.TransitionMessage (TransitionMessage)
import TimeBandits.Timeline (TimelineId, BlockHeader)
import TimeBandits.TimeMap (TimeMap)
import TimeBandits.Actor (ActorCapability(..), ActorError(..))

-- | TimeKeeper data type
data TimeKeeper = TimeKeeper
  { keeperId :: Address
  , capabilities :: [ActorCapability]
  , managedTimelines :: Map TimelineId TimelineState
  , validationRules :: Map TimelineId [ValidationRule]
  , authorizedTravelers :: Map TimelineId (Set Address)
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | TimelineState represents the current state of a timeline managed by a TimeKeeper
data TimelineState = TimelineState
  { timelineHead :: BlockHeader
  , timelineLamportTime :: LamportTime
  , timelineAccessControl :: AccessControl
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | AccessControl defines who can access and modify a timeline
data AccessControl
  = PublicTimeline  -- ^ Anyone can access and modify
  | RestrictedTimeline (Set Address)  -- ^ Only specified addresses can modify
  | PrivateTimeline Address  -- ^ Only the owner can modify
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | ValidationRule defines rules for validating messages on a timeline
data ValidationRule
  = ResourceOwnershipRule  -- ^ Validate resource ownership
  | CausalOrderRule  -- ^ Enforce causal ordering of messages
  | TimeMapFreshnessRule  -- ^ Ensure time maps are up-to-date
  | ProofVerificationRule  -- ^ Verify cryptographic proofs
  | CustomRule Text  -- ^ Custom validation rule with a description
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | TimeKeeper specification for deployment
data TimeKeeperSpec = TimeKeeperSpec
  { keeperSpecId :: Address
  , keeperSpecCapabilities :: [ActorCapability]
  , keeperSpecInitialTimelines :: Map TimelineId TimelineState
  , keeperSpecInitialRules :: Map TimelineId [ValidationRule]
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Validation error data type
data ValidationError
  = UnauthorizedSender Address
  | InvalidTimeMap Text
  | InvalidResourceProof Text
  | CausalOrderViolation Hash
  | InvalidSignature Text
  | FailedValidationRule ValidationRule Text
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | ValidationResult indicates the result of validating a message
data ValidationResult = ValidationResult
  { validationSuccess :: Bool
  , validationTimelineId :: TimelineId
  , validationMessageHash :: Hash
  , validationErrors :: [ValidationError]
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Error when applying a message to a timeline
data MessageApplyError
  = TimelineNotManaged TimelineId
  | InvalidMessageFormat Text
  | ApplicationError Text
  | TimelineRejectedMessage Text
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Result of applying a message to a timeline
data ApplyResult = ApplyResult
  { applySuccess :: Bool
  , applyTimelineId :: TimelineId
  , applyBlockHeader :: BlockHeader
  , applyNewLamportTime :: LamportTime
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Error when querying a timeline
data TimelineQueryError
  = QueryUnauthorized Address
  | TimelineNotFound TimelineId
  | InvalidQueryParameters Text
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a TimeKeeper from a specification
createTimeKeeper :: 
  (Member (Error AppError) r) => 
  TimeKeeperSpec -> 
  Sem r TimeKeeper
createTimeKeeper spec = do
  -- Check that the keeper has the required capabilities
  let requiredCapabilities = [CanValidateTransition, CanServeTimelineState]
      hasCapabilities = all (`elem` keeperSpecCapabilities spec) requiredCapabilities
  
  when (not hasCapabilities) $
    throw $ ActorError $ ActorMissingCapability "TimeKeeper requires CanValidateTransition and CanServeTimelineState capabilities"
  
  -- Create default rules for timelines if none are provided
  let defaultRules = [ResourceOwnershipRule, CausalOrderRule, TimeMapFreshnessRule]
      initialRules = if Map.null (keeperSpecInitialRules spec)
                     then Map.map (const defaultRules) (keeperSpecInitialTimelines spec)
                     else keeperSpecInitialRules spec
  
  -- Create the TimeKeeper
  pure $ TimeKeeper
    { keeperId = keeperSpecId spec
    , capabilities = keeperSpecCapabilities spec
    , managedTimelines = keeperSpecInitialTimelines spec
    , validationRules = initialRules
    , authorizedTravelers = Map.empty  -- Initially no authorized travelers
    }

-- | Validate a transition message
-- 
-- This function:
-- 1. Checks if the sender is authorized for the timeline
-- 2. Validates the message against timeline-specific rules
-- 3. Verifies resource ownership proofs
-- 4. Ensures causal ordering is preserved
validateMessage :: (Member (Error ValidationError) r, Member (Embed IO) r)
                => TimeKeeper
                -> TransitionMessage
                -> TimelineId
                -> Sem r ValidationResult
validateMessage keeper msg tid = do
  -- Check if timeline is managed by this keeper
  let maybeTimelineState = Map.lookup tid (managedTimelines keeper)
  timelineState <- case maybeTimelineState of
    Just state -> pure state
    Nothing -> throw $ UnauthorizedSender undefined
  
  -- Get rules for this timeline
  let rules = Map.findWithDefault [] tid (validationRules keeper)
  
  -- Apply each validation rule to the message
  validationResults <- forM rules $ \rule -> applyValidationRule rule msg timelineState
  
  -- Check authorization for this timeline
  let sender = extractSenderFromMessage msg
      authorized = isAuthorizedForTimeline keeper tid sender
  
  unless authorized $
    throw $ UnauthorizedSender sender
  
  -- Check for causal ordering
  let previousHash = extractPreviousHashFromMessage msg
      validCausalOrder = validateCausalOrder timelineState previousHash
  
  unless validCausalOrder $
    throw $ CausalOrderViolation previousHash
  
  -- Verify resource ownership
  let resources = extractResourcesFromMessage msg
      validResources = validateResourceOwnership resources sender
  
  unless validResources $
    throw $ InvalidResourceProof "Resource ownership validation failed"
  
  -- Check time map freshness
  let timeMap = extractTimeMapFromMessage msg
      validTimeMap = validateTimeMapFreshness timelineState timeMap
  
  unless validTimeMap $
    throw $ InvalidTimeMap "Time map is not fresh"
  
  -- Verify cryptographic signature
  let validSignature = verifyMessageSignature msg
  
  unless validSignature $
    throw $ InvalidSignature "Message signature verification failed"
  
  -- Compile validation errors
  let errors = collectValidationErrors validationResults
      success = null errors && authorized && validCausalOrder && 
                validResources && validTimeMap && validSignature
      
      -- Create message hash for result
      messageHash = computeMessageHash msg
  
  -- Return the validation result
  pure $ ValidationResult
    { validationSuccess = success
    , validationTimelineId = tid
    , validationMessageHash = messageHash
    , validationErrors = errors
    }

-- | Apply a validated message to a timeline
-- 
-- This function:
-- 1. Adds the message to the timeline
-- 2. Updates the timeline state
-- 3. Returns the new head of the timeline
applyToTimeline :: (Member (Error MessageApplyError) r, Member (Embed IO) r)
                => TimeKeeper
                -> TransitionMessage
                -> TimelineId
                -> ValidationResult
                -> Sem r (TimeKeeper, ApplyResult)
applyToTimeline keeper msg tid validResult = do
  -- Check if validation was successful
  when (not $ validationSuccess validResult) $
    throw $ InvalidMessageFormat "Message failed validation"
  
  -- Check if timeline is managed by this keeper
  let maybeTimelineState = Map.lookup tid (managedTimelines keeper)
  timelineState <- case maybeTimelineState of
    Just state -> pure state
    Nothing -> throw $ TimelineNotManaged tid
  
  -- Get the current time for the new block
  now <- embed getCurrentTime
  
  -- Create a new block for the timeline with this message
  let blockData = createBlockData msg now
      newBlockHeader = createBlockHeader tid blockData (timelineHead timelineState)
  
  -- Update the timeline's Lamport time
  let currentTime = timelineLamportTime timelineState
      -- Ensure new time is strictly greater than current
      newTime = LamportTime $ max (unLamportTime currentTime + 1) 
                                (getMessageLamportTime msg)
      
      -- Update the timeline state
      updatedState = timelineState 
        { timelineHead = newBlockHeader
        , timelineLamportTime = newTime
        }
      
      -- Update the managed timelines map
      updatedTimelines = Map.insert tid updatedState (managedTimelines keeper)
      updatedKeeper = keeper { managedTimelines = updatedTimelines }
  
  -- Create the apply result
  let result = ApplyResult
        { applySuccess = True
        , applyTimelineId = tid
        , applyBlockHeader = newBlockHeader
        , applyNewLamportTime = newTime
        }
  
  pure (updatedKeeper, result)

-- | Serve a query about timeline state
-- 
-- This function:
-- 1. Verifies the requester is authorized to query the timeline
-- 2. Retrieves the requested timeline information
-- 3. Returns the timeline state to the requester
serveTimelineQuery :: (Member (Error TimelineQueryError) r, Member (Embed IO) r)
                   => TimeKeeper
                   -> TimelineId
                   -> Address  -- ^ Requester address
                   -> Text  -- ^ Query type
                   -> Sem r TimeMap
serveTimelineQuery keeper tid requester queryType = do
  -- Check if timeline is managed by this keeper
  let maybeTimelineState = Map.lookup tid (managedTimelines keeper)
  timelineState <- case maybeTimelineState of
    Just state -> pure state
    Nothing -> throw $ TimelineNotFound tid
  
  -- Check authorization based on access control
  case timelineAccessControl timelineState of
    PublicTimeline -> pure ()  -- Everyone can query public timelines
    RestrictedTimeline allowed ->
      when (requester `Set.notMember` allowed) $
        throw $ QueryUnauthorized requester
    PrivateTimeline owner ->
      when (requester /= owner) $
        throw $ QueryUnauthorized requester
  
  -- Process the specific query type
  case queryType of
    "current_state" -> createTimeMapForTimeline tid timelineState
    "full_history" -> do
      -- Check if requester is authorized for full history
      when (not $ isAuthorizedForFullHistory keeper tid requester) $
        throw $ QueryUnauthorized requester
      createTimeMapForTimeline tid timelineState
    "head_only" -> createHeadOnlyTimeMap tid timelineState
    _ -> throw $ InvalidQueryParameters $ "Unknown query type: " <> queryType

-- | Register a new timeline
-- 
-- This function:
-- 1. Creates a new timeline
-- 2. Sets up validation rules
-- 3. Adds it to the set of managed timelines
registerTimeline :: (Member (Error AppError) r, Member (Embed IO) r)
                 => TimeKeeper
                 -> TimelineId
                 -> AccessControl
                 -> [ValidationRule]
                 -> Sem r TimeKeeper
registerTimeline keeper tid accessControl rules = do
  -- Check if keeper has the capability to manage timelines
  when (CanManageTimeline `notElem` capabilities keeper) $
    throw $ ActorError $ ActorMissingCapability "CanManageTimeline"
  
  -- Check if timeline already exists
  when (Map.member tid (managedTimelines keeper)) $
    throw $ TimelineError $ TimelineAlreadyExists tid
  
  -- Create a genesis block for the timeline
  now <- embed getCurrentTime
  let genesisHeader = createGenesisBlockHeader tid now
  
  -- Create initial timeline state
  let state = TimelineState
        { timelineHead = genesisHeader
        , timelineLamportTime = LamportTime 0
        , timelineAccessControl = accessControl
        }
  
  -- Use default rules if none provided
  let actualRules = if null rules
                   then [ResourceOwnershipRule, CausalOrderRule, TimeMapFreshnessRule]
                   else rules
  
  -- Add timeline to managed timelines
  let updatedTimelines = Map.insert tid state (managedTimelines keeper)
      updatedRules = Map.insert tid actualRules (validationRules keeper)
      updatedKeeper = keeper { managedTimelines = updatedTimelines, validationRules = updatedRules }
  
  return updatedKeeper

-- | Verify the current state of a timeline
-- 
-- This function:
-- 1. Checks timeline integrity
-- 2. Verifies the current head is valid
-- 3. Confirms all events in the timeline are properly linked
verifyTimelineState :: (Member (Error AppError) r, Member (Embed IO) r)
                    => TimeKeeper
                    -> TimelineId
                    -> Sem r Bool
verifyTimelineState keeper tid = do
  -- Check if timeline is managed by this keeper
  let maybeTimelineState = Map.lookup tid (managedTimelines keeper)
  case maybeTimelineState of
    Just state -> do
      -- Verify the chain of blocks
      let head = timelineHead state
      verifyBlockchain head
    Nothing -> throw $ TimelineError $ TimelineNotFound tid

-- | Authorize a Time Traveler for a timeline
-- 
-- This function:
-- 1. Adds a traveler to the authorized list for a timeline
-- 2. Updates access controls if necessary
authorizeTimelineAccess :: (Member (Error AppError) r)
                       => TimeKeeper
                       -> TimelineId
                       -> Address  -- ^ Traveler address to authorize
                       -> Sem r TimeKeeper
authorizeTimelineAccess keeper tid traveler = do
  -- Check if timeline exists
  let maybeTimelineState = Map.lookup tid (managedTimelines keeper)
  case maybeTimelineState of
    Just state -> do
      -- Get current authorized travelers for this timeline
      let currentAuthorized = Map.findWithDefault Set.empty tid (authorizedTravelers keeper)
          
          -- Add the traveler to the authorized set
          updatedAuthorized = Set.insert traveler currentAuthorized
          
          -- Update the authorized travelers map
          updatedTravelers = Map.insert tid updatedAuthorized (authorizedTravelers keeper)
          
          -- Update the keeper
          updatedKeeper = keeper { authorizedTravelers = updatedTravelers }
      
      pure updatedKeeper
      
    Nothing -> throw $ TimelineError $ TimelineNotFound tid

-- | Revoke a Time Traveler's authorization for a timeline
-- 
-- This function:
-- 1. Removes a traveler from the authorized list for a timeline
-- 2. Updates access controls if necessary
revokeTimelineAccess :: (Member (Error AppError) r)
                     => TimeKeeper
                     -> TimelineId
                     -> Address  -- ^ Traveler address to revoke
                     -> Sem r TimeKeeper
revokeTimelineAccess keeper tid traveler = do
  -- Check if timeline exists
  let maybeTimelineState = Map.lookup tid (managedTimelines keeper)
  case maybeTimelineState of
    Just state -> do
      -- Get current authorized travelers for this timeline
      let currentAuthorized = Map.findWithDefault Set.empty tid (authorizedTravelers keeper)
          
          -- Remove the traveler from the authorized set
          updatedAuthorized = Set.delete traveler currentAuthorized
          
          -- Update the authorized travelers map
          updatedTravelers = Map.insert tid updatedAuthorized (authorizedTravelers keeper)
          
          -- Update the keeper
          updatedKeeper = keeper { authorizedTravelers = updatedTravelers }
      
      pure updatedKeeper
      
    Nothing -> throw $ TimelineError $ TimelineNotFound tid

-- | Helper function: Apply a validation rule to a message
applyValidationRule :: ValidationRule -> TransitionMessage -> TimelineState -> Sem r (ValidationRule, Maybe Text)
applyValidationRule = undefined -- Would apply the specific rule

-- | Helper function: Extract sender from a message
extractSenderFromMessage :: TransitionMessage -> Address
extractSenderFromMessage = undefined -- Would extract sender from message

-- | Helper function: Check if an address is authorized for a timeline
isAuthorizedForTimeline :: TimeKeeper -> TimelineId -> Address -> Bool
isAuthorizedForTimeline keeper tid addr =
  let authorized = Map.findWithDefault Set.empty tid (authorizedTravelers keeper)
   in addr `Set.member` authorized

-- | Helper function: Extract previous hash from a message
extractPreviousHashFromMessage :: TransitionMessage -> Hash
extractPreviousHashFromMessage = undefined -- Would extract previous hash

-- | Helper function: Validate causal ordering of a message
validateCausalOrder :: TimelineState -> Hash -> Bool
validateCausalOrder = undefined -- Would check if hash matches current head

-- | Helper function: Extract resources from a message
extractResourcesFromMessage :: TransitionMessage -> [Resource]
extractResourcesFromMessage = undefined -- Would extract resources list

-- | Helper function: Validate resource ownership
validateResourceOwnership :: [Resource] -> Address -> Bool
validateResourceOwnership = undefined -- Would verify ownership proofs

-- | Helper function: Extract time map from a message
extractTimeMapFromMessage :: TransitionMessage -> TimeMap
extractTimeMapFromMessage = undefined -- Would extract time map

-- | Helper function: Validate time map freshness
validateTimeMapFreshness :: TimelineState -> TimeMap -> Bool
validateTimeMapFreshness = undefined -- Would check if time map is fresh

-- | Helper function: Verify message signature
verifyMessageSignature :: TransitionMessage -> Bool
verifyMessageSignature = undefined -- Would verify cryptographic signature

-- | Helper function: Collect validation errors from results
collectValidationErrors :: [(ValidationRule, Maybe Text)] -> [ValidationError]
collectValidationErrors results =
  [FailedValidationRule rule msg | (rule, Just msg) <- results]

-- | Helper function: Compute hash of a message
computeMessageHash :: TransitionMessage -> Hash
computeMessageHash msg = 
  Hash $ BS.pack $ show $ hashWith SHA256 $ encode msg

-- | Helper function: Create block data from a message
createBlockData :: TransitionMessage -> UTCTime -> ByteString
createBlockData msg time = encode (msg, time)

-- | Helper function: Create a block header
createBlockHeader :: TimelineId -> ByteString -> BlockHeader -> BlockHeader
createBlockHeader tid blockData prevHeader =
  let prevHash = Just $ Hash $ hashAsBytes $ encode prevHeader
      merkleRoot = Hash $ hashAsBytes blockData
      height = 1 + bhHeight prevHeader
   in BlockHeader
        { bhTimeline = tid
        , bhHeight = height
        , bhPrevBlockHash = prevHash
        , bhMerkleRoot = merkleRoot
        , bhTimestamp = LamportTime height
        }

-- | Helper function: Create a genesis block header
createGenesisBlockHeader :: TimelineId -> UTCTime -> BlockHeader
createGenesisBlockHeader tid time =
  let encodedTime = encode time
      merkleRoot = Hash $ hashAsBytes encodedTime
   in BlockHeader
        { bhTimeline = tid
        , bhHeight = 0
        , bhPrevBlockHash = Nothing
        , bhMerkleRoot = merkleRoot
        , bhTimestamp = LamportTime 0
        }

-- | Helper function: Create a time map for a timeline
createTimeMapForTimeline :: TimelineId -> TimelineState -> Sem r TimeMap
createTimeMapForTimeline = undefined -- Would create a time map with timeline state

-- | Helper function: Create a head-only time map
createHeadOnlyTimeMap :: TimelineId -> TimelineState -> Sem r TimeMap
createHeadOnlyTimeMap = undefined -- Would create a time map with just the head

-- | Helper function: Check if address is authorized for full history
isAuthorizedForFullHistory :: TimeKeeper -> TimelineId -> Address -> Bool
isAuthorizedForFullHistory = undefined -- Would check special permissions

-- | Helper function: Verify the integrity of a blockchain
verifyBlockchain :: BlockHeader -> Sem r Bool
verifyBlockchain = undefined -- Would verify the chain back to genesis

-- | Helper function: Hash ByteString to ByteString
hashAsBytes :: ByteString -> ByteString
hashAsBytes bs = 
  BS.pack $ show $ hashWith SHA256 bs

-- | Helper function: Get Lamport time from a message
getMessageLamportTime :: TransitionMessage -> Int
getMessageLamportTime = undefined -- Would extract Lamport time from message 