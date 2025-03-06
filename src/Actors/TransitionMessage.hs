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
This module provides the TransitionMessage abstraction and related functionality.
TransitionMessages are the primary mechanism for program advancement, containing
proofs of effect execution and resource state.

TransitionMessages:
- Are cryptographically signed by actors
- Include ZK proofs validating guards
- Are causally linked to parent effects
- Form the basis of the execution log
-}
module TimeBandits.TransitionMessage 
  ( -- * Core Types
    TransitionMessage(..)
  , TransitionMessageId
  , TransitionBatch(..)
  , Proof(..)
  , ProofType(..)
  , TransitionError(..)
  , LogEntry(..)
  , ExecutionLog(..)
  , LogVerificationResult(..)
  , LogMetadata(..)
  , EffectHash
  , StepIndex
  , ActorId
  , ZKProof
  
  -- * TransitionMessage Operations
  , createTransitionMessage
  , validateTransitionMessage
  , applyTransitionMessage
  , hashTransitionMessage
  , signTransitionMessage
  , verifyTransitionSignature
  
  -- * Batch Operations
  , createTransitionBatch
  , validateTransitionBatch
  , applyTransitionBatch
  
  -- * Proof Operations
  , verifyProof
  , generateZKProof
  , combineProofs
  
  -- * Log Operations
  , createLogEntry
  , appendToLog
  , getLogEntry
  , verifyLogEntryChain
  , verifyLogConsistency
  , extractLogChain
  , searchLogEntries
  
  -- * TimeMap Integration
  , isConsistentWithTimeMap
  , updateTimeMapWithTransition
  , synchronizeTimeMaps
  ) where

import Control.Monad (foldM, when, unless)
import Crypto.Hash (hash, SHA256(..), hashWith, Digest)
import Crypto.Hash.Algorithms (SHA256)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize, encode, decode)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.List (sortOn)
import GHC.Generics (Generic)
import Polysemy (Member, Sem, embed)
import Polysemy.Embed (Embed)
import Polysemy.Error (Error, throw, catch, fromEither)

-- Import from TimeBandits modules
import TimeBandits.Core (Hash(..), EntityHash(..))
import Core.Types
  ( AppError(..)
  , LamportTime(..)
  , TimelineHash
  , TimelineErrorType(..)
  )
import TimeBandits.Resource 
  ( Resource
  , Address
  , ResourceId
  , resourceToByteString
  )
import TimeBandits.Program 
  ( ProgramId
  , ProgramState
  , getLastEffect
  , getCurrentStep
  , applyEffect
  )
import TimeBandits.ProgramEffect 
  ( Effect(..)
  , GuardedEffect(..)
  , effectToByteString
  )
import TimeBandits.TimeMap
  ( TimeMap
  , TimeMapId
  , TimeMapEntry(..)
  , getTimelineState
  , updateTimeMap
  , isValidAdvancement
  , verifyTimeMapConsistency
  , advanceLamportClock
  )

-- | Type alias for TransitionMessage ID
type TransitionMessageId = EntityHash TransitionMessage

-- | Unique identifier for an actor
type ActorId = Address

-- | Step index in program execution
type StepIndex = Int

-- | Hash of an effect
type EffectHash = EntityHash Effect

-- | ZK Proof for guard validation
type ZKProof = ByteString

-- | Metadata about the proof
data ProofType = 
    ZeroKnowledge      -- ^ Privacy-preserving proof
  | Digital            -- ^ Standard digital signature
  | Threshold Int Int  -- ^ k-of-n threshold proof
  | MultiSig           -- ^ Multiple signature proof
  | TimeLocked UTCTime -- ^ Time-locked proof
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Proof validates a guard condition
data Proof 
  = ZKProof ZKProof ProofType           -- Zero-knowledge proof
  | SignatureProof ByteString ProofType -- Signature-based proof
  | WitnessProof ByteString ProofType   -- External witness data
  | CompositeProof [Proof] ProofType    -- Composition of multiple proofs
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A transition message for program advancement
data TransitionMessage = TransitionMessage
  { programId :: ProgramId         -- Program being advanced
  , stepIndex :: StepIndex         -- Current execution step
  , parentEffectHash :: EffectHash -- Previous effect hash (causal link)
  , timeMapId :: TimeMapId         -- TimeMap snapshot used for execution
  , effect :: Effect               -- Effect to be applied
  , proof :: Proof                 -- Proof of guard satisfaction
  , resources :: [Resource]        -- Resources involved in the transition
  , actorId :: ActorId             -- Actor submitting the transition
  , signature :: ByteString        -- Actor's signature of the message
  , timestamp :: UTCTime           -- When the message was created
  , metadata :: Map Text ByteString -- Additional metadata
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A batch of transition messages that should be applied atomically
data TransitionBatch = TransitionBatch
  { batchId :: EntityHash TransitionBatch
  , transitions :: [TransitionMessage]
  , batchSignature :: ByteString  -- Optional batch signature
  , totalResources :: [ResourceId] -- All resources affected by this batch
  , batchTimeMapId :: TimeMapId   -- TimeMap snapshot for the entire batch
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Errors that can occur during transition message processing
data TransitionError
  = InvalidSignature ActorId TransitionMessageId
  | InvalidProof StepIndex Text
  | InvalidParentHash EffectHash EffectHash
  | StepIndexMismatch StepIndex StepIndex
  | TimeMapOutdated TimeMapId TimeMapId
  | ResourceMismatch Text
  | ActorUnauthorized ActorId ProgramId
  | ProofVerificationFailed Text
  | InvalidBatch (EntityHash TransitionBatch) Text
  | TimelineConsistencyError TimelineHash
  | CircularDependencyError [TimelineHash]
  | TransitionRejected TransitionMessageId Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Metadata for log entries
data LogMetadata = LogMetadata
  { executionTime :: UTCTime         -- When the effect was executed
  , executedBy :: ActorId            -- Who executed the effect
  , resourcesChanged :: [ResourceId] -- Resources affected
  , timelineAdvanced :: TimelineHash -- Timeline that was advanced
  , additionalTags :: Map Text Text  -- Additional metadata tags
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | An entry in the execution log
data LogEntry = LogEntry
  { entryId :: EntityHash LogEntry    -- Content-addressable ID
  , transitionId :: TransitionMessageId -- Link to original transition message
  , appliedEffect :: Effect           -- The effect that was applied
  , appliedAt :: LamportTime          -- When the effect was applied (logical time)
  , resultState :: ByteString         -- Hash of resulting state
  , causalParent :: EntityHash LogEntry -- Previous log entry (causal link)
  , metadata :: LogMetadata           -- Metadata about this log entry
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | A complete execution log
data ExecutionLog = ExecutionLog
  { logEntries :: Map (EntityHash LogEntry) LogEntry
  , logHead :: EntityHash LogEntry 
  , logTimeline :: TimelineHash
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Result of log verification
data LogVerificationResult
  = Valid
  | BrokenChain (EntityHash LogEntry) (EntityHash LogEntry)
  | MissingEntry (EntityHash LogEntry)
  | InvalidEntry (EntityHash LogEntry) Text
  | TemporalViolation LamportTime LamportTime
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Create a transition message
createTransitionMessage :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  ProgramId ->
  StepIndex ->
  EffectHash ->
  TimeMapId ->
  Effect ->
  Proof ->
  [Resource] ->
  ActorId ->
  Map Text ByteString ->  -- Optional metadata
  Sem r TransitionMessage
createTransitionMessage progId step parentHash tmId eff prf res actor meta = do
  -- Validate basic structure
  if step < 0
    then throw $ AppError "Invalid step index"
    else pure ()
  
  -- Get current time
  now <- embed getCurrentTime
  
  -- Create the message without signature (will be signed separately)
  pure $ TransitionMessage 
    { programId = progId
    , stepIndex = step
    , parentEffectHash = parentHash
    , timeMapId = tmId
    , effect = eff
    , proof = prf
    , resources = res
    , actorId = actor
    , signature = BS.empty -- Will be signed separately
    , timestamp = now
    , metadata = meta
    }

-- | Sign a transition message with an actor's private key
signTransitionMessage ::
  (Member (Error AppError) r) =>
  TransitionMessage ->
  ByteString ->  -- Actor's private key
  Sem r TransitionMessage
signTransitionMessage msg privateKey = do
  -- Hash the message content (excluding signature field)
  let msgHash = hashTransitionMessage msg
  
  -- In a real implementation, this would sign the hash with the private key
  -- For this implementation, we'll simulate signing
  let signature = BS.append (BS.pack "SIGNED:") msgHash
  
  -- Return the message with the signature
  pure $ msg { signature = signature }

-- | Verify the signature of a transition message
verifyTransitionSignature ::
  (Member (Error AppError) r) =>
  TransitionMessage ->
  Sem r Bool
verifyTransitionSignature msg = do
  -- Hash the message content (excluding signature field)
  let msgHash = hashTransitionMessage msg
  
  -- In a real implementation, this would verify the signature against the actor's public key
  -- For this implementation, we'll simulate verification
  let expectedPrefix = BS.pack "SIGNED:"
  let isValidSignature = BS.isPrefixOf expectedPrefix (signature msg) &&
                         BS.drop (BS.length expectedPrefix) (signature msg) == msgHash
  
  pure isValidSignature

-- | Validate a transition message
validateTransitionMessage ::
  (Member (Error AppError) r) =>
  TransitionMessage ->
  ProgramState ->  -- Current program state
  TimeMap ->        -- Current time map
  Sem r Bool
validateTransitionMessage msg progState currentTimeMap = do
  -- 1. Verify signature
  isValidSig <- verifyTransitionSignature msg
  unless isValidSig $
    throw $ TimelineError $ TimelineGenericError $
      "Invalid signature for actor " ++ show (actorId msg)
  
  -- 2. Verify step index matches program counter
  let currentStep = getCurrentStep progState
  unless (stepIndex msg == currentStep) $
    throw $ TimelineError $ TimelineGenericError $
      "Step index mismatch: message=" ++ show (stepIndex msg) ++ 
      ", program=" ++ show currentStep
  
  -- 3. Verify parent hash matches last applied effect
  let lastEffHash = getLastEffect progState
  unless (parentEffectHash msg == lastEffHash) $
    throw $ TimelineError $ TimelineGenericError $
      "Parent effect hash mismatch"
  
  -- 4. Verify time map consistency
  isConsistent <- isConsistentWithTimeMap msg currentTimeMap
  unless isConsistent $
    throw $ TimelineError $ TimelineGenericError $
      "Transition message uses outdated time map"
  
  -- 5. Verify proofs
  proofValid <- verifyProof (proof msg) (effect msg) progState
  unless proofValid $
    throw $ TimelineError $ TimelineGenericError $
      "Proof verification failed for step " ++ show (stepIndex msg)
  
  -- If we got here, all validations passed
  pure True

-- | Apply a transition message to advance program state
applyTransitionMessage ::
  (Member (Error AppError) r, Member (Embed IO) r) =>
  TransitionMessage ->
  ProgramState ->
  TimeMap ->
  Sem r (ProgramState, LogEntry, TimeMap)
applyTransitionMessage msg progState timeMap = do
  -- Validate the transition message
  isValid <- validateTransitionMessage msg progState timeMap
  
  -- Get current time
  now <- embed getCurrentTime
  
  if not isValid
    then throw $ AppError "Cannot apply invalid transition message"
    else do
      -- 1. Apply the effect using EffectExecutor
      (newProgState, effectResult) <- applyEffect (effect msg) progState
      
      -- 2. Update the time map based on the transition
      updatedTimeMap <- updateTimeMapWithTransition timeMap msg
      
      -- 3. Create a log entry
      let logMetadata = LogMetadata
            { executionTime = now
            , executedBy = actorId msg
            , resourcesChanged = map resourceId (resources msg)
            , timelineAdvanced = undefined  -- Need to extract from program or context
            , additionalTags = Map.empty
            }
      
      let logEntry = LogEntry
            { entryId = generateLogEntryId msg effectResult
            , transitionId = EntityHash $ hashWithSHA256 $ encode msg
            , appliedEffect = effect msg
            , appliedAt = undefined  -- Should come from updated time map
            , resultState = encode effectResult
            , causalParent = undefined  -- Need to get from current log head
            , metadata = logMetadata
            }
      
      -- Return updated state, log entry, and updated time map
      pure (newProgState, logEntry, updatedTimeMap)

-- | Create a batch of transitions to be applied atomically
createTransitionBatch ::
  (Member (Error AppError) r) =>
  [TransitionMessage] ->
  TimeMapId ->
  Sem r TransitionBatch
createTransitionBatch transitions tmId = do
  -- Check if the batch is empty
  when (null transitions) $
    throw $ AppError "Cannot create empty transition batch"
  
  -- Extract all resources affected by any transition in the batch
  let allResources = concatMap resources transitions
  let resourceIds = map resourceId allResources
  
  -- Create a unique batch ID
  let batchIdBytes = mconcat [encode tmId, encode $ length transitions, encode $ head transitions]
  let batchId = EntityHash $ hashWithSHA256 batchIdBytes
  
  -- Create the batch (without signature for now)
  pure $ TransitionBatch
    { batchId = batchId
    , transitions = transitions
    , batchSignature = BS.empty
    , totalResources = resourceIds
    , batchTimeMapId = tmId
    }

-- | Validate an entire batch of transitions
validateTransitionBatch ::
  (Member (Error AppError) r) =>
  TransitionBatch ->
  Map ProgramId ProgramState ->  -- Map of program states
  TimeMap ->
  Sem r Bool
validateTransitionBatch batch progStates timeMap = do
  -- Check that the time map ID in the batch matches
  unless (batchTimeMapId batch == EntityHash (Hash "time-map-id")) $
    throw $ TimelineError $ TimelineGenericError $
      "Batch uses incompatible time map"
  
  -- Validate each transition in the batch
  foldM validateTransition True (transitions batch)
  where
    validateTransition :: (Member (Error AppError) r) => Bool -> TransitionMessage -> Sem r Bool
    validateTransition False _ = pure False  -- Short circuit if previous validation failed
    validateTransition True msg = do
      -- Look up the program state for this transition
      let progId = programId msg
      case Map.lookup progId progStates of
        Nothing -> throw $ TimelineError $ TimelineGenericError $
                     "Program state not found for " ++ show progId
        Just state -> validateTransitionMessage msg state timeMap

-- | Apply an entire batch of transitions atomically
applyTransitionBatch ::
  (Member (Error AppError) r, Member (Embed IO) r) =>
  TransitionBatch ->
  Map ProgramId ProgramState ->
  TimeMap ->
  Sem r (Map ProgramId ProgramState, [LogEntry], TimeMap)
applyTransitionBatch batch progStates initialTimeMap = do
  -- First validate the entire batch
  isValid <- validateTransitionBatch batch progStates initialTimeMap
  
  unless isValid $
    throw $ TimelineError $ TimelineGenericError $
      "Cannot apply invalid transition batch"
  
  -- Apply transitions in sequence, updating state as we go
  foldM applyTransition (progStates, [], initialTimeMap) (transitions batch)
  where
    applyTransition :: 
      (Member (Error AppError) r, Member (Embed IO) r) =>
      (Map ProgramId ProgramState, [LogEntry], TimeMap) ->
      TransitionMessage ->
      Sem r (Map ProgramId ProgramState, [LogEntry], TimeMap)
    applyTransition (states, logs, currentTimeMap) msg = do
      -- Get the program state for this transition
      let progId = programId msg
      case Map.lookup progId states of
        Nothing -> throw $ TimelineError $ TimelineGenericError $
                     "Program state not found for " ++ show progId
        Just state -> do
          -- Apply the transition
          (newState, logEntry, updatedTimeMap) <- applyTransitionMessage msg state currentTimeMap
          
          -- Update the state map
          let newStates = Map.insert progId newState states
          
          -- Add the log entry to our list
          let newLogs = logEntry : logs
          
          -- Continue with updated state
          pure (newStates, newLogs, updatedTimeMap)

-- | Verify a proof against an effect and program state
verifyProof ::
  (Member (Error AppError) r) =>
  Proof ->
  Effect ->
  ProgramState ->
  Sem r Bool
verifyProof proof effect progState = case proof of
  ZKProof zkp proofType -> 
    -- For ZK proofs, we would verify against the guard condition
    -- In this implementation, we'll simulate verification
    pure $ not $ BS.null zkp
    
  SignatureProof sig proofType -> 
    -- For signature proofs, we would verify the signature
    pure $ not $ BS.null sig
    
  WitnessProof witness proofType ->
    -- For witness proofs, we would check the witness data
    pure $ not $ BS.null witness
    
  CompositeProof proofs proofType -> do
    -- For composite proofs, verify all subproofs
    results <- mapM (\p -> verifyProof p effect progState) proofs
    pure $ all id results

-- | Generate a ZK proof for an effect guard
generateZKProof ::
  (Member (Error AppError) r) =>
  Effect ->
  ProgramState ->
  Sem r ZKProof
generateZKProof effect state = do
  -- In a real implementation, this would generate an actual ZK proof
  -- For this implementation, we'll create a dummy proof
  let proofData = mconcat [effectToByteString effect, BS.pack "proof-secret"]
  pure $ convert $ hashWith SHA256 proofData

-- | Combine multiple proofs into a composite proof
combineProofs ::
  [Proof] ->
  ProofType ->
  Proof
combineProofs proofs proofType = CompositeProof proofs proofType

-- | Hash a transition message for signing or verification
hashTransitionMessage ::
  TransitionMessage ->
  ByteString
hashTransitionMessage msg =
  -- Create a copy of the message with empty signature
  let msgForHashing = msg { signature = BS.empty }
      -- Serialize the message
      serialized = encode msgForHashing
  in
      -- Hash the serialized data with SHA-256
      convert $ hashWith SHA256 serialized

-- | Create a log entry for an applied effect
createLogEntry ::
  (Member (Error AppError) r, Member (Embed IO) r) =>
  TransitionMessage ->
  Effect ->
  LamportTime ->
  EntityHash LogEntry ->  -- Causal parent
  ByteString ->           -- Result state hash
  Sem r LogEntry
createLogEntry msg eff time parent resultHash = do
  -- Get current time
  now <- embed getCurrentTime
  
  -- Create log metadata
  let metadata = LogMetadata
        { executionTime = now
        , executedBy = actorId msg
        , resourcesChanged = map resourceId (resources msg)
        , timelineAdvanced = undefined  -- Need timeline hash from context
        , additionalTags = Map.empty
        }
  
  -- Generate a content-addressable ID for the log entry
  let entryContents = mconcat 
        [ encode eff
        , encode time
        , encode parent
        , resultHash
        , encode metadata
        ]
  
  let entryId = EntityHash $ hashWithSHA256 entryContents
  
  pure $ LogEntry
    { entryId = entryId
    , transitionId = EntityHash $ hashWithSHA256 $ encode msg
    , appliedEffect = eff
    , appliedAt = time
    , resultState = resultHash
    , causalParent = parent
    , metadata = metadata
    }

-- | Append a log entry to the execution log
appendToLog ::
  (Member (Error AppError) r) =>
  LogEntry ->
  ExecutionLog ->
  Sem r ExecutionLog
appendToLog entry log = do
  -- Verify the entry has the correct causal parent
  unless (causalParent entry == logHead log || Map.member (causalParent entry) (logEntries log)) $
    throw $ TimelineError $ TimelineGenericError $
      "Log entry has invalid causal parent: " ++ show (causalParent entry)
  
  -- Add the entry to the log
  let updatedEntries = Map.insert (entryId entry) entry (logEntries log)
  
  -- Update the log head
  let updatedLog = log 
        { logEntries = updatedEntries
        , logHead = entryId entry
        }
  
  pure updatedLog

-- | Get a log entry by its ID
getLogEntry ::
  (Member (Error AppError) r) =>
  EntityHash LogEntry ->
  ExecutionLog ->
  Sem r LogEntry
getLogEntry entryId log = do
  -- Look up the entry in the log
  case Map.lookup entryId (logEntries log) of
    Just entry -> pure entry
    Nothing -> throw $ TimelineError $ TimelineGenericError $
                 "Log entry not found: " ++ show entryId

-- | Verify the causal chain of log entries
verifyLogEntryChain ::
  (Member (Error AppError) r) =>
  LogEntry ->
  LogEntry ->
  Sem r Bool
verifyLogEntryChain entry parent = do
  -- Check if the parent entry ID matches the causal parent of the entry
  pure $ causalParent entry == entryId parent

-- | Verify the consistency of an entire execution log
verifyLogConsistency ::
  (Member (Error AppError) r) =>
  ExecutionLog ->
  Sem r LogVerificationResult
verifyLogConsistency log = do
  -- Start from the log head and walk backwards, checking the chain
  let head = logHead log
  
  -- Get the entry for the head
  result <- catch 
    (do
      headEntry <- getLogEntry head log
      walkChain headEntry
    )
    (\(err :: AppError) -> pure $ MissingEntry head)
  
  pure result
  where
    walkChain :: (Member (Error AppError) r) => LogEntry -> Sem r LogVerificationResult
    walkChain entry 
      | causalParent entry == EntityHash (Hash "genesis") = 
          -- Reached the genesis entry, chain is valid
          pure Valid
      | otherwise = do
          -- Try to get the parent entry
          parentResult <- catch
            (do
              parent <- getLogEntry (causalParent entry) log
              
              -- Check lamport time ordering
              if appliedAt parent >= appliedAt entry then
                pure $ TemporalViolation (appliedAt parent) (appliedAt entry)
              else do
                -- Check that the entry's hash verifies
                let entryContents = mconcat 
                      [ encode (appliedEffect entry)
                      , encode (appliedAt entry)
                      , encode (causalParent entry)
                      , resultState entry
                      , encode (metadata entry)
                      ]
                let expectedHash = EntityHash $ hashWithSHA256 entryContents
                
                if entryId entry /= expectedHash then
                  pure $ InvalidEntry (entryId entry) "Hash verification failed"
                else
                  -- Continue walking the chain
                  walkChain parent
            )
            (\(err :: AppError) -> pure $ MissingEntry (causalParent entry))
            
          pure parentResult
    
-- | Extract a chain of log entries starting from a specific entry ID
extractLogChain ::
  (Member (Error AppError) r) =>
  EntityHash LogEntry ->
  ExecutionLog ->
  Sem r [LogEntry]
extractLogChain startId log = do
  -- Get the starting entry
  startEntry <- getLogEntry startId log
  
  -- Build the chain
  buildChain [startEntry] (causalParent startEntry)
  where
    buildChain :: (Member (Error AppError) r) => [LogEntry] -> EntityHash LogEntry -> Sem r [LogEntry]
    buildChain chain parent
      | parent == EntityHash (Hash "genesis") = 
          -- Reached the genesis entry, chain is complete
          pure chain
      | otherwise = catch
          (do
            parentEntry <- getLogEntry parent log
            -- Continue building the chain
            buildChain (parentEntry : chain) (causalParent parentEntry)
          )
          (\(err :: AppError) -> pure chain)  -- Stop if we hit a missing entry

-- | Search for log entries matching specific criteria
searchLogEntries ::
  (Member (Error AppError) r) =>
  ExecutionLog ->
  (LogEntry -> Bool) ->  -- Predicate function
  Sem r [LogEntry]
searchLogEntries log predicate = do
  -- Filter the log entries based on the predicate
  let matches = filter predicate (Map.elems (logEntries log))
  
  -- Sort by Lamport time for consistency
  pure $ sortOn appliedAt matches

-- | Check if a transition message is consistent with a time map
isConsistentWithTimeMap ::
  (Member (Error AppError) r) =>
  TransitionMessage ->
  TimeMap ->
  Sem r Bool
isConsistentWithTimeMap msg timeMap = do
  -- In a real implementation, this would:
  -- 1. Check if the time map ID matches
  -- 2. Check if the time map is still valid for this transition
  -- 3. Verify timeline consistency
  
  -- For now, just check the ID
  pure $ timeMapId msg == EntityHash (Hash "time-map-id")  -- Placeholder

-- | Update a time map based on a transition message
updateTimeMapWithTransition ::
  (Member (Error AppError) r, Member (Embed IO) r) =>
  TimeMap ->
  TransitionMessage ->
  Sem r TimeMap
updateTimeMapWithTransition timeMap msg = do
  -- In a real implementation, this would:
  -- 1. Extract the timeline(s) affected by the transition
  -- 2. Update the Lamport clock for those timelines
  -- 3. Return the updated time map
  
  -- For now, just create an updated time map
  advanceLamportClock timeMap undefined  -- Need timeline hash from context

-- | Synchronize two time maps, taking the latest from each
synchronizeTimeMaps ::
  (Member (Error AppError) r, Member (Embed IO) r) =>
  TimeMap ->
  TimeMap ->
  Sem r TimeMap
synchronizeTimeMaps timeMap1 timeMap2 = do
  -- In a real implementation, this would:
  -- 1. Merge the time maps, taking the latest information from each
  -- 2. Ensure causal consistency
  -- 3. Return the synchronized map
  
  -- For now, just return the first time map
  pure timeMap1

-- | Generate log entry ID from transition message and result
generateLogEntryId ::
  TransitionMessage ->
  ByteString ->  -- Effect result
  EntityHash LogEntry
generateLogEntryId msg result =
  EntityHash $ hashWithSHA256 $ mconcat
    [ encode $ programId msg
    , encode $ stepIndex msg
    , encode $ effect msg
    , encode $ actorId msg
    , result
    ]

-- | Helper function to hash a ByteString with SHA-256
hashWithSHA256 :: ByteString -> Hash
hashWithSHA256 = Hash . convert . hashWith SHA256 