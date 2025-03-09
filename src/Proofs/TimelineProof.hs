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
This module provides timeline-specific proof functionality.
It connects ZK proofs to specific timelines based on timeline descriptors.

Each timeline may have different proof requirements, this module:
- Adapts generic proof requests to timeline-specific formats
- Handles different proof types across various timeline implementations
- Provides a unified interface for verification across different timelines
-}
module Proofs.TimelineProof 
  ( -- * Core Types
    TimelineProof(..)
  , ProofRequest(..)
  , ProofVerificationResult(..)
  , TimelineProofAdapter(..)
  
  -- * Proof Operations
  , generateTimelineProof
  , verifyTimelineProof
  , adaptProofToTimeline
  , getProofAdapterForTimeline
  
  -- * High-Level Functions
  , createEffectProof
  , createTransitionProof
  , createTimeMapProof
  , verifyEffectProof
  , verifyTransitionProof
  , verifyTimeMapProof
  
  -- * Registry
  , registerProofAdapter
  , lookupProofAdapter
  ) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Data.Serialize (Serialize, encode, decode)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Polysemy (Member, Sem, embed)
import Polysemy.Embed (Embed)
import Polysemy.Error (Error, throw, catch)
import System.IO.Unsafe (unsafePerformIO)
import Relude (newIORef)

-- Import from TimeBandits modules
import Core (Hash(..), EntityHash(..))
import Core.Types
  ( AppError(..)
  , LamportTime(..)
  , TimelineErrorType(..)
  )
import Core.Resource 
  ( Resource
  , Address
  )
import Core.Timeline
  ( Timeline
  , TimelineId
  , TimelineHash
  )
import Core.TimeMap
  ( TimeMap
  )
import Programs.Program 
  ( ProgramId
  , ProgramState
  )
import Programs.ProgramEffect 
  ( Effect(..)
  , Guard(..)
  , GuardedEffect(..)
  )
import Proofs.ZKProof
  ( ZKProof(..)
  , ProofInput(..)
  , ProofType(..)
  , ProofError(..)
  , generateZKProof
  , verifyZKProof
  , combineProofs
  )
import Execution.EffectAdapterGenerator
  ( TimelineDescriptor(..)
  , TimelineAdapterInterface(..)
  )

-- | Timeline-specific proof with metadata
data TimelineProof = TimelineProof
  { timelineProofTimeline :: TimelineId      -- ^ Timeline this proof is for
  , timelineProofData :: ByteString          -- ^ Timeline-specific proof data
  , timelineProofType :: ProofType           -- ^ Type of proof
  , timelineProofZK :: Maybe ZKProof         -- ^ Optional ZK proof component
  , timelineProofMetadata :: ByteString      -- ^ Additional metadata
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Request for generating a proof
data ProofRequest
  = EffectProofRequest TimelineId Effect 
  | GuardProofRequest TimelineId Guard Effect
  | OwnershipProofRequest TimelineId Resource Address
  | TimeMapProofRequest TimelineId TimeMap
  | TransitionProofRequest TimelineId ProgramId Int Effect [Resource]
  | CompositeProofRequest [ProofRequest]
  deriving stock (Eq, Show, Generic)

-- | Result of proof verification
data ProofVerificationResult
  = ProofVerified TimelineId Hash       -- ^ Proof is valid with verification hash
  | ProofRejected TimelineId Text       -- ^ Proof is invalid with reason
  | ProofIndeterminate TimelineId Text  -- ^ Proof validity cannot be determined
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Timeline-specific proof adapter
data TimelineProofAdapter = TimelineProofAdapter
  { adapterTimeline :: TimelineId
  , adapterGenerateProof :: ProofRequest -> IO (Either Text TimelineProof)
  , adapterVerifyProof :: TimelineProof -> ProofRequest -> IO (Either Text Bool)
  , adapterName :: Text
  }

-- | Generate a proof using a specific adapter
generateProofWithAdapter ::
  (Member (Embed IO) r) =>
  TimelineProofAdapter ->
  ProofRequest ->
  Sem r (Either String TimelineProof)
generateProofWithAdapter adapter request = do
  -- Use the adapter's generateProof function, embedding IO into Sem r
  result <- embed $ adapterGenerateProof adapter request
  -- Convert Text to String in the error case
  pure $ case result of
    Right proof -> Right proof
    Left err -> Left (T.unpack err)

-- | Generate a timeline proof for a request
generateTimelineProof ::
  (Member (Error AppError) r, Member (Embed IO) r) =>
  ProofRequest ->
  Sem r TimelineProof
generateTimelineProof request = do
  -- Get the timeline ID from the request
  let timelineId = getTimelineFromRequest request
  
  -- Use the appropriate adapter to generate the proof
  adapter <- getProofAdapterForTimeline timelineId
  result <- generateProofWithAdapter adapter request
  case result of
    Right proof -> pure proof
    Left err -> throw $ TimelineError $ TimelineGenericError $ T.pack $ "Failed to generate proof: " <> err

-- | Verify a timeline-specific proof
verifyTimelineProof :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  TimelineProof -> 
  ProofRequest -> 
  Sem r ProofVerificationResult
verifyTimelineProof proof request = do
  -- Get the timeline ID from the proof
  let timelineId = timelineProofTimeline proof
  
  -- Look up the proof adapter for this timeline
  adapter <- lookupProofAdapterEffect timelineId
  
  -- Verify the proof using the adapter
  result <- embed $ adapterVerifyProof adapter proof request
  
  -- Handle the result
  case result of
    Left err -> pure $ ProofIndeterminate timelineId $ "Verification error: " <> T.pack (show err)
    Right verified -> 
      if verified
        then pure $ ProofVerified timelineId $ Hash $ BS.take 32 $ timelineProofData proof
        else pure $ ProofRejected timelineId "Proof verification failed"

-- | Adapt a generic proof to a timeline-specific format
adaptProofToTimeline :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  ZKProof -> 
  TimelineId -> 
  Sem r TimelineProof
adaptProofToTimeline zkProof timelineId = do
  -- Create a basic timeline proof with the ZK proof embedded
  pure $ TimelineProof
    { timelineProofTimeline = timelineId
    , timelineProofData = proofData zkProof
    , timelineProofType = proofType zkProof
    , timelineProofZK = Just zkProof
    , timelineProofMetadata = proofMetadata zkProof
    }

-- | Get the proof adapter for a specific timeline
getProofAdapterForTimeline ::
  (Member (Error AppError) r, Member (Embed IO) r) =>
  TimelineId ->
  Sem r TimelineProofAdapter
getProofAdapterForTimeline timelineId = do
  -- In a real implementation, we would look up the descriptor
  -- For now, create a mock descriptor
  let name = "Mock Timeline " <> T.pack (show timelineId)
      
      -- Create a simple adapter based on descriptor information
      -- In a real implementation, this would construct timeline-specific logic
      adapter = TimelineProofAdapter
        { adapterTimeline = timelineId
        , adapterGenerateProof = mockGenerateProof timelineId
        , adapterVerifyProof = mockVerifyProof timelineId
        , adapterName = name
        }
  
  pure adapter

-- | Create a proof for an effect
createEffectProof ::
  (Member (Error AppError) r, Member (Embed IO) r) =>
  TimelineId ->
  Effect ->
  Sem r TimelineProof
createEffectProof timelineId effect = do
  -- Create a proof request
  let request = EffectProofRequest timelineId effect

  -- Use the appropriate adapter to generate the proof
  adapter <- getProofAdapterForTimeline timelineId
  result <- generateProofWithAdapter adapter request
  case result of
    Right proof -> pure proof
    Left err -> throw $ TimelineError $ TimelineGenericError $ T.pack $ "Failed to generate proof: " <> err

-- | Create a proof for a guard condition
createGuardProof ::
  (Member (Error AppError) r, Member (Embed IO) r) =>
  TimelineId ->
  Guard ->
  Effect ->
  ProgramState ->
  Sem r TimelineProof
createGuardProof timelineId guard effect state = do
  -- Create a proof request (without the state, as per updated ProofRequest)
  let request = GuardProofRequest timelineId guard effect

  -- Use the appropriate adapter to generate the proof
  adapter <- getProofAdapterForTimeline timelineId
  result <- generateProofWithAdapter adapter request
  case result of
    Right proof -> pure proof
    Left err -> throw $ TimelineError $ TimelineGenericError $ T.pack $ "Failed to generate proof: " <> err

-- | Create a proof for a program transition
createTransitionProof :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  TimelineId -> 
  ProgramId -> 
  Int ->  -- ^ Step index
  Effect -> 
  [Resource] -> 
  Sem r TimelineProof
createTransitionProof timelineId programId step effect resources = do
  -- Create a transition proof request
  let request = TransitionProofRequest timelineId programId step effect resources

  -- Use the appropriate adapter to generate the proof
  adapter <- getProofAdapterForTimeline timelineId
  result <- generateProofWithAdapter adapter request
  case result of
    Right proof -> pure proof
    Left err -> throw $ TimelineError $ TimelineGenericError $ T.pack $ "Failed to generate proof: " <> err

-- | Create a proof for a time map update
createTimeMapProof :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  TimelineId -> 
  TimeMap -> 
  Sem r TimelineProof
createTimeMapProof timelineId timeMap = do
  -- Create a proof request
  let request = TimeMapProofRequest timelineId timeMap
  
  -- Generate the proof
  generateTimelineProof request

-- | Verify a proof for an effect
verifyEffectProof :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  TimelineProof -> 
  Effect -> 
  Guard -> 
  ProgramState -> 
  Sem r ProofVerificationResult
verifyEffectProof proof effect guard state = do
  -- Create a proof request for verification
  let timelineId = timelineProofTimeline proof
      request = GuardProofRequest timelineId guard effect
  
  -- Verify the proof
  verifyTimelineProof proof request

-- | Verify a proof for a program transition
verifyTransitionProof :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  TimelineProof -> 
  ProgramId -> 
  Int ->  -- ^ Step index
  Effect -> 
  [Resource] -> 
  Sem r ProofVerificationResult
verifyTransitionProof proof programId stepIndex effect resources = do
  -- Create a proof request for verification
  let timelineId = timelineProofTimeline proof
      request = TransitionProofRequest timelineId programId stepIndex effect resources
  
  -- Verify the proof
  verifyTimelineProof proof request

-- | Verify a time map proof
verifyTimeMapProof ::
  (Member (Error AppError) r, Member (Embed IO) r) =>
  TimelineProof ->
  TimelineId ->
  TimeMap ->
  ProgramState ->
  Sem r Bool
verifyTimeMapProof proof timelineId timeMap state = do
  -- Create a proof request without using the state parameter
  let request = TimeMapProofRequest timelineId timeMap
  
  -- Use the appropriate adapter to verify the proof
  adapter <- getProofAdapterForTimeline timelineId
  result <- embed $ adapterVerifyProof adapter proof request
  
  -- Return the result
  pure $ either (const False) id result

-- | Global registry of proof adapters
{-# NOINLINE proofAdapterRegistry #-}
proofAdapterRegistry :: IORef (Map TimelineId TimelineProofAdapter)
proofAdapterRegistry = unsafePerformIO (newIORef Map.empty)

-- | Register a proof adapter in the global registry
registerProofAdapter :: TimelineProofAdapter -> IO ()
registerProofAdapter adapter = do
  let timelineId = adapterTimeline adapter
  atomicModifyIORef' proofAdapterRegistry $ \registry ->
    (Map.insert timelineId adapter registry, ())

-- | Look up a proof adapter from the registry
lookupProofAdapter :: TimelineId -> IO (Maybe TimelineProofAdapter)
lookupProofAdapter timelineId = do
  registry <- readIORef proofAdapterRegistry
  pure $ Map.lookup timelineId registry

-- | Effect-based adapter lookup
lookupProofAdapterEffect :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  TimelineId -> 
  Sem r TimelineProofAdapter
lookupProofAdapterEffect timelineId = do
  -- Look up the adapter in the registry
  adapterMaybe <- embed $ lookupProofAdapter timelineId
  case adapterMaybe of
    Just adapter -> pure adapter
    Nothing -> throw $ TimelineError $ TimelineGenericError $ 
      "No proof adapter found for timeline " <> T.pack (show timelineId)

-- | Get the timeline ID from a proof request
getTimelineFromRequest :: ProofRequest -> TimelineId
getTimelineFromRequest = \case
  GuardProofRequest tid _ _ -> tid
  EffectProofRequest tid _ -> tid
  OwnershipProofRequest tid _ _ -> tid
  TimeMapProofRequest tid _ -> tid
  TransitionProofRequest tid _ _ _ _ -> tid
  CompositeProofRequest (req : _) -> getTimelineFromRequest req
  CompositeProofRequest [] -> error "Cannot get timeline ID from empty composite request"

-- | Mock implementation of proof generation for testing
mockGenerateProof :: TimelineId -> ProofRequest -> IO (Either Text TimelineProof)
mockGenerateProof timelineId request = do
  -- Create a mock proof based on the request type
  let (proofData, metadata) = generateProofHash request
      
      -- Create the timeline proof
      proof = TimelineProof
        { timelineProofTimeline = timelineId
        , timelineProofData = proofData
        , timelineProofType = MockProof
        , timelineProofZK = Nothing
        , timelineProofMetadata = BS8.pack metadata
        }
  
  pure $ Right proof

-- | Mock implementation of proof verification for testing
mockVerifyProof :: TimelineId -> TimelineProof -> ProofRequest -> IO (Either Text Bool)
mockVerifyProof expectedTimelineId proof request = do
  -- Check the timeline ID matches
  if timelineProofTimeline proof /= expectedTimelineId
    then pure $ Left "Timeline ID mismatch"
    else pure $ Right True  -- Always verify in mock implementation

-- | Helper function to extract timeline ID from a request
generateProofHash :: ProofRequest -> (ByteString, String)
generateProofHash request = case request of
  GuardProofRequest _ guard effect -> 
    (BS.append (encode guard) (encode effect), "guard-proof")
  EffectProofRequest _ effect -> 
    (encode effect, "effect-proof")
  OwnershipProofRequest _ resource addr -> 
    (BS.append (encode resource) (encode addr), "ownership-proof")
  TimeMapProofRequest _ timeMap -> 
    (BS8.pack (show timeMap), "timemap-proof")
  TransitionProofRequest _ progId step effect resources -> 
    (BS.concat [encode progId, encode step, encode effect, encode resources], "transition-proof")
  CompositeProofRequest reqs -> 
    (BS.concat [one (fromIntegral $ length reqs)], "composite-proof")

-- | Create a mock proof for testing purposes
mockCreateProof :: TimelineId -> ProofRequest -> IO TimelineProof
mockCreateProof timelineId request = do
  -- Create a random proof based on request type
  let (proofData, metadata) = generateProofHash request
  
  -- Return the mock proof
  pure $ TimelineProof
    { timelineProofTimeline = timelineId
    , timelineProofData = proofData
    , timelineProofType = MockProof
    , timelineProofZK = Nothing
    , timelineProofMetadata = BS8.pack metadata  -- Convert String to ByteString
    }

-- | Create a guard proof for a guarded effect
verifyGuardProof ::
  (Member (Error AppError) r, Member (Embed IO) r) =>
  TimelineProof ->
  Guard ->
  Effect ->
  ProgramState ->
  Sem r Bool
verifyGuardProof proof guard effect state = do
  -- Create a proof request without using the state parameter
  let timelineId = timelineProofTimeline proof
      request = GuardProofRequest timelineId guard effect
  
  -- Use the appropriate adapter to verify the proof
  adapter <- getProofAdapterForTimeline timelineId
  result <- embed $ adapterVerifyProof adapter proof request
  
  -- Return the result
  pure $ either (const False) id result 