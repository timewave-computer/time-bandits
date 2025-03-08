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
This module provides the ZK proof abstraction and related functionality.
It contains mock implementations for ZK proof generation and verification.

In a production version, this would integrate with actual ZK proving systems
like Groth16, PLONK, or other SNARK/STARK systems.
-}
module Proofs.ZKProof
  ( -- * Core Types
    ZKProof(..)
  , ProofType(..)
  , ProofInput(..)
  , ProofClaim(..)
  , ProofError(..)
  
  -- * Proof Generation
  , generateZKProof
  , generateGuardProof
  , generateTimeMapProof
  , generateOwnershipProof
  
  -- * Proof Verification
  , verifyZKProof
  , verifyGuardProof
  , verifyTimeMapProof
  , verifyOwnershipProof
  
  -- * Proof Composition
  , combineProofs
  , splitProof
  
  -- * Utilities
  , proofToBytes
  , bytesToProof
  , serializeProofInputs
  , hashProof
  ) where

import Control.Monad (when)
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
import GHC.Generics (Generic)
import Polysemy (Member, Sem, embed)
import Polysemy.Embed (Embed)
import Polysemy.Error (Error, throw, catch)

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
import Programs.ProgramEffect 
  ( Effect(..)
  , Guard(..)
  , GuardedEffect(..)
  )
import Programs.Types
  ( TimeMap
  )

-- | ZK Proof with data type information
data ZKProof = ZKProof
  { proofData :: ByteString      -- ^ The actual proof data
  , proofType :: ProofType       -- ^ The type of proof
  , proofMetadata :: ByteString  -- ^ Additional metadata about the proof
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Type of proof
data ProofType = 
    ZeroKnowledgeSNARK     -- ^ zk-SNARK proof
  | ZeroKnowledgeSTARK     -- ^ zk-STARK proof
  | MockProof              -- ^ Mock proof for testing/development
  | DigitalSignature       -- ^ Simple digital signature
  | ThresholdProof Int Int -- ^ k-of-n threshold proof
  | MultiSignature         -- ^ Multiple signature proof
  | TimeLockedProof UTCTime -- ^ Time-locked proof
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Inputs for proof generation
data ProofInput
  = GuardInput Guard Effect ProgramState  -- ^ Input for guard condition
  | TimeMapInput TimeMap                  -- ^ Input for time map verification
  | OwnershipInput Resource Address       -- ^ Input for ownership verification
  | CompositeInput [ProofInput]           -- ^ Multiple inputs combined
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Claims that a proof verifies
data ProofClaim
  = GuardSatisfied Guard Effect          -- ^ Guard condition is satisfied
  | TimeMapValid TimeMap                 -- ^ Time map is valid and consistent
  | OwnershipVerified Resource Address   -- ^ Resource is owned by address
  | CompositeClaim [ProofClaim]          -- ^ Multiple claims verified
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Errors in proof operations
data ProofError
  = GenerationFailed Text
  | VerificationFailed Text
  | InvalidInput Text
  | UnsupportedProofType Text
  | SerializationError Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Generate a ZK proof for a given input
-- 
-- This is a mock implementation that simply hashes the input
-- In a real system, this would use a proper ZK proving system
generateZKProof :: 
  (Member (Error ProofError) r, Member (Embed IO) r) => 
  ProofInput -> 
  Sem r ZKProof
generateZKProof input = do
  -- In a real implementation, this would call into a ZK proving system
  -- For now, simulate proof generation with a hash
  
  -- Serialize the input to bytes
  let inputBytes = serializeProofInputs input
      
      -- Create a mock proof by hashing the input
      -- In a real ZK system, this would generate actual cryptographic proofs
      mockProofData = convert $ hashWith SHA256 inputBytes
      
      -- Add a signature to indicate this is a mock
      signedProofData = BS.append mockProofData (BS.pack "MOCK_ZK_PROOF")
      
      -- Metadata contains information about what is being proven
      metadata = case input of
        GuardInput guard effect _ -> 
          BS.append (BS.pack "Guard: ") (encode guard)
        TimeMapInput timeMap -> 
          BS.pack "TimeMap verification"
        OwnershipInput resource addr -> 
          BS.append (BS.pack "Ownership: ") (encode addr)
        CompositeInput _ -> 
          BS.pack "Composite proof"
  
  -- Add small delay to simulate computation time
  embed $ threadDelay 10000  -- 10ms
  
  -- Return the proof
  pure $ ZKProof
    { proofData = signedProofData
    , proofType = MockProof
    , proofMetadata = metadata
    }

-- | Generate a proof for a guard condition
generateGuardProof :: 
  (Member (Error ProofError) r, Member (Embed IO) r) => 
  Guard -> 
  Effect -> 
  ProgramState -> 
  Sem r ZKProof
generateGuardProof guard effect state = do
  -- Create input for guard condition
  let input = GuardInput guard effect state
  
  -- Generate the proof
  generateZKProof input

-- | Generate a proof for time map validity
generateTimeMapProof :: 
  (Member (Error ProofError) r, Member (Embed IO) r) => 
  TimeMap -> 
  Sem r ZKProof
generateTimeMapProof timeMap = do
  -- Create input for time map verification
  let input = TimeMapInput timeMap
  
  -- Generate the proof
  generateZKProof input

-- | Generate a proof of resource ownership
generateOwnershipProof :: 
  (Member (Error ProofError) r, Member (Embed IO) r) => 
  Resource -> 
  Address -> 
  Sem r ZKProof
generateOwnershipProof resource address = do
  -- Create input for ownership verification
  let input = OwnershipInput resource address
  
  -- Generate the proof
  generateZKProof input

-- | Verify a ZK proof against a claimed input
-- 
-- This is a mock implementation that simply checks the hash
-- In a real system, this would use a proper ZK verification system
verifyZKProof :: 
  (Member (Error ProofError) r, Member (Embed IO) r) => 
  ZKProof -> 
  ProofInput -> 
  Sem r Bool
verifyZKProof proof input = do
  -- Check proof type
  case proofType proof of
    MockProof -> do
      -- For mock proofs, we simulate verification by:
      -- 1. Re-generating what the proof should be
      -- 2. Comparing it with the provided proof
      
      -- Serialize the input to bytes (same as in generation)
      let inputBytes = serializeProofInputs input
          
          -- Create the expected proof data
          expectedHash = convert $ hashWith SHA256 inputBytes
          expectedSignature = BS.pack "MOCK_ZK_PROOF"
          expectedProofData = BS.append expectedHash expectedSignature
          
          -- Get the actual proof data
          actualProofData = proofData proof
      
      -- Add small delay to simulate verification time
      embed $ threadDelay 5000  -- 5ms
      
      -- Check if the proof matches what we expect
      -- In a real ZK system, this would use cryptographic verification
      pure $ actualProofData == expectedProofData
    
    -- Other proof types would have their own verification
    _ -> throw $ UnsupportedProofType "Only MockProof is currently supported"

-- | Verify a guard condition proof
verifyGuardProof :: 
  (Member (Error ProofError) r, Member (Embed IO) r) => 
  ZKProof -> 
  Guard -> 
  Effect -> 
  ProgramState -> 
  Sem r Bool
verifyGuardProof proof guard effect state = do
  -- Create input for guard condition
  let input = GuardInput guard effect state
  
  -- Verify the proof
  verifyZKProof proof input

-- | Verify a time map proof
verifyTimeMapProof :: 
  (Member (Error ProofError) r, Member (Embed IO) r) => 
  ZKProof -> 
  TimeMap -> 
  Sem r Bool
verifyTimeMapProof proof timeMap = do
  -- Create input for time map verification
  let input = TimeMapInput timeMap
  
  -- Verify the proof
  verifyZKProof proof input

-- | Verify an ownership proof
verifyOwnershipProof :: 
  (Member (Error ProofError) r, Member (Embed IO) r) => 
  ZKProof -> 
  Resource -> 
  Address -> 
  Sem r Bool
verifyOwnershipProof proof resource address = do
  -- Create input for ownership verification
  let input = OwnershipInput resource address
  
  -- Verify the proof
  verifyZKProof proof input

-- | Combine multiple proofs into a single composite proof
-- 
-- This is a simplified implementation that just concatenates proof data
-- In a real system, this would use proof composition techniques
combineProofs :: 
  (Member (Error ProofError) r) => 
  [ZKProof] -> 
  Sem r ZKProof
combineProofs [] = throw $ InvalidInput "Cannot combine empty list of proofs"
combineProofs proofs = do
  -- Concatenate all proof data
  let combinedData = BS.concat [proofData p | p <- proofs]
      
      -- Create metadata about the component proofs
      combinedMetadata = encode $ map proofType proofs
  
  -- Return the combined proof
  pure $ ZKProof
    { proofData = combinedData
    , proofType = MockProof
    , proofMetadata = combinedMetadata
    }

-- | Split a composite proof into its components
-- 
-- This is only possible if the proof was created with combineProofs
-- and the original structure is known
splitProof :: 
  (Member (Error ProofError) r) => 
  ZKProof -> 
  Int ->  -- ^ Number of component proofs
  Sem r [ZKProof]
splitProof proof n = do
  -- In a real implementation, this would deserialize the proof using the ZK system
  -- For our mock implementation, we'll just split the data
  
  -- Ensure this is a composite proof
  when (proofType proof /= MockProof) $
    throw $ InvalidInput "Only MockProof can be split"
  
  -- Try to decode the metadata to get component types
  let metadataResult = decode (proofMetadata proof) :: Either String [ProofType]
  
  case metadataResult of
    Left err -> throw $ SerializationError $ T.pack err
    Right componentTypes -> do
      -- Check if the number of components matches
      when (length componentTypes /= n) $
        throw $ InvalidInput $ "Expected " <> T.pack (show n) <> 
                               " components but found " <> 
                               T.pack (show $ length componentTypes)
      
      -- For mock implementation, just split the data evenly
      -- In a real system, this would properly deserialize each component
      let dataSize = BS.length (proofData proof) `div` n
          dataParts = splitByteString (proofData proof) dataSize
      
      -- Create proofs for each component
      pure [ZKProof 
              { proofData = dataPart
              , proofType = componentType
              , proofMetadata = BS.pack $ "Component " ++ show i
              } 
           | (i, (dataPart, componentType)) <- zip [1..] $ zip dataParts componentTypes]

-- | Convert a proof to raw bytes for serialization
proofToBytes :: ZKProof -> ByteString
proofToBytes = encode

-- | Convert raw bytes back to a proof
bytesToProof :: ByteString -> Either Text ZKProof
bytesToProof bytes = case decode bytes of
  Left err -> Left $ T.pack err
  Right proof -> Right proof

-- | Serialize proof inputs to bytes
serializeProofInputs :: ProofInput -> ByteString
serializeProofInputs = encode

-- | Hash a proof to create a unique identifier
hashProof :: ZKProof -> Hash
hashProof proof = Hash $ convert $ hashWith SHA256 $ encode proof

-- | Helper function to split a ByteString into chunks
splitByteString :: ByteString -> Int -> [ByteString]
splitByteString bs chunkSize
  | BS.null bs = []
  | otherwise = let (chunk, rest) = BS.splitAt chunkSize bs
                in chunk : splitByteString rest chunkSize

-- | ThreadDelay for IO simulation
threadDelay :: Int -> IO ()
threadDelay n = pure ()  -- Mock implementation, would use Control.Concurrent.threadDelay in real code 