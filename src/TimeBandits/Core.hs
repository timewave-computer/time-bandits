{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TimeBandits.Core (
  -- * Re-exports from Types
  module TimeBandits.Types,

  -- * Hash Functions
  computeSha256,
  computeHash,
  computeAnchorProof,
  computePubKeyHash,
  computeMessageHash,
  computeAuthMessageHash,
  computeLogEntryHash,
  computeStoredItemHash,
  eitherToError,
) where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString ()

-- For instances
import Data.Serialize (Serialize, encode)
import Polysemy
import Polysemy.Error (Error, throw)
import TimeBandits.Types

-- | Computes the SHA-256 hash of a given ByteString.
computeSha256 :: ByteString -> Hash
computeSha256 = Hash . SHA256.hash

-- | Computes a hash for any serialized content, with an optional previous hash.
computeHash :: (Serialize a) => a -> Maybe Hash -> Hash
computeHash content mPrev =
  let prevBytes = maybe mempty (\(Hash h) -> h) mPrev
      contentBytes = encode content
   in computeSha256 (prevBytes <> contentBytes)

-- | Get the content-addressable hash of a public key
computePubKeyHash :: PubKey -> ActorHash
computePubKeyHash (PubKey bytes) = EntityHash $ computeSha256 bytes

-- | Compute a cryptographic receipt anchor proof for provenance tracking.
computeAnchorProof :: [TimelineHash] -> TimelineHash -> Hash
computeAnchorProof prevChain newTimeline =
  computeSha256 $ encode (map unEntityHash prevChain, unEntityHash newTimeline)

-- | Helper function to compute a content-addressed message hash
computeMessageHash :: (Serialize a) => a -> Hash
computeMessageHash content =
  computeSha256 $ encode content

-- | Helper function to compute an authenticated message hash
computeAuthMessageHash :: (Serialize a) => AuthenticatedMessage a -> Hash
computeAuthMessageHash msg =
  computeSha256 $
    encode
      ( amSender msg
      , amDestination msg
      , amPayload msg
      , amSignature msg
      )

-- | Helper function to compute a log entry's hash
computeLogEntryHash :: (Serialize a) => LogEntry a -> Hash
computeLogEntryHash entry =
  computeSha256 $
    encode
      ( leContent entry
      , leMetadata entry
      , lePrevHash entry
      )

-- | Helper function to compute a stored item's hash
computeStoredItemHash :: TransientStoredItem -> Hash
computeStoredItemHash item =
  computeSha256 $
    encode
      ( siKey item
      , siContent item
      , siTimestamp item
      , siSignature item
      )

-- | Convert Either to Error effect
eitherToError :: (Member (Error AppError) r) => Either AppError a -> Sem r a
eitherToError = either throw return
