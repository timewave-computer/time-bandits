{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Core.Hashing
Description : Hashing utilities for Time Bandits
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides hashing utilities for Time Bandits, particularly
for implementing consistent and rendezvous hashing algorithms used in
peer discovery and resource location.
-}
module Core.Hashing
  ( -- * Hashing Functions
    computeHash
  , computeNodeScore
  
    -- * Rendezvous Hashing
  , computeRendezvousHash
  , findResponsibleNode
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Word (Word8)
import Data.Bits (xor)
import Crypto.Hash (hash, SHA256, Digest)
import qualified Crypto.Hash as Hash
import qualified Data.ByteString.Base16 as Base16

-- | Compute a SHA-256 hash of a ByteString
computeHash :: ByteString -> ByteString
computeHash input =
  let digest = hash input :: Digest SHA256
      -- Use Show instance to get the hex representation and convert to ByteString
      hashStr = C8.pack $ show digest
      -- Alternatively, we could use Base16 encoding if available
      -- hashBytes = Base16.encode $ BS.pack $ Hash.digestToByteString digest
  in hashStr

-- | Compute a node score for a key using rendezvous hashing
computeNodeScore :: ByteString -> ByteString -> Double
computeNodeScore key nodeId =
  let combined = BS.append key nodeId
      hash = computeHash combined
      -- Convert to a score between 0 and 1
      score = hashToScore hash
  in score

-- | Convert a hash to a score between 0.0 and 1.0
hashToScore :: ByteString -> Double
hashToScore hash =
  let -- Take the first 8 bytes
      bytes = BS.take 8 hash
      -- Convert to a Word64 equivalent
      value = bytesToWord64 bytes
      -- Normalize to [0, 1]
      maxVal = 2 ^ (64 :: Int) - 1
  in fromIntegral value / fromIntegral maxVal

-- | Convert 8 bytes to a Word64 equivalent
bytesToWord64 :: ByteString -> Integer
bytesToWord64 bs =
  let bytes = BS.unpack (BS.take 8 (BS.append bs (BS.replicate 8 0)))
      powers = [0, 8, 16, 24, 32, 40, 48, 56]
      terms = zipWith (\b p -> toInteger b * 2 ^ p) bytes powers
  in sum terms

-- | Compute a rendezvous hash for a key and node set
computeRendezvousHash :: ByteString -> [ByteString] -> Maybe ByteString
computeRendezvousHash _ [] = Nothing
computeRendezvousHash key nodes =
  let scores = map (\node -> (node, computeNodeScore key node)) nodes
      (bestNode, _) = maximumBy (comparing snd) scores
  in Just bestNode

-- | Find the node responsible for a given key
findResponsibleNode :: ByteString -> [ByteString] -> Maybe ByteString
findResponsibleNode = computeRendezvousHash

-- | XOR distance between two ByteStrings 
-- (Used for some DHT-like operations in overlay networks)
xorDistance :: ByteString -> ByteString -> ByteString
xorDistance a b =
  let len = max (BS.length a) (BS.length b)
      paddedA = BS.append a (BS.replicate (len - BS.length a) 0)
      paddedB = BS.append b (BS.replicate (len - BS.length b) 0)
  in BS.pack $ BS.zipWith xor paddedA paddedB 