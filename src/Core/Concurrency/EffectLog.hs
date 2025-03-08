{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Core.Concurrency.EffectLog
Description : Per-resource effect logs for concurrent execution
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module implements per-resource effect logs for the Time Bandits concurrency model,
enabling causally-consistent tracking of applied effects per resource.
-}
module Core.Concurrency.EffectLog
  ( -- * Effect Log
    EffectLog(..)
  , EffectEntry(..)
  , EffectLogError(..)
  
    -- * Effect Log Operations
  , createEffectLog
  , appendEffect
  , getEffectHistory
  , getLatestEffect
  , getEffectById
  
    -- * Log Queries
  , getEffectCount
  , getEffectRange
  , findEffectsByType
  
    -- * Content Addressing
  , computeEffectHash
  , getContentAddressedEffects
  ) where

import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar, modifyMVar_, takeMVar, putMVar)
import Control.Exception (catch, SomeException)
import Control.Monad (when, forM, filterM, foldM)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, TimeRange, getCurrentTime, diffUTCTime)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Crypto.Hash (SHA256, Digest, hash)
import qualified Crypto.Hash as Hash

-- Import from TimeBandits modules
import Core.ResourceId (ResourceId)
import Core.Types (EffectId, Effect, EffectType, Hash(..))

-- | Effect log for a specific resource
data EffectLog = EffectLog
  { resourceId :: ResourceId             -- ^ ID of the resource
  , effects :: MVar [EffectEntry]        -- ^ Append-only log of effects
  , effectsByHash :: MVar (Map Hash Int) -- ^ Effect hash to index mapping
  , effectsById :: MVar (Map EffectId Int) -- ^ Effect ID to index mapping
  , latestHash :: MVar Hash              -- ^ Hash of the latest effect
  }

-- | Entry in the effect log
data EffectEntry = EffectEntry
  { effect :: Effect          -- ^ The effect itself
  , appliedAt :: UTCTime      -- ^ When the effect was applied
  , effectHash :: Hash        -- ^ Content-addressed hash of the effect
  , previousHash :: Hash      -- ^ Hash of the previous effect (linkage)
  , entryIndex :: Int         -- ^ Position in the log
  }

-- | Errors that can occur during effect log operations
data EffectLogError
  = HashMismatch Hash Hash             -- ^ Expected hash doesn't match actual
  | EffectNotFound EffectId            -- ^ Effect ID not found in log
  | InvalidEffectApplication Text       -- ^ Invalid effect application
  | LogCorruption Text                  -- ^ Log is corrupted
  | InternalLogError Text              -- ^ Internal error
  deriving (Show)

-- | Create a new effect log for a resource
createEffectLog :: ResourceId -> IO EffectLog
createEffectLog resId = do
  effectsVar <- newMVar []
  byHashVar <- newMVar Map.empty
  byIdVar <- newMVar Map.empty
  latestHashVar <- newMVar $ Hash "initial"  -- Genesis hash
  
  return EffectLog
    { resourceId = resId
    , effects = effectsVar
    , effectsByHash = byHashVar
    , effectsById = byIdVar
    , latestHash = latestHashVar
    }

-- | Append an effect to the log
appendEffect :: EffectLog -> Effect -> IO (Either EffectLogError EffectEntry)
appendEffect log effect = do
  -- Get the current state of the log
  currentEffects <- readMVar (effects log)
  currentLatestHash <- readMVar (latestHash log)
  
  -- Compute the hash of the new effect
  now <- getCurrentTime
  let previousHash = currentLatestHash
      newEntryIndex = length currentEffects
      effectHashResult = computeEffectHash effect previousHash
  
  case effectHashResult of
    Left err -> return $ Left err
    Right newHash -> do
      -- Create the new effect entry
      let newEntry = EffectEntry
            { effect = effect
            , appliedAt = now
            , effectHash = newHash
            , previousHash = previousHash
            , entryIndex = newEntryIndex
            }
      
      -- Update the log
      modifyMVar_ (effects log) $ \es -> return $ es ++ [newEntry]
      
      -- Update the hash mappings
      modifyMVar_ (effectsByHash log) $ \byHash ->
        return $ Map.insert newHash newEntryIndex byHash
      
      modifyMVar_ (effectsById log) $ \byId ->
        return $ Map.insert (getEffectId effect) newEntryIndex byId
      
      -- Update the latest hash
      modifyMVar_ (latestHash log) $ \_ -> return newHash
      
      return $ Right newEntry

-- | Get the effect history for a time range
getEffectHistory :: EffectLog -> Maybe TimeRange -> IO [EffectEntry]
getEffectHistory log maybeTimeRange = do
  allEffects <- readMVar (effects log)
  
  -- Filter by time range if provided
  case maybeTimeRange of
    Nothing -> return allEffects
    Just timeRange ->
      return $ filter (isInTimeRange timeRange . appliedAt) allEffects

-- | Get the latest effect in the log
getLatestEffect :: EffectLog -> IO (Maybe EffectEntry)
getLatestEffect log = do
  effectsList <- readMVar (effects log)
  return $ if null effectsList
             then Nothing
             else Just $ last effectsList

-- | Get an effect by its ID
getEffectById :: EffectLog -> EffectId -> IO (Either EffectLogError EffectEntry)
getEffectById log effectId = do
  -- Look up the index in the map
  byIdMap <- readMVar (effectsById log)
  case Map.lookup effectId byIdMap of
    Nothing -> return $ Left $ EffectNotFound effectId
    Just idx -> do
      -- Get the effect at that index
      effectsList <- readMVar (effects log)
      if idx < 0 || idx >= length effectsList
        then return $ Left $ LogCorruption $ "Invalid index for effect ID: " <> T.pack (show effectId)
        else return $ Right $ effectsList !! idx

-- | Get the number of effects in the log
getEffectCount :: EffectLog -> IO Int
getEffectCount log = do
  effectsList <- readMVar (effects log)
  return $ length effectsList

-- | Get a range of effects by index
getEffectRange :: EffectLog -> Int -> Int -> IO [EffectEntry]
getEffectRange log start end = do
  effectsList <- readMVar (effects log)
  let listLength = length effectsList
      validStart = max 0 $ min start listLength
      validEnd = max validStart $ min end listLength
  return $ take (validEnd - validStart) $ drop validStart effectsList

-- | Find effects by type
findEffectsByType :: EffectLog -> EffectType -> IO [EffectEntry]
findEffectsByType log effectType = do
  effectsList <- readMVar (effects log)
  return $ filter (hasEffectType effectType . effect) effectsList

-- | Compute the hash of an effect
computeEffectHash :: Effect -> Hash -> Either EffectLogError Hash
computeEffectHash effect previousHash = do
  -- In a real implementation, this would serialize the effect and compute a cryptographic hash
  -- For now, this is a simplified version
  
  -- Get a simplified effect representation
  let effectData = getSimplifiedEffectData effect
      previousHashBytes = getHashBytes previousHash
      combined = BS.append effectData previousHashBytes
      digest = hash combined :: Digest SHA256
      hashBytes = Hash.digestToByteString digest
  
  return $ Hash $ T.pack $ show hashBytes

-- | Get content-addressed effects by hash
getContentAddressedEffects :: EffectLog -> [Hash] -> IO (Map Hash (Either EffectLogError EffectEntry))
getContentAddressedEffects log hashes = do
  byHashMap <- readMVar (effectsByHash log)
  effectsList <- readMVar (effects log)
  
  -- For each hash, try to find the corresponding effect
  return $ Map.fromList $ map (\h -> (h, lookupByHash h byHashMap effectsList)) hashes
  where
    lookupByHash :: Hash -> Map Hash Int -> [EffectEntry] -> Either EffectLogError EffectEntry
    lookupByHash h byHash allEffects =
      case Map.lookup h byHash of
        Nothing -> Left $ InvalidEffectApplication $ "Hash not found: " <> T.pack (show h)
        Just idx ->
          if idx < 0 || idx >= length allEffects
            then Left $ LogCorruption $ "Invalid index for hash: " <> T.pack (show h)
            else Right $ allEffects !! idx

-- Helper functions

-- | Check if a time is within a time range
isInTimeRange :: TimeRange -> UTCTime -> Bool
isInTimeRange (start, end) time = time >= start && time <= end

-- | Get the effect ID from an effect
getEffectId :: Effect -> EffectId
getEffectId = error "Not implemented: getEffectId"

-- | Check if an effect has a specific type
hasEffectType :: EffectType -> Effect -> Bool
hasEffectType = error "Not implemented: hasEffectType"

-- | Get a simplified representation of an effect for hashing
getSimplifiedEffectData :: Effect -> ByteString
getSimplifiedEffectData = error "Not implemented: getSimplifiedEffectData"

-- | Get the bytes of a hash
getHashBytes :: Hash -> ByteString
getHashBytes (Hash h) = BS.pack $ map fromIntegral $ T.unpack h 