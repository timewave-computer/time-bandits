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

import Prelude hiding (readMVar, newMVar)
import Control.Exception (bracket, catch, SomeException)
import Control.Monad (when, forM, filterM, foldM)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Crypto.Hash (SHA256, Digest, hash)
import qualified Crypto.Hash as Hash
import qualified Control.Concurrent as Concurrent
import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar, modifyMVar_, takeMVar, putMVar)
import Data.List ((!!))  -- Import the (!!) operator explicitly

-- Import from TimeBandits modules
import Core.ResourceId (ResourceId)
import Core.Types (EffectId, Effect)
import qualified Core.Types as CT
import Types.EffectTypes (EffectType)

-- | Our internal version of Hash to avoid conflicts
data EffectHash = EffectHash { effectHashText :: Text }
  deriving (Eq, Show)

-- | Make EffectHash orderable for use in Maps
instance Ord EffectHash where
  compare (EffectHash t1) (EffectHash t2) = compare t1 t2

-- | Convert between our EffectHash and Core.Types.Hash
toExternalHash :: EffectHash -> CT.Hash
toExternalHash (EffectHash txt) = CT.Hash $ BS.pack $ map (fromIntegral . fromEnum) $ T.unpack txt

fromExternalHash :: CT.Hash -> EffectHash
fromExternalHash (CT.Hash bs) = EffectHash $ T.pack $ map (toEnum . fromIntegral) $ BS.unpack bs

-- | Time range for querying effects
data TimeRange = TimeRange
  { startTime :: UTCTime  -- ^ Start time (inclusive)
  , endTime   :: UTCTime  -- ^ End time (inclusive)
  } deriving (Eq, Show)

-- | Effect log for a specific resource
data EffectLog = EffectLog
  { resourceId :: ResourceId             -- ^ ID of the resource
  , effects :: MVar [EffectEntry]        -- ^ Append-only log of effects
  , effectsByHash :: MVar (Map EffectHash Int) -- ^ Effect hash to index mapping
  , effectsById :: MVar (Map EffectId Int) -- ^ Effect ID to index mapping
  , latestHash :: MVar EffectHash              -- ^ Hash of the latest effect
  }

-- | Entry in the effect log
data EffectEntry = EffectEntry
  { effect :: Effect          -- ^ The effect itself
  , appliedAt :: UTCTime      -- ^ When the effect was applied
  , effectHash :: EffectHash        -- ^ Content-addressed hash of the effect
  , previousHash :: EffectHash      -- ^ Hash of the previous effect (linkage)
  , entryIndex :: Int         -- ^ Position in the log
  }

-- | Errors that can occur during effect log operations
data EffectLogError
  = HashMismatch EffectHash EffectHash             -- ^ Expected hash doesn't match actual
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
  latestHashVar <- newMVar $ EffectHash "initial"  -- Genesis hash
  
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

-- | Get the latest effect from the log
getLatestEffect :: EffectLog -> IO (Maybe EffectEntry)
getLatestEffect log = do
  effectsList <- readMVar (effects log)
  -- Use reverse and head instead of last
  return $ case reverse effectsList of
    [] -> Nothing
    (x:_) -> Just x

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
computeEffectHash :: Effect -> EffectHash -> Either EffectLogError EffectHash
computeEffectHash effect previousHash = do
  -- Get the serialized effect data
  let effectData = getEffectData effect
      -- Convert hash to ByteString
      previousHashBytes = getHashBytes previousHash
      combined = BS.append effectData previousHashBytes
      digest = hash combined :: Digest SHA256
      -- Convert digest to ByteString safely
      hashBytes = BS.pack $ map (fromIntegral . fromEnum) $ show digest
  
  -- Create Hash from the digest converted to text
  return $ EffectHash $ T.pack $ show $ BS.unpack hashBytes

-- | Get content-addressed effects by hash
getContentAddressedEffects :: EffectLog -> [EffectHash] -> IO (Map EffectHash (Either EffectLogError EffectEntry))
getContentAddressedEffects log hashes = do
  byHashMap <- readMVar (effectsByHash log)
  effectsList <- readMVar (effects log)
  
  -- For each hash, try to find the corresponding effect
  return $ Map.fromList $ map (\h -> (h, lookupByHash h byHashMap effectsList)) hashes
  where
    lookupByHash :: EffectHash -> Map EffectHash Int -> [EffectEntry] -> Either EffectLogError EffectEntry
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
isInTimeRange timeRange time = 
  time >= startTime timeRange && time <= endTime timeRange

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
getHashBytes :: EffectHash -> ByteString
getHashBytes (EffectHash h) = 
  -- Convert Text to ByteString
  BS.pack $ map (fromIntegral . fromEnum) $ T.unpack h

-- | Helper function to get the data from an effect
getEffectData :: Effect -> ByteString
getEffectData effect = 
  -- In a real implementation, this would serialize the effect
  -- For now, we'll use a simplified representation
  BS.pack $ map (fromIntegral . fromEnum) $ show effect

-- | Helper function to get the text representation of a hash
getHashText :: EffectHash -> Text
getHashText (EffectHash h) = h  -- h is already Text 