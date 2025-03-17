{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : TimeBandits.Core.Concurrency.EffectLog
Description : Per-resource effect logs for Time Bandits concurrency
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module implements per-resource effect logs for the Time Bandits concurrency model.
Each resource maintains an append-only log of effects applied to it, which can be used
for detecting conflicts, replaying effects, and maintaining a consistent view of resource
state across timelines.

@since 0.1.0
-}
module TimeBandits.Core.Concurrency.EffectLog
  ( -- * Effect Log Types
    EffectLog
  , EffectEntry(..)
  , EffectHash
  , EffectLogError(..)
    
  -- * Effect Log Operations
  , createEffectLog
  , appendEffect
  , getEffectHistory
  , getLatestEffect
  , getEffectById
  , getEffectCount
  , getEffectRange
  , findEffectsByType
    
  -- * Content-Addressed Effects
  , computeEffectHash
  , getContentAddressedEffects
  
  -- * Helper Functions
  , isInTimeRange
  , getEffectId
  , hasEffectType
  , getSimplifiedEffectData
  , getHashBytes
  , getEffectData
  , getHashText
  ) where

import Prelude hiding (atomically, readTVarIO, newTVarIO)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Hashable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Typeable
import Data.Time.Clock
import GHC.Generics
import System.IO.Unsafe
import qualified Data.List.NonEmpty as NE

import TimeBandits.Core.Types
import TimeBandits.Core.ContentAddress.Hash
import TimeBandits.Core.Concurrency.Types (ResourceId, EffectId, Effect(..), EffectType(..))

-- | A hash of an effect, used for content-addressed lookups
type EffectHash = ByteString

-- | A range of time for filtering effects
data TimeRange = TimeRange 
  { timeRangeStart :: Maybe UTCTime
  , timeRangeEnd :: Maybe UTCTime
  } deriving (Show, Eq)

-- | An effect log for a specific resource
data EffectLog = EffectLog
  { -- | The resource ID this log is for
    resourceId :: ResourceId
    -- | The append-only log of effects
  , effectsVar :: STM.TVar [EffectEntry]
    -- | Map from effect hash to its entry
  , effectsByHashVar :: STM.TVar (Map EffectHash EffectEntry)
    -- | Map from effect ID to its entry index
  , effectsByIdVar :: STM.TVar (Map Int Int)
    -- | The latest effect hash
  , latestHashVar :: STM.TVar (Maybe EffectHash)
  }

-- | An entry in the effect log
data EffectEntry = EffectEntry
  { -- | The effect
    effect :: Effect
    -- | When the effect was applied
  , appliedAt :: UTCTime
    -- | The effect's unique ID in this log
  , effectId :: Int
    -- | The effect's content hash
  , effectHash :: EffectHash
    -- | Hash of the previous effect (if any)
  , previousHash :: Maybe EffectHash
  } deriving (Show, Eq, Generic)

-- | Errors that can occur during effect log operations
data EffectLogError
  = HashMismatch EffectHash EffectHash
  | EffectNotFound EffectHash
  | InvalidEffectApplication Effect String
  | LogCorruption String
  | InternalLogError SomeException
  deriving (Show, Typeable)

instance Exception EffectLogError

-- | Create a new effect log for a resource
createEffectLog :: ResourceId -> IO EffectLog
createEffectLog resId = do
  effectsVar <- STM.newTVarIO []
  effectsByHashVar <- STM.newTVarIO Map.empty
  effectsByIdVar <- STM.newTVarIO Map.empty
  latestHashVar <- STM.newTVarIO Nothing
  return EffectLog {
    resourceId = resId,
    effectsVar = effectsVar,
    effectsByHashVar = effectsByHashVar,
    effectsByIdVar = effectsByIdVar,
    latestHashVar = latestHashVar
  }

-- | Append a new effect to the log
appendEffect :: EffectLog -> Effect -> IO (Either EffectLogError EffectEntry)
appendEffect log effect = do
  now <- getCurrentTime
  prevHash <- STM.readTVarIO (latestHashVar log)
  
  -- Compute the hash of this effect
  hashResult <- computeEffectHash effect prevHash
  case hashResult of
    Left err -> return $ Left err
    Right hash -> do
      nextId <- STM.atomically $ do
        effects <- STM.readTVar (effectsVar log)
        return $ length effects
      
      let entry = EffectEntry {
            effect = effect,
            appliedAt = now,
            effectId = nextId,
            effectHash = hash,
            previousHash = prevHash
          }
      
      -- Update all state atomically
      STM.atomically $ do
        -- Add to the log
        STM.modifyTVar' (effectsVar log) (++ [entry])
        
        -- Update the hash mapping
        STM.modifyTVar' (effectsByHashVar log) (Map.insert hash entry)
        
        -- Update the ID mapping
        STM.modifyTVar' (effectsByIdVar log) (Map.insert nextId nextId)
        
        -- Update the latest hash
        STM.writeTVar (latestHashVar log) (Just hash)
      
      return $ Right entry

-- | Get the effect history for a resource, optionally filtered by time range
getEffectHistory :: EffectLog -> Maybe TimeRange -> IO [EffectEntry]
getEffectHistory log maybeTimeRange = do
  effects <- STM.readTVarIO (effectsVar log)
  case maybeTimeRange of
    Nothing -> return effects
    Just timeRange -> return $ filter (isInTimeRange timeRange . appliedAt) effects

-- | Get the latest effect from the log
getLatestEffect :: EffectLog -> IO (Maybe EffectEntry)
getLatestEffect log = do
  effects <- STM.readTVarIO (effectsVar log)
  if null effects
    then return Nothing
    else do
      let lastIndex = length effects - 1
      return $ Just $ getAtIndex effects lastIndex
  where
    getAtIndex :: [a] -> Int -> a
    getAtIndex xs idx = 
      case NE.nonEmpty (drop idx xs) of
        Just ne -> NE.head ne
        Nothing -> error "Index out of bounds" -- Should never happen due to previous checks

-- | Get an effect by its ID
getEffectById :: EffectLog -> Int -> IO (Either EffectLogError EffectEntry)
getEffectById log effectId = do
  mIndex <- STM.atomically $ do
    idMap <- STM.readTVar (effectsByIdVar log)
    return $ Map.lookup effectId idMap
  
  case mIndex of
    Nothing -> return $ Left $ EffectNotFound (BS.pack $ map (fromIntegral . fromEnum) $ show effectId)
    Just index -> do
      effects <- STM.readTVarIO (effectsVar log)
      if index < 0 || index >= length effects
        then return $ Left $ LogCorruption $ "Invalid effect index: " ++ show index
        else return $ Right $ getAtIndex effects index
  where
    getAtIndex :: [a] -> Int -> a
    getAtIndex xs idx = 
      case NE.nonEmpty (drop idx xs) of
        Just ne -> NE.head ne
        Nothing -> error "Index out of bounds" -- Should never happen due to previous checks

-- | Get the count of effects in the log
getEffectCount :: EffectLog -> IO Int
getEffectCount log = length <$> STM.readTVarIO (effectsVar log)

-- | Get a range of effects by index
getEffectRange :: EffectLog -> Int -> Int -> IO [EffectEntry]
getEffectRange log start end = do
  effects <- STM.readTVarIO (effectsVar log)
  let len = length effects
      validStart = max 0 (min start len)
      validEnd = max validStart (min end len)
  return $ take (validEnd - validStart) $ drop validStart effects

-- | Find effects by their type
findEffectsByType :: EffectLog -> EffectType -> IO [EffectEntry]
findEffectsByType log effectType = do
  effects <- STM.readTVarIO (effectsVar log)
  return $ filter (hasEffectType effectType . effect) effects

-- | Compute a hash for an effect
computeEffectHash :: Effect -> Maybe EffectHash -> IO (Either EffectLogError EffectHash)
computeEffectHash effect prevHash = do
  let effectData = getSimplifiedEffectData effect
      dataToHash = case prevHash of
        Nothing -> effectData
        Just prev -> BS.append prev effectData
  
  -- In a real implementation, this would use a proper cryptographic hash
  return $ Right $ hash dataToHash
  where
    hash bs = BS.pack $ map (fromIntegral . fromEnum) $ show $ hashWithSalt 0 (BS.unpack bs)

-- | Get effects by their content hashes
getContentAddressedEffects :: EffectLog -> [EffectHash] -> IO (Map EffectHash (Either EffectLogError EffectEntry))
getContentAddressedEffects log hashes = do
  hashMapData <- STM.readTVarIO (effectsByHashVar log)
  return $ Map.fromList $ map (\h -> (h, maybe (Left $ EffectNotFound h) Right $ Map.lookup h hashMapData)) hashes

-- | Check if a time is within a given range
isInTimeRange :: TimeRange -> UTCTime -> Bool
isInTimeRange TimeRange{..} t =
  (maybe True (t >=) timeRangeStart) && (maybe True (t <=) timeRangeEnd)

-- | Get the unique ID for an effect
getEffectId :: EffectEntry -> Int
getEffectId = effectId

-- | Check if an effect has a specific type
hasEffectType :: EffectType -> Effect -> Bool
hasEffectType expectedType effect = 
  case effect of
    Effect _ actualType _ _ -> expectedType == actualType

-- | Get a simplified representation of an effect for hashing
getSimplifiedEffectData :: Effect -> ByteString
getSimplifiedEffectData (Effect resId effectType payload metadata) =
  BS.intercalate "|" 
    [ getResourceIdBytes resId
    , BS.pack $ map (fromIntegral . fromEnum) $ show effectType
    , getEffectPayloadBytes payload
    , BS.pack $ map (fromIntegral . fromEnum) $ show metadata
    ]
  where
    getResourceIdBytes resId = TE.encodeUtf8 $ T.pack $ show resId
    getEffectPayloadBytes p = BS.pack $ map (fromIntegral . fromEnum) $ show p

-- | Get the binary representation of a hash
getHashBytes :: EffectHash -> ByteString
getHashBytes = id

-- | Get an effect's data in a simple format
getEffectData :: Effect -> ByteString
getEffectData = getSimplifiedEffectData

-- | Convert a hash to text for display
getHashText :: EffectHash -> Text
getHashText = TE.decodeUtf8 . B16.encode 