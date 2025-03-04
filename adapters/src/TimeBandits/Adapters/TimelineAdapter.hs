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
This module provides the TimelineAdapter system, which creates appropriate adapters
for different timeline types based on TimelineDescriptor configurations.

TimelineAdapters provide a uniform interface for interacting with different timelines 
(blockchains, rollups, etc.) while abstracting away the underlying implementation details.
-}
module TimeBandits.TimelineAdapter
  ( -- * Core Types
    TimelineAdapter(..)
  , AdapterError(..)
  , AdapterConfig(..)
  , AdapterState(..)
  
  -- * Adapter Creation
  , createAdapter
  , createAdapterFromFile
  
  -- * Adapter Operations
  , executeEffect
  , queryState
  , watchTimeline
  , getLatestHead
  , getTimeMapUpdate
  
  -- * Utility Functions
  , encodeParameters
  , decodeResult
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Serialize (Serialize)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Polysemy (Member, Sem, embed)
import Polysemy.Error (Error, throw, catch)
import Polysemy.Embed (Embed)

-- Import from TimeBandits modules
import TimeBandits.Core (Hash, EntityHash(..))
import TimeBandits.Types (AppError(..), ProgramErrorType(..))
import TimeBandits.Timeline (TimelineHash, BlockHeader(..))
import TimeBandits.ProgramEffect (EffectType(..), Effect(..))
import TimeBandits.Resource (Resource(..))
import TimeBandits.TimeMap (TimeMap(..), LamportClock(..))
import TimeBandits.TimelineDescriptor
  ( TimelineDescriptor(..)
  , VMType(..)
  , EffectHandlerSpec(..)
  , loadDescriptor
  , resolveEffectHandler
  )

-- | Errors that can occur during adapter operations
data AdapterError
  = ConnectionError Text
  | DecodingError Text
  | ExecutionError Text
  | TimeoutError
  | RateLimitExceeded
  | AuthenticationError
  | HandlerNotFound EffectType
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Configuration for a timeline adapter
data AdapterConfig = AdapterConfig
  { acDescriptor :: TimelineDescriptor    -- ^ The timeline descriptor
  , acHttpManager :: Maybe Manager        -- ^ Optional HTTP connection manager
  , acMaxRetries :: Int                   -- ^ Maximum retries for operations
  , acRetryDelay :: Int                   -- ^ Delay between retries (milliseconds)
  }
  deriving stock (Show, Generic)

-- | State for a timeline adapter
data AdapterState = AdapterState
  { asLastHead :: Maybe BlockHeader       -- ^ Last observed block header
  , asLastUpdated :: UTCTime              -- ^ Last time the state was updated
  , asRequestCounter :: Int               -- ^ Number of requests made
  , asLamportClock :: LamportClock        -- ^ Logical clock for this timeline
  }
  deriving stock (Show, Generic)

-- | Timeline adapter for interacting with a specific timeline
data TimelineAdapter = TimelineAdapter
  { taConfig :: AdapterConfig              -- ^ Adapter configuration
  , taState :: AdapterState                -- ^ Adapter state
  , taExecuteEffect :: Effect -> IO (Either AdapterError ByteString)  -- ^ Function to execute effects
  , taQueryState :: ByteString -> IO (Either AdapterError ByteString)  -- ^ Function to query state
  , taGetLatestHead :: IO (Either AdapterError BlockHeader)  -- ^ Function to get latest head
  }

-- | Create a timeline adapter from a descriptor
createAdapter :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  TimelineDescriptor -> 
  Maybe Manager -> 
  Sem r TimelineAdapter
createAdapter descriptor maybeManager = do
  currentTime <- embed getCurrentTime
  
  -- Create the initial adapter state
  let initialState = AdapterState
        { asLastHead = Nothing
        , asLastUpdated = currentTime
        , asRequestCounter = 0
        , asLamportClock = LamportClock 0
        }
  
  -- Create the adapter configuration
  let config = AdapterConfig
        { acDescriptor = descriptor
        , acHttpManager = maybeManager
        , acMaxRetries = 3
        , acRetryDelay = 1000
        }
  
  -- Create the appropriate adapter based on VM type
  case tdVmType descriptor of
    EVM -> createEVMAdapter config initialState
    Solana -> createSolanaAdapter config initialState
    CosmWasm -> createCosmWasmAdapter config initialState
    MoveVM -> createMoveAdapter config initialState
    Native -> createNativeAdapter config initialState
    MockVM -> createMockAdapter config initialState

-- | Create a timeline adapter from a descriptor file
createAdapterFromFile :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  FilePath -> 
  Maybe Manager -> 
  Sem r TimelineAdapter
createAdapterFromFile filePath maybeManager = do
  descriptor <- loadDescriptor filePath
  createAdapter descriptor maybeManager

-- | Execute an effect using the appropriate adapter
executeEffect :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  TimelineAdapter -> 
  Effect -> 
  Sem r ByteString
executeEffect adapter effect = do
  result <- embed $ taExecuteEffect adapter effect
  case result of
    Left err -> throw $ ProgramError $ RuntimeError $ "Adapter error: " <> show err
    Right output -> pure output

-- | Query state from the timeline
queryState :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  TimelineAdapter -> 
  ByteString -> 
  Sem r ByteString
queryState adapter query = do
  result <- embed $ taQueryState adapter query
  case result of
    Left err -> throw $ ProgramError $ RuntimeError $ "Adapter error: " <> show err
    Right output -> pure output

-- | Watch a timeline for changes
watchTimeline :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  TimelineAdapter -> 
  (BlockHeader -> IO ()) -> 
  Sem r ()
watchTimeline adapter callback = do
  -- This would implement a polling or webhook-based system to watch for changes
  -- For now, just get the latest head once
  
  result <- embed $ taGetLatestHead adapter
  case result of
    Left err -> throw $ ProgramError $ RuntimeError $ "Adapter error: " <> show err
    Right header -> embed $ callback header

-- | Get the latest block header from a timeline
getLatestHead :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  TimelineAdapter -> 
  Sem r BlockHeader
getLatestHead adapter = do
  result <- embed $ taGetLatestHead adapter
  case result of
    Left err -> throw $ ProgramError $ RuntimeError $ "Adapter error: " <> show err
    Right header -> pure header

-- | Get a TimeMap update from a timeline
getTimeMapUpdate :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  TimelineAdapter -> 
  TimeMap -> 
  Sem r TimeMap
getTimeMapUpdate adapter currentTimeMap = do
  -- Get the latest head
  header <- getLatestHead adapter
  
  -- Get the current time
  now <- embed getCurrentTime
  
  -- Get the timeline ID from the descriptor
  let timelineId = tdId (acDescriptor $ taConfig adapter)
      
  -- Update the Lamport clock
  let currentClock = Map.findWithDefault (LamportClock 0) timelineId (tmLamportClocks currentTimeMap)
      newClock = LamportClock (unLamportClock currentClock + 1)
  
  -- Create the updated TimeMap
  pure $ currentTimeMap
    { tmObservedHeads = Map.insert timelineId header (tmObservedHeads currentTimeMap)
    , tmObservedTimestamps = Map.insert timelineId now (tmObservedTimestamps currentTimeMap)
    , tmLamportClocks = Map.insert timelineId newClock (tmLamportClocks currentTimeMap)
    }

-- | Helper function to encode parameters for RPC calls
encodeParameters :: 
  (Member (Error AppError) r) => 
  VMType -> 
  [Aeson.Value] -> 
  Sem r ByteString
encodeParameters vmType params = case vmType of
  EVM -> pure $ Aeson.encode $ object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "method" .= ("eth_call" :: Text)
    , "params" .= params
    , "id" .= (1 :: Int)
    ]
  Solana -> pure $ Aeson.encode $ object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "method" .= ("callProgram" :: Text)
    , "params" .= params
    , "id" .= (1 :: Int)
    ]
  _ -> throw $ ProgramError $ InvalidConfiguration $
       "Parameter encoding not implemented for VM type: " <> show vmType

-- | Helper function to decode RPC results
decodeResult :: 
  (Member (Error AppError) r) => 
  VMType -> 
  ByteString -> 
  Sem r Aeson.Value
decodeResult vmType result = case Aeson.decode (BS.toStrict result) of
  Just value -> pure value
  Nothing -> throw $ ProgramError $ RuntimeError $
             "Failed to decode result from " <> show vmType

-- | Create EVM adapter
createEVMAdapter :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  AdapterConfig -> 
  AdapterState -> 
  Sem r TimelineAdapter
createEVMAdapter config state = do
  -- Create the adapter with EVM-specific implementations
  pure $ TimelineAdapter
    { taConfig = config
    , taState = state
    , taExecuteEffect = mockExecuteEffect config "EVM"
    , taQueryState = mockQueryState config "EVM"
    , taGetLatestHead = mockGetLatestHead config "EVM"
    }

-- | Create Solana adapter
createSolanaAdapter :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  AdapterConfig -> 
  AdapterState -> 
  Sem r TimelineAdapter
createSolanaAdapter config state = do
  -- Create the adapter with Solana-specific implementations
  pure $ TimelineAdapter
    { taConfig = config
    , taState = state
    , taExecuteEffect = mockExecuteEffect config "Solana"
    , taQueryState = mockQueryState config "Solana"
    , taGetLatestHead = mockGetLatestHead config "Solana"
    }

-- | Create CosmWasm adapter
createCosmWasmAdapter :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  AdapterConfig -> 
  AdapterState -> 
  Sem r TimelineAdapter
createCosmWasmAdapter config state = do
  -- Create the adapter with CosmWasm-specific implementations
  pure $ TimelineAdapter
    { taConfig = config
    , taState = state
    , taExecuteEffect = mockExecuteEffect config "CosmWasm"
    , taQueryState = mockQueryState config "CosmWasm"
    , taGetLatestHead = mockGetLatestHead config "CosmWasm"
    }

-- | Create Move adapter
createMoveAdapter :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  AdapterConfig -> 
  AdapterState -> 
  Sem r TimelineAdapter
createMoveAdapter config state = do
  -- Create the adapter with Move-specific implementations
  pure $ TimelineAdapter
    { taConfig = config
    , taState = state
    , taExecuteEffect = mockExecuteEffect config "MoveVM"
    , taQueryState = mockQueryState config "MoveVM"
    , taGetLatestHead = mockGetLatestHead config "MoveVM"
    }

-- | Create Native adapter (for local/in-memory execution)
createNativeAdapter :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  AdapterConfig -> 
  AdapterState -> 
  Sem r TimelineAdapter
createNativeAdapter config state = do
  -- Create the adapter with Native-specific implementations
  pure $ TimelineAdapter
    { taConfig = config
    , taState = state
    , taExecuteEffect = mockExecuteEffect config "Native"
    , taQueryState = mockQueryState config "Native"
    , taGetLatestHead = mockGetLatestHead config "Native"
    }

-- | Create Mock adapter for testing
createMockAdapter :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  AdapterConfig -> 
  AdapterState -> 
  Sem r TimelineAdapter
createMockAdapter config state = do
  -- Create the adapter with Mock-specific implementations
  pure $ TimelineAdapter
    { taConfig = config
    , taState = state
    , taExecuteEffect = mockExecuteEffect config "MockVM"
    , taQueryState = mockQueryState config "MockVM"
    , taGetLatestHead = mockGetLatestHead config "MockVM"
    }

-- Mock implementations for testing

-- | Mock implementation for executing effects
mockExecuteEffect :: 
  AdapterConfig -> 
  String -> 
  Effect -> 
  IO (Either AdapterError ByteString)
mockExecuteEffect config vmType effect = do
  -- In a real implementation, this would make RPC calls to the blockchain
  -- For now, return mock data based on the effect type
  pure $ Right $ BS.pack $ 
    "[" ++ vmType ++ "] Executed effect: " ++ show effect

-- | Mock implementation for querying state
mockQueryState :: 
  AdapterConfig -> 
  String -> 
  ByteString -> 
  IO (Either AdapterError ByteString)
mockQueryState config vmType query = do
  -- In a real implementation, this would make RPC calls to the blockchain
  -- For now, return mock data based on the query
  pure $ Right $ BS.pack $ 
    "[" ++ vmType ++ "] Query result for: " ++ BS.unpack query

-- | Mock implementation for getting the latest block header
mockGetLatestHead :: 
  AdapterConfig -> 
  String -> 
  IO (Either AdapterError BlockHeader)
mockGetLatestHead config vmType = do
  -- In a real implementation, this would make RPC calls to the blockchain
  -- For now, return a mock block header
  currentTime <- getCurrentTime
  pure $ Right $ BlockHeader
    { blockHeight = 12345
    , blockHash = EntityHash $ Hash $ BS.pack $ vmType ++ "-mock-hash"
    , blockTimestamp = currentTime
    , blockParentHash = EntityHash $ Hash $ BS.pack $ vmType ++ "-parent-hash"
    } 