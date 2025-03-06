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
Module: Core.TimelineDescriptor
Description: Timeline descriptor configuration for different timeline types.

This module provides the TimelineDescriptor system, which defines formal configurations
for various timeline types (blockchains, rollups, event logs) and how effects are mapped
to appropriate handlers for each timeline.

TimelineDescriptors are loaded from TOML configuration files and provide a standardized
way to interact with different timelines across the system.
-}
module Core.TimelineDescriptor
  ( -- * Core Types
    TimelineDescriptor(..)
  , VMType(..)
  , EffectHandlerSpec(..)
  , EndpointConfig(..)
  , ClockType(..)
  , EffectType(..)
  
  -- * Descriptor Operations
  , loadDescriptor
  , parseDescriptor
  , validateDescriptor
  
  -- * Handler Resolution
  , resolveEffectHandler
  , createTimelineAdapter
  
  -- * Configuration
  , defaultEndpointConfig
  ) where

import Control.Monad (void, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Serialize (Serialize)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)
import Polysemy.Embed (Embed)
import qualified Data.Serialize as S
import Data.Time.Clock (UTCTime)
import qualified Core.Common as Common
import qualified Core.Types as Types
import Core.Serialize

-- | Virtual Machine types supported by timelines
data VMType
  = EVM        -- ^ Ethereum Virtual Machine
  | CosmWasm   -- ^ CosmWasm (Cosmos ecosystem)
  | MoveVM     -- ^ Move VM (Sui, Aptos)
  | Solana     -- ^ Solana Programs
  | Native     -- ^ Native Haskell implementation (off-chain)
  | MockVM     -- ^ Mock VM for testing
  deriving stock (Eq, Show, Generic)

-- | Manual Serialize instance for VMType
instance Serialize VMType where
  put EVM = S.putWord8 0
  put CosmWasm = S.putWord8 1
  put MoveVM = S.putWord8 2
  put Solana = S.putWord8 3
  put Native = S.putWord8 4
  put MockVM = S.putWord8 5
  
  get = do
    tag <- S.getWord8
    case tag of
      0 -> return EVM
      1 -> return CosmWasm
      2 -> return MoveVM
      3 -> return Solana
      4 -> return Native
      5 -> return MockVM
      _ -> fail $ "Invalid VMType tag: " ++ show tag

-- | Effect types that can be mapped to handlers
data EffectType
  = TransferAsset   -- ^ Transfer an asset between accounts
  | QueryState      -- ^ Query the state of a contract or account
  | UpdateState     -- ^ Update the state of a contract
  | CreateAsset     -- ^ Create a new asset
  | DestroyAsset    -- ^ Destroy an existing asset
  | ExecuteContract -- ^ Execute a smart contract function
  deriving stock (Eq, Show, Generic, Ord)

-- | Manual Serialize instance for EffectType
instance Serialize EffectType where
  put TransferAsset = S.putWord8 0
  put QueryState = S.putWord8 1
  put UpdateState = S.putWord8 2
  put CreateAsset = S.putWord8 3
  put DestroyAsset = S.putWord8 4
  put ExecuteContract = S.putWord8 5
  
  get = do
    tag <- S.getWord8
    case tag of
      0 -> return TransferAsset
      1 -> return QueryState
      2 -> return UpdateState
      3 -> return CreateAsset
      4 -> return DestroyAsset
      5 -> return ExecuteContract
      _ -> fail $ "Invalid EffectType tag: " ++ show tag

-- | Clock types for different timelines
data ClockType
  = BlockHeight   -- ^ Block number/height based
  | SlotNumber    -- ^ Slot number based
  | Timestamp     -- ^ Timestamp based
  | LamportClock  -- ^ Logical Lamport clock
  deriving stock (Eq, Show, Generic)

-- | Manual Serialize instance for ClockType
instance Serialize ClockType where
  put BlockHeight = S.putWord8 0
  put SlotNumber = S.putWord8 1
  put Timestamp = S.putWord8 2
  put LamportClock = S.putWord8 3
  
  get = do
    tag <- S.getWord8
    case tag of
      0 -> return BlockHeight
      1 -> return SlotNumber
      2 -> return Timestamp
      3 -> return LamportClock
      _ -> fail $ "Invalid ClockType tag: " ++ show tag

-- | Configuration for timeline endpoints
data EndpointConfig = EndpointConfig
  { ecPrimary :: ByteString           -- ^ Primary RPC endpoint
  , ecBackups :: [ByteString]         -- ^ Backup RPC endpoints
  , ecWebhook :: Maybe ByteString     -- ^ Optional webhook for event notifications
  , ecApiKey :: Maybe ByteString      -- ^ Optional API key
  , ecRateLimit :: Int                -- ^ Rate limit (requests per minute)
  , ecTimeout :: Int                  -- ^ Timeout in milliseconds
  }
  deriving stock (Eq, Show, Generic)

-- | Manual Serialize instance for EndpointConfig
instance Serialize EndpointConfig where
  put config = do
    S.put (ecPrimary config)
    S.put (ecBackups config)
    S.put (ecWebhook config)
    S.put (ecApiKey config)
    S.put (ecRateLimit config)
    S.put (ecTimeout config)
  
  get = EndpointConfig
    <$> S.get
    <*> S.get
    <*> S.get
    <*> S.get
    <*> S.get
    <*> S.get

-- | Default endpoint configuration
defaultEndpointConfig :: EndpointConfig
defaultEndpointConfig = EndpointConfig
  { ecPrimary = "http://localhost:8545"
  , ecBackups = []
  , ecWebhook = Nothing
  , ecApiKey = Nothing
  , ecRateLimit = 100
  , ecTimeout = 30000
  }

-- | Specification for an effect handler
data EffectHandlerSpec = EffectHandlerSpec
  { ehsName :: ByteString             -- ^ Handler name
  , ehsContract :: Maybe ByteString   -- ^ Contract address (if applicable)
  , ehsFunction :: ByteString         -- ^ Function name or identifier
  , ehsAbi :: Maybe ByteString        -- ^ ABI or interface definition
  , ehsGasLimit :: Maybe Int          -- ^ Gas limit (if applicable)
  , ehsRetries :: Int                 -- ^ Number of retries on failure
  }
  deriving stock (Eq, Show, Generic)

-- | Manual Serialize instance for EffectHandlerSpec
instance Serialize EffectHandlerSpec where
  put spec = do
    S.put (ehsName spec)
    S.put (ehsContract spec)
    S.put (ehsFunction spec)
    S.put (ehsAbi spec)
    S.put (ehsGasLimit spec)
    S.put (ehsRetries spec)
  
  get = EffectHandlerSpec
    <$> S.get
    <*> S.get
    <*> S.get
    <*> S.get
    <*> S.get
    <*> S.get

-- | Timeline descriptor defining a timeline's properties and effect mappings
data TimelineDescriptor = TimelineDescriptor
  { tdId :: Types.TimelineHash              -- ^ Unique timeline identifier
  , tdName :: ByteString              -- ^ Human-readable name
  , tdVmType :: VMType                -- ^ Virtual machine type
  , tdClockType :: ClockType          -- ^ Clock type
  , tdEndpoint :: EndpointConfig      -- ^ RPC endpoint configuration
  , tdEffectMappings :: Map EffectType EffectHandlerSpec  -- ^ Effect to handler mappings
  , tdMetadata :: Map ByteString ByteString  -- ^ Additional metadata
  }
  deriving stock (Eq, Show, Generic)

-- | Manual Serialize instance for TimelineDescriptor
instance Serialize TimelineDescriptor where
  put descriptor = do
    S.put (tdId descriptor)
    S.put (tdName descriptor)
    S.put (tdVmType descriptor)
    S.put (tdClockType descriptor)
    S.put (tdEndpoint descriptor)
    S.put (tdEffectMappings descriptor)
    S.put (tdMetadata descriptor)
  
  get = TimelineDescriptor
    <$> S.get
    <*> S.get
    <*> S.get
    <*> S.get
    <*> S.get
    <*> S.get
    <*> S.get

-- | Load a timeline descriptor from a TOML file
loadDescriptor :: 
  (Member (Error Types.AppError) r, Member (Embed IO) r) => 
  FilePath -> 
  Sem r TimelineDescriptor
loadDescriptor filePath = do
  -- In a real implementation, this would parse a TOML file
  -- For now, we'll return a mock descriptor based on the file name
  
  if "ethereum" `T.isInfixOf` T.pack filePath
    then pure $ createEthereumDescriptor
    else if "solana" `T.isInfixOf` T.pack filePath
      then pure $ createSolanaDescriptor
      else throw $ Types.TimelineError $ Types.TimelineGenericError $ "Unknown timeline type: " <> T.pack filePath

-- | Parse a timeline descriptor from ByteString (TOML content)
parseDescriptor :: 
  (Member (Error Types.AppError) r) => 
  ByteString -> 
  Sem r TimelineDescriptor
parseDescriptor content = do
  -- In a real implementation, this would parse TOML content
  -- For now, we'll return a mock descriptor based on the content
  
  if "ethereum" `BS.isInfixOf` content
    then pure $ createEthereumDescriptor
    else if "solana" `BS.isInfixOf` content
      then pure $ createSolanaDescriptor
      else throw $ Types.TimelineError $ Types.TimelineGenericError $ "Unknown timeline type in content"

-- | Validate a timeline descriptor
validateDescriptor :: 
  (Member (Error Types.AppError) r) => 
  TimelineDescriptor -> 
  Sem r ()
validateDescriptor descriptor = do
  -- Check that all required effect handlers are present
  let requiredEffects = [TransferAsset, QueryState, UpdateState]
      missingEffects = filter (\e -> not $ Map.member e (tdEffectMappings descriptor)) requiredEffects
  
  if null missingEffects
    then pure ()
    else throw $ Types.TimelineError $ Types.TimelineGenericError $ "Missing required effect handlers: " <> T.pack (show missingEffects)

-- | Get the effect handler for a specific effect type
resolveEffectHandler :: 
  (Member (Error Types.AppError) r) => 
  TimelineDescriptor -> 
  EffectType -> 
  Sem r EffectHandlerSpec
resolveEffectHandler descriptor effectType = do
  case Map.lookup effectType (tdEffectMappings descriptor) of
    Just handlerSpec -> pure handlerSpec
    Nothing -> throw $ Types.TimelineError $ Types.TimelineGenericError $ "No handler found for effect: " <> T.pack (show effectType)

-- | Create a timeline adapter from a descriptor
-- This is a factory function that will create the appropriate adapter
-- implementation based on the timeline type
createTimelineAdapter :: 
  (Member (Error Types.AppError) r) => 
  TimelineDescriptor -> 
  Sem r ByteString
createTimelineAdapter descriptor = do
  -- In a real implementation, this would instantiate an adapter class
  -- For now, return a mock adapter name
  pure $ "Adapter for " <> tdName descriptor

-- | Create a mock Ethereum descriptor
createEthereumDescriptor :: TimelineDescriptor
createEthereumDescriptor = TimelineDescriptor
  { tdId = Types.EntityHash $ Types.Hash "ethereum-mainnet"
  , tdName = "Ethereum Mainnet"
  , tdVmType = EVM
  , tdClockType = BlockHeight
  , tdEndpoint = EndpointConfig
      { ecPrimary = "https://mainnet.infura.io/v3/YOUR_API_KEY"
      , ecBackups = ["https://eth-mainnet.alchemyapi.io/v2/YOUR_API_KEY"]
      , ecWebhook = Just "https://webhook.example.com/eth-events"
      , ecApiKey = Just "YOUR_API_KEY"
      , ecRateLimit = 100
      , ecTimeout = 30000
      }
  , tdEffectMappings = Map.fromList
      [ (TransferAsset, EffectHandlerSpec
          { ehsName = "ERC20Transfer"
          , ehsContract = Just "0xTokenAddress"
          , ehsFunction = "transfer"
          , ehsAbi = Just "[{\"inputs\":[{\"name\":\"recipient\",\"type\":\"address\"},{\"name\":\"amount\",\"type\":\"uint256\"}],\"name\":\"transfer\",\"outputs\":[{\"name\":\"\",\"type\":\"bool\"}],\"stateMutability\":\"nonpayable\",\"type\":\"function\"}]"
          , ehsGasLimit = Just 100000
          , ehsRetries = 3
          })
      , (QueryState, EffectHandlerSpec
          { ehsName = "ERC20Balance"
          , ehsContract = Just "0xTokenAddress"
          , ehsFunction = "balanceOf"
          , ehsAbi = Just "[{\"inputs\":[{\"name\":\"account\",\"type\":\"address\"}],\"name\":\"balanceOf\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"stateMutability\":\"view\",\"type\":\"function\"}]"
          , ehsGasLimit = Nothing
          , ehsRetries = 3
          })
      , (UpdateState, EffectHandlerSpec
          { ehsName = "ContractCall"
          , ehsContract = Just "0xContractAddress"
          , ehsFunction = "updateState"
          , ehsAbi = Just "[{\"inputs\":[{\"name\":\"newState\",\"type\":\"bytes\"}],\"name\":\"updateState\",\"outputs\":[],\"stateMutability\":\"nonpayable\",\"type\":\"function\"}]"
          , ehsGasLimit = Just 200000
          , ehsRetries = 3
          })
      ]
  , tdMetadata = Map.fromList
      [ ("chainId", "1")
      , ("blockTime", "12")
      , ("confirmations", "12")
      ]
  }

-- | Create a mock Solana descriptor
createSolanaDescriptor :: TimelineDescriptor
createSolanaDescriptor = TimelineDescriptor
  { tdId = Types.EntityHash $ Types.Hash "solana-mainnet"
  , tdName = "Solana Mainnet"
  , tdVmType = Solana
  , tdClockType = SlotNumber
  , tdEndpoint = EndpointConfig
      { ecPrimary = "https://api.mainnet-beta.solana.com"
      , ecBackups = ["https://solana-api.projectserum.com"]
      , ecWebhook = Nothing
      , ecApiKey = Nothing
      , ecRateLimit = 200
      , ecTimeout = 15000
      }
  , tdEffectMappings = Map.fromList
      [ (TransferAsset, EffectHandlerSpec
          { ehsName = "SPLTransfer"
          , ehsContract = Just "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"
          , ehsFunction = "transfer"
          , ehsAbi = Nothing
          , ehsGasLimit = Nothing
          , ehsRetries = 3
          })
      , (QueryState, EffectHandlerSpec
          { ehsName = "AccountInfo"
          , ehsContract = Nothing
          , ehsFunction = "getAccountInfo"
          , ehsAbi = Nothing
          , ehsGasLimit = Nothing
          , ehsRetries = 3
          })
      , (UpdateState, EffectHandlerSpec
          { ehsName = "ProgramCall"
          , ehsContract = Just "ProgramAddress"
          , ehsFunction = "instruction"
          , ehsAbi = Nothing
          , ehsGasLimit = Nothing
          , ehsRetries = 3
          })
      ]
  , tdMetadata = Map.fromList
      [ ("commitment", "confirmed")
      , ("skipPreflight", "true")
      ]
  }

-- | Get the adapter name for a timeline descriptor
getAdapterName :: 
  (Member (Error Types.AppError) r) => 
  TimelineDescriptor -> 
  Sem r ByteString
getAdapterName descriptor = do
  pure $ "Adapter for " <> tdName descriptor 