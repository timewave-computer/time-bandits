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

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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

-- Import from Core modules
import Core.Common (Hash, EntityHash(..), computeSha256)
import Core.Types (AppError(..), TimelineHash, TimelineErrorType(..))
import Core.Serialize ()  -- Import Serialize instances

-- | Virtual Machine types supported by timelines
data VMType
  = EVM        -- ^ Ethereum Virtual Machine
  | CosmWasm   -- ^ CosmWasm (Cosmos ecosystem)
  | MoveVM     -- ^ Move VM (Sui, Aptos)
  | Solana     -- ^ Solana Programs
  | Native     -- ^ Native Haskell implementation (off-chain)
  | MockVM     -- ^ Mock VM for testing
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

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
  deriving anyclass (Serialize)

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
  deriving anyclass (Serialize)

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
  deriving anyclass (Serialize)

-- | Timeline descriptor defining a timeline's properties and effect mappings
data TimelineDescriptor = TimelineDescriptor
  { tdId :: TimelineHash              -- ^ Unique timeline identifier
  , tdName :: ByteString              -- ^ Human-readable name
  , tdVmType :: VMType                -- ^ Virtual machine type
  , tdClockType :: ClockType          -- ^ Clock type
  , tdEndpoint :: EndpointConfig      -- ^ RPC endpoint configuration
  , tdEffectMappings :: Map EffectType EffectHandlerSpec  -- ^ Effect to handler mappings
  , tdMetadata :: Map ByteString ByteString  -- ^ Additional metadata
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Load a timeline descriptor from a TOML file
loadDescriptor :: 
  (Member (Error AppError) r, Member (Embed IO) r) => 
  FilePath -> 
  Sem r TimelineDescriptor
loadDescriptor filePath = do
  -- In a real implementation, this would parse a TOML file
  -- For now, we'll return a mock descriptor based on the file name
  
  if "ethereum" `BS.isInfixOf` BS.pack filePath
    then pure $ createEthereumDescriptor
    else if "solana" `BS.isInfixOf` BS.pack filePath
      then pure $ createSolanaDescriptor
      else throw $ TimelineError $ TimelineGenericError $ "Unknown timeline type: " <> T.pack filePath

-- | Parse a timeline descriptor from ByteString (TOML content)
parseDescriptor :: 
  (Member (Error AppError) r) => 
  ByteString -> 
  Sem r TimelineDescriptor
parseDescriptor content = do
  -- In a real implementation, this would parse TOML content
  -- For now, return a mock descriptor based on content
  
  if "ethereum" `BS.isInfixOf` content
    then pure $ createEthereumDescriptor
    else if "solana" `BS.isInfixOf` content
      then pure $ createSolanaDescriptor
      else throw $ TimelineError $ TimelineGenericError "Unknown timeline configuration"

-- | Validate a timeline descriptor
validateDescriptor :: 
  (Member (Error AppError) r) => 
  TimelineDescriptor -> 
  Sem r Bool
validateDescriptor descriptor = do
  -- Verify that required effect handlers are mapped
  let requiredEffects = [TransferAsset, QueryState, UpdateState]
      mappedEffects = Map.keysSet (tdEffectMappings descriptor)
      missingEffects = filter (\e -> not $ Set.member e mappedEffects) requiredEffects
  
  if null missingEffects
    then pure True
    else throw $ TimelineError $ TimelineGenericError $ "Missing required effect handlers: " <> T.pack (show missingEffects)

-- | Resolve an effect handler for a specific effect type
resolveEffectHandler :: 
  (Member (Error AppError) r) => 
  TimelineDescriptor -> 
  EffectType -> 
  Sem r EffectHandlerSpec
resolveEffectHandler descriptor effectType = do
  case Map.lookup effectType (tdEffectMappings descriptor) of
    Just handlerSpec -> pure handlerSpec
    Nothing -> throw $ TimelineError $ TimelineGenericError $ "No handler found for effect: " <> show effectType

-- | Create a timeline adapter from a descriptor
-- This is a factory function that will create the appropriate adapter
-- implementation based on the timeline type
createTimelineAdapter :: 
  (Member (Error AppError) r) => 
  TimelineDescriptor -> 
  Sem r ByteString
createTimelineAdapter descriptor = do
  -- In a real implementation, this would instantiate an adapter class
  -- For now, return a mock adapter name
  pure $ "Adapter for " <> tdName descriptor

-- | Create a mock Ethereum descriptor
createEthereumDescriptor :: TimelineDescriptor
createEthereumDescriptor = TimelineDescriptor
  { tdId = EntityHash $ computeSha256 "ethereum-mainnet"
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
  { tdId = EntityHash $ computeSha256 "solana-mainnet"
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