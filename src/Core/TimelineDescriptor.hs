{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Core.TimelineDescriptor
Description: Timeline descriptors for configuring timeline adapters

This module defines timeline descriptors, which are configuration objects
that describe how to connect to and interact with specific timelines
(blockchains, event logs, or other systems of record).

Descriptors include:
- Timeline network details
- VM types and configurations
- Effect handler specifications
- RPC endpoint information

These descriptors enable the time-bandits system to work with heterogeneous
timelines in a uniform manner.
-}
module Core.TimelineDescriptor
  ( -- * Core Types
    TimelineDescriptor(..)
  , VMType(..)
  , EffectHandlerSpec(..)
  
  -- * Descriptor Operations
  , loadDescriptor
  , resolveEffectHandler
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Data.Serialize (Serialize, Get)
import qualified Data.Serialize as S

import Core.TimelineId (TimelineId)
import Core.SerializeInstances ()

-- | Virtual machine types that can be used by timelines
data VMType
  = EVM         -- ^ Ethereum Virtual Machine
  | CosmWasm    -- ^ CosmWasm VM (used by Cosmos chains)
  | WASM        -- ^ Generic WebAssembly
  | Move        -- ^ Move VM (used by Sui, Aptos)
  | CustomVM Text  -- ^ Custom VM with identifier
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialize)

-- | Effect handler specification for a timeline
data EffectHandlerSpec = EffectHandlerSpec
  { handlerName :: Text                    -- ^ Name of the handler
  , handlerVersion :: Text                -- ^ Handler version
  , handlerParams :: Map Text ByteString  -- ^ Handler parameters
  , handlerEndpoints :: [Text]            -- ^ RPC endpoints
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialize)

-- | Timeline descriptor with configuration information
data TimelineDescriptor = TimelineDescriptor
  { timelineId :: TimelineId              -- ^ Unique timeline identifier
  , timelineName :: Text                  -- ^ Human-readable name
  , timelineVMType :: VMType             -- ^ VM type for this timeline
  , timelineHandlers :: [EffectHandlerSpec]  -- ^ Available effect handlers
  , timelineEndpoints :: [Text]           -- ^ RPC endpoints
  , timelineExtraConfig :: Map Text ByteString  -- ^ Extra configuration
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialize)

-- | Load a timeline descriptor from configuration
loadDescriptor :: ByteString -> Either Text TimelineDescriptor
loadDescriptor _ = Left "loadDescriptor not implemented"

-- | Resolve an effect handler for a timeline
resolveEffectHandler :: TimelineDescriptor -> Text -> Either Text EffectHandlerSpec
resolveEffectHandler _ _ = Left "resolveEffectHandler not implemented"
