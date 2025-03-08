{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

{- |
Module      : Adapters.NetworkAdapter
Description : Core P2P communication interface for Time Bandits
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module defines the core network adapter typeclass and related functions
for P2P communication between Time Bandits nodes. It provides a unified interface
for different network implementations (in-memory, QUIC, etc.).
-}
module Adapters.NetworkAdapter
  ( -- * Core Network Typeclass
    NetworkAdapter(..)
  , createNetworkAdapter
  
    -- * Network Types
  , PeerId(..)
  , MessageType(..)
  , MessageId(..)
  , Message(..)
  , SubscriptionId(..)
  , PeerStatus(..)
  , NetworkError(..)
  
    -- * Configuration
  , NetworkConfig(..)
  , defaultNetworkConfig
  
    -- * Re-exports
  , module Types.Network
  ) where

import Control.Concurrent (ThreadId)
import Control.Monad (forM, void, when)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Time (UTCTime, getCurrentTime)

-- Import from TimeBandits modules
import Types.Network
import Core.Common (PubKey, Actor)
import Adapters.Network (P2PConfig, P2PNetwork)
import Polysemy (Sem)

-- | Natural transformation type
type (~>) f g = forall x. f x -> g x

-- | Core NetworkAdapter typeclass that defines the P2P communication interface
class NetworkAdapter a where
  -- Core communication functions
  broadcastMessage :: a -> MessageType -> ByteString -> IO (Either NetworkError MessageId)
  sendDirectMessage :: a -> PeerId -> MessageType -> ByteString -> IO (Either NetworkError MessageId)
  
  -- Subscription management
  subscribeToMessages :: a -> MessageType -> (PeerId -> ByteString -> IO ()) -> IO SubscriptionId
  unsubscribeFromMessages :: a -> SubscriptionId -> IO ()
  
  -- Peer management
  discoverPeers :: a -> IO [PeerId]
  getPeerStatus :: a -> PeerId -> IO (Maybe PeerStatus)
  
  -- Lifecycle management
  startAdapter :: a -> IO ()
  stopAdapter :: a -> IO ()

-- | Create a network adapter with the given configuration
-- This function will be implemented by specific adapter implementations
createNetworkAdapter :: NetworkConfig -> IO (forall a. NetworkAdapter a => a)
createNetworkAdapter = error "createNetworkAdapter must be implemented by specific adapter"

-- | Helper function to create a new message ID
newMessageId :: IO MessageId
newMessageId = MessageId <$> nextRandom

-- | Helper function to create a new subscription ID
newSubscriptionId :: IO SubscriptionId
newSubscriptionId = SubscriptionId <$> nextRandom

-- | Helper function to create a new message
createMessage :: PeerId -> Maybe PeerId -> MessageType -> ByteString -> IO Message
createMessage sender recipient msgType payload = do
  msgId <- newMessageId
  timestamp <- getCurrentTime
  return Message
    { messageId = msgId
    , messageType = msgType
    , sender = sender
    , recipient = recipient
    , timestamp = timestamp
    , payload = payload
    , signature = Nothing
    }

-- Backward compatibility helpers for existing code that uses QUIC directly
-- These functions will be updated or removed in future iterations

-- | Adapt the P2P Network effect to use QUIC transport (backward compatibility)
adaptNetworkToQuic ::
  P2PConfig ->
  Actor ->
  PubKey ->
  (P2PNetwork ~> Sem r)
adaptNetworkToQuic = error "Deprecated: Use NetworkAdapter instead"

-- | Convert a P2P network configuration to a QUIC configuration (backward compatibility)
p2pConfigToQuicConfig :: P2PConfig -> QuicConfig
p2pConfigToQuicConfig = error "Deprecated: Use NetworkAdapter instead"

-- | Convert a QUIC configuration back to a P2P network configuration (backward compatibility)
quicConfigToP2pConfig :: QuicConfig -> P2PConfig
quicConfigToP2pConfig = error "Deprecated: Use NetworkAdapter instead"

-- | Interpret network for geo-distributed mode (backward compatibility)
interpretNetworkForGeoDistributed :: a
interpretNetworkForGeoDistributed = error "Deprecated: Use NetworkAdapter instead" 