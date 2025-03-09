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
import Polysemy (Sem, Member, Embed, interpret, makeSem)
import Polysemy.Error (Error, throw)
import Adapters.NetworkQUIC (QuicConfig)
import Adapters.Network (P2PConfig, P2PNetwork)

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

  -- | Get the network configuration
  getNetworkConfig :: a -> NetworkConfig
  
  -- | Connect to a peer
  connectToPeer :: a -> PeerId -> IO (Either Text ())
  
  -- | Disconnect from a peer
  disconnectFromPeer :: a -> PeerId -> IO ()
  
  -- | Send a message to a specific peer
  sendMessage :: a -> PeerId -> ByteString -> IO (Either Text ())
  
  -- | Subscribe to messages of a specific type
  subscribe :: a -> Text -> (PeerId -> ByteString -> IO ()) -> IO ()
  
  -- | Unsubscribe from messages of a specific type
  unsubscribe :: a -> Text -> IO ()
  
  -- | Get all connected peers
  getConnectedPeers :: a -> IO [PeerId]
  
  -- | Get the local peer ID
  getLocalPeerId :: a -> IO PeerId
  
  -- | Get the public key for a peer
  getPeerPublicKey :: a -> PeerId -> IO (Maybe PubKey)

-- | Basic adapter implementation to use as a concrete type
data BasicNetworkAdapter = BasicNetworkAdapter 
  { adapterConfig :: NetworkConfig
  }

-- | Create a network adapter with the given configuration
createNetworkAdapter :: NetworkConfig -> IO BasicNetworkAdapter
createNetworkAdapter config = pure $ BasicNetworkAdapter { adapterConfig = config }

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
adaptNetworkToQuic :: P2PConfig -> Actor -> PubKey -> a -> a
adaptNetworkToQuic _ _ _ a = a  -- Simplified implementation, marked as deprecated

-- | Convert a P2P network configuration to a QUIC configuration (backward compatibility)
p2pConfigToQuicConfig :: P2PConfig -> QuicConfig
p2pConfigToQuicConfig = error "Deprecated: Use NetworkAdapter instead"

-- | Convert a QUIC configuration back to a P2P network configuration (backward compatibility)
quicConfigToP2pConfig :: QuicConfig -> P2PConfig
quicConfigToP2pConfig = error "Deprecated: Use NetworkAdapter instead"

-- | Interpret network for geo-distributed mode (backward compatibility)
interpretNetworkForGeoDistributed :: a
interpretNetworkForGeoDistributed = error "Deprecated: Use NetworkAdapter instead"

-- | Run a P2P network action in the IO monad
runP2PNetwork :: P2PNetwork r a -> P2PConfig -> IO a
runP2PNetwork = error "runP2PNetwork must be implemented by specific adapter" 