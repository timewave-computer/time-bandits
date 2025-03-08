{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Network 
  ( -- * Network Identity Types
    PeerId(..)
  , SubscriptionId(..)
  
    -- * Message Types
  , MessageType(..)
  , MessageId(..)
  , Message(..)
  
    -- * Network Status
  , PeerStatus(..)
  , NetworkError(..)
  
    -- * Configuration
  , NetworkConfig(..)
  , defaultNetworkConfig
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Network.Socket (SockAddr)

-- | Unique identifier for a peer in the network
newtype PeerId = PeerId { unPeerId :: Text }
  deriving (Eq, Ord, Show, Generic)

-- | Unique identifier for a message subscription
newtype SubscriptionId = SubscriptionId { unSubscriptionId :: UUID }
  deriving (Eq, Ord, Show, Generic)

-- | Unique identifier for a message
newtype MessageId = MessageId { unMessageId :: UUID }
  deriving (Eq, Ord, Show, Generic)

-- | Types of messages that can be sent over the network
data MessageType 
  = FactBroadcast       -- ^ Broadcast a new fact
  | EffectPropagation   -- ^ Propagate an effect
  | PeerDiscovery       -- ^ Discover new peers
  | TimeMapUpdate       -- ^ Update the time map
  | ResourceUpdate      -- ^ Update a resource
  | ProgramSync         -- ^ Synchronize program state
  | ControlMessage      -- ^ Network control message
  deriving (Eq, Ord, Show, Generic)

-- | A network message
data Message = Message
  { messageId :: MessageId         -- ^ Unique message ID
  , messageType :: MessageType     -- ^ Type of message
  , sender :: PeerId               -- ^ Sender peer ID
  , recipient :: Maybe PeerId      -- ^ Optional recipient (Nothing = broadcast)
  , timestamp :: UTCTime           -- ^ Time the message was created
  , payload :: ByteString          -- ^ Message content
  , signature :: Maybe ByteString  -- ^ Optional signature
  } deriving (Show, Generic)

-- | Status of a peer
data PeerStatus
  = Connected      -- ^ Peer is connected and active
  | Connecting     -- ^ Connection to peer is in progress
  | Disconnected   -- ^ Peer is not connected
  | Unreachable    -- ^ Peer cannot be reached
  | Banned         -- ^ Peer is banned
  deriving (Eq, Show, Generic)

-- | Network errors
data NetworkError
  = ConnectionError Text     -- ^ Connection failed
  | MessageError Text        -- ^ Message sending/receiving failed
  | PeerNotFound PeerId      -- ^ Peer not found
  | SubscriptionError Text   -- ^ Subscription failed
  | ProtocolError Text       -- ^ Protocol error
  | SecurityError Text       -- ^ Security error
  deriving (Eq, Show, Generic)

-- | Network configuration
data NetworkConfig = NetworkConfig
  { listenAddress :: SockAddr          -- ^ Address to listen on
  , bootstrapPeers :: [SockAddr]       -- ^ Initial peers to connect to
  , maxConnections :: Int              -- ^ Maximum number of concurrent connections
  , connectionTimeout :: Int           -- ^ Connection timeout in milliseconds
  , useEncryption :: Bool              -- ^ Whether to use encryption
  , nodeName :: Text                   -- ^ Human-readable node identifier
  , messageBufferSize :: Int           -- ^ Size of message buffer
  , discoveryInterval :: Int           -- ^ Peer discovery interval in seconds
  , heartbeatInterval :: Int           -- ^ Peer heartbeat interval in seconds
  } deriving (Show, Generic)

-- | Default network configuration
defaultNetworkConfig :: NetworkConfig
defaultNetworkConfig = NetworkConfig
  { listenAddress = error "Listen address must be specified"
  , bootstrapPeers = []
  , maxConnections = 50
  , connectionTimeout = 5000
  , useEncryption = True
  , nodeName = "time-bandit-node"
  , messageBufferSize = 1000
  , discoveryInterval = 60
  , heartbeatInterval = 30
  } 