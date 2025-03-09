{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

-- | Common types and utilities used throughout the Time Bandits system
module Core.Common 
  ( 
    -- * Simulation Types
    SimulationMode(..)
  , DeploymentMode
    
    -- * Hash Types
  , Hash(..)
  , EntityHash(..)
  , ActorHash
  , ResourceHash
  , TimelineHash
  , computeHash
  
    -- * Cryptographic Types
  , PubKey(..)
  , PrivKey(..)
  , Signature(..)
  
    -- * Time Types
  , LamportTime(..)
  
    -- * Timeline Type
  , Timeline(..)
    
    -- * Actor-related Types
  , Actor(..)
  , Address
  , Asset(..)
  ) where

import Data.ByteString ()
import qualified Data.ByteString as BS
import qualified Crypto.Hash as Hash
import Crypto.Hash (SHA256, Digest)
import qualified Data.Serialize as S
import GHC.Generics ()
import Data.Text ()
import Data.Word ()
import Data.Proxy ()
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import qualified Data.Text.Encoding as TE
import Control.DeepSeq ()

-- | Simulation mode for the Time Bandits system
data SimulationMode
  = InMemory      -- ^ All actors run in the same process 
  | MultiProcess  -- ^ Actors run in separate processes
  | Distributed   -- ^ Actors run on separate machines
  deriving stock (Eq, Show, Generic)
  deriving anyclass (S.Serialize)

-- | Type alias for deployment mode
type DeploymentMode = SimulationMode

-- | Hash type for content-addressed data
newtype Hash = Hash { unHash :: ByteString }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (S.Serialize, NFData)

-- | Entity-specific hash type with phantom type for type safety
newtype EntityHash (a :: Symbol) = EntityHash { unEntityHash :: Hash }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (S.Serialize, NFData)

-- | Ord instance for EntityHash
instance Ord (EntityHash a) where
  compare (EntityHash h1) (EntityHash h2) = compare (unHash h1) (unHash h2)

-- | Ord instance for Hash
instance Ord Hash where
  compare (Hash bs1) (Hash bs2) = compare bs1 bs2

-- | Type alias for actor hash
type ActorHash = EntityHash "Actor"

-- | Type alias for resource hash
type ResourceHash = EntityHash "Resource"

-- | Type alias for timeline hash
type TimelineHash = EntityHash "Timeline"

-- | Compute a SHA-256 hash of a ByteString
computeHash :: ByteString -> Hash
computeHash bs = 
  let digest = Hash.hash bs :: Digest SHA256
      hashStr = show digest
      hashBytes = BS.pack $ map (fromIntegral . fromEnum) hashStr
  in Hash hashBytes

-- | Public key for cryptographic operations
newtype PubKey = PubKey { unPubKey :: ByteString }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (S.Serialize)

-- | Private key for cryptographic operations
newtype PrivKey = PrivKey { unPrivKey :: ByteString }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (S.Serialize)

-- | Cryptographic signature
newtype Signature = Signature { unSignature :: ByteString }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (S.Serialize)

-- | Lamport logical clock timestamp
newtype LamportTime = LamportTime { unLamportTime :: Word64 }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (S.Serialize)

-- | Timeline type
data Timeline = Timeline
  { timelineId :: TimelineHash
  , timelineName :: Text
  , timelineCreator :: ActorHash
  }
  deriving stock (Eq, Show, Generic)

-- Manual instance for Serialize Timeline since Text doesn't have a Serialize instance
instance S.Serialize Timeline where
  put (Timeline tid name creator) = do
    S.put tid
    S.put (TE.encodeUtf8 name)
    S.put creator
  
  get :: S.Get Timeline
  get = do
    tid <- S.get
    nameBytes <- S.get
    creator <- S.get
    return $ Timeline tid (TE.decodeUtf8 nameBytes) creator

-- | Basic Actor representation for identification and routing
data Actor = Actor
  { actorId :: ActorHash     -- ^ Hash of the actor's public key
  , actorRole :: Text        -- ^ The role this actor serves in the system
  , actorEndpoint :: Text    -- ^ Network endpoint for the actor
  }
  deriving stock (Eq, Show, Generic)

-- Manual instance for Actor since Text doesn't have a Serialize instance
instance S.Serialize Actor where
  put (Actor aid role endpoint) = do
    S.put aid
    S.put (TE.encodeUtf8 role)
    S.put (TE.encodeUtf8 endpoint)
  
  get = do
    aid <- S.get
    roleBytes <- S.get
    endpointBytes <- S.get
    return $ Actor aid (TE.decodeUtf8 roleBytes) (TE.decodeUtf8 endpointBytes)

-- | Network address for actors
type Address = Text

-- | Asset representation
data Asset = Asset
  { assetId :: ResourceHash   -- ^ Unique identifier for the asset
  , assetType :: Text         -- ^ Type of asset
  , assetQuantity :: Integer  -- ^ Amount of the asset
  }
  deriving stock (Eq, Show, Generic)

-- | Ordering instance for Asset, comparing by ID, then type, then quantity
instance Ord Asset where
  compare a1 a2 = 
    case compare (assetId a1) (assetId a2) of
      EQ -> case compare (assetType a1) (assetType a2) of
              EQ -> compare (assetQuantity a1) (assetQuantity a2)
              other -> other
      other -> other

-- Manual instance for Asset since Text doesn't have a Serialize instance
instance S.Serialize Asset where
  put (Asset aid atype quantity) = do
    S.put aid
    S.put (TE.encodeUtf8 atype)
    S.put quantity
  
  get = do
    aid <- S.get
    typeBytes <- S.get
    quantity <- S.get
    return $ Asset aid (TE.decodeUtf8 typeBytes) quantity
