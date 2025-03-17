{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : TimeBandits.Core.Common.Types
Description : Common types used throughout the Time Bandits system
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides fundamental types used throughout the Time Bandits system.
It serves as the foundation for the type system and should have minimal dependencies.
-}
module TimeBandits.Core.Common.Types 
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

    -- * Error Types  
  , AppError(..)
  , NetworkError(..)
  , TimelineError(..)
  , ResourceError(..)
  , isResourceNotFoundError
  ) where

-- Import necessary modules with explicit imports for clarity
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), Value(..), object)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Crypto.Hash (Digest, SHA256, hash)
import qualified Crypto.Hash as CH
import Data.String (IsString(..))
import qualified Data.ByteString.Base64 as B64
import TimeBandits.Core.Common.Extensions
import GHC.TypeLits (Symbol)
import Control.DeepSeq (NFData)
import Data.Word (Word64)

-- Copy the relevant content from Core/Common.hs here
-- This would include SimulationMode, Hash, EntityHash, etc.

-- | Simulation mode for the Time Bandits system
data SimulationMode
  = InMemory      -- ^ All actors run in the same process 
  | MultiProcess  -- ^ Actors run in separate processes
  | Distributed   -- ^ Actors run on separate machines
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- Manual JSON instances
instance ToJSON SimulationMode where
  toJSON InMemory = A.String "in_memory"
  toJSON MultiProcess = A.String "multi_process"
  toJSON Distributed = A.String "distributed"

instance FromJSON SimulationMode where
  parseJSON = A.withText "SimulationMode" $ \t ->
    case t of
      "in_memory" -> return InMemory
      "multi_process" -> return MultiProcess
      "distributed" -> return Distributed
      _ -> fail $ "Unknown SimulationMode: " ++ T.unpack t

-- | Alias for clarity in configuration
type DeploymentMode = SimulationMode

-- | Generic hash representation
newtype Hash = Hash { unHash :: ByteString }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Serialize, NFData)

-- Manual JSON instances for Hash
instance ToJSON Hash where
  toJSON (Hash bs) = A.String $ TE.decodeUtf8 $ B64.encode bs

instance FromJSON Hash where
  parseJSON = A.withText "Hash" $ \t ->
    case B64.decode (TE.encodeUtf8 t) of
      Left err -> fail $ "Invalid base64: " ++ err
      Right bs -> return $ Hash bs

-- | Hash for a specific entity type
newtype EntityHash (a :: Symbol) = EntityHash { unEntityHash :: Hash }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Serialize, NFData)

-- Manual JSON instances for EntityHash
instance ToJSON (EntityHash a) where
  toJSON (EntityHash h) = toJSON h

instance FromJSON (EntityHash a) where
  parseJSON v = EntityHash <$> parseJSON v

-- Type aliases for clarity
type ActorHash = EntityHash "Actor"
type ResourceHash = EntityHash "Resource"
type TimelineHash = EntityHash "Timeline"

-- | Timeline type
data Timeline = Timeline
  { timelineId :: TimelineHash    -- ^ Unique identifier for the timeline
  , timelineName :: Text          -- ^ Human-readable name
  , timelineCreator :: ActorHash  -- ^ Actor who created the timeline
  }
  deriving stock (Eq, Show, Generic)

-- Manual Serialize instance for Timeline
instance Serialize Timeline where
  put (Timeline tid name creator) = do
    S.put tid
    S.put (TE.encodeUtf8 name)
    S.put creator
  
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

-- Manual Serialize instance for Actor to handle Text fields
instance Serialize Actor where
  put (Actor aid role endpoint) = do
    S.put aid
    S.put (TE.encodeUtf8 role)
    S.put (TE.encodeUtf8 endpoint)
  
  get = do
    aid <- S.get
    roleBytes <- S.get
    endpointBytes <- S.get
    return $ Actor aid (TE.decodeUtf8 roleBytes) (TE.decodeUtf8 endpointBytes)

-- Manual JSON instances
instance ToJSON Actor where
  toJSON (Actor aid role endpoint) = object
    [ "id" .= aid
    , "role" .= role
    , "endpoint" .= endpoint
    ]

instance FromJSON Actor where
  parseJSON = A.withObject "Actor" $ \v -> Actor
    <$> v .: "id"
    <*> v .: "role"
    <*> v .: "endpoint"

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

-- Manual Serialize instance for Asset
instance Serialize Asset where
  put (Asset aid atype quantity) = do
    S.put aid
    S.put (TE.encodeUtf8 atype)
    S.put quantity
  
  get = do
    aid <- S.get
    typeBytes <- S.get
    quantity <- S.get
    return $ Asset aid (TE.decodeUtf8 typeBytes) quantity

-- | Public key for cryptographic operations
newtype PubKey = PubKey { unPubKey :: ByteString }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Serialize)

-- Manual JSON instances
instance ToJSON PubKey where
  toJSON (PubKey bs) = A.String $ TE.decodeUtf8 $ B64.encode bs

instance FromJSON PubKey where
  parseJSON = A.withText "PubKey" $ \t ->
    case B64.decode (TE.encodeUtf8 t) of
      Left err -> fail $ "Invalid base64: " ++ err
      Right bs -> return $ PubKey bs

-- | Private key for cryptographic operations
newtype PrivKey = PrivKey { unPrivKey :: ByteString }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Serialize)

-- | Cryptographic signature
data Signature = Signature
  { sigValue :: ByteString
  , sigMeta :: Text  -- ^ Additional metadata about the signature
  } deriving stock (Eq, Show, Generic)

-- Manual Serialize instance for Signature to handle Text fields
instance Serialize Signature where
  put (Signature val meta) = do
    S.put val
    S.put (TE.encodeUtf8 meta)
  
  get = do
    val <- S.get
    metaBytes <- S.get
    return $ Signature val (TE.decodeUtf8 metaBytes)

-- Manual JSON instances
instance ToJSON Signature where
  toJSON (Signature val meta) = object
    [ "value" .= (TE.decodeUtf8 $ B64.encode val)
    , "meta" .= meta
    ]

instance FromJSON Signature where
  parseJSON = A.withObject "Signature" $ \v -> do
    valText <- v .: "value"
    meta <- v .: "meta"
    case B64.decode (TE.encodeUtf8 valText) of
      Left err -> fail $ "Invalid base64: " ++ err
      Right bs -> return $ Signature bs meta

-- | Lamport logical clock for partial ordering of events
newtype LamportTime = LamportTime { unLamportTime :: Word64 }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Serialize)

-- Manual JSON instances
instance ToJSON LamportTime where
  toJSON (LamportTime t) = A.Number (fromIntegral t)

instance FromJSON LamportTime where
  parseJSON = A.withScientific "LamportTime" $ \n ->
    return $ LamportTime (round n)

-- | Compute a cryptographic hash for a ByteString
computeHash :: ByteString -> Hash
computeHash bs = 
  let digest = CH.hash bs :: CH.Digest CH.SHA256
      hashStr = show digest
      hashBytes = BS.pack $ map (fromIntegral . fromEnum) hashStr
  in Hash hashBytes

-- | Application error types
data AppError
  = NetworkError NetworkError
  | TimelineError TimelineError
  | ResourceError ResourceError
  | ConfigError Text
  | PermissionError Text
  | ValidationError Text
  | ExecutionError Text
  | SerializationError Text
  | UnknownError Text
  deriving stock (Show, Eq, Generic)

-- Manual JSON instances
instance ToJSON AppError where
  toJSON (NetworkError e) = object ["type" .= ("network" :: Text), "error" .= e]
  toJSON (TimelineError e) = object ["type" .= ("timeline" :: Text), "error" .= e]
  toJSON (ResourceError e) = object ["type" .= ("resource" :: Text), "error" .= e]
  toJSON (ConfigError e) = object ["type" .= ("config" :: Text), "message" .= e]
  toJSON (PermissionError e) = object ["type" .= ("permission" :: Text), "message" .= e]
  toJSON (ValidationError e) = object ["type" .= ("validation" :: Text), "message" .= e]
  toJSON (ExecutionError e) = object ["type" .= ("execution" :: Text), "message" .= e]
  toJSON (SerializationError e) = object ["type" .= ("serialization" :: Text), "message" .= e]
  toJSON (UnknownError e) = object ["type" .= ("unknown" :: Text), "message" .= e]

instance FromJSON AppError where
  parseJSON = A.withObject "AppError" $ \v -> do
    typ <- v .: "type" :: AT.Parser Text
    case typ of
      "network" -> NetworkError <$> v .: "error"
      "timeline" -> TimelineError <$> v .: "error"
      "resource" -> ResourceError <$> v .: "error"
      "config" -> ConfigError <$> v .: "message"
      "permission" -> PermissionError <$> v .: "message"
      "validation" -> ValidationError <$> v .: "message"
      "execution" -> ExecutionError <$> v .: "message"
      "serialization" -> SerializationError <$> v .: "message"
      "unknown" -> UnknownError <$> v .: "message"
      _ -> fail $ "Unknown error type: " ++ T.unpack typ

-- | Network-specific errors
data NetworkError
  = ConnectionFailed Text
  | MessageTooLarge Int
  | PeerNotFound Text
  | ProtocolError Text
  deriving stock (Show, Eq, Generic)

-- Manual JSON instances
instance ToJSON NetworkError where
  toJSON (ConnectionFailed msg) = object ["code" .= ("connection_failed" :: Text), "message" .= msg]
  toJSON (MessageTooLarge size) = object ["code" .= ("message_too_large" :: Text), "size" .= size]
  toJSON (PeerNotFound peer) = object ["code" .= ("peer_not_found" :: Text), "peer" .= peer]
  toJSON (ProtocolError msg) = object ["code" .= ("protocol_error" :: Text), "message" .= msg]

instance FromJSON NetworkError where
  parseJSON = A.withObject "NetworkError" $ \v -> do
    code <- v .: "code" :: AT.Parser Text
    case code of
      "connection_failed" -> ConnectionFailed <$> v .: "message"
      "message_too_large" -> MessageTooLarge <$> v .: "size"
      "peer_not_found" -> PeerNotFound <$> v .: "peer"
      "protocol_error" -> ProtocolError <$> v .: "message"
      _ -> fail $ "Unknown network error code: " ++ T.unpack code

-- | Timeline-specific errors
data TimelineError
  = TimelineNotFound TimelineHash
  | InvalidTimeline Text
  | ObservationError Text
  deriving stock (Show, Eq, Generic)

-- Manual JSON instances
instance ToJSON TimelineError where
  toJSON (TimelineNotFound hash) = object ["code" .= ("not_found" :: Text), "hash" .= hash]
  toJSON (InvalidTimeline msg) = object ["code" .= ("invalid" :: Text), "message" .= msg]
  toJSON (ObservationError msg) = object ["code" .= ("observation" :: Text), "message" .= msg]

instance FromJSON TimelineError where
  parseJSON = A.withObject "TimelineError" $ \v -> do
    code <- v .: "code" :: AT.Parser Text
    case code of
      "not_found" -> TimelineNotFound <$> v .: "hash"
      "invalid" -> InvalidTimeline <$> v .: "message"
      "observation" -> ObservationError <$> v .: "message"
      _ -> fail $ "Unknown timeline error code: " ++ T.unpack code

-- | Resource-specific errors
data ResourceError
  = ResourceNotFound ResourceHash
  | ResourceAlreadyExists ResourceHash
  | InvalidResource Text
  | ResourceAccessDenied ResourceHash
  deriving stock (Show, Eq, Generic)

-- Manual JSON instances
instance ToJSON ResourceError where
  toJSON (ResourceNotFound hash) = object ["code" .= ("not_found" :: Text), "hash" .= hash]
  toJSON (ResourceAlreadyExists hash) = object ["code" .= ("already_exists" :: Text), "hash" .= hash]
  toJSON (InvalidResource msg) = object ["code" .= ("invalid" :: Text), "message" .= msg]
  toJSON (ResourceAccessDenied hash) = object ["code" .= ("access_denied" :: Text), "hash" .= hash]

instance FromJSON ResourceError where
  parseJSON = A.withObject "ResourceError" $ \v -> do
    code <- v .: "code" :: AT.Parser Text
    case code of
      "not_found" -> ResourceNotFound <$> v .: "hash"
      "already_exists" -> ResourceAlreadyExists <$> v .: "hash"
      "invalid" -> InvalidResource <$> v .: "message"
      "access_denied" -> ResourceAccessDenied <$> v .: "hash"
      _ -> fail $ "Unknown resource error code: " ++ T.unpack code

-- | Helper function to check if an error is ResourceNotFound
isResourceNotFoundError :: AppError -> Bool
isResourceNotFoundError (ResourceError (ResourceNotFound _)) = True
isResourceNotFoundError _ = False 