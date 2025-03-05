{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Module: Core.Effects
Description: Defines the core effect types that form the Time Bandits effect system

This module provides the definitions of all effect types used in the Time Bandits system.
It defines only the data types and type classes representing effects, not their interpretation,
which is handled by the Execution.EffectInterpreter module.

The effect system is based on the Polysemy library for effect handling, allowing for
modular, composable, and testable effect definitions.
-}
module Core.Effects (
    -- * Resource Operations
    ResourceOps (..),
    ResourceOperationEffect (..),
    
    -- * Logical Clock
    LogicalClock (..),
    getLamportTime,
    incrementTime,
    updateTime,
    
    -- * Key Management
    KeyManagement (..),
    generateKeyPair,
    registerPublicKey,
    lookupPublicKey,
    signData,
    verifyWithPublicKey,
    registerActorType,
    lookupActorType,
    
    -- * P2P Network
    P2PNetwork (..),
    P2PNode (..),
    P2PNodeId,
    discoverNodes,
    connectToNode,
    disconnectFromNode,
    sendMessage,
    broadcastMessage,
    receiveMessage,
    
    -- * Transaction Management
    TransactionEffect (..),
    beginTransaction,
    commitTransaction,
    rollbackTransaction,
    
    -- * Application Effects Type
    AppEffects,
    
    -- * Type Exports
    Resource(..),
    ResourceCapability(..),
    Actor(..),
    ActorType(..),
    LamportTime(..),
    ContentAddressedMessage(..),
    AuthenticatedMessage(..),
    UnifiedResourceTransaction(..)
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Serialize (encode)
import Data.Text (Text)
import GHC.Generics (Generic)
import Polysemy (Member, Members, Sem, makeSem)

-- Core module imports
import Core.Common (Hash(..), Signature(..))
import Core.ResourceId (ResourceId)
import Core.TimelineId (TimelineId)
import Core.ProgramId (ProgramId)
import Core.Types
    ( ActorHash
    , ActorType(..)
    , AppError(..)
    , ResourceErrorType(..)
    , TransactionValidationResult(..)
    , LamportTime(..)
    )

-- | Type for P2P node identifiers
type P2PNodeId = ByteString

-- | Representation of a P2P network node
data P2PNode = P2PNode
    { nodeId :: P2PNodeId
    , nodeAddress :: ByteString
    , nodeType :: ActorType
    }
    deriving (Show, Eq, Generic)

-- | Data structure for a resource
data Resource = Resource
    { resourceId :: ResourceId
    , resourceOrigin :: TimelineId
    , resourceOwner :: ActorHash
    , resourceCapabilities :: [ResourceCapability]
    , resourceMeta :: ByteString
    , resourceSpentBy :: Maybe Hash
    , resourceParents :: [ResourceId]
    , resourceTimestamp :: LamportTime
    , resourceProvenanceChain :: [TimelineId]
    }
    deriving (Show, Eq, Generic)

-- | Capabilities that can be assigned to resources
data ResourceCapability
    = TransferCapability  -- ^ Can be transferred to another actor
    | UpdateCapability    -- ^ Can be updated
    | ConsumeCapability   -- ^ Can be consumed
    | DelegateCapability  -- ^ Can delegate capabilities to other actors
    deriving (Show, Eq, Generic)

-- | Actor in the system
data Actor = Actor
    { actorId :: ActorHash
    , actorType :: ActorType
    }
    deriving (Show, Eq, Generic)

-- | Content-addressed message with a hash derived from its content
data ContentAddressedMessage a = ContentAddressedMessage
    { camHash :: Hash
    , camContent :: a
    }
    deriving (Show, Eq, Generic)

-- | Authenticated message with cryptographic signature
data AuthenticatedMessage a = AuthenticatedMessage
    { amHash :: Hash
    , amSender :: Actor
    , amTimestamp :: Maybe LamportTime
    , amPayload :: ContentAddressedMessage a
    , amSignature :: Signature
    }
    deriving (Show, Eq, Generic)

-- | Unified transaction representation for resource transfers
data UnifiedResourceTransaction = UnifiedResourceTransaction
    { urtInputs :: [AuthenticatedMessage Resource]
    , urtOutputs :: [ContentAddressedMessage Resource]
    , urtMetadata :: ByteString
    , urtTimestamp :: LamportTime
    , urtSigner :: Actor
    , urtSignature :: Signature
    , urtProvenanceChain :: [TimelineId]
    }
    deriving (Show, Eq, Generic)

-- | Logical clock for tracking causal ordering in timelines
-- Uses Lamport timestamps to establish a partial ordering of events
-- across distributed nodes, maintaining causal consistency even without
-- perfect clock synchronization.
data LogicalClock m a where
    GetLamportTime :: LogicalClock m LamportTime
    IncrementTime :: LogicalClock m LamportTime
    UpdateTime :: LamportTime -> LogicalClock m LamportTime

makeSem ''LogicalClock

-- | Resource operations typeclass defining core resource manipulation capabilities
class ResourceOps r where
    createResource :: ByteString -> ActorHash -> TimelineId -> Sem r (Either AppError Resource)
    transferResource :: Resource -> ActorHash -> TimelineId -> Sem r (Either AppError Resource)
    consumeResource :: Resource -> Sem r (Either AppError Resource)
    verifyResource :: Resource -> Sem r (Either AppError Bool)
    getResource :: Hash -> Sem r (Either AppError Resource)
    getResourcesByOwner :: ActorHash -> Sem r (Either AppError [Resource])
    getResourcesByTimeline :: TimelineId -> Sem r (Either AppError [Resource])
    createTransaction :: [Resource] -> [Resource] -> ActorHash -> TimelineId -> Sem r (Either AppError UnifiedResourceTransaction)
    validateTransaction :: UnifiedResourceTransaction -> Sem r (Either AppError TransactionValidationResult)
    executeTransaction :: UnifiedResourceTransaction -> Sem r (Either AppError [Resource])
    transactionHistory :: Hash -> Sem r (Either AppError [UnifiedResourceTransaction])

-- | Effect for resource operations
data ResourceOperationEffect m a where
    OpCreateResource :: ByteString -> ActorHash -> TimelineId -> ResourceOperationEffect m (Either AppError Resource)
    OpTransferResource :: Resource -> ActorHash -> TimelineId -> ResourceOperationEffect m (Either AppError Resource)
    OpConsumeResource :: Resource -> ResourceOperationEffect m (Either AppError Resource)
    OpVerifyResource :: Resource -> ResourceOperationEffect m (Either AppError Bool)
    OpGetResource :: Hash -> ResourceOperationEffect m (Either AppError Resource)
    OpGetResourcesByOwner :: ActorHash -> ResourceOperationEffect m (Either AppError [Resource])
    OpGetResourcesByTimeline :: TimelineId -> ResourceOperationEffect m (Either AppError [Resource])
    OpCreateTransaction :: [Resource] -> [Resource] -> ActorHash -> TimelineId -> ResourceOperationEffect m (Either AppError UnifiedResourceTransaction)
    OpValidateTransaction :: UnifiedResourceTransaction -> ResourceOperationEffect m (Either AppError TransactionValidationResult)
    OpExecuteTransaction :: UnifiedResourceTransaction -> ResourceOperationEffect m (Either AppError [Resource])
    OpTransactionHistory :: Hash -> ResourceOperationEffect m (Either AppError [UnifiedResourceTransaction])

makeSem ''ResourceOperationEffect

-- | Instance of ResourceOps for the ResourceOperationEffect
-- Maps the typeclass operations to their effect constructors, allowing
-- any code using the ResourceOps typeclass to work with the effect system.
instance (Member ResourceOperationEffect r) => ResourceOps r where
    createResource metadata owner timeline = opCreateResource metadata owner timeline
    transferResource resource actor timeline = opTransferResource resource actor timeline
    consumeResource resource = opConsumeResource resource
    verifyResource resource = opVerifyResource resource
    getResource hash = opGetResource hash
    getResourcesByOwner owner = opGetResourcesByOwner owner
    getResourcesByTimeline timeline = opGetResourcesByTimeline timeline
    createTransaction inputs outputs actor timeline = opCreateTransaction inputs outputs actor timeline
    validateTransaction transaction = opValidateTransaction transaction
    executeTransaction transaction = opExecuteTransaction transaction
    transactionHistory hash = opTransactionHistory hash

-- | Key management for cryptographic operations
data KeyManagement m a where
    GenerateKeyPair :: KeyManagement m (ByteString, ByteString)  -- (private key, public key)
    RegisterPublicKey :: ActorHash -> ByteString -> KeyManagement m ()
    LookupPublicKey :: ActorHash -> KeyManagement m (Maybe ByteString)
    SignData :: ByteString -> ByteString -> KeyManagement m (Maybe Signature)
    VerifyWithPublicKey :: ByteString -> ByteString -> Signature -> KeyManagement m Bool
    RegisterActorType :: ActorHash -> ActorType -> KeyManagement m ()
    LookupActorType :: ActorHash -> KeyManagement m (Maybe ActorType)

makeSem ''KeyManagement

-- | Peer-to-peer network communication capabilities
data P2PNetwork m a where
    DiscoverNodes :: P2PNetwork m [P2PNode]
    ConnectToNode :: P2PNode -> P2PNetwork m Bool
    DisconnectFromNode :: P2PNodeId -> P2PNetwork m ()
    SendMessage :: P2PNodeId -> ByteString -> P2PNetwork m Bool
    BroadcastMessage :: ByteString -> P2PNetwork m Int  -- Returns count of nodes message sent to
    ReceiveMessage :: P2PNetwork m (Maybe (P2PNodeId, ByteString))

makeSem ''P2PNetwork

-- | ACID transaction management for consistent state updates
data TransactionEffect m a where
    BeginTransaction :: TransactionEffect m ByteString  -- Transaction ID
    CommitTransaction :: ByteString -> TransactionEffect m Bool
    RollbackTransaction :: ByteString -> TransactionEffect m ()

makeSem ''TransactionEffect

-- | Core application effects stack
-- Defines the standard set of effects used throughout the application.
-- This stack combines resource management, networking, state tracking,
-- cryptography, logging, and error handling into a unified effect system.
type AppEffects r =
    '[ ResourceOperationEffect
     , P2PNetwork
     , KeyManagement
     , LogicalClock
     , TransactionEffect
     ]
