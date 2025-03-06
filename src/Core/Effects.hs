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
Description: Core effect types for the Time-Bandits effect system.

This module defines core effect types for the Time-Bandits effect system.
The effect system is based on the Polysemy library, which allows for modular
and composable effect definitions.
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
    
    -- * Type Exports from Core.Types
    Types.Resource,
    Types.ResourceCapability,
    Types.Actor,
    Types.ActorType,
    Types.LamportTime,
    Types.ContentAddressedMessage,
    Types.AuthenticatedMessage,
    UnifiedResourceTransaction(..)
) where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Serialize (encode)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Polysemy (Member, Members, Sem, makeSem)
import Polysemy.Error
import Polysemy.State

-- Internal imports
import qualified Core.Common as Common
import qualified Core.Types as Types
import Core.ResourceId
import Core.TimelineId
import Core.ProgramId
import Core.Common (Hash(..))

-- | Type for P2P node identifiers
type P2PNodeId = ByteString

-- | Representation of a P2P network node
data P2PNode = P2PNode
    { nodeId :: P2PNodeId
    , nodeAddress :: ByteString
    , nodeType :: Types.ActorType
    }
    deriving stock (Show, Eq, Generic)

-- | Transaction for moving resources across timelines
data UnifiedResourceTransaction = UnifiedResourceTransaction
    { urtInputs :: [ResourceId]
    , urtOutputs :: [ResourceId]
    , urtMetadata :: ByteString
    , urtSignature :: Types.Signature
    }
    deriving stock (Show, Eq, Generic)

-- | Operations on resources
data ResourceOps m a where
    CreateResource :: ResourceId -> TimelineId -> Types.ActorHash -> [Types.ResourceCapability] -> ByteString -> ResourceOps m Types.Resource
    GetResource :: ResourceId -> ResourceOps m (Maybe Types.Resource)
    TransferResource :: ResourceId -> Types.ActorHash -> ResourceOps m Bool
    ConsumeResource :: ResourceId -> ResourceOps m Bool
    GetResourceHistory :: ResourceId -> ResourceOps m [Types.LogEntry ResourceOperationEffect]

-- | Resource operations that can be performed
data ResourceOperationEffect
    = CreateEffect
    | TransferEffect
    | ConsumeEffect
    | DelegateEffect
    deriving stock (Show, Eq, Generic)

-- | Logical clock for maintaining causal ordering
data LogicalClock m a where
    GetLamportTime :: LogicalClock m Types.LamportTime
    IncrementTime :: LogicalClock m Types.LamportTime
    UpdateTime :: Types.LamportTime -> LogicalClock m Types.LamportTime

makeSem ''LogicalClock

-- | Key management operations
data KeyManagement m a where
    GenerateKeyPair :: KeyManagement m (Types.PubKey, Types.PrivKey)
    RegisterPublicKey :: Types.PubKey -> Types.ActorType -> KeyManagement m Types.Actor
    LookupPublicKey :: Types.ActorHash -> KeyManagement m (Maybe Types.PubKey)
    SignData :: Types.PrivKey -> ByteString -> KeyManagement m (Maybe Types.Signature)
    VerifyWithPublicKey :: Types.PubKey -> ByteString -> Types.Signature -> KeyManagement m Bool
    RegisterActorType :: Types.ActorHash -> Types.ActorType -> KeyManagement m ()
    LookupActorType :: Types.ActorHash -> KeyManagement m (Maybe Types.ActorType)

makeSem ''KeyManagement

-- | P2P network operations
data P2PNetwork m a where
    DiscoverNodes :: P2PNetwork m [P2PNode]
    ConnectToNode :: P2PNode -> P2PNetwork m Bool
    DisconnectFromNode :: P2PNode -> P2PNetwork m Bool
    SendMessage :: P2PNode -> ByteString -> P2PNetwork m Bool
    BroadcastMessage :: ByteString -> P2PNetwork m Int
    ReceiveMessage :: P2PNetwork m (Maybe (P2PNode, ByteString))

makeSem ''P2PNetwork

-- | Transaction management
data TransactionEffect m a where
    BeginTransaction :: TransactionEffect m Types.Hash
    CommitTransaction :: Types.Hash -> TransactionEffect m Bool
    RollbackTransaction :: Types.Hash -> TransactionEffect m Bool

makeSem ''TransactionEffect

-- | Combined application effects
type AppEffects = '[ResourceOps, LogicalClock, KeyManagement, P2PNetwork, TransactionEffect]
