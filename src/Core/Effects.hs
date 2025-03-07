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
Description: Defines the polysemy-based effect system for the Time Bandits framework

This module provides the integration between the Core.Effect data types and the 
Polysemy effect system. It defines the interfaces for effect handlers, allowing
for composable, testable implementations of the Time Bandits effect system.

This module uses the unified Effect model from Core.Effect as its foundation,
but exposes it through a more convenient Polysemy-based interface.
-}
module Core.Effects (
    -- * Resource Operations
    ResourceOps (..),
    
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
    
    -- * Effect Application
    EffectHandler (..),
    applyEffect,
    validateEffect,
    
    -- * Application Effects Type
    AppEffects,
    
    -- * Re-exports from Core.Effect
    Effect (..),
    EffectResult (..),
    EffectDAG (..),
    EffectNode (..),
    EffectMetadata (..),
    FactSnapshot (..),
    
    -- * Re-exports from Core.Types
    ActorType (..),
    ActorInfo (..),
    ResourceInfo (..),
    ResourceCapability (..),
    LamportTime (..)
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Serialize (encode)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Polysemy (Member, Members, Sem, makeSem)

-- Core module imports
import Core.Common (Hash(..), Signature(..), PubKey(..), PrivKey(..), LamportTime(..))
import Core.ResourceId (ResourceId)
import Core.TimelineId (TimelineId)
import Core.ProgramId (ProgramId)
import Core.Types (
    ActorHash,
    ActorType (..),
    ActorInfo (..),
    ResourceInfo (..),
    ResourceCapability (..),
    AppError(..),
    TimelineErrorType(..),
    ResourceErrorType(..)
  )

-- Import the unified effect model from Core.Effect
import Core.Effect (
    Effect (..),
    EffectResult (..),
    EffectDAG (..),
    EffectNode (..),
    EffectMetadata (..),
    FactSnapshot (..),
    validateEffect
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

-- | Interface for resource operations
data ResourceOps m a where
    CreateResource :: ByteString -> ActorHash -> TimelineId -> ResourceOps m (Either AppError ResourceInfo)
    TransferResource :: ResourceInfo -> ActorHash -> TimelineId -> ResourceOps m (Either AppError ResourceInfo)
    ConsumeResource :: ResourceInfo -> ResourceOps m (Either AppError ResourceInfo)
    VerifyResource :: ResourceInfo -> ResourceOps m (Either AppError Bool)
    GetResource :: Hash -> ResourceOps m (Either AppError ResourceInfo)
    GetResourcesByOwner :: ActorHash -> ResourceOps m (Either AppError [ResourceInfo])
    GetResourcesByTimeline :: TimelineId -> ResourceOps m (Either AppError [ResourceInfo])

makeSem ''ResourceOps

-- | Interface for logical clock operations
data LogicalClock m a where
    GetLamportTime :: LogicalClock m LamportTime
    IncrementTime :: LogicalClock m LamportTime
    UpdateTime :: LamportTime -> LogicalClock m LamportTime

makeSem ''LogicalClock

-- | Interface for key management operations
data KeyManagement m a where
    GenerateKeyPair :: KeyManagement m (PubKey, PrivKey)
    RegisterPublicKey :: ActorHash -> PubKey -> KeyManagement m ()
    LookupPublicKey :: ActorHash -> KeyManagement m (Maybe PubKey)
    SignData :: PrivKey -> ByteString -> KeyManagement m (Either Text Signature)
    VerifyWithPublicKey :: PubKey -> ByteString -> Signature -> KeyManagement m Bool
    RegisterActorType :: ActorHash -> ActorType -> KeyManagement m ()
    LookupActorType :: ActorHash -> KeyManagement m (Maybe ActorType)

makeSem ''KeyManagement

-- | Interface for P2P network operations
data P2PNetwork m a where
    DiscoverNodes :: P2PNetwork m [P2PNode]
    ConnectToNode :: P2PNode -> P2PNetwork m Bool
    DisconnectFromNode :: P2PNode -> P2PNetwork m Bool
    SendMessage :: P2PNode -> ByteString -> P2PNetwork m Bool
    BroadcastMessage :: ByteString -> P2PNetwork m Int
    ReceiveMessage :: P2PNetwork m (Maybe (P2PNode, ByteString))

makeSem ''P2PNetwork

-- | Interface for transaction management
data TransactionEffect m a where
    BeginTransaction :: TransactionEffect m ()
    CommitTransaction :: TransactionEffect m Bool
    RollbackTransaction :: TransactionEffect m Bool

makeSem ''TransactionEffect

-- | Interface for effect handling
data EffectHandler m a where
    ApplyEffect :: Effect -> EffectMetadata -> EffectHandler m EffectResult
    ValidateEffectPreconditions :: Effect -> [FactSnapshot] -> EffectHandler m Bool

makeSem ''EffectHandler

-- | Type alias for all application effects
type AppEffects r =
    '[ ResourceOps
     , LogicalClock
     , KeyManagement
     , P2PNetwork
     , TransactionEffect
     , EffectHandler
     ]
