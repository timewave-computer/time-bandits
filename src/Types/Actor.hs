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
Module: Types.Actor
Description: Shared actor types for the system

This module defines the shared actor types needed by various components
of the Time Bandits system. By placing these in a separate module,
we can avoid circular dependencies between Core, Actors, and Programs modules.

Types defined here include:
- Actor capabilities
- Actor roles
- Minimal actor representations
- Basic actor specifications
-}
module Types.Actor
  ( -- * Actor Roles
    ActorRole(..)
  
  -- * Actor Capabilities  
  , ActorCapability(..)
  
  -- * Actor Specifications  
  , ActorSpec(..)
  
  -- * Utility Types  
  , ActorId
  ) where

import Data.Text (Text)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Core.Common (EntityHash, ActorHash)
import qualified Data.Text.Encoding as TE
import qualified Data.Serialize as S
import Types.EffectBase ()

-- | Actor ID type
type ActorId = Text

-- | Actor roles define what an actor can do in the system
data ActorRole = 
    TimeTravelerRole  -- ^ Creates and manages programs
  | TimeKeeperRole    -- ^ Manages timeline state and validates transitions
  | TimeBanditRole    -- ^ Executes program logic and maintains P2P network
  | ControllerRole    -- ^ Coordinates the system and manages resources
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Actor capabilities define what actions an actor can perform
data ActorCapability =
    CanCreateProgram      -- ^ Can create new programs
  | CanExecuteProgram     -- ^ Can execute program logic
  | CanTransferResource   -- ^ Can transfer resources between programs
  | CanValidateTransition -- ^ Can validate transition messages
  | CanGenerateProof      -- ^ Can generate proofs for transitions
  | CanServeTimelineState -- ^ Can serve timeline state to other actors
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Actor specification for initialization
data ActorSpec = ActorSpec
  { actorSpecId :: Text              -- ^ Unique ID for the actor
  , actorSpecRole :: ActorRole          -- ^ Role the actor plays in the system
  , actorSpecCapabilities :: [ActorCapability]  -- ^ Capabilities the actor has
  , actorSpecInitialPrograms :: [Text]  -- ^ Initial programs to deploy
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Instance for serializing Text
-- instance S.Serialize Text where
--   put = S.put . TE.encodeUtf8
--   get = TE.decodeUtf8 <$> S.get 