{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Core.ActorId
Description: Defines the ActorId type for identifying actors in the Time-Bandits system

This module defines the ActorId type, which is used to uniquely identify actors
(Time Travelers) in the Time-Bandits system. Each actor owns exactly one Account Program.
-}
module Core.ActorId 
  ( -- * Core ActorId Type
    ActorId(..)
  
  -- * ActorId Operations
  , actorIdToText
  , actorIdFromText
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import GHC.Generics (Generic)
import Data.Hashable (Hashable)

-- | ActorId uniquely identifies an actor in the Time-Bandits system
newtype ActorId = ActorId { unActorId :: T.Text }
  deriving (Eq, Ord, Show, Generic, Hashable)

instance S.Serialize ActorId where
  put (ActorId t) = S.put (TE.encodeUtf8 t)
  get = ActorId . TE.decodeUtf8 <$> S.get

-- | Convert an ActorId to Text
actorIdToText :: ActorId -> T.Text
actorIdToText = unActorId

-- | Create an ActorId from Text
actorIdFromText :: T.Text -> ActorId
actorIdFromText = ActorId 