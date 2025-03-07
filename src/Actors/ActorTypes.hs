{-# LANGUAGE DeriveAnyClass #-}

module Actors.ActorTypes 
  ( -- * Common Types
    ActorType(..)
  , SimulationMode(..)
  
  -- * Re-exports from Types.Actor
  , ActorRole(..)
  , ActorCapability(..)
  , ActorSpec(..)
  , ActorId
  
  -- * Actor Handles
  , ActorHandle(..)
  , ActorMessage(..)
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Control.Concurrent (ThreadId)
import System.Process (ProcessHandle)
import Network.Socket (Socket)
import Control.Concurrent.Chan (Chan)
import Data.ByteString (ByteString)

import Core.Common (SimulationMode(..))
import Types.Actor
  ( ActorRole(..)
  , ActorCapability(..)
  , ActorSpec(..)
  , ActorId
  )

-- | Actor types in the system
data ActorType = 
    TimeTravelerActor
  | TimeKeeperActor
  | TimeBanditActor
  deriving stock (Eq, Show) 

-- | Messages that can be sent between actors
data ActorMessage
  = TransitionRequest ByteString
  | TransitionResponse (Either Text Text)
  | ResourceRequest ByteString
  | ResourceResponse (Either Text ByteString)
  | PingRequest
  | PongResponse
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | The Actor handle for interaction with an actor
data ActorHandle r = ActorHandle
  { handleId :: ActorId
  , handleType :: ActorType
  , handleProcess :: Maybe ProcessHandle
  , handleThread :: Maybe ThreadId
  , handleSocket :: Maybe Socket
  , handleMailbox :: Chan ActorMessage
  }
  deriving stock (Generic) 