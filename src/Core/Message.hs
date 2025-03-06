{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module: Core.Message
Description: Message typeclass and related functionality for the Time-Bandits system.

This module provides the fundamental Message typeclass used throughout the system
for communication between actors. It's separated into its own module to avoid
circular dependencies between Core modules.

The Message typeclass defines the interface for all messages in the system,
including methods for accessing message properties and verifying message signatures.
-}
module Core.Message
  ( -- * Core Message typeclass
    Message(..)
  ) where

import Data.ByteString (ByteString)
import Data.Serialize (Serialize)

import Core.Types
  ( Actor
  , ActorHash
  , Signature
  , Hash
  , EventContent
  )

-- | Core type class for messages in the system
-- Messages are used for communication between actors and can contain events.
-- All messages are authenticated (signed) and can be verified against the sender's public key.
-- This enables secure peer-to-peer communication in the decentralized network.
class (Serialize m) => Message m where
  messageHash :: m -> Hash
  messageSender :: m -> Actor
  messageDestination :: m -> Maybe ActorHash
  messageSignature :: m -> Signature
  messageContent :: m -> ByteString
  toEvent :: m -> Maybe EventContent
  verifyMessageSignature :: m -> Bool 