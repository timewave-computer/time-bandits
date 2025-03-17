{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TimeBandits.Common.ContentAddress
Description : Content addressing for resources
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides content addressing functionality for resources,
allowing resources to be identified by their content hash.
-}
module TimeBandits.Common.ContentAddress
  ( -- * Content Address
    ContentAddress
  , mkContentAddress
  , unContentAddress
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Hashable (Hashable, hashWithSalt)
import GHC.Generics (Generic)
import Data.Serialize (Serialize)

import TimeBandits.Core.Common.Types (Hash(..))

-- | A content address that uniquely identifies a resource by its content
newtype ContentAddress = ContentAddress { unContentAddress :: Hash }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Serialize)

-- Manual instance for Hashable since we can't derive it (Hash doesn't have Hashable)
instance Hashable ContentAddress where
  hashWithSalt salt (ContentAddress (Hash bs)) = hashWithSalt salt (BS.unpack bs)

-- | Create a content address from a ByteString
mkContentAddress :: ByteString -> ContentAddress
mkContentAddress = ContentAddress . Hash 