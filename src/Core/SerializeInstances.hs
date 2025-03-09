{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Core.SerializeInstances
Description : Centralized Serialize instances for common types
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides centralized Serialize instances for common types
to avoid duplicate instances across the codebase.
-}
module Core.SerializeInstances
  ( -- * Re-exports
    Serialize(..)
  ) where

import Data.Serialize (Serialize(..))
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)

-- | Serialize instance for Text
-- This is the single source of truth for serializing Text values
instance Serialize Text where
  put = S.put . TE.encodeUtf8
  get = TE.decodeUtf8 <$> S.get 