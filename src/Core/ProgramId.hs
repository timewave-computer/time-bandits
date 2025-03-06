{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Core.ProgramId
Description: Strongly typed identifiers for programs

This module provides a strongly typed ProgramId newtype to prevent accidental
mixing of different identifier types. It includes functionality for creating,
validating, and working with program identifiers.
-}
module Core.ProgramId
  ( -- * Types
    ProgramId(..)
  , ProgramIdError(..)
  
  -- * Construction
  , fromText
  , fromByteString
  , fromHash
  
  -- * Conversion
  , programIdToText
  , toByteString
  , toHash
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize (Serialize)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

-- | Unique identifier for programs in the Time Bandits system
newtype ProgramId = ProgramId { unProgramId :: ByteString }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Serialize)

-- | Allows using string literals for ProgramId
instance IsString ProgramId where
  fromString = ProgramId . TE.encodeUtf8 . T.pack

-- | Errors that can occur when working with program IDs
data ProgramIdError
  = InvalidProgramIdFormat Text
  | EmptyProgramId
  deriving (Show, Eq)

-- | Create a ProgramId from Text
fromText :: Text -> Either ProgramIdError ProgramId
fromText text
  | T.null text = Left EmptyProgramId
  | otherwise = Right $ ProgramId $ TE.encodeUtf8 text

-- | Create a ProgramId from ByteString
fromByteString :: ByteString -> Either ProgramIdError ProgramId
fromByteString bs
  | BS.null bs = Left EmptyProgramId
  | otherwise = Right $ ProgramId bs

-- | Create a ProgramId from a hash value
fromHash :: ByteString -> ProgramId
fromHash = ProgramId

-- | Convert a ProgramId to Text
programIdToText :: ProgramId -> Text
programIdToText = TE.decodeUtf8 . unProgramId

-- | Convert a ProgramId to ByteString
toByteString :: ProgramId -> ByteString
toByteString = unProgramId

-- | Convert a ProgramId to a hash representation (currently just the underlying ByteString)
toHash :: ProgramId -> ByteString
toHash = unProgramId
