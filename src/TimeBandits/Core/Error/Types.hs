{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

{- |
Module: TimeBandits.Core.Error.Types
Description: Common error types for the Time-Bandits system

This module defines the basic error types used throughout the Time-Bandits system,
providing a consistent way to represent errors across all components.
This module is imported by both TimeBandits.Core.Error and TimeBandits.Core.Types
to avoid circular dependencies.

@since 0.1.0
-}
module TimeBandits.Core.Error.Types
  ( -- * Error types
    CryptoErrorType(..)
  ) where

-- Import documentation of standard extensions
import TimeBandits.Core.Common.Extensions

-- External libraries
import Data.Text (Text)
import Data.Serialize (Serialize, put, get, putWord8, getWord8)
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

-- | Crypto-specific error types
data CryptoErrorType
  = InvalidSignatureError
  | InvalidKeyPairError
  | SigningError Text
  deriving stock (Show, Eq, Generic)

-- Manual implementation of Serialize for CryptoErrorType
instance Serialize CryptoErrorType where
  put InvalidSignatureError = putWord8 0
  put InvalidKeyPairError = putWord8 1
  put (SigningError txt) = do
    putWord8 2
    S.put (TE.encodeUtf8 txt)
    
  get = do
    tag <- getWord8
    case tag of
      0 -> pure InvalidSignatureError
      1 -> pure InvalidKeyPairError
      2 -> do
        bs <- S.get
        pure $ SigningError (TE.decodeUtf8 bs)
      _ -> fail $ "Unknown CryptoErrorType tag: " ++ show tag 