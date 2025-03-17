{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TimeBandits.Core.ContentAddress.Hash
Description : Hash functions for content-addressable code
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides hash generation functions for content-addressable code storage,
ensuring that code can be uniquely identified by its content.
-}
module TimeBandits.Core.ContentAddress.Hash
  ( -- * Hash Generation
    generateCodeHash
  , hashFunction
  , hashModule
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Crypto.Hash (hash, SHA256, Digest)

import TimeBandits.Core.ContentAddress.Types (CodeHash, mkCodeHash)

-- | Generate a hash for a code definition based on its content
generateCodeHash :: Text -> CodeHash
generateCodeHash content =
  let digest = hash (TE.encodeUtf8 content) :: Digest SHA256
      hashBytes = C8.pack $ show digest
  in mkCodeHash hashBytes

-- | Hash a function definition
hashFunction :: Text -> Text -> CodeHash
hashFunction functionName functionBody =
  let content = T.concat ["function:", functionName, ":", functionBody]
  in generateCodeHash content

-- | Hash a module definition
hashModule :: Text -> [Text] -> Text -> CodeHash
hashModule moduleName imports moduleBody =
  let importsText = T.intercalate ";" imports
      content = T.concat ["module:", moduleName, ":", importsText, ":", moduleBody]
  in generateCodeHash content 