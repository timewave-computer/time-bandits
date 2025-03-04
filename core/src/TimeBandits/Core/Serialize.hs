{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TimeBandits.Core.Serialize
  ( -- * Re-exports
    Serialize(..)
    -- * Instances
  , serializeText
  , deserializeText
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Serialize (Serialize(..), Get, Put, getByteString, putByteString)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

-- | Serialize instance for Text
instance Serialize Text where
  put = serializeText
  get = deserializeText

-- | Serialize Text to ByteString
serializeText :: Text -> Put
serializeText t = do
  let bs = TE.encodeUtf8 t
  S.put (BS.length bs)
  putByteString bs

-- | Deserialize Text from ByteString
deserializeText :: Get Text
deserializeText = do
  len <- S.get :: Get Int
  bs <- getByteString len
  pure $ TE.decodeUtf8 bs 