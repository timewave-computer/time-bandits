{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TimeBandits.Core.TEL.ContentAddressable
Description : Content-addressable storage for TEL
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides integration between the Temporal Effect Language (TEL) and 
the content-addressable code system. It allows TEL programs and expressions to be
stored, retrieved, and referenced by their content hash, ensuring immutability
and reliable dependency tracking.

@since 0.1.0
-}
module TimeBandits.Core.TEL.ContentAddressable 
  ( -- * Content-Addressable Storage
    storeExpression
  , storeProgram
  , retrieveExpression
  , retrieveProgram
  
  -- * Hash References
  , resolveHashReference
  , HashResolutionError(..)
  , AssetType(..)
  
  -- * Serialization
  , serializeExpression
  , serializeProgram
  , deserializeExpression
  , deserializeProgram
  
  -- * Hash Calculation
  , calculateExpressionHash
  , calculateProgramHash
  ) where

import Control.Exception (Exception)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Serialize (encode, decode, Serialize)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Crypto.Hash (hash, SHA256, Digest)
import qualified Data.ByteString.Char8 as C8
import TimeBandits.Core.TEL.AST

-- | Serialize instances for Expression and Program using JSON as an intermediate format
instance Serialize Expression where
  put expr = S.put (LBS.toStrict $ JSON.encode expr)
  get = do
    bs <- S.get
    case JSON.eitherDecode (LBS.fromStrict bs) of
      Left err -> fail $ "Failed to deserialize Expression: " ++ err
      Right expr -> return expr

instance Serialize Program where
  put prog = S.put (LBS.toStrict $ JSON.encode prog)
  get = do
    bs <- S.get
    case JSON.eitherDecode (LBS.fromStrict bs) of
      Left err -> fail $ "Failed to deserialize Program: " ++ err
      Right prog -> return prog

-- | Type alias for hash values
type Hash = String

-- | Types of assets that can be stored in the content-addressable system
data AssetType 
  = ExpressionAsset -- ^ A TEL expression
  | ProgramAsset    -- ^ A complete TEL program
  | DefinitionAsset -- ^ A TEL definition
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Error that can occur when resolving a hash reference
data HashResolutionError
  = HashNotFound Hash
  | InvalidHashFormat Text
  | AssetTypeMismatch AssetType AssetType
  | DeserializationError Text
  deriving (Show, Eq, Generic)

instance Exception HashResolutionError

-- | Base directory for TEL content storage
telStorageDir :: FilePath
telStorageDir = "tel-store"

-- | Calculate hash for a ByteString
calculateHash :: ByteString -> Hash
calculateHash content =
  let digest = hash content :: Digest SHA256
  in show digest

-- | Store a TEL expression in the content-addressable store
storeExpression :: Expression -> IO Hash
storeExpression expr = do
  let hash = calculateExpressionHash expr
  let serialized = serializeExpression expr
  storeContent hash serialized ExpressionAsset
  return hash

-- | Store a TEL program in the content-addressable store
storeProgram :: Program -> IO Hash
storeProgram prog = do
  let hash = calculateProgramHash prog
  let serialized = serializeProgram prog
  storeContent hash serialized ProgramAsset
  return hash

-- | Internal function to store content with metadata
storeContent :: Hash -> ByteString -> AssetType -> IO ()
storeContent hash content assetType = do
  createDirectoryIfMissing True telStorageDir
  let contentPath = telStorageDir </> hash
  let metadataPath = contentPath ++ ".meta"
  BS.writeFile contentPath content
  let metadata = JSON.encode assetType
  LBS.writeFile metadataPath metadata

-- | Retrieve a TEL expression from the content-addressable store
retrieveExpression :: Hash -> IO (Either HashResolutionError Expression)
retrieveExpression hash = do
  result <- retrieveContent hash
  case result of
    Left err -> return $ Left err
    Right (content, assetType) -> 
      case assetType of
        ExpressionAsset -> 
          case deserializeExpression content of
            Left err -> return $ Left $ DeserializationError $ T.pack err
            Right expr -> return $ Right expr
        _ -> return $ Left $ AssetTypeMismatch ExpressionAsset assetType

-- | Retrieve a TEL program from the content-addressable store
retrieveProgram :: Hash -> IO (Either HashResolutionError Program)
retrieveProgram hash = do
  result <- retrieveContent hash
  case result of
    Left err -> return $ Left err
    Right (content, assetType) -> 
      case assetType of
        ProgramAsset -> 
          case deserializeProgram content of
            Left err -> return $ Left $ DeserializationError $ T.pack err
            Right prog -> return $ Right prog
        _ -> return $ Left $ AssetTypeMismatch ProgramAsset assetType

-- | Internal function to retrieve content with metadata
retrieveContent :: Hash -> IO (Either HashResolutionError (ByteString, AssetType))
retrieveContent hash = do
  let contentPath = telStorageDir </> hash
  let metadataPath = contentPath ++ ".meta"
  
  contentExists <- doesFileExist contentPath
  metadataExists <- doesFileExist metadataPath
  
  if not contentExists || not metadataExists
    then return $ Left $ HashNotFound hash
    else do
      content <- BS.readFile contentPath
      metadataBytes <- LBS.readFile metadataPath
      case JSON.decode metadataBytes of
        Nothing -> return $ Left $ DeserializationError "Failed to parse metadata"
        Just assetType -> return $ Right (content, assetType)

-- | Resolve a hash reference to its underlying content
resolveHashReference :: ByteString -> IO (Either HashResolutionError Expression)
resolveHashReference hashBytes = do
  let hashText = TE.decodeUtf8 hashBytes
  let hash = T.unpack hashText
  retrieveExpression hash

-- | Calculate the hash of an expression
calculateExpressionHash :: Expression -> Hash
calculateExpressionHash expr = calculateHash $ serializeExpression expr

-- | Calculate the hash of a program
calculateProgramHash :: Program -> Hash
calculateProgramHash prog = calculateHash $ serializeProgram prog

-- | Serialize an expression to a ByteString
serializeExpression :: Expression -> ByteString
serializeExpression = encode

-- | Serialize a program to a ByteString
serializeProgram :: Program -> ByteString
serializeProgram = encode

-- | Deserialize an expression from a ByteString
deserializeExpression :: ByteString -> Either String Expression
deserializeExpression = decode

-- | Deserialize a program from a ByteString
deserializeProgram :: ByteString -> Either String Program
deserializeProgram = decode 