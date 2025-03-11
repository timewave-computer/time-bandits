{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Core.TEL.ContentAddressable
Description : Content-addressable storage for TEL
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides integration between the Temporal Effect Language (TEL) and 
the content-addressable code system. It allows TEL programs and expressions to be
stored, retrieved, and referenced by their content hash, ensuring immutability
and reliable dependency tracking.
-}
module Core.TEL.ContentAddressable 
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
import Data.Serialize (encode, decode)
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

import Core.TEL.AST

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

-- | Retrieve a TEL expression from the content-addressable store
retrieveExpression :: Hash -> IO (Either HashResolutionError Expression)
retrieveExpression hash = do
  content <- retrieveContent hash
  case content of
    Left err -> return $ Left err
    Right (serialized, assetType) ->
      case assetType of
        ExpressionAsset -> 
          case deserializeExpression serialized of
            Left err -> return $ Left $ DeserializationError $ T.pack err
            Right expr -> return $ Right expr
        _ -> return $ Left $ AssetTypeMismatch ExpressionAsset assetType

-- | Retrieve a TEL program from the content-addressable store
retrieveProgram :: Hash -> IO (Either HashResolutionError Program)
retrieveProgram hash = do
  content <- retrieveContent hash
  case content of
    Left err -> return $ Left err
    Right (serialized, assetType) ->
      case assetType of
        ProgramAsset -> 
          case deserializeProgram serialized of
            Left err -> return $ Left $ DeserializationError $ T.pack err
            Right prog -> return $ Right prog
        _ -> return $ Left $ AssetTypeMismatch ProgramAsset assetType

-- | Resolve a hash reference expression to the actual expression
resolveHashReference :: Text -> IO (Either HashResolutionError Expression)
resolveHashReference hash = 
  retrieveExpression (T.unpack hash)

-- | Calculate the hash of a TEL expression
calculateExpressionHash :: Expression -> Hash
calculateExpressionHash expr = 
  calculateHash $ serializeExpression expr

-- | Calculate the hash of a TEL program
calculateProgramHash :: Program -> Hash
calculateProgramHash prog = 
  calculateHash $ serializeProgram prog

-- | Serialize a TEL expression to a ByteString
serializeExpression :: Expression -> ByteString
serializeExpression expr = 
  LBS.toStrict $ JSON.encode expr

-- | Serialize a TEL program to a ByteString
serializeProgram :: Program -> ByteString
serializeProgram prog = 
  LBS.toStrict $ JSON.encode prog

-- | Deserialize a TEL expression from a ByteString
deserializeExpression :: ByteString -> Either String Expression
deserializeExpression bs = 
  case JSON.eitherDecode (LBS.fromStrict bs) of
    Left err -> Left err
    Right expr -> Right expr

-- | Deserialize a TEL program from a ByteString
deserializeProgram :: ByteString -> Either String Program
deserializeProgram bs = 
  case JSON.eitherDecode (LBS.fromStrict bs) of
    Left err -> Left err
    Right prog -> Right prog

-- | Store content in the content-addressable store
storeContent :: Hash -> ByteString -> AssetType -> IO ()
storeContent hash content assetType = do
  -- Create the storage directory if it doesn't exist
  createDirectoryIfMissing True telStorageDir
  
  -- Store the content
  let contentPath = telStorageDir </> hash
  BS.writeFile contentPath content
  
  -- Store metadata
  let metadataPath = contentPath ++ ".meta"
  let metadata = LBS.toStrict $ JSON.encode $ assetType
  BS.writeFile metadataPath metadata

-- | Retrieve content from the content-addressable store
retrieveContent :: Hash -> IO (Either HashResolutionError (ByteString, AssetType))
retrieveContent hash = do
  let contentPath = telStorageDir </> hash
  let metadataPath = contentPath ++ ".meta"
  
  -- Check if the content exists
  contentExists <- doesFileExist contentPath
  metadataExists <- doesFileExist metadataPath
  
  if not contentExists || not metadataExists
    then return $ Left $ HashNotFound hash
    else do
      -- Read the content and metadata
      content <- BS.readFile contentPath
      metadataBS <- BS.readFile metadataPath
      
      -- Parse the metadata
      case JSON.eitherDecode (LBS.fromStrict metadataBS) of
        Left err -> return $ Left $ DeserializationError $ T.pack err
        Right assetType -> return $ Right (content, assetType) 