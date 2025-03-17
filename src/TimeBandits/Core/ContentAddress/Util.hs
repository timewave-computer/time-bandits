{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      : TimeBandits.Core.ContentAddress.Util
Description : Utilities for content-addressable code storage
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides utilities for working with content-addressable code,
including functions to traverse the codebase, parse code into ASTs,
and compute their content hashes.
-}
module TimeBandits.Core.ContentAddress.Util
  ( -- * Codebase Traversal
    traverseCodebase
  , processFile
  , processDirectory
    
  -- * AST Handling
  , parseModule
  , parseFunction
  , extractFunctions
  
  -- * Repository Population
  , populateRepositoryFromDirectory
  , addFileToRepository
  , addModuleToRepository
  , addFunctionToRepository
  , extractModuleName
  , extractImports
  ) where

import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.List (isSuffixOf)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.FilePath ((</>), takeExtension)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import TimeBandits.Core.ContentAddress.Types
  ( CodeHash
  , CodeDefinition(..)
  , CodeRepository(..)
  , DefType(..)
  )
import TimeBandits.Core.ContentAddress.Repository
  ( newCodeRepository
  , storeDefinition
  , registerName
  )
import TimeBandits.Core.ContentAddress.Hash
  ( hashFunction
  , hashModule
  )

-- | Traverse a codebase starting from a root directory
traverseCodebase :: MonadIO m => FilePath -> CodeRepository -> m ()
traverseCodebase rootDir repo = liftIO $ do
  absRoot <- makeAbsolute rootDir
  doesDirectoryExist absRoot >>= \case
    True -> processDirectory absRoot repo
    False -> putStrLn $ "Directory not found: " ++ absRoot

-- | Process a directory, recursively traversing its contents
processDirectory :: FilePath -> CodeRepository -> IO ()
processDirectory dir repo = do
  entries <- listDirectory dir
  let fullPaths = map (dir </>) entries
  
  -- Process files and subdirectories
  forM_ fullPaths $ \path -> do
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path
    
    when isFile $ do
      -- Only process Haskell files
      when (".hs" `isSuffixOf` path) $ processFile path repo
    
    when isDir $ processDirectory path repo

-- | Process a single Haskell source file
processFile :: FilePath -> CodeRepository -> IO ()
processFile filePath repo = do
  content <- TIO.readFile filePath
  
  -- Extract the module name from the file
  let moduleName = extractModuleName content
      imports = extractImports content
  
  case moduleName of
    Just name -> do
      -- Add the module to the repository
      addModuleToRepository repo name imports content
      
      -- Extract and add individual functions
      let functions = extractFunctions content
      forM_ functions $ \(funcName, funcBody) ->
        addFunctionToRepository repo funcName funcBody
      
    Nothing -> putStrLn $ "Could not determine module name for file: " ++ filePath

-- | Extract module name from source text
extractModuleName :: Text -> Maybe Text
extractModuleName content = 
  let moduleLines = filter (T.isPrefixOf "module ") (T.lines content)
  in case moduleLines of
       (line:_) -> 
         let parts = T.words line
         in if length parts >= 2
            then Just $ T.takeWhile (/= ' ') (T.drop 7 line)
            else Nothing
       _ -> Nothing

-- | Extract imports from source text
extractImports :: Text -> [Text]
extractImports content =
  let importLines = filter (T.isPrefixOf "import ") (T.lines content)
  in map T.strip importLines

-- | Parse a module into its components
parseModule :: Text -> Maybe (Text, [Text], Text)
parseModule content = do
  name <- extractModuleName content
  let imports = extractImports content
      body = content
  return (name, imports, body)

-- | Extract functions from a module
extractFunctions :: Text -> [(Text, Text)]
extractFunctions content =
  let lines = T.lines content
      functionDecls = filter isFunctionDecl lines
  in mapMaybe extractFunctionDef functionDecls
  where
    isFunctionDecl line = 
      not (T.isPrefixOf "--" line) && 
      not (T.isPrefixOf "import " line) &&
      not (T.isPrefixOf "module " line) &&
      T.isInfixOf "::" line
      
    extractFunctionDef line =
      case T.splitOn "::" line of
        [nameStr, typeStr] -> 
          let name = T.strip nameStr
              body = ":: " <> T.strip typeStr <> "\n" <> 
                     getBody content name
          in Just (name, body)
        _ -> Nothing
    
    getBody fullText fnName =
      let lines = T.lines fullText
          startLine = findLine lines (fnName <> " ")
          bodyLines = takeWhile (not . T.null) (drop startLine lines)
      in T.unlines bodyLines
      
    findLine lines str =
      case filter (T.isInfixOf str) lines of
        (_:_) -> length $ takeWhile (not . T.isInfixOf str) lines
        [] -> 0

-- | Parse a function definition
parseFunction :: Text -> Maybe (Text, Text)
parseFunction text =
  let lines = T.lines text
  in case filter (T.isInfixOf "::") lines of
       (line:_) ->
         let parts = T.splitOn "::" line
         in if length parts >= 2
            then case NE.nonEmpty parts of
                  Just ne -> Just (T.strip (NE.head ne), text)
                  Nothing -> Nothing
            else Nothing
       _ -> Nothing

-- | Populate a code repository from a directory
populateRepositoryFromDirectory :: MonadIO m => FilePath -> m CodeRepository
populateRepositoryFromDirectory rootDir = do
  repo <- newCodeRepository
  traverseCodebase rootDir repo
  return repo

-- | Add a file to the repository
addFileToRepository :: MonadIO m => CodeRepository -> FilePath -> m CodeHash
addFileToRepository repo filePath = liftIO $ do
  content <- TIO.readFile filePath
  
  -- Extract the module name from the file
  let moduleName = extractModuleName content
      imports = extractImports content
  
  case moduleName of
    Just name -> do
      -- Add the module to the repository
      addModuleToRepository repo name imports content
      
      -- Extract and add individual functions
      let functions = extractFunctions content
      mapM_ (\(funcName, funcBody) ->
              addFunctionToRepository repo funcName funcBody) functions
      
      -- Return the module hash
      return $ hashModule name imports content
      
    Nothing -> error $ toText ("Could not determine module name for file: " ++ filePath)
    where
      toText :: String -> Text
      toText = T.pack

-- | Add a module to the repository
addModuleToRepository :: MonadIO m => CodeRepository -> Text -> [Text] -> Text -> m CodeHash
addModuleToRepository repo moduleName imports moduleBody = do
  let hash = hashModule moduleName imports moduleBody
      def = CodeDefinition
        { cdHash = hash
        , cdSource = moduleBody
        , cdType = ModuleDef
        }
  _ <- storeDefinition repo def
  registerName repo moduleName hash
  return hash

-- | Add a function to the repository
addFunctionToRepository :: MonadIO m => CodeRepository -> Text -> Text -> m CodeHash
addFunctionToRepository repo functionName functionBody = do
  let hash = hashFunction functionName functionBody
      def = CodeDefinition
        { cdHash = hash
        , cdSource = functionBody
        , cdType = FunctionDef
        }
  _ <- storeDefinition repo def
  registerName repo functionName hash
  return hash 