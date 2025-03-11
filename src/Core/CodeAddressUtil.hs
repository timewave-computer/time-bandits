{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      : Core.CodeAddressUtil
Description : Utilities for content-addressable code in Time Bandits
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides utilities for working with content-addressable code,
including functions to traverse the codebase, parse code into ASTs,
and compute their content hashes.
-}
module Core.CodeAddressUtil
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
  ) where

import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (isSuffixOf, (!!))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.FilePath ((</>), takeExtension)
import Relude (viaNonEmpty)

import Core.CodeAddress (CodeHash, CodeDefinition(..), CodeRepository, 
                          newCodeRepository, storeDefinition, hashFunction, 
                          hashModule, registerName, DefType(..))

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
            then case parts of
                 (_:name:_) -> Just name
                 _ -> Nothing
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
  moduleName <- extractModuleName content
  let imports = extractImports content
      bodyLines = filter (not . T.isPrefixOf "module ") 
                $ filter (not . T.isPrefixOf "import ") 
                $ T.lines content
      body = T.unlines bodyLines
  return (moduleName, imports, body)

-- | Parse a function definition
parseFunction :: Text -> Maybe (Text, Text)
parseFunction content =
  let lines' = T.lines content
      -- Very simple heuristic: Look for lines with '=' that are likely function definitions
      defLines = filter (\l -> T.any (== '=') l && not (T.isPrefixOf "--" (T.strip l))) lines'
  in case defLines of
       (line:_) ->
         let parts = T.splitOn "=" line
         in if length parts >= 2
            then case viaNonEmpty head parts of
                   Just firstPart -> 
                     case viaNonEmpty tail parts of
                       Just restParts -> Just (T.strip firstPart, T.strip (T.unlines restParts))
                       Nothing -> Nothing
                   Nothing -> Nothing
            else Nothing
       _ -> Nothing

-- | Extract function definitions from a module
extractFunctions :: Text -> [(Text, Text)]
extractFunctions content =
  let lines' = T.lines content
      -- Group lines that might form function definitions
      groups = groupFunctionLines lines'
  in mapMaybe parseFunction groups

-- | Group lines that might form function definitions
groupFunctionLines :: [Text] -> [Text]
groupFunctionLines [] = []
groupFunctionLines (l:ls)
  | T.any (== '=') l && not (T.isPrefixOf "--" (T.strip l)) =
      let (funcBody, rest) = span (\x -> T.isPrefixOf " " x || T.isPrefixOf "\t" x) ls
      in T.unlines (l : funcBody) : groupFunctionLines rest
  | otherwise = groupFunctionLines ls

-- | Populate a code repository from a directory
populateRepositoryFromDirectory :: MonadIO m => FilePath -> m CodeRepository
populateRepositoryFromDirectory rootDir = do
  repo <- newCodeRepository
  traverseCodebase rootDir repo
  return repo

-- | Add a file to the repository
addFileToRepository :: MonadIO m => CodeRepository -> FilePath -> m ()
addFileToRepository repo filePath = liftIO $ do
  content <- TIO.readFile filePath
  
  -- Extract the module name from the file
  let moduleName = extractModuleName content
      imports = extractImports content
  
  case moduleName of
    Just name -> do
      -- Add the module to the repository
      void $ addModuleToRepository repo name imports content
      
      -- Extract and add individual functions
      let functions = extractFunctions content
      forM_ functions $ \(funcName, funcBody) ->
        void $ addFunctionToRepository repo funcName funcBody
      
    Nothing -> putStrLn $ "Could not determine module name for file: " ++ filePath

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