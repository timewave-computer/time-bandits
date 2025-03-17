{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TimeBandits.Core.ContentAddress.ModuleLayout
Description : Documents the organization of the ContentAddress module
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides documentation and examples for using the ContentAddress
module system, which implements content-addressable code storage in Time Bandits.
-}
module TimeBandits.Core.ContentAddress.ModuleLayout
  ( -- * Module Organization
    -- $organization
    
    -- * Usage Examples
    -- $examples
    
    -- * Integration with Other Modules
    -- $integration
  ) where

import TimeBandits.Core.ContentAddress.Types
import TimeBandits.Core.ContentAddress.Hash
import TimeBandits.Core.ContentAddress.Repository
import TimeBandits.Core.ContentAddress.Util

{- $organization
   The ContentAddress module is organized as follows:

   * "TimeBandits.Core.ContentAddress.Types": Core type definitions
     - 'CodeHash': Unique identifier for code content
     - 'CodeDefinition': A stored code entity with metadata
     - 'CodeRepository': Storage system for code definitions

   * "TimeBandits.Core.ContentAddress.Hash": Hash generation functions
     - 'hashFunction': Generate a hash for a function
     - 'hashModule': Generate a hash for a module

   * "TimeBandits.Core.ContentAddress.Repository": Storage and retrieval
     - 'newCodeRepository': Create a new empty repository
     - 'storeDefinition': Store a code definition
     - 'lookupByHash': Retrieve code by its hash
     - 'lookupByName': Retrieve code by its name
     - 'registerName': Associate a name with a hash

   * "TimeBandits.Core.ContentAddress.Util": Utility functions
     - 'traverseCodebase': Process an entire codebase
     - 'populateRepositoryFromDirectory': Build a repository from existing code
     - 'addFileToRepository': Add a source file to a repository
-}

{- $examples
   Here are examples of using the ContentAddress module:

   Creating and populating a code repository:

   @
   import Control.Monad.IO.Class (liftIO)
   import qualified Data.Text as T
   import TimeBandits.Core.ContentAddress.Types
   import TimeBandits.Core.ContentAddress.Repository
   import TimeBandits.Core.ContentAddress.Util

   -- Create a new empty repository
   main :: IO ()
   main = do
     repo <- newCodeRepository
     
     -- Add a function to the repository
     let funcName = "factorial"
         funcBody = T.unlines
           [ "factorial :: Integer -> Integer"
           , "factorial 0 = 1"
           , "factorial n = n * factorial (n - 1)"
           ]
     hash <- addFunctionToRepository repo funcName funcBody
     
     -- Look up the function by name
     result <- lookupByName repo funcName
     case result of
       Just def -> putStrLn $ "Found: " ++ T.unpack (cdSource def)
       Nothing  -> putStrLn "Function not found."
     
     -- Alternatively, populate from a directory
     repo2 <- populateRepositoryFromDirectory "./src"
     putStrLn "Repository populated from directory."
   @

   Using the repository for code execution:

   @
   import TimeBandits.Core.ContentAddress.Repository
   import TimeBandits.Execution.ContentAddressableExecutor

   executeExample :: IO ()
   executeExample = do
     repo <- newCodeRepository
     
     -- ... populate repository ...
     
     -- Execute a function by name
     result <- executeByName repo "factorial" [5]
     print result  -- Should print: 120
     
     -- Execute a function by hash
     hash <- lookupByName repo "factorial"
     case hash of
       Just h -> do
         result <- executeByHash repo h [5]
         print result
       Nothing -> putStrLn "Function not found"
   @
-}

{- $integration
   The ContentAddress module integrates with other Time Bandits modules:

   * Integration with Common:
     - Uses common types and serialization
     - Provides hashing and repository functionality

   * Integration with Execution:
     - Enables execution of content-addressable code
     - Supports dynamic loading and execution

   * Integration with TimeMachine:
     - Supports versioning and history tracking
     - Enables time-travel debugging

   Example of integration with the Execution module:

   @
   import TimeBandits.Core.ContentAddress.Repository
   import TimeBandits.Execution.ContentAddressableExecutor
   import TimeBandits.TimeMachine.History

   integratedExample :: IO ()
   integratedExample = do
     -- Set up repository
     repo <- newCodeRepository
     -- ... populate repository ...
     
     -- Execute code
     executor <- newExecutor repo
     result <- execute executor "myFunction" [arg1, arg2]
     
     -- Record execution in history
     history <- newHistory
     recordExecution history "myFunction" [arg1, arg2] result
     
     -- Later, retrieve from history
     pastExecutions <- getExecutionsForFunction history "myFunction"
     print pastExecutions
   @
-} 