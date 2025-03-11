{-# LANGUAGE OverloadedStrings #-}

module Core.ContentAddressableTest (testContentAddressable) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Core.CodeAddress (CodeHash, CodeDefinition(..), CodeRepository, 
                          newCodeRepository, storeDefinition, hashFunction, 
                          hashModule, lookupByHash, lookupByName, registerName, 
                          DefType(..))
import Core.CodeAddressUtil (populateRepositoryFromDirectory, addFileToRepository)
import Execution.ContentAddressableExecutor (ContentAddressableExecutor,
                                            newExecutor, executeByHash, executeByName,
                                            ExecutionContext, newContext)

-- | Helper function for testing if Text contains substring
textShouldContain :: Text -> Text -> Expectation
textShouldContain text substr = 
  unless (T.isInfixOf substr text) $
    expectationFailure $ "Expected text to contain: " ++ T.unpack substr ++ "\nBut got: " ++ T.unpack text

-- | Main test suite for content-addressable code
testContentAddressable :: Spec
testContentAddressable = do
  describe "Content-Addressable Code" $ do
    testCodeHash
    testCodeRepository
    testContentAddressableExecutor
    testRoundTrip

-- | Test the hash generation functionality
testCodeHash :: Spec
testCodeHash = do
  describe "Hash Generation" $ do
    it "produces deterministic hashes for functions" $ do
      let hash1 = hashFunction "addNumbers" "x + y"
          hash2 = hashFunction "addNumbers" "x + y"
      hash1 `shouldBe` hash2
      
    it "produces different hashes for different functions" $ do
      let hash1 = hashFunction "addNumbers" "x + y"
          hash2 = hashFunction "multiplyNumbers" "x * y"
      hash1 `shouldNotBe` hash2
      
    it "produces different hashes for same function with different body" $ do
      let hash1 = hashFunction "addNumbers" "x + y"
          hash2 = hashFunction "addNumbers" "a + b"
      hash1 `shouldNotBe` hash2

-- | Test the code repository functionality
testCodeRepository :: Spec
testCodeRepository = do
  describe "Code Repository" $ do
    it "stores and retrieves code definitions" $ do
      repo <- newCodeRepository
      
      -- Store a function definition
      let functionName = "addNumbers"
          functionBody = "x + y"
          funcHash = hashFunction functionName functionBody
          funcDef = CodeDefinition
            { cdHash = funcHash
            , cdSource = functionBody
            , cdType = FunctionDef
            }
      
      _ <- storeDefinition repo funcDef
      registerName repo functionName funcHash
      
      -- Retrieve by hash
      result1 <- lookupByHash repo funcHash
      case result1 of
        Just def -> do
          cdHash def `shouldBe` funcHash
          cdSource def `shouldBe` functionBody
          cdType def `shouldBe` FunctionDef
        Nothing -> expectationFailure "Failed to retrieve definition by hash"
      
      -- Retrieve by name
      result2 <- lookupByName repo functionName
      case result2 of
        Just def -> do
          cdHash def `shouldBe` funcHash
          cdSource def `shouldBe` functionBody
          cdType def `shouldBe` FunctionDef
        Nothing -> expectationFailure "Failed to retrieve definition by name"

-- | Test the content-addressable executor
testContentAddressableExecutor :: Spec
testContentAddressableExecutor = do
  describe "Content-Addressable Executor" $ do
    it "executes a function by hash" $ do
      repo <- newCodeRepository
      
      -- Create a simple function
      let funcName = "add"
          funcBody = T.pack "x + y"
          funcHash = hashFunction funcName funcBody
          funcDef = CodeDefinition funcHash funcBody FunctionDef
      
      -- Store the function
      _ <- storeDefinition repo funcDef
      registerName repo funcName funcHash
      
      -- Create an executor
      executor <- newExecutor repo
      
      -- Execute the function
      let context = newContext
      (result, _) <- executeByHash executor funcHash context
      case result of
        Just output -> do
          output `textShouldContain` T.pack "Executed function with hash:"
          output `textShouldContain` T.pack "x + y"
        Nothing -> expectationFailure "Function execution failed"
    
    it "executes a function by name" $ do
      repo <- newCodeRepository
      
      -- Create a simple function
      let funcName = "add"
          funcBody = T.pack "x + y"
          funcHash = hashFunction funcName funcBody
          funcDef = CodeDefinition funcHash funcBody FunctionDef
      
      -- Store the function
      _ <- storeDefinition repo funcDef
      registerName repo funcName funcHash
      
      -- Create an executor
      executor <- newExecutor repo
      
      -- Execute the function by name
      let context = newContext
      (result, _) <- executeByName executor funcName context
      case result of
        Just output -> do
          output `textShouldContain` T.pack "Executed function with hash:"
          output `textShouldContain` T.pack "x + y"
        Nothing -> expectationFailure "Function execution failed"
    
    it "supports function composition" $ do
      repo <- newCodeRepository
      
      -- Create two functions
      let addFuncName = "add"
          addFuncBody = T.pack "x + y"
          addFuncHash = hashFunction addFuncName addFuncBody
          addFuncDef = CodeDefinition addFuncHash addFuncBody FunctionDef
          
          multiplyFuncName = "multiply"
          multiplyFuncBody = T.pack "x * y"
          multiplyFuncHash = hashFunction multiplyFuncName multiplyFuncBody
          multiplyFuncDef = CodeDefinition multiplyFuncHash multiplyFuncBody FunctionDef
      
      -- Store both functions
      _ <- storeDefinition repo addFuncDef
      _ <- storeDefinition repo multiplyFuncDef
      registerName repo addFuncName addFuncHash
      registerName repo multiplyFuncName multiplyFuncHash
      
      -- Create a module that uses both functions
      let moduleName = "Calculator"
          moduleBody = T.unlines ["module for calculations", 
                                "uses " `T.append` addFuncName `T.append` " with hash " `T.append` (T.pack $ show addFuncHash),
                                "uses " `T.append` multiplyFuncName `T.append` " with hash " `T.append` (T.pack $ show multiplyFuncHash)]
          moduleHash = hashModule moduleName [] moduleBody
          moduleDef = CodeDefinition moduleHash moduleBody ModuleDef
      
      -- Store the module
      _ <- storeDefinition repo moduleDef
      registerName repo moduleName moduleHash
      
      -- Verify the module references the functions
      result <- lookupByName repo moduleName
      case result of
        Just def -> do
          -- Check that the module source contains the function hash references
          cdSource def `textShouldContain` T.pack (show addFuncHash)
          cdSource def `textShouldContain` T.pack (show multiplyFuncHash)
          
          -- Execute one of the functions to verify it works
          executor <- newExecutor repo
          let context = newContext
          (result, _) <- executeByName executor addFuncName context
          case result of
            Just output -> output `textShouldContain` T.pack "x + y"
            Nothing -> expectationFailure "Function execution failed"
        Nothing -> expectationFailure "Module not found"

-- | Test round-trip functionality
testRoundTrip :: Spec
testRoundTrip = do
  describe "Round-trip functionality" $ do
    it "preserves code when storing and retrieving" $ do
      repo <- newCodeRepository
      
      -- Create a simple function
      let addFuncName = "add"
          addFuncBody = T.pack "x + y"
          addFuncHash = hashFunction addFuncName addFuncBody
          addFuncDef = CodeDefinition addFuncHash addFuncBody FunctionDef
      
      -- Store the function
      _ <- storeDefinition repo addFuncDef
      registerName repo addFuncName addFuncHash
      
      -- Retrieve by name
      result <- lookupByName repo addFuncName
      case result of
        Just def -> do
          cdSource def `textShouldContain` T.pack "x + y"
          executor <- newExecutor repo
          let context = newContext
          (result, _) <- executeByName executor addFuncName context
          case result of
            Just output -> output `textShouldContain` T.pack "x + y"
            Nothing -> expectationFailure "Function execution failed"
        Nothing -> expectationFailure "Function not found by name" 