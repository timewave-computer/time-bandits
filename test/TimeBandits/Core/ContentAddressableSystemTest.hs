{-# LANGUAGE OverloadedStrings #-}

module TimeBandits.Core.ContentAddressableSystemTest (spec, testContentAddressableSystem) where

import Test.Hspec
import Test.QuickCheck

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Control.Monad (unless)

import TimeBandits.Core.ContentAddress.Types (CodeHash, CodeDefinition(..), CodeRepository, DefType(..))
import TimeBandits.Core.ContentAddress.Hash (generateCodeHash, hashFunction, hashModule)
import TimeBandits.Core.ContentAddress.Repository (newCodeRepository, storeDefinition, lookupByHash, lookupByName, registerName)
import TimeBandits.Core.ContentAddress.Util (populateRepositoryFromDirectory, addFileToRepository, addModuleToRepository)
import Execution.ContentAddressableExecutor (ContentAddressableExecutor,
                                            newExecutor, executeByHash, executeByName,
                                            ExecutionContext, newContext)

-- | Main test suite for the content-addressable code system
testContentAddressableSystem :: Spec
testContentAddressableSystem = do
  describe "Content-Addressable Code System (ADR-011)" $ do
    testImmutability
    testDecoupledNaming
    testDependencyConflictResolution
    testContentAwareExecution
    testRefactoringSupport

-- | Test immutability of code definitions
testImmutability :: Spec
testImmutability = do
  describe "Code Immutability" $ do
    it "ensures code definitions remain unchanged after creation" $ do
      repo <- newCodeRepository
      
      -- Create an initial function definition
      let funcName = "add"
          funcBody = "x + y"
          hash1 = hashFunction funcName funcBody
          def1 = CodeDefinition hash1 funcBody FunctionDef
      
      -- Store the definition
      _ <- storeDefinition repo def1
      registerName repo funcName hash1
      
      -- Try to "modify" by creating a new definition with the same name but different body
      let newFuncBody = "a + b"
          hash2 = hashFunction funcName newFuncBody
          def2 = CodeDefinition hash2 newFuncBody FunctionDef
      
      -- Store the "modified" definition
      _ <- storeDefinition repo def2
      registerName repo funcName hash2
      
      -- Verify both versions exist with different hashes
      result1 <- lookupByHash repo hash1
      result2 <- lookupByHash repo hash2
      
      -- Both definitions should exist independently
      case (result1, result2) of
        (Just def1', Just def2') -> do
          cdHash def1' `shouldBe` hash1
          cdSource def1' `shouldBe` funcBody
          cdHash def2' `shouldBe` hash2
          cdSource def2' `shouldBe` newFuncBody
        _ -> expectationFailure "Both code versions should exist independently"
      
      -- The name now points to the newer version
      result3 <- lookupByName repo funcName
      case result3 of
        Just def -> cdHash def `shouldBe` hash2
        Nothing -> expectationFailure "Function name should point to the newer version"

-- | Test decoupled naming system
testDecoupledNaming :: Spec
testDecoupledNaming = do
  describe "Decoupled Naming System" $ do
    it "allows renaming without affecting the underlying code" $ do
      repo <- newCodeRepository
      
      -- Create a function definition
      let originalName = "calculateTotal"
          newName = "computeSum"
          funcBody = "values.reduce((a, b) => a + b, 0)"
          hash = hashFunction originalName funcBody
          def = CodeDefinition hash funcBody FunctionDef
      
      -- Store with original name
      _ <- storeDefinition repo def
      registerName repo originalName hash
      
      -- Look up by original name
      result1 <- lookupByName repo originalName
      case result1 of
        Just def1 -> cdHash def1 `shouldBe` hash
        Nothing -> expectationFailure "Function should be found by original name"
      
      -- "Rename" by registering the same hash with a new name
      registerName repo newName hash
      
      -- Look up by new name
      result2 <- lookupByName repo newName
      case result2 of
        Just def2 -> cdHash def2 `shouldBe` hash
        Nothing -> expectationFailure "Function should be found by new name"
      
      -- The content hash should remain the same regardless of name
      case (result1, result2) of
        (Just def1, Just def2) -> cdHash def1 `shouldBe` cdHash def2
        _ -> expectationFailure "Both names should reference the same code"

-- | Helper function for testing if Text contains substring
textShouldContain :: Text -> Text -> Expectation
textShouldContain text substr = 
  unless (T.isInfixOf substr text) $
    expectationFailure $ "Expected to find " <> T.unpack substr <> " in " <> T.unpack text

-- | Test dependency conflict resolution
testDependencyConflictResolution :: Spec
testDependencyConflictResolution = do
  describe "Dependency Conflict Resolution" $ do
    it "handles multiple functions with the same name but different implementations" $ do
      repo <- newCodeRepository
      
      -- Create two different implementations of the same function
      let funcName = "format"
          impl1 = "str.toLowerCase()"
          impl2 = "str.toUpperCase()"
          hash1 = hashFunction funcName impl1
          hash2 = hashFunction funcName impl2
          def1 = CodeDefinition hash1 impl1 FunctionDef
          def2 = CodeDefinition hash2 impl2 FunctionDef
      
      -- Store both implementations
      _ <- storeDefinition repo def1
      _ <- storeDefinition repo def2
      
      -- Create namespaces by using qualified names
      let ns1Name = "ns1.format"
          ns2Name = "ns2.format"
      
      -- Register with namespaced names
      registerName repo ns1Name hash1
      registerName repo ns2Name hash2
      
      -- Look up both implementations
      result1 <- lookupByName repo ns1Name
      result2 <- lookupByName repo ns2Name
      
      -- Both should be accessible under their respective namespaces
      case result1 of
        Just def -> cdSource def `shouldBe` impl1
        Nothing -> expectationFailure "First implementation not found"
      
      case result2 of
        Just def -> cdSource def `shouldBe` impl2
        Nothing -> expectationFailure "Second implementation not found"
      
    it "allows different modules to use different versions of the same function" $ do
      repo <- newCodeRepository
      
      -- Create two versions of a utility function
      let utilName = "parseData"
          v1Body = "JSON.parse(data)"
          v2Body = "improved JSON parsing logic"
          utilHash1 = hashFunction utilName v1Body
          utilHash2 = hashFunction utilName v2Body
          utilDef1 = CodeDefinition utilHash1 v1Body FunctionDef
          utilDef2 = CodeDefinition utilHash2 v2Body FunctionDef
      
      -- Store both versions
      _ <- storeDefinition repo utilDef1
      _ <- storeDefinition repo utilDef2
      
      -- Create two modules that depend on different versions
      let module1Name = "LegacyModule"
          module2Name = "NewModule"
          module1Body = T.unlines ["module that uses old parseData", 
                                  "reference to " `T.append` T.pack (show utilHash1)]
          module2Body = T.unlines ["module that uses new parseData", 
                                  "reference to " `T.append` T.pack (show utilHash2)]
          module1Hash = hashModule module1Name [] module1Body
          module2Hash = hashModule module2Name [] module2Body
          moduleDef1 = CodeDefinition module1Hash module1Body ModuleDef
          moduleDef2 = CodeDefinition module2Hash module2Body ModuleDef
      
      -- Store both modules
      _ <- storeDefinition repo moduleDef1
      _ <- storeDefinition repo moduleDef2
      registerName repo module1Name module1Hash
      registerName repo module2Name module2Hash
      
      -- Verify each module references the correct version
      result1 <- lookupByName repo module1Name
      result2 <- lookupByName repo module2Name
      
      case result1 of
        Just def -> cdSource def `textShouldContain` T.pack (show utilHash1)
        Nothing -> expectationFailure "Legacy module not found"
      
      case result2 of
        Just def -> cdSource def `textShouldContain` T.pack (show utilHash2)
        Nothing -> expectationFailure "New module not found"

-- | Test content-aware execution
testContentAwareExecution :: Spec
testContentAwareExecution = do
  describe "Content-Aware Execution" $ do
    it "executes code based on its content hash rather than name" $ do
      repo <- newCodeRepository
      
      -- Create a function
      let funcName = "greet"
          funcBody = T.pack $ "Hello, " ++ show "World" ++ "!"
          hash = hashFunction funcName funcBody
          def = CodeDefinition hash funcBody FunctionDef
      
      -- Store the function
      _ <- storeDefinition repo def
      registerName repo funcName hash
      
      -- Create executor
      executor <- newExecutor repo
      let context = newContext
      
      -- Execute by hash
      (result1, _) <- executeByHash executor hash context
      
      -- Execute by name
      (result2, _) <- executeByName executor funcName context
      
      -- Both should produce the same result
      case (result1, result2) of
        (Just output1, Just output2) -> do
          output1 `textShouldContain` funcBody
          output2 `textShouldContain` funcBody
        _ -> expectationFailure "Both execution methods should succeed"
    
    it "executes the version of a function referenced by a specific hash, not just the latest" $ do
      repo <- newCodeRepository
      
      -- Create two versions of a function
      let funcName = "calculate"
          v1Body = T.pack "x + y"
          v2Body = T.pack "x * y"
          hash1 = hashFunction funcName v1Body
          hash2 = hashFunction funcName v2Body
          def1 = CodeDefinition hash1 v1Body FunctionDef
          def2 = CodeDefinition hash2 v2Body FunctionDef
      
      -- Store both versions
      _ <- storeDefinition repo def1
      _ <- storeDefinition repo def2
      
      -- Name points to the latest version
      registerName repo funcName hash2
      
      -- Create executor
      executor <- newExecutor repo
      let context = newContext
      
      -- Execute first version by hash
      (result1, _) <- executeByHash executor hash1 context
      
      -- Execute second version by hash
      (result2, _) <- executeByHash executor hash2 context
      
      -- Execute by name (should be second version)
      (result3, _) <- executeByName executor funcName context
      
      -- Verify each execution produced the expected result
      case result1 of
        Just output -> output `textShouldContain` v1Body
        Nothing -> expectationFailure "First version execution failed"
      
      case result2 of
        Just output -> output `textShouldContain` v2Body
        Nothing -> expectationFailure "Second version execution failed"
      
      case result3 of
        Just output -> output `textShouldContain` v2Body
        Nothing -> expectationFailure "Named execution failed"

-- | Test refactoring support
testRefactoringSupport :: Spec
testRefactoringSupport = do
  describe "Refactoring Support" $ do
    it "preserves references when renaming" $ do
      repo <- newCodeRepository
      
      -- Create a utility function
      let utilName = "formatData"
          utilBody = T.pack "data.toString()"
          utilHash = hashFunction utilName utilBody
          utilDef = CodeDefinition utilHash utilBody FunctionDef
      
      -- Store utility function
      _ <- storeDefinition repo utilDef
      registerName repo utilName utilHash
      
      -- Create a module that uses the utility function
      let moduleName = "DataProcessor"
          moduleBody = T.unlines ["module for processing data", 
                                "uses " `T.append` utilName,
                                "hash: " `T.append` (T.pack $ show utilHash)]
          moduleHash = hashModule moduleName [] moduleBody
          moduleDef = CodeDefinition moduleHash moduleBody ModuleDef
      
      -- Store the module
      _ <- storeDefinition repo moduleDef
      registerName repo moduleName moduleHash
      
      -- "Rename" the utility function
      let newUtilName = "stringifyData"
      registerName repo newUtilName utilHash
      
      -- The module still references the same utility function by hash
      result <- lookupByName repo moduleName
      case result of
        Just def -> cdSource def `textShouldContain` T.pack (show utilHash)
        Nothing -> expectationFailure "Module not found"
      
      -- Can look up utility function by new name
      utilResult <- lookupByName repo newUtilName
      case utilResult of
        Just def -> cdHash def `shouldBe` utilHash
        Nothing -> expectationFailure "Cannot find utility by new name"
      
    it "allows module restructuring without breaking references" $ do
      repo <- newCodeRepository
      
      -- Create a collection of functions
      let func1Name = "validate"
          func1Body = T.pack "input.length > 0"
          func1Hash = hashFunction func1Name func1Body
          func1Def = CodeDefinition func1Hash func1Body FunctionDef
          
          func2Name = "transform"
          func2Body = T.pack "input.toUpperCase()"
          func2Hash = hashFunction func2Name func2Body
          func2Def = CodeDefinition func2Hash func2Body FunctionDef
      
      -- Store both functions
      _ <- storeDefinition repo func1Def
      _ <- storeDefinition repo func2Def
      registerName repo func1Name func1Hash
      registerName repo func2Name func2Hash
      
      -- Create an original module structure with both functions
      let oldModuleName = "StringUtils"
          oldModuleBody = T.unlines ["module with string utilities", 
                                   "uses " `T.append` func1Name `T.append` " with hash " `T.append` (T.pack $ show func1Hash),
                                   "uses " `T.append` func2Name `T.append` " with hash " `T.append` (T.pack $ show func2Hash)]
          oldModuleHash = hashModule oldModuleName [] oldModuleBody
          oldModuleDef = CodeDefinition oldModuleHash oldModuleBody ModuleDef
      
      -- Store the original module
      _ <- storeDefinition repo oldModuleDef
      registerName repo oldModuleName oldModuleHash
      
      -- Create new module structure by splitting into two modules
      let validationModuleName = "Validation"
          transformModuleName = "Transformation"
          
          validationModuleBody = T.unlines ["module with validation utilities", 
                                         "uses " `T.append` func1Name `T.append` " with hash " `T.append` (T.pack $ show func1Hash)]
          
          transformModuleBody = T.unlines ["module with transformation utilities", 
                                        "uses " `T.append` func2Name `T.append` " with hash " `T.append` (T.pack $ show func2Hash)]
          
          validationModuleHash = hashModule validationModuleName [] validationModuleBody
          transformModuleHash = hashModule transformModuleName [] transformModuleBody
          
          validationModuleDef = CodeDefinition validationModuleHash validationModuleBody ModuleDef
          transformModuleDef = CodeDefinition transformModuleHash transformModuleBody ModuleDef
      
      -- Store the new modules
      _ <- storeDefinition repo validationModuleDef
      _ <- storeDefinition repo transformModuleDef
      registerName repo validationModuleName validationModuleHash
      registerName repo transformModuleName transformModuleHash
      
      -- All versions of the code should be accessible and correct
      oldResult <- lookupByName repo oldModuleName
      validationResult <- lookupByName repo validationModuleName
      transformResult <- lookupByName repo transformModuleName
      
      case oldResult of
        Just def -> do
          cdSource def `textShouldContain` T.pack (show func1Hash)
          cdSource def `textShouldContain` T.pack (show func2Hash)
        Nothing -> expectationFailure "Original module not found"
      
      case validationResult of
        Just def -> cdSource def `textShouldContain` T.pack (show func1Hash)
        Nothing -> expectationFailure "Validation module not found"
      
      case transformResult of
        Just def -> cdSource def `textShouldContain` T.pack (show func2Hash)
        Nothing -> expectationFailure "Transformation module not found"

-- | Export the new spec function for use with Spec.hs
spec :: Spec
spec = testContentAddressableSystem 