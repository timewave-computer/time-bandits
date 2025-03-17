{-# LANGUAGE OverloadedStrings #-}

module TimeBandits.Core.ContentAddress.SystemSpec (spec) where

import Test.Hspec
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Control.Monad (unless)

import TimeBandits.Core.ContentAddress.Types
import TimeBandits.Core.ContentAddress.Hash
import TimeBandits.Core.ContentAddress.Repository
import TimeBandits.Core.ContentAddress.Util

-- | Main test spec
spec :: Spec
spec = do
  describe "ContentAddress.System" $ do
    describe "Repository and filesystem integration" $ do
      it "can populate a repository from a directory" $ do
        withSystemTempDirectory "ca-test" $ \tempDir -> do
          -- Create a test directory with some code files
          let srcDir = tempDir </> "source"
          createDirectoryIfMissing True srcDir
          
          -- Write a test function file with a proper module declaration
          let funcFile = srcDir </> "test_function.hs"
          writeFile funcFile "module TestFunction where\n\ntestFunction x = x + 1"
          
          -- Write a test module file
          let moduleFile = srcDir </> "test_module.hs"
          writeFile moduleFile "module TestModule where\nimport Data.Text\n\ntestFunc x = x * 2"
          
          -- Create a repository from the directory
          repo <- populateRepositoryFromDirectory srcDir
          
          -- Lookup one of the files by name to verify it was loaded
          result <- lookupByName repo "TestFunction"
          result `shouldSatisfy` maybe False (const True)
      
      it "can add individual files to a repository" $ do
        withSystemTempDirectory "ca-test" $ \tempDir -> do
          -- Create a repository
          repo <- newCodeRepository
          
          -- Create a test file with a proper module declaration
          let testFile = tempDir </> "test.hs"
          writeFile testFile "module TestFunction where\n\ntestFunction x = x + 1"
          
          -- Create a name for the test
          let testName = "TestFunction"
          
          -- Add the file to the repository
          codeHash <- addFileToRepository repo testFile
          
          -- Register a name for the hash
          registerName repo testName codeHash
          
          -- Look up the file by hash
          retrieved <- lookupByHash repo codeHash
          retrieved `shouldSatisfy` maybe False (const True)
          
          -- Look up the file by name
          namedResult <- lookupByName repo testName
          namedResult `shouldSatisfy` maybe False (const True)
    
    describe "Hash consistency" $ do
      it "generates consistent hashes for the same content" $ do
        -- Create some test content
        let content1 = "test content" :: Text
            content2 = "test content" :: Text
            
        -- Generate hashes
        let hash1 = generateCodeHash content1
            hash2 = generateCodeHash content2
            
        -- Hashes should be equal
        hash1 `shouldBe` hash2
        
      it "generates different hashes for different content" $ do
        -- Create different test content
        let content1 = "test content 1" :: Text
            content2 = "test content 2" :: Text
            
        -- Generate hashes
        let hash1 = generateCodeHash content1
            hash2 = generateCodeHash content2
            
        -- Hashes should be different
        hash1 `shouldNotBe` hash2 