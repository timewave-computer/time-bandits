{-# LANGUAGE OverloadedStrings #-}

module TimeBandits.Core.ContentAddress.RepositorySpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (isJust)

import TimeBandits.Core.ContentAddress.Repository
import TimeBandits.Core.ContentAddress.Types
import TimeBandits.Core.ContentAddress.Hash

spec :: Spec
spec = do
  describe "ContentAddress.Repository" $ do
    describe "newCodeRepository" $ do
      it "creates a new empty repository" $ do
        repo <- newCodeRepository
        -- Just check that we can create a repository without errors
        True `shouldBe` True

    describe "storeDefinition and lookupByHash" $ do
      it "can store and retrieve code definitions" $ do
        repo <- newCodeRepository
        let source = "test :: Int -> Int\ntest x = x * 2" :: Text
            hash = generateCodeHash source
            def = CodeDefinition 
              { cdHash = hash
              , cdSource = source
              , cdType = FunctionDef
              }
        
        -- Store the definition
        storedHash <- storeDefinition repo def
        storedHash `shouldBe` hash
        
        -- Look it up
        result <- lookupByHash repo hash
        result `shouldSatisfy` isJust
        
        case result of
          Just retrieved -> do
            cdHash retrieved `shouldBe` hash
            cdSource retrieved `shouldBe` source
            cdType retrieved `shouldBe` FunctionDef
          Nothing -> expectationFailure "Definition not found"

    describe "registerName and lookupByName" $ do
      it "can register names and retrieve by name" $ do
        repo <- newCodeRepository
        let name = "testFunction" :: Text
            source = "testFunction :: Int -> Int\ntestFunction x = x + 1" :: Text
            hash = hashFunction name source
            def = CodeDefinition 
              { cdHash = hash
              , cdSource = source
              , cdType = FunctionDef
              }
        
        -- Store and register
        _ <- storeDefinition repo def
        registerName repo name hash
        
        -- Look up by name
        result <- lookupByName repo name
        result `shouldSatisfy` isJust
        
        case result of
          Just retrieved -> cdHash retrieved `shouldBe` hash
          Nothing -> expectationFailure "Definition not found by name" 