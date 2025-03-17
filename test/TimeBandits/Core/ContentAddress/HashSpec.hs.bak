{-# LANGUAGE OverloadedStrings #-}

module TimeBandits.Core.ContentAddress.HashSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

import TimeBandits.Core.ContentAddress.Hash
import TimeBandits.Core.ContentAddress.Types

spec :: Spec
spec = do
  describe "ContentAddress.Hash" $ do
    describe "generateCodeHash" $ do
      it "generates hashes consistently for the same content" $ do
        let content1 = "test content" :: Text
            content2 = "test content" :: Text
            content3 = "different content" :: Text
            
            hash1 = generateCodeHash content1
            hash2 = generateCodeHash content2
            hash3 = generateCodeHash content3
            
        hash1 `shouldBe` hash2
        hash1 `shouldNotBe` hash3

    describe "hashFunction" $ do
      it "hashes functions consistently" $ do
        let name = "testFunction" :: Text
            body = "testFunction :: Int -> Int\ntestFunction x = x + 1" :: Text
            
            hash1 = hashFunction name body
            hash2 = hashFunction name body
            hash3 = hashFunction "otherFunction" body
            
        hash1 `shouldBe` hash2
        hash1 `shouldNotBe` hash3

    describe "hashModule" $ do
      it "hashes modules consistently" $ do
        let name = "TestModule" :: Text
            imports = ["import Data.Text", "import Control.Monad"] :: [Text]
            body = "module TestModule where\n\nfunc :: Int -> Int\nfunc x = x * 2" :: Text
            
            hash1 = hashModule name imports body
            hash2 = hashModule name imports body
            hash3 = hashModule "OtherModule" imports body
            
        hash1 `shouldBe` hash2
        hash1 `shouldNotBe` hash3 