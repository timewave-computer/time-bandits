{-# LANGUAGE OverloadedStrings #-}

module TimeBandits.Core.ContentAddress.TypesSpec (spec) where

import Test.Hspec
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T

import TimeBandits.Core.ContentAddress.Types
import TimeBandits.Core.Common.Types (Hash(..))

spec :: Spec
spec = do
  describe "ContentAddress.Types" $ do
    describe "CodeHash" $ do
      it "can be created from a ByteString" $ do
        let bs = "test-hash-content" :: ByteString
            hash = mkCodeHash bs
        unCodeHash hash `shouldBe` Hash bs

    describe "DefType" $ do
      it "has different constructors for different definition types" $ do
        ModuleDef `shouldNotBe` FunctionDef
        FunctionDef `shouldNotBe` TypeDef
        TypeDef `shouldNotBe` ClassDef
        ClassDef `shouldNotBe` InstanceDef

    describe "HashAlgorithm" $ do
      it "supports multiple hash algorithms" $ do
        SHA256 `shouldNotBe` Blake2b
        Blake2b `shouldNotBe` SHA3_256
        SHA3_256 `shouldNotBe` SHA256 