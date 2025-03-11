{-# LANGUAGE OverloadedStrings #-}

module Core.TEL.ContentAddressableTest (testTELContentAddressable) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import System.Directory (removeDirectoryRecursive)
import System.IO.Temp (withSystemTempDirectory)
import Control.Exception (catch, SomeException)
import Control.Monad (void)

import Core.TEL
import Core.TEL.ContentAddressable
import Core.CodeAddress (Hash)

-- | Main test suite for TEL content addressable integration
testTELContentAddressable :: Spec
testTELContentAddressable = do
  describe "TEL Content Addressable Storage" $ do
    describe "Expression Storage and Retrieval" $ do
      testExpressionStorage
      testExpressionRetrieval
      testExpressionRoundTrip
    
    describe "Program Storage and Retrieval" $ do
      testProgramStorage
      testProgramRetrieval
      testProgramRoundTrip
    
    describe "Hash References" $ do
      testHashReferenceResolution

-- | Test storage of TEL expressions
testExpressionStorage :: Spec
testExpressionStorage = do
  it "stores a simple expression and returns a hash" $ do
    withCleanStore $ do
      let expr = LiteralExpr (IntLiteral 42)
      hash <- storeExpression expr
      hash `shouldSatisfy` (not . null)

  it "stores a complex expression and returns a hash" $ do
    withCleanStore $ do
      let expr = InfixExpr 
                   (LiteralExpr (IntLiteral 5)) 
                   AddOp 
                   (InfixExpr 
                     (LiteralExpr (IntLiteral 3)) 
                     MulOp 
                     (LiteralExpr (IntLiteral 2)))
      hash <- storeExpression expr
      hash `shouldSatisfy` (not . null)

-- | Test retrieval of TEL expressions
testExpressionRetrieval :: Spec
testExpressionRetrieval = do
  it "retrieves a stored expression" $ do
    withCleanStore $ do
      let expr = LiteralExpr (IntLiteral 42)
      hash <- storeExpression expr
      result <- retrieveExpression hash
      result `shouldBe` Right expr

  it "returns an error for non-existent hash" $ do
    withCleanStore $ do
      let nonExistentHash = "0123456789abcdef"
      result <- retrieveExpression nonExistentHash
      case result of
        Left (HashNotFound _) -> return ()
        _ -> expectationFailure "Expected HashNotFound error"

-- | Test round-trip storage and retrieval
testExpressionRoundTrip :: Spec
testExpressionRoundTrip = do
  it "round-trips various expressions" $ do
    withCleanStore $ do
      let expressions = [
              LiteralExpr (IntLiteral 42),
              LiteralExpr (BoolLiteral True),
              LiteralExpr (TextLiteral "Hello"),
              VariableExpr "x",
              LambdaExpr [VarPattern "x"] (VariableExpr "x"),
              InfixExpr 
                (LiteralExpr (IntLiteral 1)) 
                AddOp 
                (LiteralExpr (IntLiteral 2)),
              IfExpr 
                (LiteralExpr (BoolLiteral True)) 
                (LiteralExpr (IntLiteral 1)) 
                (LiteralExpr (IntLiteral 2))
            ]
      
      forM_ expressions $ \expr -> do
        hash <- storeExpression expr
        result <- retrieveExpression hash
        result `shouldBe` Right expr

-- | Test storage of TEL programs
testProgramStorage :: Spec
testProgramStorage = do
  it "stores a simple program and returns a hash" $ do
    withCleanStore $ do
      let prog = Program [
              Definition 
                (Just (TypeSignature "main" (BasicType "Int")))
                (SimpleFunctionDef "main" [] (LiteralExpr (IntLiteral 42)))
            ] Nothing
      hash <- storeProgram prog
      hash `shouldSatisfy` (not . null)

-- | Test retrieval of TEL programs
testProgramRetrieval :: Spec
testProgramRetrieval = do
  it "retrieves a stored program" $ do
    withCleanStore $ do
      let prog = Program [
              Definition 
                (Just (TypeSignature "main" (BasicType "Int")))
                (SimpleFunctionDef "main" [] (LiteralExpr (IntLiteral 42)))
            ] Nothing
      hash <- storeProgram prog
      result <- retrieveProgram hash
      result `shouldBe` Right prog

-- | Test round-trip storage and retrieval of programs
testProgramRoundTrip :: Spec
testProgramRoundTrip = do
  it "round-trips a complex program" $ do
    withCleanStore $ do
      let prog = Program [
              Definition 
                (Just (TypeSignature "add" (FunctionType (BasicType "Int") (FunctionType (BasicType "Int") (BasicType "Int")))))
                (SimpleFunctionDef "add" [VarPattern "x", VarPattern "y"] 
                  (InfixExpr (VariableExpr "x") AddOp (VariableExpr "y"))),
              Definition 
                (Just (TypeSignature "main" (BasicType "Int")))
                (SimpleFunctionDef "main" [] 
                  (ApplicationExpr (VariableExpr "add") [
                    LiteralExpr (IntLiteral 1), 
                    LiteralExpr (IntLiteral 2)
                  ]))
            ] Nothing
      hash <- storeProgram prog
      result <- retrieveProgram prog
      result `shouldBe` Right prog

-- | Test resolution of hash references
testHashReferenceResolution :: Spec
testHashReferenceResolution = do
  it "resolves a hash reference to an expression" $ do
    withCleanStore $ do
      let expr = LiteralExpr (IntLiteral 42)
      hash <- storeExpression expr
      let hashRef = HashRefExpr (T.pack hash)
      result <- resolveHashReference hashRef
      result `shouldBe` Right expr

-- | Helper to run a test with a clean storage directory
withCleanStore :: IO a -> IO a
withCleanStore action = do
  withSystemTempDirectory "tel-test" $ \dir -> do
    let originalDir = telStorageDir
    -- Use the temporary directory for storage
    let telStorageDir = dir
    result <- action
    -- Clean up
    catch (removeDirectoryRecursive dir) (\(_ :: SomeException) -> return ())
    return result

-- Helper for monadic operations
forM_ :: (Monad m) => [a] -> (a -> m b) -> m ()
forM_ xs f = sequence_ (map f xs) 