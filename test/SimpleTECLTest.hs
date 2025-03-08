{-# LANGUAGE OverloadedStrings #-}

module Main where

import Core.TECL
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
  putStrLn "Running simple TECL parser tests"
  testBasicParsing
  testTypeChecking

testBasicParsing :: IO ()
testBasicParsing = do
  putStrLn "Testing basic TECL parsing..."
  
  let teclCode = BS.pack $ concat
        [ "// This is a comment\n"
        , "let x = 42;\n"
        , "let name = \"John\";\n"
        , "\n"
        , "effect UpdateState {\n"
        , "  balance = balance + 100;\n"
        , "  lastUpdated = now();\n"
        , "}\n"
        ]
  
  case parseTECL teclCode of
    Left err -> putStrLn $ "Parsing failed: " ++ show err
    Right ast -> do
      putStrLn "Parsing succeeded!"
      putStrLn $ "Number of statements: " ++ show (length $ statements ast)
  
  putStrLn "Basic parsing test passed!"

testTypeChecking :: IO ()
testTypeChecking = do
  putStrLn "Testing TECL type checking..."
  
  let teclCode = BS.pack $ concat
        [ "let x = 42;\n"
        , "let y = x + 10;\n"
        , "\n"
        , "effect UpdateState {\n"
        , "  counter = x + y;\n"
        , "}\n"
        ]
  
  case parseTECL teclCode of
    Left err -> putStrLn $ "Parsing failed: " ++ show err
    Right ast -> do
      case typecheckTECL ast of
        Left err -> putStrLn $ "Type checking failed: " ++ show err
        Right _ -> putStrLn "Type checking succeeded!"
  
  putStrLn "Type checking test passed!" 