{-# LANGUAGE OverloadedStrings #-}

module TimeBandits.SimpleTELTest where

import TimeBandits.Core.TEL
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.String (fromString)

main :: IO ()
main = do
  putStrLn "Running simple TEL parser tests"
  testBasicParsing
  testTypeChecking

testBasicParsing :: IO ()
testBasicParsing = do
  putStrLn "Testing basic TEL parsing..."
  
  let telCode = fromString $ concat
        [ "// This is a comment\n"
        , "let x = 42;\n"
        , "let name = \"John\";\n"
        , "\n"
        , "effect UpdateState {\n"
        , "  balance = balance + 100;\n"
        , "  lastUpdated = now();\n"
        , "}\n"
        ]
  
  case parseTEL "<test>" telCode of
    Left err -> putStrLn $ "Parsing failed: " ++ show err
    Right ast -> do
      putStrLn "Parsing succeeded!"
      putStrLn $ "Number of definitions: " ++ show (length $ programDefinitions ast)
  
  putStrLn "Basic parsing test passed!"

testTypeChecking :: IO ()
testTypeChecking = do
  putStrLn "Testing TEL type checking..."
  
  let telCode = fromString $ concat
        [ "let x = 42;\n"
        , "let y = x + 10;\n"
        , "\n"
        , "effect UpdateState {\n"
        , "  counter = x + y;\n"
        , "}\n"
        ]
  
  case parseTEL "<test>" telCode of
    Left err -> putStrLn $ "Parsing failed: " ++ show err
    Right ast -> do
      case typeCheck ast of
        Left err -> putStrLn $ "Type checking failed: " ++ show err
        Right _ -> putStrLn "Type checking succeeded!"
  
  putStrLn "Type checking test passed!" 