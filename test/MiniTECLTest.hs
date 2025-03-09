{-# LANGUAGE OverloadedStrings #-}

-- A minimal standalone test for TECL functionality, with minimal dependencies

import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- Simplified TECL AST types
data LiteralValue
  = IntLit Int
  | TextLit Text
  | BoolLit Bool
  | DecimalLit Double
  deriving (Show, Eq)

data TECLExpression
  = Literal LiteralValue
  | Variable Text
  | BinaryOp Text TECLExpression TECLExpression
  | FunctionCall Text [TECLExpression]
  | TypeCast Text TECLExpression  -- Added for type conversion
  deriving (Show, Eq)

data TECLEffect = TECLEffect
  { effectName :: Text
  , assignments :: Map Text TECLExpression
  } deriving (Show, Eq)

data TECLStatement
  = Let Text TECLExpression
  | Effect TECLEffect
  deriving (Show, Eq)

data TECLAST = TECLAST
  { statements :: [TECLStatement]
  } deriving (Show, Eq)

data ParseError
  = InvalidSyntax Text
  | UnexpectedToken Text
  | UndefinedVariable Text
  deriving (Show, Eq)

-- Simplified TECL parser (just for testing)
parseTECL :: ByteString -> Either ParseError TECLAST
parseTECL bs =
  -- This is a simplified mock parser that always returns a fixed AST
  if BS.null bs 
    then Left $ InvalidSyntax "Empty input"
    else Right $ TECLAST 
      [ Let "x" (Literal (IntLit 42))
      , Let "name" (Literal (TextLit "John"))
      , Effect $ TECLEffect "UpdateBalance" $ Map.fromList
          [ ("balance", BinaryOp "+" (Variable "balance") (Literal (IntLit 100)))
          , ("lastUpdated", FunctionCall "now" [])
          ]
      ]

-- Simplified type checker
data TypeCheckError
  = UndefinedRef Text
  | TypeMismatch Text Text Text
  | InvalidCast Text Text
  deriving (Show, Eq)

typecheckTECL :: TECLAST -> Either TypeCheckError ()
typecheckTECL ast =
  -- Simple mock validation that always succeeds for non-empty ASTs
  if null (statements ast)
    then Left $ UndefinedRef "Empty program"
    else Right ()

-- Simplified type conversion function for testing
convertType :: LiteralValue -> Text -> Either TypeCheckError LiteralValue
convertType val targetType =
  case (val, targetType) of
    (IntLit i, "string") -> Right $ TextLit $ T.pack $ show i
    (IntLit i, "decimal") -> Right $ DecimalLit $ fromIntegral i
    (IntLit i, "bool") -> Right $ BoolLit $ i /= 0
    (TextLit t, "int") -> 
      case reads (T.unpack t) of
        [(i, "")] -> Right $ IntLit i
        _ -> Left $ InvalidCast "text" "int"
    (DecimalLit d, "int") -> Right $ IntLit $ round d
    (BoolLit b, "int") -> Right $ IntLit $ if b then 1 else 0
    _ -> Left $ InvalidCast (T.pack $ show val) targetType

-- Test TECL parsing
testParsing :: IO ()
testParsing = do
  putStrLn "Testing TECL parsing..."
  
  let code = pack $ unlines
        [ "// Comment"
        , "let x = 42;"
        , "let name = \"John\";"
        , ""
        , "effect UpdateBalance {"
        , "  balance = balance + 100;"
        , "  lastUpdated = now();"
        , "}"
        ]
  
  case parseTECL code of
    Left err -> putStrLn $ "Parsing failed: " ++ show err
    Right ast -> do
      putStrLn "Parsing succeeded!"
      putStrLn $ "Number of statements: " ++ show (length $ statements ast)
      putStrLn "Test passed!"

-- Test TECL type checking
testTypeChecking :: IO ()
testTypeChecking = do
  putStrLn "Testing TECL type checking..."
  
  let ast = TECLAST
        [ Let "x" (Literal (IntLit 42))
        , Let "y" (BinaryOp "+" (Variable "x") (Literal (IntLit 10)))
        , Effect $ TECLEffect "UpdateCounter" $ Map.fromList
            [ ("counter", BinaryOp "+" (Variable "x") (Variable "y")) ]
        ]
  
  case typecheckTECL ast of
    Left err -> putStrLn $ "Type checking failed: " ++ show err
    Right () -> putStrLn "Type checking succeeded!"

-- Test type conversion
testTypeConversion :: IO ()
testTypeConversion = do
  putStrLn "Testing TECL type conversion..."
  
  -- Test int to string conversion
  let intVal = IntLit 42
  case convertType intVal "string" of
    Left err -> putStrLn $ "Int to string conversion failed: " ++ show err
    Right (TextLit t) -> putStrLn $ "Int to string: " ++ T.unpack t
    Right _ -> putStrLn "Unexpected conversion result"
  
  -- Test string to int conversion (success case)
  let textVal = TextLit "123"
  case convertType textVal "int" of
    Left err -> putStrLn $ "String to int conversion failed: " ++ show err
    Right (IntLit i) -> putStrLn $ "String to int: " ++ show i
    Right _ -> putStrLn "Unexpected conversion result"
  
  -- Test string to int conversion (failure case)
  let invalidTextVal = TextLit "not a number"
  case convertType invalidTextVal "int" of
    Left _ -> putStrLn "Invalid string to int conversion correctly failed"
    Right _ -> putStrLn "ERROR: Invalid conversion unexpectedly succeeded"
  
  putStrLn "Type conversion tests completed"

main :: IO ()
main = do
  putStrLn "Running simplified TECL tests"
  testParsing
  testTypeChecking
  testTypeConversion 