{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : TimeBandits.Core.TEL.Parser
Description : Parser for the Temporal Effect Language
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides a parser for the Temporal Effect Language (TEL) as defined in ADR-013.
It converts text to an AST representation using Megaparsec.
-}
module TimeBandits.Core.TEL.Parser
  ( -- * Parsing Functions
    parseTEL
  , parseFile
  , parseExpr
  
  -- * Error Types
  , ParseError
  , errorBundlePretty
  ) where

import Control.Monad (void)
import Control.Applicative (empty, (<|>))
import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Char (isAlphaNum, isAlpha, isDigit)
import Data.Functor (($>))
import Data.List (foldl')
import Data.Maybe (catMaybes)
import Text.Megaparsec hiding (many, some, ParseError)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Hex as Hex
import qualified Data.Text.Encoding as TE

import TimeBandits.Core.TEL.AST
import qualified TimeBandits.Core.TEL.AST as AST

-- | Parser type for TEL
type Parser = Parsec Void Text

-- | Parse error type
type ParseError = MP.ParseErrorBundle Text Void

-- | Lexer configuration for whitespace handling
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "--"
    blockComment = L.skipBlockComment "{-" "-}"

-- | Consume whitespace after a lexeme
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a string and consume trailing whitespace
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parse a reserved word
reserved :: Text -> Parser ()
reserved w = lexeme $ string w *> notFollowedBy alphaNumChar

-- | Parse contents within parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse contents within braces
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Parse contents within brackets
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Parse an integer
integer :: Parser Integer
integer = lexeme L.decimal

-- | Parse a double
double :: Parser Double
double = lexeme L.float

-- | Parse a string literal
stringLiteral :: Parser Text
stringLiteral = lexeme $ do
  void $ char '"'
  s <- manyTill L.charLiteral (char '"')
  return $ T.pack s

-- | Parse an identifier
identifier :: Parser Text
identifier = lexeme $ do
  first <- satisfy (\c -> isAlpha c || c == '_')
  rest <- many (satisfy (\c -> isAlphaNum c || c == '_'))
  let ident = T.pack (first:rest)
  if ident `elem` reservedWords
    then fail $ "Keyword " ++ T.unpack ident ++ " cannot be used as an identifier"
    else return ident

-- | Reserved words that cannot be used as identifiers
reservedWords :: [Text]
reservedWords =
  [ "let", "in", "if", "then", "else", "case", "of", "after", "within"
  , "at", "deposit", "withdraw", "transfer", "observe", "emit", "invoke"
  , "True", "False", "seconds", "minutes", "hours", "days"
  , "do", "where"
  ]

-- Table for operator symbols and their corresponding AST operators
operatorTable :: [(Text, AST.Operator)]
operatorTable =
  [ ("*", AST.MulOp), ("/", AST.DivOp)
  , ("+", AST.AddOp), ("-", AST.SubOp)
  , ("==", AST.EqOp), ("!=", AST.NeqOp), ("<", AST.LtOp), (">", AST.GtOp), ("<=", AST.LeOp), (">=", AST.GeOp)
  ]

operatorParser :: Parser AST.Operator
operatorParser = choice [symbol op $> op' | (op, op') <- operatorTable]

-- | Parse a TEL program from text
parseTEL :: FilePath -> Text -> Either ParseError Program
parseTEL = runParser programParser

-- | Program parser (placeholder - to be implemented)
programParser :: Parser Program
programParser = undefined -- Complete implementation will be added here

-- | Parse a TEL program from a file
parseFile :: FilePath -> IO (Either ParseError Program)
parseFile file = parseTEL file <$> TIO.readFile file

-- | Parse an expression from text
parseExpr :: Text -> Either ParseError Expression
parseExpr = runParser (between sc eof expressionParser) ""

-- | Expression parser (placeholder - to be implemented)
expressionParser :: Parser Expression
expressionParser = undefined -- Complete implementation will be added here

-- | Helper operators for expression parsing
makeExprParser :: Parser Expression -> [[ExprOperator Parser Expression]] -> Parser Expression
makeExprParser = undefined -- Implementation would be added here

-- | Types of operators
data ExprOperator m a
  = InfixN (m (a -> a -> a))
  | InfixL (m (a -> a -> a))
  | InfixR (m (a -> a -> a))

-- Remove the redundant instance that's already provided by Megaparsec 