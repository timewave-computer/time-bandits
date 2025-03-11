{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Core.TEL.Parser
Description : Parser for the Temporal Effect Language
Copyright   : (c) Time Bandits, 2023-2024
License     : MIT
Maintainer  : time-bandits@example.com

This module provides a parser for the Temporal Effect Language (TEL) as defined in ADR-013.
It converts text to an AST representation using Megaparsec.
-}
module Core.TEL.Parser
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

import Core.TEL.AST

-- | Parser type for TEL
type Parser = Parsec Void Text

-- | Parse error type
type ParseError = ParseErrorBundle Text Void

-- | Parse a TEL program from text
parseTEL :: FilePath -> Text -> Either ParseError Program
parseTEL sourceName input = 
  parse programP sourceName input

-- | Parse a TEL program from a file
parseFile :: FilePath -> IO (Either ParseError Program)
parseFile path = do
  content <- TIO.readFile path
  return $ parseTEL path content

-- | Parse a TEL expression from text
parseExpr :: Text -> Either ParseError Expression
parseExpr = parse (spaceConsumer *> expressionP <* eof) "<expression>"

-- | Parse a complete TEL program
programP :: Parser Program
programP = do
  spaceConsumer
  defs <- many definitionP
  eof
  sourcePath <- getSourcePos >>= \pos -> return $ Just $ sourceName pos
  return $ Program defs sourcePath

-- | Parse a TEL definition
definitionP :: Parser Definition
definitionP = do
  sig <- optional typeSignatureP
  func <- functionDefP
  return $ Definition sig func

-- | Parse a type signature
typeSignatureP :: Parser TypeSignature
typeSignatureP = do
  name <- identifierP
  symbol "::"
  ty <- typeExprP
  return $ TypeSignature name ty

-- | Parse a function definition
functionDefP :: Parser FunctionDef
functionDefP = choice
  [ try simpleFunctionDefP
  , patternMatchingFunctionDefP
  ]

-- | Parse a simple function definition
simpleFunctionDefP :: Parser FunctionDef
simpleFunctionDefP = do
  name <- identifierP
  params <- MP.many patternP
  symbol "="
  body <- expressionP
  return $ SimpleFunctionDef name params body

-- | Parse a pattern matching function definition
patternMatchingFunctionDefP :: Parser FunctionDef
patternMatchingFunctionDefP = do
  name <- identifierP
  patternsList <- MP.many patternP
  guardedExprs <- guardedExprP
  return $ GuardedFunctionDef name patternsList guardedExprs

-- | Parse a pattern
patternP :: Parser Pattern
patternP = choice
  [ VarPattern <$> identifierP
  , LiteralPattern <$> literalP
  , WildcardPattern <$ symbol "_"
  , ConstructorPattern "Tuple" <$> between (symbol "(") (symbol ")") patternListP
  ]
  where
    patternListP = do
      first <- patternP
      rest <- MP.many (symbol "," *> patternP)
      return (first : rest)

-- | Parse a guarded expression
guardedExprP :: Parser [GuardedExpr]
guardedExprP = choice
  [ do
      symbol "="
      expr <- expressionP
      return [Guard expr expr]
  , MP.some $ do
      symbol "|"
      cond <- expressionP
      symbol "="
      expr <- expressionP
      return $ Guard cond expr
  ]

-- | Parse a let declaration
declarationP :: Parser Declaration
declarationP = do
  name <- identifierP
  symbol "="
  expr <- expressionP
  return $ Declaration name expr

-- | Parse a type expression
typeExprP :: Parser TypeExpr
typeExprP = do
  t <- simpleTypeExprP
  option t $ do
    symbol "->"
    ret <- typeExprP
    return $ FunctionType t ret

-- | Parse a simple type expression
simpleTypeExprP :: Parser TypeExpr
simpleTypeExprP = choice
  [ BasicType <$> identifierP
  , TupleType <$> between (symbol "(") (symbol ")") typeListP
  ]
  where
    typeListP = do
      first <- typeExprP
      rest <- MP.some $ symbol "," *> typeExprP
      return (first : rest)

-- | Parse an expression
expressionP :: Parser Expression
expressionP = do
  expr <- simpleExprP
  option expr $ choice
    [ do
        op <- operatorP
        rhs <- expressionP
        return $ InfixExpr expr op rhs
    , do
        args <- MP.some simpleExprP
        return $ ApplicationExpr expr args
    ]

-- | Parse a simple expression
simpleExprP :: Parser Expression
simpleExprP = choice
  [ LiteralExpr <$> literalP
  , try $ do
      symbol "("
      e <- expressionP
      symbol ")"
      return e
  , VariableExpr <$> identifierP
  , LambdaExpr <$> (symbol "\\" *> MP.some patternP) <*> (symbol "->" *> expressionP)
  , LetExpr <$> (symbol "let" *> MP.some declarationP) <*> (symbol "in" *> expressionP)
  , IfExpr <$> (symbol "if" *> expressionP) <*> (symbol "then" *> expressionP) <*> (symbol "else" *> expressionP)
  , CaseExpr <$> (symbol "case" *> expressionP <* symbol "of") <*> (MP.many $ do
      pattern <- patternP
      expr <- expressionP
      return (pattern, expr))
  , TimeExpr <$> timeExprP
  , EffectExpr <$> effectExprP
  , DoExpr <$> (symbol "do" *> MP.some doStatementP)
  , HashRefExpr <$> (symbol "#" *> (TE.encodeUtf8 <$> hashP))
  ]

-- | Parse a hash reference
hashP :: Parser Text
hashP = do
  hashStr <- MP.many hexDigitChar
  case hashStr of
    [] -> fail "Empty hash"
    _ -> return $ T.pack hashStr

-- | Parse a time expression
timeExprP :: Parser TimeExpr
timeExprP = choice
  [ afterExprP
  , withinExprP
  , atExprP
  ]

-- | Parse an after expression
afterExprP :: Parser TimeExpr
afterExprP = AfterExpr <$> (symbol "after" *> expressionP) <*> timeUnitP <*> expressionP

-- | Parse a within expression
withinExprP :: Parser TimeExpr
withinExprP = WithinExpr <$> (symbol "within" *> expressionP) <*> timeUnitP <*> expressionP

-- | Parse an at expression
atExprP :: Parser TimeExpr
atExprP = AtExpr <$> (symbol "at" *> expressionP) <*> expressionP

-- | Parse a time unit
timeUnitP :: Parser TimeUnit
timeUnitP = choice
  [ Seconds <$ symbol "seconds"
  , Minutes <$ symbol "minutes"
  , Hours <$ symbol "hours"
  , Days <$ symbol "days"
  ]

-- | Parse an effect expression
effectExprP :: Parser EffectExpr
effectExprP = choice
  [ DepositExpr <$> (symbol "deposit" *> expressionP) <*> (symbol "to" *> expressionP) <*> (symbol "timeline" *> expressionP)
  , WithdrawExpr <$> (symbol "withdraw" *> expressionP) <*> (symbol "from" *> expressionP) <*> (symbol "timeline" *> expressionP)
  , TransferExpr <$> (symbol "transfer" *> expressionP) <*> (symbol "from" *> expressionP) <*> (symbol "to" *> expressionP) <*> (symbol "timeline" *> expressionP)
  , InvokeExpr <$> (symbol "invoke" *> expressionP)
  ]

-- | Parse a do statement
doStatementP :: Parser DoStatement
doStatementP = choice
  [ BindStmt <$> (identifierP <* symbol "<-") <*> expressionP
  , ExprStmt <$> expressionP
  , LetStmt <$> (symbol "let" *> MP.some declarationP)
  ]

-- | Parse an operator
operatorP :: Parser Operator
operatorP = choice
  [ AddOp <$ symbol "+"
  , SubOp <$ symbol "-"
  , MulOp <$ symbol "*"
  , DivOp <$ symbol "/"
  , EqOp <$ symbol "=="
  , NeqOp <$ symbol "/="
  , LtOp <$ symbol "<"
  , GtOp <$ symbol ">"
  , LeOp <$ symbol "<="
  , GeOp <$ symbol ">="
  , AndOp <$ symbol "&&"
  , OrOp <$ symbol "||"
  ]

-- | Parse a literal expression
literalP :: Parser LiteralExpr
literalP = choice
  [ IntLiteral <$> lexeme L.decimal
  , DoubleLiteral <$> lexeme L.float
  , TextLiteral <$> lexeme (T.pack <$> (char '"' *> manyTill L.charLiteral (char '"')))
  , BoolLiteral <$> (true <|> false)
  ]
  where
    true = symbol "True" $> True
    false = symbol "False" $> False

-- Lexer Helpers

-- | Space consumer
spaceConsumer :: Parser ()
spaceConsumer = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

-- | Lexeme parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | Symbol parser
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

-- | Reserved word
reserved :: Text -> Parser ()
reserved w = lexeme $ string w *> notFollowedBy alphaNumChar

-- | Parse an identifier
identifierP :: Parser Text
identifierP = lexeme $ do
  first <- satisfy (\c -> isAlpha c || c == '_')
  rest <- MP.many (satisfy (\c -> isAlphaNum c || c == '_'))
  let ident = T.pack (first : rest)
  if ident `elem` keywords
    then fail $ "Keyword " ++ T.unpack ident ++ " cannot be used as an identifier"
    else return ident

-- | Parse a type identifier
typeIdentifierP :: Parser Text
typeIdentifierP = lexeme $ do
  first <- satisfy isAlpha
  rest <- MP.many (satisfy (\c -> isAlphaNum c || c == '_'))
  return $ T.pack (first : rest)

-- | Keywords that cannot be used as identifiers
keywords :: [Text]
keywords =
  [ "let", "in", "if", "then", "else", "case", "of", "do"
  , "after", "within", "at", "seconds", "minutes", "hours", "days"
  , "deposit", "withdraw", "transfer", "invoke", "to", "from", "timeline"
  ] 