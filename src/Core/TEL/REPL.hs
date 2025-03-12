{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Core.TEL.REPL
  ( -- * REPL
    runREPL
  , REPLState(..)
  , Command(..)
  ) where

import Prelude hiding (get, modify, runReader, evalState)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import System.Console.Haskeline (InputT, defaultSettings, runInputT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Core.TEL.AST (Identifier, Program)
import Core.TEL.Parser (parseExpr, errorBundlePretty)
import qualified Core.TEL.Interpreter as Interp
import Core.TEL.PrettyPrinter
import qualified Core.TEL.TypeChecker as TC
import Core.Common (LamportTime(..))
import qualified Data.ByteString.Char8 as C8
import Control.Monad (foldM)
import qualified Core.CodeAddress as CA

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)

-- | REPL state
data REPLState = REPLState
  { replEnv :: Map Identifier Interp.Value
  , replPrograms :: Map Text Program
  , replHashes :: Map Text Text
  }

-- | Initial REPL state
initialREPLState :: REPLState
initialREPLState = REPLState
  { replEnv = Map.empty
  , replPrograms = Map.empty
  , replHashes = Map.empty
  }

-- | REPL commands
data Command
  = Eval Text
  | TypeOf Text
  | Load FilePath
  | Save Text Text
  | Quit
  | Help
  | RunProgram Text
  | StoreHash Text
  | ListHashes
  | ListPrograms
  | Unknown Text
  deriving (Show, Eq)

-- | Run the REPL
runREPL :: IO ()
runREPL = runInputT defaultSettings $ evalStateT (repl True) initialREPLState

-- | REPL loop
repl :: Bool -> StateT REPLState (InputT IO) ()
repl _ = return ()

-- | Evaluate an expression in the REPL
evaluateExpression :: Text -> Map Identifier Interp.Value -> IO (Either Interp.InterpreterError Interp.Value)
evaluateExpression input _ = 
  case parseExpr input of
    Left err -> return $ Left $ Interp.InternalError $ T.pack $ errorBundlePretty err
    Right _ -> return $ Right $ Interp.VText "Not implemented" 