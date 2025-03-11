{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import System.Directory (doesFileExist)
import System.FilePath (takeFileName)
import System.IO (hPutStrLn, stderr)
import qualified System.Exit as Exit
import qualified System.Environment as Env

import Core.CodeAddress (CodeDefinition(..), 
                         newCodeRepository, lookupByHash, lookupByName)
import Core.CodeAddressUtil (addFileToRepository)
import Execution.ContentAddressableExecutor (newExecutor, executeByHash, executeByName,
                                            newContext)

-- | Main entry point
main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    ["store", filePath] -> storeFile filePath
    ["lookup", identifier] -> lookupCode identifier
    ["execute", identifier] -> executeCode identifier
    ["help"] -> showHelp
    _ -> do
      hPutStrLn stderr "Invalid arguments. Use 'help' for usage information."
      Exit.exitFailure

-- | Store a file in the content-addressable repository
storeFile :: FilePath -> IO ()
storeFile filePath = do
  exists <- doesFileExist filePath
  if exists
    then do
      repo <- newCodeRepository
      hash <- addFileToRepository repo filePath
      putStrLn $ "File stored with hash: " ++ show hash
      putStrLn $ "Registered with name: " ++ takeFileName filePath
    else do
      hPutStrLn stderr $ "File not found: " ++ filePath
      Exit.exitFailure

-- | Look up code by name or hash
lookupCode :: String -> IO ()
lookupCode identifier = do
  repo <- newCodeRepository
  
  -- Try to look up by name first
  nameResult <- lookupByName repo (T.pack identifier)
  case nameResult of
    Just def -> do
      putStrLn $ "Found by name: " ++ identifier
      putStrLn $ "Hash: " ++ C8.unpack (cdHash def)
      putStrLn $ "Type: " ++ show (cdType def)
      putStrLn "Source:"
      TIO.putStrLn (cdSource def)
      return ()
    
    Nothing -> do
      -- Try to look up by hash
      hashResult <- lookupByHash repo (C8.pack identifier)
      case hashResult of
        Just def -> do
          putStrLn $ "Found by hash: " ++ identifier
          putStrLn $ "Type: " ++ show (cdType def)
          putStrLn "Source:"
          TIO.putStrLn (cdSource def)
        Nothing -> do
          hPutStrLn stderr $ "Code not found with identifier: " ++ identifier
          Exit.exitFailure

-- | Execute code by name or hash
executeCode :: String -> IO ()
executeCode identifier = do
  repo <- newCodeRepository
  executor <- newExecutor repo
  let context = newContext
  
  -- Try to execute by name first
  nameResult <- executeByName executor (T.pack identifier) context
  case nameResult of
    (Just output, _) -> do
      putStrLn "Executed by name:"
      TIO.putStrLn output
    
    (Nothing, _) -> do
      -- Try to execute by hash
      hashResult <- executeByHash executor (C8.pack identifier) context
      case hashResult of
        (Just output, _) -> do
          putStrLn "Executed by hash:"
          TIO.putStrLn output
        (Nothing, _) -> do
          hPutStrLn stderr $ "Failed to execute code with identifier: " ++ identifier
          Exit.exitFailure

-- | Show help information
showHelp :: IO ()
showHelp = do
  putStrLn "Content-Addressable Code Tool"
  putStrLn "Usage:"
  putStrLn "  content-addressable-tool store <file>      - Store a file in the repository"
  putStrLn "  content-addressable-tool lookup <id>       - Look up code by name or hash"
  putStrLn "  content-addressable-tool execute <id>      - Execute code by name or hash"
  putStrLn "  content-addressable-tool help              - Show this help message" 