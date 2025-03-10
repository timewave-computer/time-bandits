{- |
Module: Main
Description: Main entry point for the Time-Bandits application.
-}
module Main (main) where

import qualified System.IO as IO
import qualified System.Environment as Env
import qualified System.Exit as Exit

-- | Simple main function that handles basic commands
main :: IO ()
main = do
  IO.putStrLn "Time Bandits CLI"
  args <- Env.getArgs
  case args of
    ["version"] -> do
      IO.putStrLn "Time Bandits version 0.1.0"
      Exit.exitSuccess
    ["help"] -> do
      IO.putStrLn "Usage: time-bandits [command]"
      IO.putStrLn ""
      IO.putStrLn "Available commands:"
      IO.putStrLn "  help      Show this help message"
      IO.putStrLn "  version   Show version information"
      Exit.exitSuccess
    _ -> do
      IO.putStrLn "Unknown command. Try 'time-bandits help' for usage information."
      Exit.exitFailure 