module Main where

import qualified Test.Tasty as Tasty
import qualified SimpleNetworkConfig

main :: IO ()
main = Tasty.defaultMain SimpleNetworkConfig.tests 