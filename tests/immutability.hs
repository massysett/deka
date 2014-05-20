module Main where

import System.Posix.Signals
import Test.Tasty
import AllModules ()

tests :: TestTree
tests = testGroup "all tests"
  []

main :: IO ()
main = do
  _ <- installHandler sigFPE Ignore Nothing
  defaultMain tests
