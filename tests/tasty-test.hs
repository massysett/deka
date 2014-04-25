{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import Deka.Decoded.Generators
import Deka.Internal.Quad.Decoding.Generators
import Deka.Internal.Quad.Tests.Util
import Deka.Internal.Context.Generators
import Deka.Internal.Quad.CtxFree.Tests
import qualified Deka.Internal.Quad.Immutability
import System.Posix.Signals
import Test.Tasty

tests :: TestTree
tests = testGroup "all tests"
  [ Deka.Internal.Quad.Immutability.tests ]

main :: IO ()
main = do
  _ <- installHandler sigFPE Ignore Nothing
  defaultMain tests
