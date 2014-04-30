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
import qualified Deka.Quad.Tests
import Dectest.Parse
import Dectest.Parse.Tokens
import Dectest.Lookup
import Dectest.Lookup.Quad
import qualified Dectest.Binary
import qualified Dectest.Parse.Operand
import qualified Dectest.Parse.Octothorpe
import qualified Dectest.Util
import qualified Dectest

tests :: TestTree
tests = testGroup "all tests"
  [ Deka.Internal.Quad.Immutability.tests
  , Deka.Quad.Tests.tests
  , Dectest.Binary.tests
  ]

main :: IO ()
main = do
  _ <- installHandler sigFPE Ignore Nothing
  defaultMain tests
