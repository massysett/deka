{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import Deka.Decoded.Generators
import Deka.Internal.Quad.Decoding.Generators
import Deka.Internal.Quad.Tests.Util
import Deka.Internal.Context.Generators
import Deka.Internal.Quad.CtxFree.Tests
import qualified Deka.Internal.Quad.Immutability
import qualified Deka.Internal.Double.Immutability
import qualified Deka.Internal.Single.Immutability
import System.Posix.Signals
import Test.Tasty
import qualified Deka.Fixed.Quad.Tests
import qualified Deka.Fixed.Double.Tests
import qualified Deka.Fixed.Single.Tests
import Dectest.Parse
import Dectest.Parse.Tokens
import Dectest.Lookup
import Dectest.Lookup.Quad
import Dectest.Lookup.Double
import qualified Dectest.Binary
import qualified Dectest.Parse.Operand
import qualified Dectest.Interp.Octothorpe
import qualified Dectest.Util
import qualified Dectest
import qualified Dectest.Interp.Operand
import qualified Dectest.Interp.Result
import qualified Dectest.Interp.Directive

tests :: TestTree
tests = testGroup "all tests"
  [ Deka.Internal.Quad.Immutability.tests
  , Deka.Internal.Double.Immutability.tests
  , Deka.Internal.Single.Immutability.tests
  , Deka.Fixed.Quad.Tests.tests
  , Deka.Fixed.Double.Tests.tests
  , Deka.Fixed.Single.Tests.tests
  , Dectest.Binary.tests
  ]

main :: IO ()
main = do
  _ <- installHandler sigFPE Ignore Nothing
  defaultMain tests
