module Main where

import qualified Deka.Internal.Quad.Immutability
import qualified Deka.Internal.Double.Immutability
import qualified Deka.Internal.Single.Immutability
import System.Posix.Signals
import Test.Tasty
import qualified Deka.Fixed.Quad.Tests
import qualified Deka.Fixed.Double.Tests
import qualified Deka.Fixed.Single.Tests
import qualified Dectest.Binary

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
