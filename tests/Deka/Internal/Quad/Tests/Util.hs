-- | Test utilities

module Deka.Internal.Quad.Tests.Util where

import Deka.Internal.Decnumber.DecQuad
import Deka.Internal.Quad.Quad
import Deka.Internal.Quad.Decoding
import Deka.Internal.Quad.Decoding.Generators
import Foreign.Safe
import qualified Data.ByteString as BS
import Test.QuickCheck
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty (testGroup, TestTree)
import Test.QuickCheck.Monadic

-- | Verify that the contents of a Quad did not change.

noChange
  :: Quad
  -- ^ Quad to inspect
  -> IO a
  -- ^ Run this IO action after initial inspection of the quad
  -> IO Bool
  -- ^ True if the Quad did NOT change after running the IO action.
  -- False if the Quad changed.
noChange q a =
  withForeignPtr (unQuad q) $ \ptr ->
  BS.packCStringLen (castPtr ptr, c'decQuad'sizeOf) >>= \before ->
  a >>
  BS.packCStringLen (castPtr ptr, c'decQuad'sizeOf) >>= \after ->
  return (before == after)

pNoChange :: Quad -> IO a -> Property
pNoChange q a = monadicIO $ do
  r <- run $ noChange q a
  assert r

-- | Unary function does not change first argument
unary
  :: (Quad -> IO a)
  -> TestTree
unary f = testProperty desc test
  where
    desc = "unary function - does not change only argument"
    test = forAll genDecoded $ \dcd -> monadicIO $ do
      q <- run $ fromBCD dcd
      r <- run $ noChange q (f q)
      assert r

binary
  :: (Quad -> Quad -> IO a)
  -> TestTree
binary f = testGroup desc tests
  where
    desc = "binary function - does not change argument:"
    tests = [ testProperty "first" t1, testProperty "second" t2]
    t1 = forAll genDecoded $ \dcd1 ->
      forAll genDecoded $ \dcd2 ->
      monadicIO $
      run (fromBCD dcd1) >>= \q1 ->
      run (fromBCD dcd2) >>= \q2 ->
      run (noChange q1 (f q1 q2)) >>=
      assert

    t2 = forAll genDecoded $ \dcd1 ->
      forAll genDecoded $ \dcd2 ->
      monadicIO $
      run (fromBCD dcd1) >>= \q1 ->
      run (fromBCD dcd2) >>= \q2 ->
      run (noChange q2 (f q1 q2)) >>=
      assert

ternary
  :: (Quad -> Quad -> Quad -> IO a)
  -> TestTree
ternary f = testGroup desc tests
  where
    desc = "ternary function - does not change argument:"
    tests = [ testProperty "first" t1, testProperty "second" t2,
              testProperty "third" t3]
    t1 = forAll genDecoded $ \dcd1 ->
      forAll genDecoded $ \dcd2 ->
      forAll genDecoded $ \dcd3 ->
      monadicIO $
      run (fromBCD dcd1) >>= \q1 ->
      run (fromBCD dcd2) >>= \q2 ->
      run (fromBCD dcd3) >>= \q3 ->
      run (noChange q1 (f q1 q2 q3)) >>=
      assert

    t2 = forAll genDecoded $ \dcd1 ->
      forAll genDecoded $ \dcd2 ->
      forAll genDecoded $ \dcd3 ->
      monadicIO $
      run (fromBCD dcd1) >>= \q1 ->
      run (fromBCD dcd2) >>= \q2 ->
      run (fromBCD dcd3) >>= \q3 ->
      run (noChange q2 (f q1 q2 q3)) >>=
      assert

    t3 = forAll genDecoded $ \dcd1 ->
      forAll genDecoded $ \dcd2 ->
      forAll genDecoded $ \dcd3 ->
      monadicIO $
      run (fromBCD dcd1) >>= \q1 ->
      run (fromBCD dcd2) >>= \q2 ->
      run (fromBCD dcd3) >>= \q3 ->
      run (noChange q3 (f q1 q2 q3)) >>=
      assert

