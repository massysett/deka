{-# LANGUAGE Trustworthy #-}
-- | Internal types - for Quad use only
--
-- This module is not listed for export in the cabal file.  It
-- contains types that library users have no access to, but which
-- are needed by multiple Deka modules or that the test suite needs
-- access to.
module Deka.Internal.Quad.Quad where

import Foreign.Safe
import Foreign.C
import qualified Data.ByteString.Char8 as BS8
import Deka.Internal.Decnumber.DecQuad
import Deka.Internal.Unsafe

-- # Helpers

type Boolean
  = Ptr C'decQuad
  -> IO Word32

boolean
  :: Boolean
  -> Quad
  -> IO Bool
boolean f d =
  withForeignPtr (unQuad d) $ \pD ->
  f pD >>= \r ->
  return $ case r of
    1 -> True
    0 -> False
    _ -> error "boolean: bad return value"

-- | Creates a new Quad.  Uninitialized, so don't export this
-- function.
newQuad :: IO Quad
newQuad = fmap Quad (mallocForeignPtrBytes c'decQuad'sizeOf)

type BinaryCtxFree
  = Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO (Ptr C'decQuad)

binaryCtxFree
  :: BinaryCtxFree
  -> Quad
  -> Quad
  -> IO Quad
binaryCtxFree f x y =
  newQuad >>= \r ->
  withForeignPtr (unQuad r) $ \pR ->
  withForeignPtr (unQuad x) $ \pX ->
  withForeignPtr (unQuad y) $ \pY ->
  f pR pX pY >>
  return r

-- | Decimal number.  As indicated in the General Decimal
-- Arithmetic specification, a 'Quad' might be a finite number
-- (perhaps the most common type) or it might be infinite or a
-- not-a-number.  'decClass' will tell you a little more about a
-- particular 'Quad'.
newtype Quad = Quad { unQuad :: ForeignPtr C'decQuad }

-- | The Eq instance depends on an IEEE 754 total ordering.  In
-- particular, note that, for example, @7.5@ is not equal to @7.50@.
-- See
--
-- <http://speleotrove.com/decimal/decifaq4.html#order>

instance Eq Quad where
  x == y = case compareTotal x y of
    EQ -> True
    _ -> False

-- | Like the 'Eq' instance, this uses an IEEE 754 total ordering.
instance Ord Quad where
  compare = compareTotal

-- | The Show instance uses 'toByteString'.
instance Show Quad where
  show = BS8.unpack . toByteString

-- | Converts a 'Quad' to a string.  May use non-scientific
-- notation, but only if that's unambiguous; otherwise, uses
-- scientific notation.
--
-- In the decNumber C library, this is called @toString@; the name
-- was changed here because this function doesn't return a Haskell
-- 'String'.
toByteStringIO :: Quad -> IO BS8.ByteString
toByteStringIO = mkString c'decQuadToString

toByteString :: Quad -> BS8.ByteString
toByteString = unsafe1 toByteStringIO

type MkString
  = Ptr C'decQuad
  -> CString
  -> IO CString

mkString
  :: MkString
  -> Quad
  -> IO BS8.ByteString
mkString f d =
  withForeignPtr (unQuad d) $ \pD ->
  allocaBytes c'DECQUAD_String $ \pS ->
  f pD pS
  >> BS8.packCString pS

-- | Compares using an IEEE 754 total ordering, which takes into
-- account the exponent.  IEEE 754 says that this function might
-- return different results depending upon whether the operands are
-- canonical; 'Quad' are always canonical so you don't need to worry
-- about that here.
compareTotalIO :: Quad -> Quad -> IO Ordering
compareTotalIO x y = f >>= getR
  where
    f = binaryCtxFree c'decQuadCompareTotal x y
    getR c = switchM
      [(isNegative c, LT), (isZero c, EQ), (isPositive c, GT)]
      (error "compareTotal: unknown result")

switchM
  :: Monad m
  => [(m Bool, a)]
  -- ^ Test for truth; return a for first True value
  -> a
  -- ^ Return this if no true value found
  -> m a
switchM ls a = case ls of
  [] -> return a
  (act, r):ps -> do
    b <- act
    if b then return r else switchM ps a

compareTotal :: Quad -> Quad -> Ordering
compareTotal = unsafe2 compareTotalIO

-- # Tests

-- | True only if @x@ is less than zero and is not an NaN.
isNegative :: Quad -> IO Bool
isNegative = boolean c'decQuadIsNegative

-- | True only if @x@ is a zero.
isZero :: Quad -> IO Bool
isZero = boolean c'decQuadIsZero

-- | True only if @x@ is greater than zero and is not an NaN.
isPositive :: Quad -> IO Bool
isPositive = boolean c'decQuadIsPositive

