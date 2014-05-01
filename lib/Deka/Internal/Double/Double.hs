{-# LANGUAGE Trustworthy #-}
-- | Internal types - for Double use only
--
-- This module is not listed for export in the cabal file.  It
-- contains types that library users have no access to, but which
-- are needed by multiple Deka modules or that the test suite needs
-- access to.
module Deka.Internal.Double.Double where

import Foreign.Safe
import Foreign.C
import qualified Data.ByteString.Char8 as BS8
import Deka.Internal.Decnumber.DecDouble
import Deka.Internal.Unsafe
import Prelude hiding (Double)

-- # Helpers

type Boolean
  = Ptr C'decDouble
  -> IO Word32

boolean
  :: Boolean
  -> Double
  -> IO Bool
boolean f d =
  withForeignPtr (unDouble d) $ \pD ->
  f pD >>= \r ->
  return $ case r of
    1 -> True
    0 -> False
    _ -> error "boolean: bad return value"

-- | Creates a new Double.  Uninitialized, so don't export this
-- function.
newDouble :: IO Double
newDouble = fmap Double (mallocForeignPtrBytes c'decDouble'sizeOf)

type BinaryCtxFree
  = Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> IO (Ptr C'decDouble)

binaryCtxFree
  :: BinaryCtxFree
  -> Double
  -> Double
  -> IO Double
binaryCtxFree f x y =
  newDouble >>= \r ->
  withForeignPtr (unDouble r) $ \pR ->
  withForeignPtr (unDouble x) $ \pX ->
  withForeignPtr (unDouble y) $ \pY ->
  f pR pX pY >>
  return r

-- | Decimal number.  As indicated in the General Decimal
-- Arithmetic specification, a 'Double' might be a finite number
-- (perhaps the most common type) or it might be infinite or a
-- not-a-number.  'decClass' will tell you a little more about a
-- particular 'Double'.
newtype Double = Double { unDouble :: ForeignPtr C'decDouble }

-- | The Eq instance depends on an IEEE 754 total ordering.  In
-- particular, note that, for example, @7.5@ is not equal to @7.50@.
-- See
--
-- <http://speleotrove.com/decimal/decifaq4.html#order>

instance Eq Double where
  x == y = case compareTotal x y of
    EQ -> True
    _ -> False

-- | Like the 'Eq' instance, this uses an IEEE 754 total ordering.
instance Ord Double where
  compare = compareTotal

-- | The Show instance uses 'toByteString'.
instance Show Double where
  show = BS8.unpack . toByteString

-- | Converts a 'Double' to a string.  May use non-scientific
-- notation, but only if that's unambiguous; otherwise, uses
-- scientific notation.
--
-- In the decNumber C library, this is called @toString@; the name
-- was changed here because this function doesn't return a Haskell
-- 'String'.
toByteStringIO :: Double -> IO BS8.ByteString
toByteStringIO = mkString c'decDoubleToString

toByteString :: Double -> BS8.ByteString
toByteString = unsafe1 toByteStringIO

type MkString
  = Ptr C'decDouble
  -> CString
  -> IO CString

mkString
  :: MkString
  -> Double
  -> IO BS8.ByteString
mkString f d =
  withForeignPtr (unDouble d) $ \pD ->
  allocaBytes c'DECDOUBLE_String $ \pS ->
  f pD pS
  >> BS8.packCString pS

-- | Compares using an IEEE 754 total ordering, which takes into
-- account the exponent.  IEEE 754 says that this function might
-- return different results depending upon whether the operands are
-- canonical; 'Double' are always canonical so you don't need to worry
-- about that here.
compareTotalIO :: Double -> Double -> IO Ordering
compareTotalIO x y = f >>= getR
  where
    f = binaryCtxFree c'decDoubleCompareTotal x y
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

compareTotal :: Double -> Double -> Ordering
compareTotal = unsafe2 compareTotalIO

-- # Tests

-- | True only if @x@ is less than zero and is not an NaN.
isNegative :: Double -> IO Bool
isNegative = boolean c'decDoubleIsNegative

-- | True only if @x@ is a zero.
isZero :: Double -> IO Bool
isZero = boolean c'decDoubleIsZero

-- | True only if @x@ is greater than zero and is not an NaN.
isPositive :: Double -> IO Bool
isPositive = boolean c'decDoubleIsPositive

