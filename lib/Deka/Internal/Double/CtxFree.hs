{-# LANGUAGE Safe, DeriveDataTypeable #-}

-- | Floating-point decimals.
--
-- This uses the decNumber C library, so you will want to read the
-- documentation about it to fully understand this module:
--
-- <http://speleotrove.com/decimal/decnumber.html>
--
-- <http://speleotrove.com/decimal/decarith.html>
--
-- <http://speleotrove.com/decimal/>
--
-- Many of the comments on what these functions do are taken
-- directly from the documentation for the decNumber C library.
--
-- In particular, this module implements the decDouble type.  decDouble
-- supports up to 34 digits of precision and exponents between -6176
-- and 6111.  It doesn't silently round, overflow, or underflow;
-- rather, the library will notify you if these things happen.
--
-- Many functions in this module clash with Prelude names, so you
-- might want to do
--
-- > import qualified Data.Deka.Double as Q
module Deka.Internal.Double.CtxFree where

-- # Imports

import Control.Monad
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe
import Foreign.Safe hiding
  ( void
  , isSigned
  , rotate
  , shift
  , xor
  )
import Prelude hiding
  ( abs
  , and
  , compare
  , isInfinite
  , isNaN
  , max
  , min
  , or
  , subtract
  , significand
  , exponent
  , Double
  )
import Deka.Internal.Class
import Deka.Context

import Deka.Internal.Decnumber.DecDouble
import Deka.Internal.Decnumber.Decimal128
import Deka.Internal.DecNum.DecNum
import Deka.Internal.DecNum.Util
import Deka.Internal.Double.Double
import Deka.Internal.Double.Ctx (compare)

-- # Helpers.  Do not export these.

type UnaryGet a
  = Ptr C'decDouble
  -> IO a

unaryGet
  :: UnaryGet a
  -> Double
  -> IO a
unaryGet f d =
  withForeignPtr (unDouble d) $ \pD -> f pD

-- # End Helpers

-- # Functions from decDouble. In alphabetical order.


-- | More information about a particular 'Double'.
decClass :: Double -> IO Class
decClass = fmap Class . unaryGet c'decDoubleClass

-- | True for NaNs.
isNaN :: Double -> IO Bool
isNaN = boolean c'decDoubleIsNaN

-- | Wrapper for 'compare' that returns an 'Ordering' rather than a
-- 'Double'.  Returns @Just LT@ rather than -1, @Just EQ@ rather than
-- 0, and @Just GT@ rather than 1, and @Nothing@ rather than NaN.
-- This is a pure function; it does not affect the 'Ctx'.

compareOrd :: Double -> Double -> IO (Maybe Ordering)
compareOrd x y = do
  let c = runCtx initDecimal128 $ compare x y
  switchM [ (isNaN c, Nothing), (isNegative c, Just LT),
            (isZero c, Just EQ), (isPositive c, Just GT)]
          (error "compareOrd: unknown result")


-- | Same as 'compareTotal' but compares the absolute value of the
-- two arguments.
compareTotalMag :: Double -> Double -> IO Ordering
compareTotalMag x y = do
  c <- binaryCtxFree c'decDoubleCompareTotalMag x y
  switchM [ (isNegative c, LT), (isZero c, EQ),
            (isPositive c, GT) ]
          (error "compareTotalMag: unknown result")


-- decNumber's CopySign copies the contents from pS to PN, except
-- that the sign is copied from pP to pN

-- | @copySign x y@ returns @z@, which is a copy of @x@ but has the
-- sign of @y@.  This function never raises any signals.
copySign :: Double -> Double -> IO Double
copySign s p =
  newDouble >>= \n ->
  withForeignPtr (unDouble n) $ \pN ->
  withForeignPtr (unDouble s) $ \pS ->
  withForeignPtr (unDouble p) $ \pP ->
  c'decDoubleCopySign pN pS pP >>
  return n

-- | Number of significant digits.  If zero or infinite, returns 1.
-- If NaN, returns number of digits in the payload.
digits :: Double -> IO Int
digits = fmap fromIntegral . unaryGet c'decDoubleDigits

fromInt32 :: Int32 -> IO Double
fromInt32 i =
  newDouble >>= \r ->
  withForeignPtr (unDouble r) $ \pR ->
  c'decDoubleFromInt32 pR i
  >> return r

fromUInt32 :: Word32 -> IO Double
fromUInt32 i =
  newDouble >>= \r ->
  withForeignPtr (unDouble r) $ \pR ->
  c'decDoubleFromUInt32 pR i >>
  return r

-- | True if @x@ is neither infinite nor a NaN.
isFinite :: Double -> IO Bool
isFinite = boolean c'decDoubleIsFinite

-- | True for infinities.
isInfinite :: Double -> IO Bool
isInfinite = boolean c'decDoubleIsInfinite

-- | True if @x@ is finite and has exponent of @0@; False otherwise.
-- This tests the exponent, not the /adjusted/ exponent.  This can
-- lead to results you may not expect:
--
-- >>> isInteger . evalCtx . fromByteString . pack $ "3.00e2"
-- True
--
-- >>> isInteger . evalCtx . fromByteString . pack $ "3e2"
-- False
--
-- >>> isInteger . evalCtx . fromByteString . pack $ "3.00e0"
-- False
isInteger :: Double -> IO Bool
isInteger = boolean c'decDoubleIsInteger

-- | True only if @x@ is zero or positive, an integer (finite with
-- exponent of 0), and the coefficient is only zeroes and/or ones.
isLogical :: Double -> IO Bool
isLogical = boolean c'decDoubleIsLogical

-- | True only if @x@ is finite, non-zero, and not subnormal.
isNormal :: Double -> IO Bool
isNormal = boolean c'decDoubleIsNormal

-- | True only if @x@ is a signaling NaN.
isSignaling :: Double -> IO Bool
isSignaling = boolean c'decDoubleIsSignaling

-- | True only if @x@ has a sign of 1.  Note that zeroes and NaNs
-- may have sign of 1.
isSigned :: Double -> IO Bool
isSigned = boolean c'decDoubleIsSigned

-- | True only if @x@ is subnormal - that is, finite, non-zero, and
-- with a magnitude less than 10 ^ emin.
isSubnormal :: Double -> IO Bool
isSubnormal = boolean c'decDoubleIsSubnormal

-- | True only if both operands have the same exponent or are both
-- NaNs (quiet or signaling) or both infinite.
sameQuantum :: Double -> Double -> IO Bool
sameQuantum x y =
  withForeignPtr (unDouble x) $ \pX ->
  withForeignPtr (unDouble y) $ \pY ->
  c'decDoubleSameQuantum pX pY >>= \r ->
  return $ case r of
    1 -> True
    0 -> False
    _ -> error "sameQuantum: error: invalid result"


-- | Returns a string in engineering notation.
--
-- In the decNumber C library, this is called @toEngString@; the
-- name is changed here because the function does not return a
-- regular Haskell 'String'.
toEngByteString :: Double -> IO BS8.ByteString
toEngByteString = mkString c'decDoubleToEngString

-- | Identifies the version of the decNumber C library.
version :: IO BS8.ByteString
version =
  c'decDoubleVersion >>= BS8.packCString

-- | A Double whose coefficient, exponent, and sign are all 0.
zero :: IO Double
zero =
  newDouble >>= \d ->
  withForeignPtr (unDouble d) $ \pD ->
  c'decDoubleZero pD >>
  return d

-- Conversions to decNumber

-- | Converts a Double to a decNumber.
toNumber :: Double -> IO DecNum
toNumber (Double fp) =
  newDecNumSize c'DECDOUBLE_Pmax >>= \dn ->
  withForeignPtr fp $ \ptrDouble ->
  withForeignPtr (unDecNum dn) $ \ptrDn ->
  c'decimal128ToNumber (castPtr ptrDouble) ptrDn >>
  return dn

