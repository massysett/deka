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
-- In particular, this module implements the decQuad type.  decQuad
-- supports up to 34 digits of precision and exponents between -6176
-- and 6111.  It doesn't silently round, overflow, or underflow;
-- rather, the library will notify you if these things happen.
--
-- Many functions in this module clash with Prelude names, so you
-- might want to do
--
-- > import qualified Data.Deka.Quad as Q
module Deka.Internal.Quad.CtxFree where

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
  )
import Deka.Internal.Class
import Deka.Context

import Deka.Internal.Decnumber.DecQuad
import Deka.Internal.Decnumber.Decimal128
import Deka.Internal.DecNum.DecNum
import Deka.Internal.DecNum.Util
import Deka.Internal.Quad.Quad
import Deka.Internal.Quad.Ctx (compare)

-- # Helpers.  Do not export these.

type UnaryGet a
  = Ptr C'decQuad
  -> IO a

unaryGet
  :: UnaryGet a
  -> Quad
  -> IO a
unaryGet f d =
  withForeignPtr (unQuad d) $ \pD -> f pD

-- # End Helpers

-- # Functions from decQuad. In alphabetical order.


-- | More information about a particular 'Quad'.
decClass :: Quad -> IO Class
decClass = fmap Class . unaryGet c'decQuadClass

-- | True for NaNs.
isNaN :: Quad -> IO Bool
isNaN = boolean c'decQuadIsNaN

-- | Wrapper for 'compare' that returns an 'Ordering' rather than a
-- 'Quad'.  Returns @Just LT@ rather than -1, @Just EQ@ rather than
-- 0, and @Just GT@ rather than 1, and @Nothing@ rather than NaN.
-- This is a pure function; it does not affect the 'Ctx'.

compareOrd :: Quad -> Quad -> IO (Maybe Ordering)
compareOrd x y = do
  let c = runCtx initDecimal128 $ compare x y
  switchM [ (isNaN c, Nothing), (isNegative c, Just LT),
            (isZero c, Just EQ), (isPositive c, Just GT)]
          (error "compareOrd: unknown result")


-- | Same as 'compareTotal' but compares the absolute value of the
-- two arguments.
compareTotalMag :: Quad -> Quad -> IO Ordering
compareTotalMag x y = do
  c <- binaryCtxFree c'decQuadCompareTotalMag x y
  switchM [ (isNegative c, LT), (isZero c, EQ),
            (isPositive c, GT) ]
          (error "compareTotalMag: unknown result")


-- decNumber's CopySign copies the contents from pS to PN, except
-- that the sign is copied from pP to pN

-- | @copySign x y@ returns @z@, which is a copy of @x@ but has the
-- sign of @y@.  This function never raises any signals.
copySign :: Quad -> Quad -> IO Quad
copySign s p =
  newQuad >>= \n ->
  withForeignPtr (unQuad n) $ \pN ->
  withForeignPtr (unQuad s) $ \pS ->
  withForeignPtr (unQuad p) $ \pP ->
  c'decQuadCopySign pN pS pP >>
  return n

-- | Number of significant digits.  If zero or infinite, returns 1.
-- If NaN, returns number of digits in the payload.
digits :: Quad -> IO Int
digits = fmap fromIntegral . unaryGet c'decQuadDigits

fromInt32 :: Int32 -> IO Quad
fromInt32 i =
  newQuad >>= \r ->
  withForeignPtr (unQuad r) $ \pR ->
  c'decQuadFromInt32 pR i
  >> return r

fromUInt32 :: Word32 -> IO Quad
fromUInt32 i =
  newQuad >>= \r ->
  withForeignPtr (unQuad r) $ \pR ->
  c'decQuadFromUInt32 pR i >>
  return r

-- | True if @x@ is neither infinite nor a NaN.
isFinite :: Quad -> IO Bool
isFinite = boolean c'decQuadIsFinite

-- | True for infinities.
isInfinite :: Quad -> IO Bool
isInfinite = boolean c'decQuadIsInfinite

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
isInteger :: Quad -> IO Bool
isInteger = boolean c'decQuadIsInteger

-- | True only if @x@ is zero or positive, an integer (finite with
-- exponent of 0), and the coefficient is only zeroes and/or ones.
isLogical :: Quad -> IO Bool
isLogical = boolean c'decQuadIsLogical

-- | True only if @x@ is finite, non-zero, and not subnormal.
isNormal :: Quad -> IO Bool
isNormal = boolean c'decQuadIsNormal

-- | True only if @x@ is a signaling NaN.
isSignaling :: Quad -> IO Bool
isSignaling = boolean c'decQuadIsSignaling

-- | True only if @x@ has a sign of 1.  Note that zeroes and NaNs
-- may have sign of 1.
isSigned :: Quad -> IO Bool
isSigned = boolean c'decQuadIsSigned

-- | True only if @x@ is subnormal - that is, finite, non-zero, and
-- with a magnitude less than 10 ^ emin.
isSubnormal :: Quad -> IO Bool
isSubnormal = boolean c'decQuadIsSubnormal

-- | True only if both operands have the same exponent or are both
-- NaNs (quiet or signaling) or both infinite.
sameQuantum :: Quad -> Quad -> IO Bool
sameQuantum x y =
  withForeignPtr (unQuad x) $ \pX ->
  withForeignPtr (unQuad y) $ \pY ->
  c'decQuadSameQuantum pX pY >>= \r ->
  return $ case r of
    1 -> True
    0 -> False
    _ -> error "sameQuantum: error: invalid result"


-- | Returns a string in engineering notation.
--
-- In the decNumber C library, this is called @toEngString@; the
-- name is changed here because the function does not return a
-- regular Haskell 'String'.
toEngByteString :: Quad -> IO BS8.ByteString
toEngByteString = mkString c'decQuadToEngString

-- | Identifies the version of the decNumber C library.
version :: IO BS8.ByteString
version =
  c'decQuadVersion >>= BS8.packCString

-- | A Quad whose coefficient, exponent, and sign are all 0.
zero :: IO Quad
zero =
  newQuad >>= \d ->
  withForeignPtr (unQuad d) $ \pD ->
  c'decQuadZero pD >>
  return d

-- Conversions to decNumber

-- | Converts a Quad to a decNumber.
toNumber :: Quad -> IO DecNum
toNumber (Quad fp) =
  newDecNumSize c'DECQUAD_Pmax >>= \dn ->
  withForeignPtr fp $ \ptrQuad ->
  withForeignPtr (unDecNum dn) $ \ptrDn ->
  c'decimal128ToNumber (castPtr ptrQuad) ptrDn >>
  return dn

