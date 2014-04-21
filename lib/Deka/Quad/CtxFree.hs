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
module Deka.Quad.CtxFree where

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
import qualified Prelude
import Deka.Class.Internal
import Deka.Context

import Deka.Decnumber.DecQuad
import Deka.Decnumber.Types
import Deka.Quad.Quad
import Deka.Quad.Ctx (compare)

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
decClass = fmap Class . unaryGet unsafe'c'decQuadClass

-- | True for NaNs.
isNaN :: Quad -> IO Bool
isNaN = boolean unsafe'c'decQuadIsNaN

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
  c <- binaryCtxFree unsafe'c'decQuadCompareTotalMag x y
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
  unsafe'c'decQuadCopySign pN pS pP >>
  return n

-- | Number of significant digits.  If zero or infinite, returns 1.
-- If NaN, returns number of digits in the payload.
digits :: Quad -> IO Int
digits = fmap fromIntegral . unaryGet unsafe'c'decQuadDigits

fromInt32 :: C'int32_t -> IO Quad
fromInt32 i =
  newQuad >>= \r ->
  withForeignPtr (unQuad r) $ \pR ->
  unsafe'c'decQuadFromInt32 pR i
  >> return r

fromUInt32 :: C'uint32_t -> IO Quad
fromUInt32 i =
  newQuad >>= \r ->
  withForeignPtr (unQuad r) $ \pR ->
  unsafe'c'decQuadFromUInt32 pR i >>
  return r

-- | True if @x@ is neither infinite nor a NaN.
isFinite :: Quad -> IO Bool
isFinite = boolean unsafe'c'decQuadIsFinite

-- | True for infinities.
isInfinite :: Quad -> IO Bool
isInfinite = boolean unsafe'c'decQuadIsInfinite

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
isInteger = boolean unsafe'c'decQuadIsInteger

-- | True only if @x@ is zero or positive, an integer (finite with
-- exponent of 0), and the coefficient is only zeroes and/or ones.
isLogical :: Quad -> IO Bool
isLogical = boolean unsafe'c'decQuadIsLogical

-- | True only if @x@ is finite, non-zero, and not subnormal.
isNormal :: Quad -> IO Bool
isNormal = boolean unsafe'c'decQuadIsNormal

-- | True only if @x@ is a signaling NaN.
isSignaling :: Quad -> IO Bool
isSignaling = boolean unsafe'c'decQuadIsSignaling

-- | True only if @x@ has a sign of 1.  Note that zeroes and NaNs
-- may have sign of 1.
isSigned :: Quad -> IO Bool
isSigned = boolean unsafe'c'decQuadIsSigned

-- | True only if @x@ is subnormal - that is, finite, non-zero, and
-- with a magnitude less than 10 ^ emin.
isSubnormal :: Quad -> IO Bool
isSubnormal = boolean unsafe'c'decQuadIsSubnormal

-- | True only if both operands have the same exponent or are both
-- NaNs (quiet or signaling) or both infinite.
sameQuantum :: Quad -> Quad -> IO Bool
sameQuantum x y =
  withForeignPtr (unQuad x) $ \pX ->
  withForeignPtr (unQuad y) $ \pY ->
  unsafe'c'decQuadSameQuantum pX pY >>= \r ->
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
toEngByteString = mkString unsafe'c'decQuadToEngString

-- | Identifies the version of the decNumber C library.
version :: IO BS8.ByteString
version =
  unsafe'c'decQuadVersion >>= BS8.packCString

-- | A Quad whose coefficient, exponent, and sign are all 0.
zero :: IO Quad
zero =
  newQuad >>= \d ->
  withForeignPtr (unQuad d) $ \pD ->
  unsafe'c'decQuadZero pD >>
  return d

