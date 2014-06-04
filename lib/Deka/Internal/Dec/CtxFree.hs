{-# LANGUAGE EmptyDataDecls, Trustworthy #-}

module Deka.Internal.Dec.CtxFree where

import Foreign.Safe
import qualified Data.ByteString.Char8 as BS8
import Prelude
import Foreign.C.Types
import Deka.Internal.Mpdec
import System.IO.Unsafe (unsafePerformIO)

numToOrd :: (Num a, Ord a) => a -> Ordering
numToOrd a
  | a < 0 = LT
  | a > 0 = GT
  | otherwise = EQ

-- | @compareTotal x y@ compares to numbers using the IEEE 754 total
-- ordering.  If @x@ is less
-- than @y@, returns @-1@.  If they are equal (that is, when
-- subtracted the result would be 0), returns @0@.  If @y@ is
-- greater than @x@, returns @1@.  
--
-- Here is the total ordering:
--
-- @-NaN < -sNaN < -Infinity < -finites < -0 < +0 < +finites
--  < +Infinity < +SNaN < +NaN@
--
-- Also, @1.000@ < @1.0@ (etc.) and NaNs are ordered by payload.
compareTotal :: Dec -> Dec -> Ordering
compareTotal x y = unsafePerformIO $
  withDec x $ \px ->
  withDec y $ \py ->
  c'mpd_cmp_total px py >>= \i ->
  return (numToOrd i)

-- | Same as 'compareTotal' except that the signs of the operands
-- are ignored and taken to be 0 (non-negative).

compareTotalMag :: Dec -> Dec -> Ordering
compareTotalMag x y = unsafePerformIO $
  withDec x $ \px ->
  withDec y $ \py ->
  c'mpd_cmp_total_mag px py >>= \i ->
  return (numToOrd i)

-- | Converts a number to engineering notation.
toEngByteString :: Dec -> BS8.ByteString
toEngByteString dn = unsafePerformIO $
  withDec dn $ \pDn ->
  c'mpd_to_eng pDn capitalize >>= \bytes ->
  BS8.packCString bytes >>= \bs ->
  free bytes >>= \_ ->
  return bs

-- | Converts a number to scientific notation.
toByteString :: Dec -> BS8.ByteString
toByteString dn = unsafePerformIO $
  withDec dn $ \pDn ->
  c'mpd_to_sci pDn capitalize >>= \bytes ->
  BS8.packCString bytes >>= \bs ->
  free bytes >>= \_ ->
  return bs

-- | True if both operands have the same exponent; False otherwise.
sameQuantum :: Dec -> Dec -> Bool
sameQuantum x y = unsafePerformIO $
  withDec x $ \px ->
  withDec y $ \py ->
  c'mpd_same_quantum px py >>= \r ->
  return $ if r == 0 then False else True

version :: BS8.ByteString
version = c'MPD_VERSION

testBool
  :: (CMpd -> IO CInt)
  -> Dec
  -> Bool
testBool f d = unsafePerformIO $
  withDec d $ \pd ->
  f pd >>= \bl ->
  return (toBool bl)

isFinite :: Dec -> Bool
isFinite = testBool c'mpd_isfinite

isInfinite :: Dec -> Bool
isInfinite = testBool c'mpd_isinfinite

isNaN :: Dec -> Bool
isNaN = testBool c'mpd_isnan

isNegative :: Dec -> Bool
isNegative = testBool c'mpd_isnegative

isPositive :: Dec -> Bool
isPositive = testBool c'mpd_ispositive

isSigned :: Dec -> Bool
isSigned = testBool c'mpd_issigned

isQNaN :: Dec -> Bool
isQNaN = testBool c'mpd_isqnan

isSNaN :: Dec -> Bool
isSNaN = testBool c'mpd_issnan

isSpecial :: Dec -> Bool
isSpecial = testBool c'mpd_isspecial

isZero :: Dec -> Bool
isZero = testBool c'mpd_iszero

isZeroCoeff :: Dec -> Bool
isZeroCoeff = testBool c'mpd_iszerocoeff

isOddCoeff :: Dec -> Bool
isOddCoeff = testBool c'mpd_isoddcoeff

-- | The sign of a number.
data Sign
  = Sign0
  -- ^ A sign of zero; used for positive numbers and for zero.

  | Sign1
  -- ^ A sign of one; used for negative numbers and the negative
  -- zero.
  deriving (Eq, Ord, Show)

sign :: Dec -> Sign
sign d = unsafePerformIO $
  withDec d $ \pd ->
  c'mpd_sign pd >>= \i ->
  return $ if i == 0 then Sign0 else Sign1

data EvenOdd = Even | Odd
  deriving (Eq, Show)

evenOdd :: Dec -> (Maybe EvenOdd)
evenOdd d = unsafePerformIO $
  withDec d $ \pd ->
  c'mpd_isinteger pd >>= \isint ->
  if isint /= 0
    then c'mpd_isodd pd >>= \oddR ->
      return $ if oddR == 0 then Just Even else Just Odd
    else return Nothing

