{-# LANGUAGE Trustworthy #-}
module Deka.Quad
  (
    -- * Quad
    Q.Quad

  -- * Converting to and from strings
  , fromByteString
  , Q.toByteString
  , toEngByteString

  -- * Converting to and from integers
  , C'int32_t
  , C'uint32_t
  , fromInt32
  , fromUInt32
  , toInt32
  , toInt32Exact
  , toUInt32
  , toUInt32Exact

  -- * Arithmetic
  , add
  , subtract
  , multiply
  , fma
  , divide
  , divideInteger
  , remainder
  , remainderNear

  -- * Exponent and coefficient adjustment
  , quantize
  , reduce

  -- * Comparisons
  , compare
  , compareOrd
  , compareSignal
  , Q.compareTotal
  , compareTotalMag
  , max
  , maxMag
  , min
  , minMag
  , sameQuantum

  -- * Tests
  , decClass
  , isFinite
  , isInfinite
  , isInteger
  , isLogical
  , isNaN
  , isNegative
  , isNormal
  , isPositive
  , isSignaling
  , isSigned
  , isSubnormal
  , isZero

  -- * Signs
  , plus
  , minus
  , abs
  , copySign

  -- * Increment and decrement
  , nextMinus
  , nextPlus
  , nextToward

  -- * Digit-wise
  , and
  , or
  , xor
  , invert
  , shift
  , rotate

  -- * log and scale
  , logB
  , scaleB

  -- * Attributes
  , digits

  -- * Integral rounding

  -- | If you want to round but not to an integral value (e.g. round
  -- to two decimal places), see 'quantize'.
  , toIntegralExact
  , toIntegralValue

  -- * Constants
  , zero
  , one
  , version

  -- * Running a computation
  , runQuad
  , runQuadStatus

  -- * Context
  , module Deka.Context

  -- * Complete encoding and decoding

  -- | These convert a 'Quad' to a 'Decoded', which is a pure
  -- Haskell type containing all the information in the 'Quad'.

  -- ** Digits
  , Digit(..)
  , digitToInt
  , intToDigit
  , digitToChar
  , digitsToInteger
  , integralToDigits

  -- ** Coefficients
  , coefficientLen
  , payloadLen
  , Coefficient
  , coefficient
  , unCoefficient
  , zeroCoefficient
  , oneCoefficient
  , Payload
  , payload
  , unPayload
  , zeroPayload

  -- ** Exponents
  , Exponent
  , exponent
  , unExponent
  , zeroExponent
  , minMaxExp
  , AdjustedExp
  , adjustedExp
  , unAdjustedExp
  , minNormalAdj
  , minNormalExp
  , adjustedToExponent

  -- ** Sign, NaN, Value, Decoded
  , Sign(..)
  , NaN(..)
  , Value(..)
  , Decoded(..)

  --- ** Conversion functions
  , fromBCD
  , toBCD
  , scientific
  , ordinary
  , decodedToRational

  -- ** Decoded predicates

  -- *** Duplicates of Quad tests that return Bool
  -- | These duplicate the tests that are available for the Quad
  -- type directly.
  , dIsFinite
  , dIsInfinite
  , dIsInteger
  , dIsLogical
  , dIsNaN
  , dIsNegative
  , dIsNormal
  , dIsPositive
  , dIsSignaling
  , dIsSigned
  , dIsSubnormal
  , dIsZero
  , dDigits

  -- *** Duplicates of Quad tests that return 'DecClass'
  , dIsSNaN
  , dIsQNaN
  , dIsNegInf
  , dIsNegNormal
  , dIsNegSubnormal
  , dIsNegZero
  , dIsPosZero
  , dIsPosSubnormal
  , dIsPosNormal
  , dIsPosInf

  ) where

import qualified Deka.Quad.CtxFree as C
import Deka.Quad.Ctx
import Deka.Digit
import qualified Deka.Quad.Decoding as D
import Deka.Quad.Decoding hiding (fromBCD, toBCD, one)
import Deka.Context
import Deka.Decnumber.Types
import qualified Data.ByteString.Char8 as BS8
import Deka.Class
import qualified Deka.Quad.Quad as Q
import Deka.Quad.Quad (Quad)
import Deka.Unsafe
import Prelude (Bool, Maybe, Ordering, Int)

-- | More information about a particular 'Quad'.
decClass :: Quad -> Class
decClass = unsafe1 C.decClass

-- | True for NaNs.
isNaN :: Quad -> Bool
isNaN = unsafe1 C.isNaN

-- | Wrapper for 'compare' that returns an 'Ordering' rather than a
-- 'Quad'.  Returns @Just LT@ rather than -1, @Just EQ@ rather than
-- 0, and @Just GT@ rather than 1, and @Nothing@ rather than NaN.
-- This is a pure function; it does not affect the 'Ctx'.

compareOrd :: Quad -> Quad -> Maybe Ordering
compareOrd = unsafe2 C.compareOrd


-- | Same as 'compareTotal' but compares the absolute value of the
-- two arguments.
compareTotalMag :: Quad -> Quad -> Ordering
compareTotalMag = unsafe2 C.compareTotalMag


-- | @copySign x y@ returns @z@, which is a copy of @x@ but has the
-- sign of @y@.  This function never raises any signals.
copySign :: Quad -> Quad -> Quad
copySign = unsafe2 C.copySign


-- | Number of significant digits.  If zero or infinite, returns 1.
-- If NaN, returns number of digits in the payload.
digits :: Quad -> Int
digits = unsafe1 C.digits

fromInt32 :: C'int32_t -> Quad
fromInt32 = unsafe1 C.fromInt32

fromUInt32 :: C'uint32_t -> Quad
fromUInt32 = unsafe1 C.fromUInt32

-- | True if @x@ is neither infinite nor a NaN.
isFinite :: Quad -> Bool
isFinite = unsafe1 C.isFinite

-- | True for infinities.
isInfinite :: Quad -> Bool
isInfinite = unsafe1 C.isInfinite

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
isInteger :: Quad -> Bool
isInteger = unsafe1 C.isInteger

-- | True only if @x@ is zero or positive, an integer (finite with
-- exponent of 0), and the coefficient is only zeroes and/or ones.
isLogical :: Quad -> Bool
isLogical = unsafe1 C.isLogical

-- | True only if @x@ is finite, non-zero, and not subnormal.
isNormal :: Quad -> Bool
isNormal = unsafe1 C.isNormal

-- | True only if @x@ is a signaling NaN.
isSignaling :: Quad -> Bool
isSignaling = unsafe1 C.isSignaling

-- | True only if @x@ has a sign of 1.  Note that zeroes and NaNs
-- may have sign of 1.
isSigned :: Quad -> Bool
isSigned = unsafe1 C.isSigned

-- | True only if @x@ is subnormal - that is, finite, non-zero, and
-- with a magnitude less than 10 ^ emin.
isSubnormal :: Quad -> Bool
isSubnormal = unsafe1 C.isSubnormal

-- | True only if both operands have the same exponent or are both
-- NaNs (quiet or signaling) or both infinite.
sameQuantum :: Quad -> Quad -> Bool
sameQuantum = unsafe2 C.sameQuantum


-- | Returns a string in engineering notation.
--
-- In the decNumber C library, this is called @toEngString@; the
-- name is changed here because the function does not return a
-- regular Haskell 'String'.
toEngByteString :: Quad -> BS8.ByteString
toEngByteString = unsafe1 C.toEngByteString

-- | Identifies the version of the decNumber C library.
version :: BS8.ByteString
version = unsafe0 C.version

-- | A Quad whose coefficient, exponent, and sign are all 0.
zero :: Quad
zero = unsafe0 C.zero

isNegative :: Quad -> Bool
isNegative = unsafe1 Q.isNegative

isZero :: Quad -> Bool
isZero = unsafe1 Q.isZero

isPositive :: Quad -> Bool
isPositive = unsafe1 Q.isPositive

one :: Quad
one = unsafe0 D.one

fromBCD :: Decoded -> Quad
fromBCD = unsafe1 D.fromBCD

toBCD :: Quad -> Decoded
toBCD = unsafe1 D.toBCD
