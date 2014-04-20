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
module Deka.Quad.Safe where

-- # Imports

import Control.Monad
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe
import Data.Ratio
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
import Deka.Digit
import Deka.Class.Internal
import Deka.Context hiding (C'int32_t)
import Deka.Context.Internal

import Deka.Decnumber.DecQuad
import Deka.Decnumber.Context
import Deka.Decnumber.Types
import Deka.Quad.Quad

-- # Helpers.  Do not export these.

type Unary
  = Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

unary
  :: Unary
  -> Quad
  -> Ctx Quad
unary f d = Ctx $ \ptrC ->
  newQuad >>= \r ->
  withForeignPtr (unQuad d) $ \ptrX ->
  withForeignPtr (unQuad r) $ \ptrR ->
  f ptrR ptrX ptrC >>
  return r

type Binary
  = Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

binary
  :: Binary
  -> Quad
  -> Quad
  -> Ctx Quad
binary f x y = Ctx $ \pC ->
  newQuad >>= \r ->
  withForeignPtr (unQuad r) $ \pR ->
  withForeignPtr (unQuad x) $ \pX ->
  withForeignPtr (unQuad y) $ \pY ->
  f pR pX pY pC >>
  return r

type UnaryGet a
  = Ptr C'decQuad
  -> IO a

unaryGet
  :: UnaryGet a
  -> Quad
  -> IO a
unaryGet f d =
  withForeignPtr (unQuad d) $ \pD -> f pD

type Ternary
  = Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

ternary
  :: Ternary
  -> Quad
  -> Quad
  -> Quad
  -> Ctx Quad
ternary f x y z = Ctx $ \pC ->
  newQuad >>= \r ->
  withForeignPtr (unQuad r) $ \pR ->
  withForeignPtr (unQuad x) $ \pX ->
  withForeignPtr (unQuad y) $ \pY ->
  withForeignPtr (unQuad z) $ \pZ ->
  f pR pX pY pZ pC
  >> return r

type GetRounded a
  = Ptr C'decQuad
  -> Ptr C'decContext
  -> C'rounding
  -> IO a

getRounded
  :: GetRounded a
  -> Round
  -> Quad
  -> Ctx a
getRounded f (Round r) d = Ctx $ \pC ->
  withForeignPtr (unQuad d) $ \pD ->
  f pD pC r

-- # End Helpers

-- # Functions from decQuad. In alphabetical order.

-- | Absolute value.  NaNs are handled normally (the sign of an NaN
-- is not affected, and an sNaN sets 'invalidOperation'.
abs :: Quad -> Ctx Quad
abs = unary unsafe'c'decQuadAbs

add :: Quad -> Quad -> Ctx Quad
add = binary unsafe'c'decQuadAdd


-- | Digit-wise logical and.  Operands must be:
--
-- * zero or positive
--
-- * integers
--
-- * comprise only zeroes and/or ones
--
-- If not, 'invalidOperation' is set.
and :: Quad -> Quad -> Ctx Quad
and = binary unsafe'c'decQuadAnd

-- | More information about a particular 'Quad'.
decClass :: Quad -> IO Class
decClass = fmap Class . unaryGet unsafe'c'decQuadClass

-- | Compares two 'Quad' numerically.  The result might be @-1@, @0@,
-- @1@, or NaN, where @-1@ means x is less than y, @0@ indicates
-- numerical equality, @1@ means y is greater than x.  NaN is
-- returned only if x or y is an NaN.
--
-- Thus, this function does not return an 'Ordering' because the
-- result might be an NaN.
--
compare :: Quad -> Quad -> Ctx Quad
compare = binary unsafe'c'decQuadCompare

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


-- | Same as 'compare', but a quietNaN is treated like a signaling
-- NaN (sets 'invalidOperation').
compareSignal :: Quad -> Quad -> Ctx Quad
compareSignal = binary unsafe'c'decQuadCompareSignal

-- | Same as 'compareTotal' but compares the absolute value of the
-- two arguments.
compareTotalMag :: Quad -> Quad -> IO Ordering
compareTotalMag x y = do
  c <- binaryCtxFree unsafe'c'decQuadCompareTotalMag x y
  switchM [ (isNegative c, LT), (isZero c, EQ),
            (isPositive c, GT) ]
          (error "compareTotalMag: unknown result")


{-
-- decNumber's CopySign copies the contents from pS to PN, except
-- that the sign is copied from pP to pN

-- | @copySign x y@ returns @z@, which is a copy of @x@ but has the
-- sign of @y@.  This function never raises any signals.
copySign :: Quad -> Quad -> Quad
copySign s p = unsafePerformIO $
  newQuad >>= \n ->
  withForeignPtr (unQuad n) $ \pN ->
  withForeignPtr (unQuad s) $ \pS ->
  withForeignPtr (unQuad p) $ \pP ->
  unsafe'c'decQuadCopySign pN pS pP >>
  return n

-- | Number of significant digits.  If zero or infinite, returns 1.
-- If NaN, returns number of digits in the payload.
digits :: Quad -> Int
digits = fromIntegral . unaryGet unsafe'c'decQuadDigits

divide :: Quad -> Quad -> Ctx Quad
divide = binary unsafe'c'decQuadDivide

-- | @divideInteger x y@ returns the integer part of the result
-- (rounded toward zero), with an exponent of 0.  If the the result
-- would not fit because it has too many digits,
-- 'divisionImpossible' is set.
divideInteger :: Quad -> Quad -> Ctx Quad
divideInteger = binary unsafe'c'decQuadDivideInteger

-- | Fused multiply add; @fma x y z@ calculates @x * y + z@.  The
-- multiply is carried out first and is exact, so the result has
-- only one final rounding.
fma :: Quad -> Quad -> Quad -> Ctx Quad
fma = ternary unsafe'c'decQuadFMA

fromInt32 :: C'int32_t -> Quad
fromInt32 i = unsafePerformIO $
  newQuad >>= \r ->
  withForeignPtr (unQuad r) $ \pR ->
  unsafe'c'decQuadFromInt32 pR i
  >> return r

-- | Reads a ByteString, which can be in scientific, engineering, or
-- \"regular\" decimal notation.  Also reads NaN, Infinity, etc.
-- Will return a signaling NaN and set 'invalidOperation' if the
-- string given is invalid.
--
-- In the decNumber C library, this function was called
-- @fromString@; the name was changed here because it doesn't take a
-- regular Haskell 'String'.
fromByteString :: BS8.ByteString -> Ctx Quad
fromByteString s = Ctx $ \pC ->
  newQuad >>= \r ->
  withForeignPtr (unQuad r) $ \pR ->
  BS8.useAsCString s $ \pS ->
  unsafe'c'decQuadFromString pR pS pC >>
  return r

fromUInt32 :: C'uint32_t -> Quad
fromUInt32 i = unsafePerformIO $
  newQuad >>= \r ->
  withForeignPtr (unQuad r) $ \pR ->
  unsafe'c'decQuadFromUInt32 pR i >>
  return r

-- | Digit-wise logical inversion.  The operand must be:
--
-- * zero or positive
--
-- * integers
--
-- * comprise only zeroes and/or ones
--
-- If not, 'invalidOperation' is set.
invert :: Quad -> Ctx Quad
invert = unary unsafe'c'decQuadInvert

-- | True if @x@ is neither infinite nor a NaN.
isFinite :: Quad -> Bool
isFinite = boolean unsafe'c'decQuadIsFinite

-- | True for infinities.
isInfinite :: Quad -> Bool
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
isInteger :: Quad -> Bool
isInteger = boolean unsafe'c'decQuadIsInteger

-- | True only if @x@ is zero or positive, an integer (finite with
-- exponent of 0), and the coefficient is only zeroes and/or ones.
isLogical :: Quad -> Bool
isLogical = boolean unsafe'c'decQuadIsLogical

-- | True only if @x@ is finite, non-zero, and not subnormal.
isNormal :: Quad -> Bool
isNormal = boolean unsafe'c'decQuadIsNormal

-- | True only if @x@ is a signaling NaN.
isSignaling :: Quad -> Bool
isSignaling = boolean unsafe'c'decQuadIsSignaling

-- | True only if @x@ has a sign of 1.  Note that zeroes and NaNs
-- may have sign of 1.
isSigned :: Quad -> Bool
isSigned = boolean unsafe'c'decQuadIsSigned

-- | True only if @x@ is subnormal - that is, finite, non-zero, and
-- with a magnitude less than 10 ^ emin.
isSubnormal :: Quad -> Bool
isSubnormal = boolean unsafe'c'decQuadIsSubnormal

-- | @logB x@ Returns the adjusted exponent of x, according to IEEE
-- 754 rules.  If @x@ is infinite, returns +Infinity.  If @x@ is
-- zero, the result is -Infinity, and 'divisionByZero' is set.  If
-- @x@ is less than zero, the absolute value of @x@ is used.  If @x@
-- is one, the result is 0.  NaNs are propagated as for arithmetic
-- operations.
logB :: Quad -> Ctx Quad
logB = unary unsafe'c'decQuadLogB

-- | @max x y@ returns the larger argument; if either (but not both)
-- @x@ or @y@ is a quiet NaN then the other argument is the result;
-- otherwise, NaNs, are handled as for arithmetic operations.
max :: Quad -> Quad -> Ctx Quad
max = binary unsafe'c'decQuadMax

-- | Like 'max' but the absolute values of the arguments are used.
maxMag :: Quad -> Quad -> Ctx Quad
maxMag = binary unsafe'c'decQuadMaxMag

-- | @min x y@ returns the smaller argument; if either (but not both)
-- @x@ or @y@ is a quiet NaN then the other argument is the result;
-- otherwise, NaNs, are handled as for arithmetic operations.
min :: Quad -> Quad -> Ctx Quad
min = binary unsafe'c'decQuadMin

-- | Like 'min' but the absolute values of the arguments are used.
minMag :: Quad -> Quad -> Ctx Quad
minMag = binary unsafe'c'decQuadMinMag

-- | Negation.  Result has the same effect as @0 - x@ when the
-- exponent of the zero is the same as that of @x@, if @x@ is
-- finite.
minus :: Quad -> Ctx Quad
minus = unary unsafe'c'decQuadMinus

multiply :: Quad -> Quad -> Ctx Quad
multiply = binary unsafe'c'decQuadMultiply

-- | Decrements toward negative infinity.
nextMinus :: Quad -> Ctx Quad
nextMinus = unary unsafe'c'decQuadNextMinus

-- | Increments toward positive infinity.
nextPlus :: Quad -> Ctx Quad
nextPlus = unary unsafe'c'decQuadNextPlus

-- | @nextToward x y@ returns the next 'Quad' in the direction of
-- @y@.
nextToward :: Quad -> Quad -> Ctx Quad
nextToward = binary unsafe'c'decQuadNextToward

-- | Digit wise logical inclusive Or.  Operands must be:
--
-- * zero or positive
--
-- * integers
--
-- * comprise only zeroes and/or ones
--
-- If not, 'invalidOperation' is set.
or :: Quad -> Quad -> Ctx Quad
or = binary unsafe'c'decQuadOr

-- | Same effect as @0 + x@ where the exponent of the zero is the
-- same as that of @x@ if @x@ is finite).  NaNs are handled as for
-- arithmetic operations.
plus :: Quad -> Ctx Quad
plus = unary unsafe'c'decQuadPlus

-- | @quantize x y@ returns @z@ which is @x@ set to have the same
-- quantum as @y@; that is, numerically the same value but rounded
-- or padded if necessary to have the same exponent as @y@.  Useful
-- for rounding monetary quantities.
quantize :: Quad -> Quad -> Ctx Quad
quantize = binary unsafe'c'decQuadQuantize

-- | Reduces coefficient to its shortest possible form without
-- changing the value of the result by removing all possible
-- trailing zeroes.
reduce :: Quad -> Ctx Quad
reduce = unary unsafe'c'decQuadReduce

-- | Remainder from integer division.  If the intermediate integer
-- does not fit within a Quad, 'divisionImpossible' is raised.
remainder :: Quad -> Quad -> Ctx Quad
remainder = binary unsafe'c'decQuadRemainder

-- | Like 'remainder' but the nearest integer is used for for the
-- intermediate result instead of the result from 'divideInteger'.
remainderNear :: Quad -> Quad -> Ctx Quad
remainderNear = binary unsafe'c'decQuadRemainderNear

-- | @rotate x y@ rotates the digits of x to the left (if @y@ is
-- positive) or right (if @y@ is negative) without adjusting the
-- exponent or sign of @x@.  @y@ is the number of positions to
-- rotate and must be in the range @negate 'coefficientLen'@ to
-- @'coefficentLen'@.
--
-- NaNs are propagated as usual.  No status is set unless @y@ is
-- invalid or an operand is an NaN.
rotate :: Quad -> Quad -> Ctx Quad
rotate = binary unsafe'c'decQuadRotate

-- | True only if both operands have the same exponent or are both
-- NaNs (quiet or signaling) or both infinite.
sameQuantum :: Quad -> Quad -> Bool
sameQuantum x y = unsafePerformIO $
  withForeignPtr (unQuad x) $ \pX ->
  withForeignPtr (unQuad y) $ \pY ->
  unsafe'c'decQuadSameQuantum pX pY >>= \r ->
  return $ case r of
    1 -> True
    0 -> False
    _ -> error "sameQuantum: error: invalid result"

-- | @scaleB x y@ calculates @x * 10 ^ y@.  @y@ must be an integer
-- (finite with exponent of 0) in the range of plus or minus @2 *
-- 'coefficientLen' + 'coefficientLen')@, typically resulting from
-- 'logB'.  Underflow and overflow might occur; NaNs propagate as
-- usual.
scaleB :: Quad -> Quad -> Ctx Quad
scaleB = binary unsafe'c'decQuadScaleB

-- | @shift x y@ shifts digits the digits of x to the left (if @y@
-- is positive) or right (if @y@ is negative) without adjusting the
-- exponent or sign of @x@.  Any digits shifted in from the left or
-- right will be 0.
--
-- @y@ is a count of positions to shift; it must be a finite
-- integer in the range @negate 'coefficientLen'@ to
-- 'coefficientLen'.  NaNs propagate as usual.  If @x@ is infinite
-- the result is an infinity of the same sign.  No status is set
-- unless y is invalid or the operand is an NaN.
shift :: Quad -> Quad -> Ctx Quad
shift = binary unsafe'c'decQuadShift

-- omitted: Show

subtract :: Quad -> Quad -> Ctx Quad
subtract = binary unsafe'c'decQuadSubtract

-- | Returns a string in engineering notation.
--
-- In the decNumber C library, this is called @toEngString@; the
-- name is changed here because the function does not return a
-- regular Haskell 'String'.
toEngByteString :: Quad -> BS8.ByteString
toEngByteString = mkString unsafe'c'decQuadToEngString

-- | Uses the rounding method given rather than the one in the
-- 'Ctx'.  If the operand is infinite, an NaN, or if the result of
-- rounding is outside the range of a 'C'int32_t', then
-- 'invalidOperation' is set.  'inexact' is not set even if rounding
-- occurred.
toInt32 :: Round -> Quad -> Ctx C'int32_t
toInt32 = getRounded unsafe'c'decQuadToInt32

-- | Like 'toInt32' but if rounding removes non-zero digits then
-- 'inexact' is set.
toInt32Exact :: Round -> Quad -> Ctx C'int32_t
toInt32Exact = getRounded unsafe'c'decQuadToInt32Exact

-- | Rounds to an integral using the rounding mode set in the 'Ctx'.
-- If the operand is infinite, an infinity of the same sign is
-- returned.  If the operand is an NaN, the result is the same as
-- for other arithmetic operations.  If rounding removes non-zero
-- digits then 'inexact' is set.
toIntegralExact :: Quad -> Ctx Quad
toIntegralExact = unary unsafe'c'decQuadToIntegralExact

-- | @toIntegralValue r x@ returns an integral value of @x@ using
-- the rounding mode @r@ rather than the one specified in the 'Ctx'.
-- If the operand is an NaN, the result is the same as for other
-- arithmetic operations.  'inexact' is not set even if rounding
-- occurred.
toIntegralValue :: Round -> Quad -> Ctx Quad
toIntegralValue (Round rnd) d = Ctx $ \pC ->
  withForeignPtr (unQuad d) $ \pD ->
  newQuad >>= \r ->
  withForeignPtr (unQuad r) $ \pR ->
  unsafe'c'decQuadToIntegralValue pR pD pC rnd >>
  return r

-- toByteString - moved to Internal so that Quad can Show in a
-- non-orphan instance

-- | @toUInt32 r x@ returns the value of @x@, rounded to an integer
-- if necessary using the rounding mode @r@ rather than the one
-- given in the 'Ctx'.  If @x@ is infinite, or outside of the range
-- of a 'C'uint32_t', then 'invalidOperation' is set.  'inexact' is
-- not set even if rounding occurs.
--
-- The negative zero converts to 0 and is valid, but negative
-- numbers are not valid.
toUInt32 :: Round -> Quad -> Ctx C'uint32_t
toUInt32 = getRounded unsafe'c'decQuadToUInt32

-- | Same as 'toUInt32' but if rounding removes non-zero digits then
-- 'inexact' is set.
toUInt32Exact :: Round -> Quad -> Ctx C'uint32_t
toUInt32Exact = getRounded unsafe'c'decQuadToUInt32Exact

-- | Identifies the version of the decNumber C library.
version :: BS8.ByteString
version = unsafePerformIO $
  unsafe'c'decQuadVersion >>= BS8.packCString

-- | Runs a computation with the decimal128 default context.
runQuad :: Ctx a -> a
runQuad = runCtx initDecimal128

-- | Runs a computation with the decimal128 default context, and
-- returns any resulting flags.
runQuadStatus :: Ctx a -> (a, [Flag])
runQuadStatus a = runQuad $ do
  r <- a
  f <- getStatus
  return (r, f)

-- | Digit-wise logical exclusive or.  Operands must be:
--
-- * zero or positive
--
-- * integers
--
-- * comprise only zeroes and/or ones
--
-- If not, 'invalidOperation' is set.

xor :: Quad -> Quad -> Ctx Quad
xor = binary unsafe'c'decQuadXor

-- | A Quad whose coefficient, exponent, and sign are all 0.
zero :: Quad
zero = unsafePerformIO $
  newQuad >>= \d ->
  withForeignPtr (unQuad d) $ \pD ->
  unsafe'c'decQuadZero pD >>
  return d

-- | A Quad with coefficient 'D1', exponent 0, and sign 'Sign0'.
one :: Quad
one = fromBCD
  $ Decoded Sign0 (Finite (Coefficient [D1]) (Exponent 0))

-- # Conversions

data Sign
  = Sign0
  -- ^ The number is positive or is zero
  | Sign1
  -- ^ The number is negative or the negative zero
  deriving (Eq, Ord, Show, Enum, Bounded)

data NaN
  = Quiet
  | Signaling
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Decimal Arithmetic Specification version 1.70, page 10, says that
-- the minimum and maximum adjusted exponent is given by
--
-- @-x - (c - 1) + 1@ and @x - (c - 1)@
--
-- where @x@ the upper limit on the absolute value of exponent, and
-- @c@ is the length of the coefficient in decimal digits.
--
-- However, the lower bound of the above formula only accounts for
-- normal numbers.  When subnormal numbers are enabled (as they are
-- here), the lower bound on exponents is
--
-- @m - (p - 1)@
--
-- where @m@ is the smallest possible adjusted exponent for normal
-- numbers (called Emin), and p is the working precision.
--
-- Also, the upper bound is different too, becuase decQuad is
-- clamped; see decNumber manual, page 23.  This means the maximum
-- exponent is limited to
--
-- @t - (p - 1)@
--
-- where @t@ is the maximum possible adjusted exponent and p is the
-- working precision.
--
-- The function below uses the minimum and maximum accounting for
-- the clamp and the subnormals.

-- | The minimum and maximum possible exponent.
minMaxExp :: (Int, Int)
minMaxExp = (l, h)
  where
    l = c'DECQUAD_Emin - c'DECQUAD_Pmax + 1
    h = c'DECQUAD_Emax - c'DECQUAD_Pmax + 1

-- | The smallest possible adjusted exponent that is still normal.
-- Adjusted exponents smaller than this are subnormal.
minNormalAdj :: AdjustedExp
minNormalAdj = AdjustedExp c'DECQUAD_Emin

-- | Like 'minNormalAdj', but returns the size of the regular exponent
-- rather than the adjusted exponent.
minNormalExp :: Coefficient -> Exponent
minNormalExp c = adjustedToExponent c $ minNormalAdj

-- | The signed integer which indicates the power of ten by which
-- the coefficient is multiplied.
newtype Exponent = Exponent { unExponent :: Int }
  deriving (Eq, Ord, Show)

instance Bounded Exponent where
  minBound = Exponent . fst $ minMaxExp
  maxBound = Exponent . snd $ minMaxExp

instance Enum Exponent where
  toEnum i
    | r < minBound = error e
    | r > maxBound = error e
    | otherwise = r
    where
      r = Exponent i
      e = "Deka.Exponent.toEnum: integer out of range"

  fromEnum (Exponent i) = i

-- | Ensures that the exponent is within the range allowed by
-- 'minMaxExp'.
exponent :: Int -> Maybe Exponent
exponent i
  | i < l = Nothing
  | i > h = Nothing
  | otherwise = Just . Exponent $ i
  where
    (l, h) = minMaxExp

-- | An Exponent whose value is 0.
zeroExponent :: Exponent
zeroExponent = Exponent 0

data Value
  = Finite Coefficient Exponent
  | Infinite
  | NaN NaN Payload
  deriving (Eq, Ord, Show)

-- | A pure Haskell type which holds information identical to that
-- in a 'Quad'.
data Decoded = Decoded
  { dSign :: Sign
  , dValue :: Value
  } deriving (Eq, Ord, Show)


-- | Decodes a 'Quad' to a pure Haskell type which holds identical
-- information.
toBCD :: Quad -> Decoded
toBCD d = unsafePerformIO $
  withForeignPtr (unQuad d) $ \pD ->
  allocaBytes c'DECQUAD_Pmax $ \pArr ->
  alloca $ \pExp ->
  unsafe'c'decQuadToBCD pD pExp pArr >>= \sgn ->
  peek pExp >>= \ex ->
  peekArray c'DECQUAD_Pmax pArr >>= \coef ->
  return (getDecoded sgn ex coef)

-- | Encodes a new 'Quad'.
fromBCD :: Decoded -> Quad
fromBCD dcd = unsafePerformIO $
  newQuad >>= \d ->
  withForeignPtr (unQuad d) $ \pD ->
  let (expn, digs, sgn) = toDecNumberBCD dcd in
  withArray digs $ \pArr ->
  unsafe'c'decQuadFromBCD pD expn pArr sgn >>
  return d


-- ## Decoding and encoding helpers

toDecNumberBCD :: Decoded -> (C'int32_t, [C'uint8_t], C'int32_t)
toDecNumberBCD (Decoded s v) = (e, ds, sgn)
  where
    sgn = case s of { Sign0 -> 0; Sign1 -> c'DECFLOAT_Sign }
    (e, ds) = case v of
      Infinite -> (c'DECFLOAT_Inf, replicate c'DECQUAD_Pmax 0)
      NaN n (Payload ps) -> (ns, np)
        where
          ns = case n of
            Quiet -> c'DECFLOAT_qNaN
            Signaling -> c'DECFLOAT_sNaN
          np = pad ++ map digitToInt ps
          pad = replicate (c'DECQUAD_Pmax - length ps) 0
      Finite (Coefficient digs) (Exponent ex) ->
        ( fromIntegral ex, pad ++ map digitToInt digs )
        where
          pad = replicate (c'DECQUAD_Pmax - length digs) 0

getDecoded
  :: C'int32_t
  -- ^ Sign. Zero if sign is zero; non-zero if sign is not zero
  -- (that is, is negavite.)
  -> C'int32_t
  -- ^ Exponent
  -> [C'uint8_t]
  -- ^ Coefficient
  -> Decoded
getDecoded sgn ex coef = Decoded s v
  where
    s = if sgn == 0 then Sign0 else Sign1
    v | ex == c'DECFLOAT_qNaN = NaN Quiet pld
      | ex == c'DECFLOAT_sNaN = NaN Signaling pld
      | ex == c'DECFLOAT_Inf = Infinite
      | otherwise = Finite coe (Exponent $ fromIntegral ex)
      where
        pld = Payload . toDigs . tail $ coef
        coe = Coefficient . toDigs $ coef
        toDigs c = case dropWhile (== D0) . map intToDigit $ c of
          [] -> [D0]
          xs -> xs

-- ## Decoded to scientific and ordinary notation

-- | Converts a Decoded to scientific notation.  Unlike
-- 'toByteString' this will always use scientific notation.  For
-- NaNs and infinities, the notation is identical to that of
-- decNumber  (see Decimal Arithmetic Specification page 19).  This
-- means that a quiet NaN is @NaN@ while a signaling NaN is @sNaN@,
-- and infinity is @Infinity@.
--
-- Like decQuadToString, the payload of an NaN is not shown if it is
-- zero.

scientific :: Decoded -> String
scientific d = sign ++ rest
  where
    sign = case dSign d of
      Sign0 -> ""
      Sign1 -> "-"
    rest = case dValue d of
      Infinite -> "Infinity"
      Finite c e -> sciFinite c e
      NaN n p -> sciNaN n p

sciFinite :: Coefficient -> Exponent -> String
sciFinite c e = sCoe ++ 'E':sExp
  where
    sCoe = case unCoefficient c of
      x:xs -> digitToChar x : case xs of
        [] -> []
        _ -> '.' : map digitToChar xs
      [] -> error "sciFinite: empty coefficient"
    sExp = show . unAdjustedExp . adjustedExp c $ e

sciNaN :: NaN -> Payload -> String
sciNaN n p = nStr ++ pStr
  where
    nStr = case n of { Quiet -> "NaN"; Signaling -> "sNaN" }
    pStr = case unPayload p of
      [D0] -> ""
      xs -> map digitToChar xs

-- | Converts Decoded to ordinary decimal notation.  For NaNs and
-- infinities, the notation is identical to that of 'scientific'.
-- Unlike 'scientific', though the result can always be converted back
-- to a 'Quad' using 'fromByteString', the number of significant
-- digits might change.  For example, though @1.2E3@ has two
-- significant digits, using @ordinary@ on this value and then
-- reading it back in with @fromByteString@ will give you @1200E0@,
-- which has four significant digits.

ordinary :: Decoded -> String
ordinary d = sign ++ rest
  where
    sign = case dSign d of
      Sign0 -> ""
      Sign1 -> "-"
    rest = case dValue d of
      Infinite -> "Infinity"
      Finite c e -> onyFinite c e
      NaN n p -> sciNaN n p

onyFinite :: Coefficient -> Exponent -> String
onyFinite c e
  | coe == [D0] = "0"
  | ex >= 0 = map digitToChar coe ++ replicate ex '0'
  | aex < lCoe =
      let (lft, rt) = splitAt (lCoe - aex) coe
      in map digitToChar lft ++ "." ++ map digitToChar rt
  | otherwise =
      let numZeroes = aex - lCoe
      in "0." ++ replicate numZeroes '0' ++ map digitToChar coe
  where
    ex = unExponent e
    coe = unCoefficient c
    aex = Prelude.abs ex
    lCoe = length coe

-- | Converts a Decoded to a Rational.  Returns Nothing if the
-- Decoded is not finite.
decodedToRational :: Decoded -> Maybe Rational
decodedToRational d = case dValue d of
  (Finite c e) ->
    let int = digitsToInteger . unCoefficient $ c
        ex = unExponent e
        mkSgn = if dSign d == Sign0 then id else negate
        mult = if ex < 0 then 1 % (10 ^ Prelude.abs ex) else 10 ^ ex
    in Just . mkSgn $ fromIntegral int * mult
  _ -> Nothing

-- | A list of digits, less than or equal to 'coefficientLen' long.
-- Corresponds only to finite numbers.
newtype Coefficient = Coefficient { unCoefficient :: [Digit] }
  deriving (Eq, Ord, Show)

instance Bounded Coefficient where
  minBound = Coefficient [D0]
  maxBound = Coefficient $ replicate coefficientLen D9

instance Enum Coefficient where
  toEnum i
    | i < 0 = error $ "Deka.Quad.Coefficient.toEnum: argument "
      ++ "out of range; is negative"
    | length r > coefficientLen = error $ "Deka.Quad.Coefficient."
        ++ "toEnum: argument too large"
    | otherwise = Coefficient r
    where
      r = integralToDigits i

  fromEnum i
    | r > (fromIntegral (maxBound :: Int)) =
        error $ "Deka.Quad.Coefficient.fromEnum:"
          ++ " argument too large to fit into Int"
    | otherwise = fromIntegral r
    where
      r = digitsToInteger . unCoefficient $ i

-- | Creates a 'Coefficient'.  Checks to ensure it is not null and
-- that it is not longer than 'coefficientLen' and that it does not
-- have leading zeroes (if it is 0, a single 'D0' is allowed).
coefficient :: [Digit] -> Maybe Coefficient
coefficient ls
  | null ls = Nothing
  | length ls > 1 && head ls == D0 = Nothing
  | length ls > coefficientLen = Nothing
  | otherwise = Just . Coefficient $ ls

-- | Coefficient of 'D0'
zeroCoefficient :: Coefficient
zeroCoefficient = Coefficient [D0]

-- | Coefficient of 'D1'
oneCoefficient :: Coefficient
oneCoefficient = Coefficient [D1]

-- | A list of digits, less than or equal to 'payloadLen'
-- long.  Accompanies an NaN, potentially with diagnostic
-- information (I do not know if decNumber actually makes use of
-- this.)
newtype Payload = Payload { unPayload :: [Digit] }
  deriving (Eq, Ord, Show)

instance Bounded Payload where
  minBound = Payload [D0]
  maxBound = Payload $ replicate payloadLen D9

instance Enum Payload where
  toEnum i
    | i < 0 = error $ "Deka.Quad.Payload.toEnum: argument "
      ++ "out of range; is negative"
    | length r > payloadLen = error $ "Deka.Quad.Payload."
        ++ "toEnum: argument too large"
    | otherwise = Payload r
    where
      r = integralToDigits i

  fromEnum i
    | r > (fromIntegral (maxBound :: Int)) =
        error $ "Deka.Quad.Payload.fromEnum:"
          ++ " argument too large to fit into Int"
    | otherwise = fromIntegral r
    where
      r = digitsToInteger . unPayload $ i

-- | Creates a 'Payload'.  Checks to ensure it is not null, not
-- longer than 'payloadLen' and that it does not have leading zeroes
-- (if it is 0, a single 'D0' is allowed).
payload :: [Digit] -> Maybe Payload
payload ds
  | null ds = Nothing
  | length ds > 1 && head ds == D0 = Nothing
  | length ds > payloadLen = Nothing
  | otherwise = Just . Payload $ ds

-- | Payload of [D0]
zeroPayload :: Payload
zeroPayload = Payload [D0]


-- | Maximum number of digits in a coefficient.
coefficientLen :: Int
coefficientLen = c'DECQUAD_Pmax

-- | Maximum number of digits in a payload.
payloadLen :: Int
payloadLen = c'DECQUAD_Pmax - 1

-- # Decoded predicates

dIsFinite :: Decoded -> Bool
dIsFinite (Decoded _ v) = case v of
  Finite _ _ -> True
  _ -> False

dIsInfinite :: Decoded -> Bool
dIsInfinite (Decoded _ v) = case v of
  Infinite -> True
  _ -> False

dIsInteger :: Decoded -> Bool
dIsInteger (Decoded _ v) = case v of
  Finite _ e -> unExponent e == 0
  _ -> False

-- | True only if @x@ is zero or positive, an integer (finite with
-- exponent of 0), and the coefficient is only zeroes and/or ones.
-- The sign must be Sign0 (that is, you cannot have a negative
-- zero.)
dIsLogical :: Decoded -> Bool
dIsLogical (Decoded s v) = fromMaybe False $ do
  guard $ s == Sign0
  (d, e) <- case v of
    Finite ds ex -> return (ds, ex)
    _ -> Nothing
  guard $ e == zeroExponent
  return
    . all (\x -> x == D0 || x == D1)
    . unCoefficient $ d

dIsNaN :: Decoded -> Bool
dIsNaN (Decoded _ v) = case v of
  NaN _ _ -> True
  _ -> False

-- | True only if @x@ is less than zero and is not an NaN.  It's not
-- enough for the sign to be Sign1; the coefficient (if finite) must
-- be greater than zero.
dIsNegative :: Decoded -> Bool
dIsNegative (Decoded s v) = fromMaybe False $ do
  guard $ s == Sign1
  return $ case v of
    Finite d _ -> any (/= D0) . unCoefficient $ d
    Infinite -> True
    _ -> False

dIsNormal :: Decoded -> Bool
dIsNormal (Decoded _ v) = case v of
  Finite d e
    | adjustedExp d e < minNormalAdj -> False
    | otherwise -> any (/= D0) . unCoefficient $ d
  _ -> False

dIsPositive :: Decoded -> Bool
dIsPositive (Decoded s v)
  | s == Sign1 = False
  | otherwise = case v of
      Finite d _ -> any (/= D0) . unCoefficient $ d
      Infinite -> True
      _ -> False

dIsSignaling :: Decoded -> Bool
dIsSignaling (Decoded _ v) = case v of
  NaN Signaling _ -> True
  _ -> False


dIsSigned :: Decoded -> Bool
dIsSigned (Decoded s _) = s == Sign1

dIsSubnormal :: Decoded -> Bool
dIsSubnormal (Decoded _ v) = case v of
  Finite d e -> adjustedExp d e < minNormalAdj
  _ -> False

-- | True for any zero (negative or positive zero).
dIsZero :: Decoded -> Bool
dIsZero (Decoded _ v) = case v of
  Finite d _ -> all (== D0) . unCoefficient $ d
  _ -> False

-- | The number of significant digits. Zero returns 1.
dDigits :: Coefficient -> Int
dDigits (Coefficient ds) = case dropWhile (== D0) ds of
  [] -> 1
  rs -> length rs

-- | An adjusted exponent is the value of an exponent of a number
-- when that number is expressed as though in scientific notation
-- with one digit before any decimal point.  This is the finite
-- exponent + (number of significant digits - 1).
newtype AdjustedExp = AdjustedExp { unAdjustedExp :: Int }
  deriving (Eq, Show, Ord)

instance Bounded AdjustedExp where
  minBound = AdjustedExp $ fst minMaxExp
  maxBound = AdjustedExp $ snd minMaxExp + coefficientLen - 1

instance Enum AdjustedExp where
  toEnum i
    | r < minBound = error e
    | r > maxBound = error e
    | otherwise = r
    where
      r = AdjustedExp i
      e = "Deka.AdjustedExp.toEnum: integer out of range"

  fromEnum (AdjustedExp i) = i

adjustedExp :: Coefficient -> Exponent -> AdjustedExp
adjustedExp ds e = AdjustedExp $ unExponent e
  + dDigits ds - 1

adjustedToExponent :: Coefficient -> AdjustedExp -> Exponent
adjustedToExponent ds e = Exponent $ unAdjustedExp e -
  dDigits ds + 1

-- # DecClass-like Decoded predicates

dIsSNaN :: Decoded -> Bool
dIsSNaN d = case dValue d of
  NaN n _ -> n == Signaling
  _ -> False

dIsQNaN :: Decoded -> Bool
dIsQNaN d = case dValue d of
  NaN n _ -> n == Quiet
  _ -> False

dIsNegInf :: Decoded -> Bool
dIsNegInf d
  | dSign d == Sign0 = False
  | otherwise = dValue d == Infinite

dIsNegNormal :: Decoded -> Bool
dIsNegNormal d
  | dSign d == Sign0 = False
  | otherwise = case dValue d of
      Finite c e -> e >= minNormalExp c
      _ -> False

dIsNegSubnormal :: Decoded -> Bool
dIsNegSubnormal d
  | dSign d == Sign0 = False
  | otherwise = case dValue d of
      Finite c e -> e < minNormalExp c
      _ -> False

dIsNegZero :: Decoded -> Bool
dIsNegZero d
  | dSign d == Sign0 = False
  | otherwise = case dValue d of
      Finite c _ -> unCoefficient c == [D0]
      _ -> False

dIsPosZero :: Decoded -> Bool
dIsPosZero d
  | dSign d == Sign1 = False
  | otherwise = case dValue d of
      Finite c _ -> unCoefficient c == [D0]
      _ -> False

dIsPosSubnormal :: Decoded -> Bool
dIsPosSubnormal d
  | dSign d == Sign1 = False
  | otherwise = case dValue d of
      Finite c e -> e < minNormalExp c
      _ -> False

dIsPosNormal :: Decoded -> Bool
dIsPosNormal d
  | dSign d == Sign1 = False
  | otherwise = case dValue d of
      Finite c e -> e >= minNormalExp c
      _ -> False

dIsPosInf :: Decoded -> Bool
dIsPosInf d
  | dSign d == Sign1 = False
  | otherwise = dValue d == Infinite

-}

-- # decQuad functions not recreated here:

-- skipped: classString - not needed
-- skipped: copy - not needed
-- skipped: copyAbs - use abs instead
-- skipped: copyNegate - use negate instead
-- skipped: fromNumber - not needed
-- skipped: fromPacked - use fromPackedChecked instead
-- skipped: fromWider - not needed
-- skipped: getExponent, setExponent - use toBCD, fromBCD
-- skipped: getCoefficient, setCoefficient - use toBCD, fromBCD
-- skipped: isCanonical - not needed
-- skipped: radix - not needed
-- skipped: toNumber - not needed
-- skipped: toPacked - use decode function instead
-- skipped: toWider - not needed
-- skipped: show - not needed; impure
