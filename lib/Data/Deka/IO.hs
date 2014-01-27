{-# LANGUAGE Safe #-}

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
-- In particular, this module implements the decQuad type.  decQuad
-- supports up to 34 digits of precision and exponents up to
-- (roughly) 6144.  It doesn't silently round, overflow, or
-- underflow; rather, the library will notify you if these things
-- happen.
--
-- Every function in this module returns canonical 'Dec' (contrast
-- the decNumber library which will, under limited circumstances,
-- return non-canonical decimals).  I am not certain that canonical
-- vs. non-canonical matters in typical use, though it may matter
-- when using 'compareTotal'.
--
-- Many functions in this module clash with Prelude names, so you
-- might want to do
--
-- > import qualified Data.Deka.Pure as D
module Data.Deka.IO
  (
    -- * Dec
    Dec

    -- * Rounding
    -- | For more on the rounding algorithms, see
    --
    -- <http://speleotrove.com/decimal/damodel.html>
  , Round
  , roundCeiling
  , roundUp
  , roundHalfUp
  , roundHalfEven
  , roundHalfDown
  , roundDown
  , roundFloor
  , round05Up
  , roundMax

  -- * Flags
  --
  -- | For more on possible flags, see
  --
  -- <http://speleotrove.com/decimal/damodel.html>
  , Flag
  , divisionUndefined
  , divisionByZero
  , divisionImpossible
  , invalidOperation
  , inexact
  , invalidContext
  , underflow
  , overflow
  , conversionSyntax

  , Flags
  , setFlag
  , clearFlag
  , checkFlag
  , emptyFlags
  , displayFlags

  -- * Env monad
  , Env
  , getStatus
  , setStatus
  , getRound
  , setRound
  , runEnvIO

  -- * Class
  , DecClass
  , sNan
  , qNan
  , negInf
  , negNormal
  , negSubnormal
  , negZero
  , posZero
  , posSubnormal
  , posNormal
  , posInf
  , decClass

  -- * Conversions
  -- ** Complete encoding and decoding
  , Coefficient
  , coefficient
  , unCoefficient
  , CoeffExp
  , seCoeff
  , seExp
  , coeffExp
  , minMaxExp
  , Payload
  , unPayload
  , payload
  , zeroPayload
  , Sign(..)
  , NaN(..)
  , Value(..)
  , Decoded(..)
  , decode
  , encode

  -- ** Strings
  , fromString
  , toString
  , toEngString

  -- ** Integers
  , fromInt32
  , fromUInt32
  , toInt32
  , toInt32Exact
  , toUInt32
  , toUInt32Exact

  -- ** Other Dec
  , toIntegralExact
  , toIntegralValue

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
  , compareSignal
  , compareTotal
  , compareTotalMag
  , max
  , maxMag
  , min
  , minMag
  , sameQuantum

  -- * Tests
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

  -- * Logical, bitwise, digit shifting
  , and
  , or
  , shift
  , xor
  , rotate
  , invert

  -- * Transcendental
  , logB
  , scaleB

  -- * Attributes
  , digits

  -- * Constants
  , zero

  -- * Library info
  , version

  ) where

import Control.Applicative
import Control.Monad
import Foreign.Safe hiding
  ( void
  , isSigned
  , rotate
  , shift
  , xor
  )
import Foreign.C
import Data.Deka.Decnumber
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
import qualified Data.ByteString.Char8 as BS8
import Data.List (foldl', unfoldr, genericLength)
import Control.Monad.Trans.Writer

-- # Rounding

newtype Round = Round { unRound :: CInt }
  deriving (Eq, Ord, Show)

roundCeiling :: Round
roundCeiling = Round c'DEC_ROUND_CEILING

roundUp :: Round
roundUp = Round c'DEC_ROUND_UP

roundHalfUp :: Round
roundHalfUp = Round c'DEC_ROUND_HALF_UP

roundHalfEven :: Round
roundHalfEven = Round c'DEC_ROUND_HALF_EVEN

roundHalfDown :: Round
roundHalfDown = Round c'DEC_ROUND_HALF_DOWN

roundDown :: Round
roundDown = Round c'DEC_ROUND_DOWN

roundFloor :: Round
roundFloor = Round c'DEC_ROUND_FLOOR

round05Up :: Round
round05Up = Round c'DEC_ROUND_05UP

roundMax :: Round
roundMax = Round c'DEC_ROUND_MAX

-- # Status

newtype Flag = Flag C'uint32_t
  deriving (Eq, Ord, Show)

-- Docs are a bit unclear about what status flags can actually be
-- set; the source code reveals that these can be set.

divisionUndefined :: Flag
divisionUndefined = Flag c'DEC_Division_undefined

divisionByZero :: Flag
divisionByZero = Flag c'DEC_Division_by_zero

divisionImpossible :: Flag
divisionImpossible = Flag c'DEC_Division_impossible

invalidOperation :: Flag
invalidOperation = Flag c'DEC_Invalid_operation

inexact :: Flag
inexact = Flag c'DEC_Inexact

invalidContext :: Flag
invalidContext = Flag c'DEC_Invalid_context

underflow :: Flag
underflow = Flag c'DEC_Underflow

overflow :: Flag
overflow = Flag c'DEC_Overflow

conversionSyntax :: Flag
conversionSyntax = Flag c'DEC_Conversion_syntax

-- | A container for multiple 'Flag' indicating which are set and
-- which are not.
newtype Flags = Flags { unFlags :: C'uint32_t }
  deriving (Eq, Ord, Show)

setFlag :: Flag -> Flags -> Flags
setFlag (Flag f1) (Flags fA) = Flags (f1 .|. fA)

clearFlag :: Flag -> Flags -> Flags
clearFlag (Flag f1) (Flags fA) = Flags (complement f1 .&. fA)

-- | Is this 'Flag' set?
checkFlag :: Flag -> Flags -> Bool
checkFlag (Flag f1) (Flags fA) = (f1 .&. fA) /= 0

-- | A 'Flags' with no 'Flag' set.
emptyFlags :: Flags
emptyFlags = Flags 0

-- | Gives a list of strings, one for each flag that is set.
displayFlags :: Flags -> [String]
displayFlags fl = execWriter $ do
  let f s g
        | checkFlag g fl = tell [s]
        | otherwise = return ()
  f "divisionUndefined" divisionUndefined
  f "divisionByZero" divisionByZero
  f "divisionImpossible" divisionImpossible
  f "invalidOperation" invalidOperation
  f "inexact" inexact
  f "invalidContext" invalidContext
  f "underflow" underflow
  f "overflow" overflow
  f "conversionSyntax" conversionSyntax


-- | The Env monad
--
-- Since this is a binding to a C library, all the work happens in
-- the mutable land of C pointers.  In addition, the General Decimal
-- Arithmetic specification states that most computations occur
-- within a @context@, which affects the manner in which
-- computations are done (for instance, the context determines the
-- rounding algorithm).  The context also carries some results from
-- computations (for instance, a computation might set a flag to
-- indicate that the result is rounded or inexact or was a division
-- by zero.)
--
-- The Env monad captures both the context and the IO.  Because
-- 'Dec' is exposed only as an immutable type, and because there is
-- no way to do arbitrary IO in the Env monad (which is why it is
-- not an instance of the 'MonadIO' class), it is safe to put an
-- 'unsafePerformIO' on Env computations so they can be done in pure
-- functions.
newtype Env a = Env { unEnv :: Ptr C'decContext -> IO a }

instance Functor Env where
  fmap = liftM

instance Applicative Env where
  pure = return
  (<*>) = ap

instance Monad Env where
  return a = Env $ \_ -> return a
  Env a >>= f = Env $ \p -> do
    r1 <- a p
    let b = unEnv $ f r1
    b p
  fail s = Env $ \_ -> fail s

-- | The current status flags, which indicate results from previous
-- computations.
getStatus :: Env Flags
getStatus = Env $ \cPtr -> do
  let pSt = p'decContext'status cPtr
  fmap Flags . peek $ pSt

-- | Set the current status to whatever you wish.
setStatus :: Flags -> Env ()
setStatus f = Env $ \cPtr -> do
  let pSt = p'decContext'status cPtr
  poke pSt . unFlags $ f

-- | The current rounding method
getRound :: Env Round
getRound = Env $ \cPtr -> do
  let pR = p'decContext'round cPtr
  fmap Round . peek $ pR

-- | Change the current rounding method
setRound :: Round -> Env ()
setRound r = Env $ \cPtr -> do
  let pR = p'decContext'round cPtr
  poke pR . unRound $ r

-- | By default, rounding is half even.  No status flags are set
-- initially.  Returns the final status flags along with the result
-- of the computation.
runEnvIO :: Env a -> IO (a, Flags)
runEnvIO (Env k) = do
  fp <- mallocForeignPtr
  withForeignPtr fp $ \pCtx -> do
    _ <- c'decContextDefault pCtx c'DEC_INIT_DECQUAD
    res <- k pCtx
    fl' <- fmap Flags . peek . p'decContext'status $ pCtx
    return (res, fl')

-- # Class

-- | Different categories of 'Dec'.
newtype DecClass = DecClass CInt
  deriving (Eq, Ord, Show)

sNan :: DecClass
sNan = DecClass c'DEC_CLASS_SNAN

qNan :: DecClass
qNan = DecClass c'DEC_CLASS_QNAN

negInf :: DecClass
negInf = DecClass c'DEC_CLASS_NEG_INF

negNormal :: DecClass
negNormal = DecClass c'DEC_CLASS_NEG_NORMAL

negSubnormal :: DecClass
negSubnormal = DecClass c'DEC_CLASS_NEG_SUBNORMAL

negZero :: DecClass
negZero = DecClass c'DEC_CLASS_NEG_ZERO

posZero :: DecClass
posZero = DecClass c'DEC_CLASS_POS_ZERO

posSubnormal :: DecClass
posSubnormal = DecClass c'DEC_CLASS_POS_SUBNORMAL

posNormal :: DecClass
posNormal = DecClass c'DEC_CLASS_POS_NORMAL

posInf :: DecClass
posInf = DecClass c'DEC_CLASS_POS_INF



-- | Decimal number.  This is immutable, like any Haskell value you
-- would ordinarily work with.  (Actually, that is a bit of a lie.
-- Under the covers it is a pointer to a C struct, which is in fact
-- mutable like anything in C.  However, the API exposed in this
-- module does not mutate a Dec after a function returns one.)
--
-- As indicated in the General Decimal Arithmetic specification,
-- a 'Dec' might be a finite number (perhaps the most common type)
-- or it might be infinite or a not-a-number.  'decClass' will tell
-- you a little more about a particular 'Dec'.
newtype Dec = Dec { unDec :: ForeignPtr C'decQuad }

-- | Creates a new Dec.  Uninitialized, so don't export this
-- function.
newDec :: IO Dec
newDec = fmap Dec mallocForeignPtr

-- # Helpers

type Unary
  = Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

unary
  :: Unary
  -> Dec
  -> Env Dec
unary f d = Env $ \ptrC ->
  newDec >>= \r ->
  withForeignPtr (unDec d) $ \ptrX ->
  withForeignPtr (unDec r) $ \ptrR ->
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
  -> Dec
  -> Dec
  -> Env Dec
binary f x y = Env $ \pC ->
  newDec >>= \r ->
  withForeignPtr (unDec r) $ \pR ->
  withForeignPtr (unDec x) $ \pX ->
  withForeignPtr (unDec y) $ \pY ->
  f pR pX pY pC >>
  return r

type BinaryCtxFree
  = Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO (Ptr C'decQuad)

binaryCtxFree
  :: BinaryCtxFree
  -> Dec
  -> Dec
  -> Env Dec
binaryCtxFree f x y = Env $ \_ ->
  newDec >>= \r ->
  withForeignPtr (unDec r) $ \pR ->
  withForeignPtr (unDec x) $ \pX ->
  withForeignPtr (unDec y) $ \pY ->
  f pR pX pY >>
  return r

type UnaryGet a
  = Ptr C'decQuad
  -> IO a

unaryGet
  :: UnaryGet a
  -> Dec
  -> Env a
unaryGet f d = Env $ \_ ->
  withForeignPtr (unDec d) $ \pD -> f pD

type Ternary
  = Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

ternary
  :: Ternary
  -> Dec
  -> Dec
  -> Dec
  -> Env Dec
ternary f x y z = Env $ \pC ->
  newDec >>= \r ->
  withForeignPtr (unDec r) $ \pR ->
  withForeignPtr (unDec x) $ \pX ->
  withForeignPtr (unDec y) $ \pY ->
  withForeignPtr (unDec z) $ \pZ ->
  f pR pX pY pZ pC
  >> return r

type Boolean
  = Ptr C'decQuad
  -> IO C'uint32_t

boolean
  :: Boolean
  -> Dec
  -> Env Bool
boolean f d = Env $ \_ ->
  withForeignPtr (unDec d) $ \pD ->
  f pD >>= \r ->
  return $ case r of
    1 -> True
    _ -> False

type MkString
  = Ptr C'decQuad
  -> CString
  -> IO CString

mkString
  :: MkString
  -> Dec
  -> Env BS8.ByteString
mkString f d = Env $ \_ ->
  withForeignPtr (unDec d) $ \pD ->
  allocaBytes c'DECQUAD_String $ \pS ->
  f pD pS
  >> BS8.packCString pS

type GetRounded a
  = Ptr C'decQuad
  -> Ptr C'decContext
  -> CInt
  -> IO a

getRounded
  :: GetRounded a
  -> Round
  -> Dec
  -> Env a
getRounded f (Round r) d = Env $ \pC ->
  withForeignPtr (unDec d) $ \pD ->
  f pD pC r

-- # End Helpers

-- # Functions from decQuad. In alphabetical order.

-- | Absolute value.  NaNs are handled normally (the sign of an NaN
-- is not affected, and an sNaN sets 'invalidOperation'.
abs :: Dec -> Env Dec
abs = unary c'decQuadAbs

add :: Dec -> Dec -> Env Dec
add = binary c'decQuadAdd

-- | Digit-wise logical and.  Operands must be:
--
-- * zero or positive
-- * integers
-- * comprise only zeroes and/or ones
--
-- If not, 'invalidOperation' is set.
and :: Dec -> Dec -> Env Dec
and = binary c'decQuadAnd

-- | More information about a particular 'Dec'.
decClass :: Dec -> Env DecClass
decClass = fmap DecClass . unaryGet c'decQuadClass

-- | Compares two 'Dec' numerically.  The result might be @-1@, @0@,
-- @1@, or NaN, where @-1@ means x is less than y, @0@ indicates
-- numerical equality, @1@ means y is greater than x.  NaN is
-- returned only if x or y is an NaN.
--
-- Thus, this function does not return an 'Ordering' because the
-- result might be an NaN.
--
compare :: Dec -> Dec -> Env Dec
compare = binary c'decQuadCompare

-- | Same as 'compare', but a quietNaN is treated like a signaling
-- NaN (sets 'invalidOperation').
compareSignal :: Dec -> Dec -> Env Dec
compareSignal = binary c'decQuadCompareSignal

-- | Compares using an IEEE 754 total ordering, which takes into
-- account the exponent.  IEEE 754 says that this function might
-- return different results depending upon whether the operands are
-- canonical; 'Dec' are always canonical so you don't need to worry
-- about that here.
compareTotal :: Dec -> Dec -> Env Dec
compareTotal = binaryCtxFree c'decQuadCompareTotal

-- | Same as 'compareTotal' but compares the absolute value of the
-- two arguments.
compareTotalMag :: Dec -> Dec -> Env Dec
compareTotalMag = binaryCtxFree c'decQuadCompareTotalMag

-- | @copySign x y@ returns @z@, which is a copy of @x@ but has the
-- sign of @y@.  Unlike @decQuadCopySign@, the result is always
-- canonical.  This function never raises any signals.
copySign :: Dec -> Dec -> Env Dec
copySign fr to = Env $ \_ ->
  newDec >>= \r ->
  withForeignPtr (unDec r) $ \pR ->
  withForeignPtr (unDec fr) $ \pF ->
  withForeignPtr (unDec to) $ \pT ->
  c'decQuadCopySign pR pF pT >>
  c'decQuadCanonical pR pR >>
  return r

digits :: Dec -> Env Int
digits = fmap fromIntegral . unaryGet c'decQuadDigits

divide :: Dec -> Dec -> Env Dec
divide = binary c'decQuadDivide

divideInteger :: Dec -> Dec -> Env Dec
divideInteger = binary c'decQuadDivideInteger

-- | fused multiply add.
fma :: Dec -> Dec -> Dec -> Env Dec
fma = ternary c'decQuadFMA

fromInt32 :: C'int32_t -> Env Dec
fromInt32 i = Env $ \_ ->
  newDec >>= \r ->
  withForeignPtr (unDec r) $ \pR ->
  c'decQuadFromInt32 pR i
  >> return r

fromString :: BS8.ByteString -> Env Dec
fromString s = Env $ \pC ->
  newDec >>= \r ->
  withForeignPtr (unDec r) $ \pR ->
  BS8.useAsCString s $ \pS ->
  c'decQuadFromString pR pS pC >>
  return r

fromUInt32 :: C'uint32_t -> Env Dec
fromUInt32 i = Env $ \_ ->
  newDec >>= \r ->
  withForeignPtr (unDec r) $ \pR ->
  c'decQuadFromUInt32 pR i >>
  return r

invert :: Dec -> Env Dec
invert = unary c'decQuadInvert

isFinite :: Dec -> Env Bool
isFinite = boolean c'decQuadIsFinite

isInfinite :: Dec -> Env Bool
isInfinite = boolean c'decQuadIsInfinite

isInteger :: Dec -> Env Bool
isInteger = boolean c'decQuadIsInteger

isLogical :: Dec -> Env Bool
isLogical = boolean c'decQuadIsLogical

isNaN :: Dec -> Env Bool
isNaN = boolean c'decQuadIsNaN

isNegative :: Dec -> Env Bool
isNegative = boolean c'decQuadIsNegative

isNormal :: Dec -> Env Bool
isNormal = boolean c'decQuadIsNormal

isPositive :: Dec -> Env Bool
isPositive = boolean c'decQuadIsPositive

isSignaling :: Dec -> Env Bool
isSignaling = boolean c'decQuadIsSignaling

isSigned :: Dec -> Env Bool
isSigned = boolean c'decQuadIsSigned

isSubnormal :: Dec -> Env Bool
isSubnormal = boolean c'decQuadIsSubnormal

isZero :: Dec -> Env Bool
isZero = boolean c'decQuadIsZero

logB :: Dec -> Env Dec
logB = unary c'decQuadLogB

max :: Dec -> Dec -> Env Dec
max = binary c'decQuadMax

maxMag :: Dec -> Dec -> Env Dec
maxMag = binary c'decQuadMaxMag

min :: Dec -> Dec -> Env Dec
min = binary c'decQuadMin

minMag :: Dec -> Dec -> Env Dec
minMag = binary c'decQuadMinMag

minus :: Dec -> Env Dec
minus = unary c'decQuadMinus

multiply :: Dec -> Dec -> Env Dec
multiply = binary c'decQuadMultiply

nextMinus :: Dec -> Env Dec
nextMinus = unary c'decQuadNextMinus

nextPlus :: Dec -> Env Dec
nextPlus = unary c'decQuadNextPlus

nextToward :: Dec -> Dec -> Env Dec
nextToward = binary c'decQuadNextToward

-- | Digit wise logical inclusive Or.  Operands must be:
--
-- * zero or positive
-- * integers
-- * comprise only zeroes and/or ones
--
-- If not, 'invalidOperation' is set.
or :: Dec -> Dec -> Env Dec
or = binary c'decQuadOr

-- | Same effect as @0 + x@ where the exponent of the zero is the
-- same as that of @x@ if @x@ is finite).  NaNs are handled as for
-- arithmetic operations.
plus :: Dec -> Env Dec
plus = unary c'decQuadPlus

-- | @quantize x y@ returns @z@ which is @x@ set to have the same
-- quantum as @y@; that is, numerically the same value but rounded
-- or padded if necessary to have the same exponent as @y@.  Useful
-- for rounding monetary quantities.
quantize :: Dec -> Dec -> Env Dec
quantize = binary c'decQuadQuantize

reduce :: Dec -> Env Dec
reduce = unary c'decQuadReduce

remainder :: Dec -> Dec -> Env Dec
remainder = binary c'decQuadRemainder

remainderNear :: Dec -> Dec -> Env Dec
remainderNear = binary c'decQuadRemainderNear

rotate :: Dec -> Dec -> Env Dec
rotate = binary c'decQuadRotate

sameQuantum :: Dec -> Dec -> Env Bool
sameQuantum x y = Env $ \_ ->
  withForeignPtr (unDec x) $ \pX ->
  withForeignPtr (unDec y) $ \pY ->
  c'decQuadSameQuantum pX pY >>= \r ->
  return $ case r of
    1 -> True
    _ -> False

scaleB :: Dec -> Dec -> Env Dec
scaleB = binary c'decQuadScaleB

-- skipped: SetCoefficient
-- skipped : SetExponent

shift :: Dec -> Dec -> Env Dec
shift = binary c'decQuadShift

-- omitted: Show

subtract :: Dec -> Dec -> Env Dec
subtract = binary c'decQuadSubtract

toEngString :: Dec -> Env BS8.ByteString
toEngString = mkString c'decQuadToEngString

toInt32 :: Round -> Dec -> Env C'int32_t
toInt32 = getRounded c'decQuadToInt32

toInt32Exact :: Round -> Dec -> Env C'int32_t
toInt32Exact = getRounded c'decQuadToInt32Exact

toIntegralExact :: Dec -> Env Dec
toIntegralExact = unary c'decQuadToIntegralExact

toIntegralValue :: Round -> Dec -> Env Dec
toIntegralValue (Round rnd) d = Env $ \pC ->
  withForeignPtr (unDec d) $ \pD ->
  newDec >>= \r ->
  withForeignPtr (unDec r) $ \pR ->
  c'decQuadToIntegralValue pR pD pC rnd >>
  return r

toString :: Dec -> Env BS8.ByteString
toString = mkString c'decQuadToString

toUInt32 :: Round -> Dec -> Env C'uint32_t
toUInt32 = getRounded c'decQuadToUInt32

toUInt32Exact :: Round -> Dec -> Env C'uint32_t
toUInt32Exact = getRounded c'decQuadToUInt32Exact

version :: Env BS8.ByteString
version = Env $ \_ ->
  c'decQuadVersion >>= BS8.packCString

xor :: Dec -> Dec -> Env Dec
xor = binary c'decQuadXor

zero :: Env Dec
zero = Env $ \_ ->
  newDec >>= \d ->
  withForeignPtr (unDec d) $ \pD ->
  c'decQuadZero pD >>
  return d

-- # Conversions

data Sign
  = Positive
  -- ^ The number is positive or is zero
  | Negative
  -- ^ The number is negative or the negative zero
  deriving (Eq, Ord, Show, Enum, Bounded)

data NaN
  = Quiet
  | Signaling
  deriving (Eq, Ord, Show, Enum, Bounded)

data Coefficient = Coefficient { unCoefficient :: Integer }
  deriving (Eq, Ord, Show)

-- | Coefficients must be zero or positive, and the number of digits
-- must be less than or equal to c'DECQUAD_Pmax
coefficient :: Integer -> Either String Coefficient
coefficient i
  | i < 0 = Left "coefficient cannot be negative"
  | len > c'DECQUAD_Pmax = Left "coefficient has too many digits"
  | otherwise = Right . Coefficient $ i
  where
    len = genericLength . show $ i
    _types = len :: Integer

-- | Significand and exponent for a finite number.
data CoeffExp = CoeffExp
  { seCoeff :: Coefficient
  , seExp :: Int
  } deriving (Eq, Ord, Show)

-- | Enforces relationship between significand and exponent.
--
-- If the coefficient has c digits, and Emax is x, the exponent e
-- is within the closed-ended range
--
-- @-x - (c - 1) + 1@ and @x - (c - 1)@
--
-- See Decimal Arithmetic Specification version 1.70, page 10.

coeffExp
  :: Coefficient
  -> Int
  -- ^ Exponent
  -> Either String CoeffExp
coeffExp c e
  | e > h = Left "exponent is too large"
  | e < l = Left "exponent is too small"
  | otherwise = Right $ CoeffExp c e
  where
    (l, h) = minMaxExp c

-- | The minimum and maximum possible exponent for a given
-- significand.
minMaxExp :: Coefficient -> (Int, Int)
minMaxExp (Coefficient s) = (l, h)
  where
    l = negate x - (c - 1) + 1
    h = x - (c - 1)
    x = c'DECQUAD_Emax
    c = length . show $ s

-- | A Payload is associated with an NaN.  It is always zero or
-- positive.  The number of digits is always less than or equal to
-- Pmax - 1 (33 digits).
data Payload = Payload { unPayload :: Integer }
  deriving (Eq, Ord, Show)

payload :: Integer -> Maybe Payload
payload i
  | i < 0 = Nothing
  | length (show i) > c'DECQUAD_Pmax - 1 = Nothing
  | otherwise = Just . Payload $ i

zeroPayload :: Payload
zeroPayload = Payload 0

data Value
  = Finite CoeffExp
  | Infinite
  | NaN NaN Payload
  deriving (Eq, Ord, Show)

data Decoded = Decoded
  { dSign :: Sign
  , dValue :: Value
  } deriving (Eq, Ord, Show)


decode :: Dec -> Env Decoded
decode d = Env $ \_ ->
  withForeignPtr (unDec d) $ \pD ->
  allocaBytes c'DECQUAD_Pmax $ \pArr ->
  alloca $ \pExp ->
  c'decQuadToBCD pD pExp pArr >>= \sgn ->
  peek pExp >>= \ex ->
  peekArray c'DECQUAD_Pmax pArr >>= \coef ->
  return (getDecoded sgn ex coef)

-- | Encodes a new 'Dec'.  The result is always canonical.  However,
-- the function does not signal if the result is an sNaN.
encode :: Decoded -> Env Dec
encode dcd = Env $ \_ ->
  newDec >>= \d ->
  withForeignPtr (unDec d) $ \pD ->
  let (expn, arr, sgn) = packDecoded dcd in
  withArray arr $ \pArr ->
  c'decQuadFromBCD pD expn pArr sgn >>
  c'decQuadCanonical pD pD >>
  return d

-- Converts a BCD to an Integer.
fromBCD
  :: Int
  -- ^ List length
  -> [C'uint8_t]
  -> Integer
fromBCD len = snd . foldl' f (len, 0)
  where
    f (c, t) i =
      let c' = c - 1
          t' = t + fromIntegral i * 10 ^ e
          e = fromIntegral c'
          _types = e :: Int
      in c' `seq` t' `seq` (c', t')

toBCD
  :: Int
  -- ^ BCD will be this long
  -> Integer
  -> [C'uint8_t]
toBCD len start = unfoldr f (len, start)
  where
    f (c, r) =
      let c' = c - 1
          (q, r') = r `quotRem` (10 ^ c')
          g | c == 0 = Nothing
            | q > 9 = error "toBCD: number out of range"
            | otherwise = Just (fromIntegral q, (c', r'))
      in g


packDecoded :: Decoded -> (C'int32_t, [C'uint8_t], C'int32_t)
packDecoded (Decoded s v) = (expn, bcd, sign)
  where
    sign = case s of
      Positive -> 0
      Negative -> c'DECFLOAT_Sign
    (expn, bcd) = case v of
      Finite (CoeffExp coe ex) ->
        (fromIntegral ex, toBCD c'DECQUAD_Pmax (unCoefficient coe))
      Infinite ->
        (c'DECFLOAT_Inf, toBCD c'DECQUAD_Pmax 0)
      NaN n p -> (ex, bc)
        where
          ex = case n of
            Signaling -> c'DECFLOAT_sNaN
            Quiet -> c'DECFLOAT_qNaN
          bc = 0 : toBCD (c'DECQUAD_Pmax - 1) (unPayload p)


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
    s = if sgn == 0 then Positive else Negative
    v | ex == c'DECFLOAT_qNaN = NaN Quiet pld
      | ex == c'DECFLOAT_sNaN = NaN Signaling pld
      | ex == c'DECFLOAT_Inf = Infinite
      | otherwise = Finite (CoeffExp coe (fromIntegral ex))
      where
        pld = Payload $ fromBCD (c'DECQUAD_Pmax - 1) (tail coef)
        coe = Coefficient $ fromBCD c'DECQUAD_Pmax coef

-- decQuad functions not recreated here:

-- skipped: classString - not needed
-- skipped: copy - not needed
-- skipped: copyAbs - use abs instead
-- skipped: copyNegate - use negate instead
-- skipped: fromBCD - use encode function instead
-- skipped: fromNumber - not needed
-- skipped: fromPacked - use encode function instead
-- skipped: fromPackedChecked - use encode function instead
-- skipped: fromWider - not needed
-- skipped: getCoefficient - use decode function instead
-- skipped: getExponent - use decode function instead
-- skipped: isCanonical - not needed
-- skipped: radix - not needed
-- skipped: setCoefficient - use encode function instead
-- skipped: setExponent - use encode function instead
-- skipped: toBCD - use decode function instead
-- skipped: toNumber - not needed
-- skipped: toPacked - use decode function instead
-- skipped: toWider - not needed
-- skipped: show - not needed; impure
