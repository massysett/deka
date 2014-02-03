{-# LANGUAGE Trustworthy #-}

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
-- Every function in this module returns canonical 'Quad' (contrast
-- the decNumber library which will, under limited circumstances,
-- return non-canonical decimals).  I am not certain that canonical
-- vs. non-canonical matters in typical use, though it may matter
-- when using 'compareTotal'.
--
-- Many functions in this module clash with Prelude names, so you
-- might want to do
--
-- > import qualified Data.Deka.Pure as D
module Data.Deka.Quad
  (
    -- * Quad
    Quad

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
  , flagList

  -- * Ctx monad
  , Ctx
  , getStatus
  , setStatus
  , getRound
  , setRound
  , runCtx
  , evalCtx

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
  -- ** Digits
  , Digit(..)
  , digitsToIntegral
  , integralToDigits

  -- ** Complete encoding and decoding

  -- *** Coefficients
  , coefficientLen
  , payloadDigitsLen
  , Coefficient
  , coefficient
  , unCoefficient
  , Payload
  , payloadDigits
  , unPayload

  -- *** Exponents
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

  -- *** Sign, NaN, Value, Decoded
  , Sign(..)
  , NaN(..)
  , Value(..)
  , Decoded(..)

  --- *** Conversion functions
  , fromBCD
  , toBCD

  -- ** Strings
  , fromByteString
  , toByteString
  , toEngByteString

  -- ** Integers
  , fromInt32
  , fromUInt32
  , toInt32
  , toInt32Exact
  , toUInt32
  , toUInt32Exact

  -- ** Other Quad
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

  -- * Decoded predicates

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

  ) where

-- # Imports

import Data.Maybe
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
import qualified Prelude
import qualified Data.ByteString.Char8 as BS8
import Control.Monad.Trans.Writer
import System.IO.Unsafe (unsafePerformIO)

-- # Rounding

newtype Round = Round { unRound :: C'rounding }
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
flagList :: Flags -> [String]
flagList fl = execWriter $ do
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


-- | The Ctx monad
--
-- The General Decimal Arithmetic specification states that most
-- computations occur within a @context@, which affects the manner
-- in which computations are done (for instance, the context
-- determines the rounding algorithm).  The context also carries
-- some results from computations (for instance, a computation might
-- set a flag to indicate that the result is rounded or inexact or
-- was a division by zero.) The Ctx monad carries this context.
newtype Ctx a = Ctx { unCtx :: Ptr C'decContext -> IO a }

instance Functor Ctx where
  fmap = liftM

instance Applicative Ctx where
  pure = return
  (<*>) = ap

instance Monad Ctx where
  return a = Ctx $ \_ -> return a
  Ctx a >>= f = Ctx $ \p -> do
    r1 <- a p
    let b = unCtx $ f r1
    b p
  fail s = Ctx $ \_ -> fail s

-- | The current status flags, which indicate results from previous
-- computations.
getStatus :: Ctx Flags
getStatus = Ctx $ \cPtr -> do
  let pSt = p'decContext'status cPtr
  fmap Flags . peek $ pSt

-- | Set the current status to whatever you wish.
setStatus :: Flags -> Ctx ()
setStatus f = Ctx $ \cPtr -> do
  let pSt = p'decContext'status cPtr
  poke pSt . unFlags $ f

-- | The current rounding method
getRound :: Ctx Round
getRound = Ctx $ \cPtr -> do
  let pR = p'decContext'round cPtr
  fmap Round . peek $ pR

-- | Change the current rounding method
setRound :: Round -> Ctx ()
setRound r = Ctx $ \cPtr -> do
  let pR = p'decContext'round cPtr
  poke pR . unRound $ r

-- | By default, rounding is half even.  No status flags are set
-- initially.  Returns the final status flags along with the result
-- of the computation.
runCtx :: Ctx a -> (a, Flags)
runCtx (Ctx k) = unsafePerformIO $ do
  fp <- mallocForeignPtr
  withForeignPtr fp $ \pCtx -> do
    _ <- unsafe'c'decContextDefault pCtx c'DEC_INIT_DECQUAD
    res <- k pCtx
    fl' <- fmap Flags . peek . p'decContext'status $ pCtx
    return (res, fl')

-- | Like 'runCtx' but does not return the final flags.
evalCtx :: Ctx a -> a
evalCtx (Ctx k) = unsafePerformIO $ do
  fp <- mallocForeignPtr
  withForeignPtr fp $ \pCtx -> do
    _ <- unsafe'c'decContextDefault pCtx c'DEC_INIT_DECQUAD
    k pCtx



-- # Class

-- | Different categories of 'Quad'.
newtype DecClass = DecClass C'decClass
  deriving (Eq, Ord)

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

instance Show DecClass where
  show (DecClass x)
    | x == c'DEC_CLASS_SNAN = "sNaN"
    | x == c'DEC_CLASS_QNAN = "NaN"
    | x == c'DEC_CLASS_NEG_INF = "-Infinity"
    | x == c'DEC_CLASS_NEG_NORMAL = "-Normal"
    | x == c'DEC_CLASS_NEG_SUBNORMAL = "-Subnormal"
    | x == c'DEC_CLASS_NEG_ZERO = "-Zero"
    | x == c'DEC_CLASS_POS_ZERO = "+Zero"
    | x == c'DEC_CLASS_POS_SUBNORMAL = "+Subnormal"
    | x == c'DEC_CLASS_POS_NORMAL = "+Normal"
    | x == c'DEC_CLASS_POS_INF = "+Infinity"
    | otherwise = error "decClass show: invalid value"


-- | Decimal number.  This is immutable, like any Haskell value you
-- would ordinarily work with.  (Actually, that is a bit of a lie.
-- Under the covers it is a pointer to a C struct, which is in fact
-- mutable like anything in C.  However, no function exposed in this
-- API mutates a Quad's pointee after the Quad is created.)
--
-- As indicated in the General Decimal Arithmetic specification,
-- a 'Quad' might be a finite number (perhaps the most common type)
-- or it might be infinite or a not-a-number.  'decClass' will tell
-- you a little more about a particular 'Quad'.
newtype Quad = Quad { unDec :: ForeignPtr C'decQuad }

-- | Creates a new Quad.  Uninitialized, so don't export this
-- function.
newQuad :: IO Quad
newQuad = fmap Quad mallocForeignPtr

-- # Helpers

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
  -> Quad
  -> Quad
  -> Ctx Quad
binary f x y = Ctx $ \pC ->
  newQuad >>= \r ->
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
  -> Quad
  -> Quad
  -> Quad
binaryCtxFree f x y = unsafePerformIO $
  newQuad >>= \r ->
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
  -> Quad
  -> a
unaryGet f d = unsafePerformIO $
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
  -> Quad
  -> Quad
  -> Quad
  -> Ctx Quad
ternary f x y z = Ctx $ \pC ->
  newQuad >>= \r ->
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
  -> Quad
  -> Bool
boolean f d = unsafePerformIO $
  withForeignPtr (unDec d) $ \pD ->
  f pD >>= \r ->
  return $ case r of
    1 -> True
    0 -> False
    _ -> error "boolean: bad return value"

type MkString
  = Ptr C'decQuad
  -> CString
  -> IO CString

mkString
  :: MkString
  -> Quad
  -> BS8.ByteString
mkString f d = unsafePerformIO $
  withForeignPtr (unDec d) $ \pD ->
  allocaBytes c'DECQUAD_String $ \pS ->
  f pD pS
  >> BS8.packCString pS

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
  withForeignPtr (unDec d) $ \pD ->
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
-- * integers
-- * comprise only zeroes and/or ones
--
-- If not, 'invalidOperation' is set.
and :: Quad -> Quad -> Ctx Quad
and = binary unsafe'c'decQuadAnd

-- | More information about a particular 'Quad'.
decClass :: Quad -> DecClass
decClass = DecClass . unaryGet unsafe'c'decQuadClass

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

-- | Same as 'compare', but a quietNaN is treated like a signaling
-- NaN (sets 'invalidOperation').
compareSignal :: Quad -> Quad -> Ctx Quad
compareSignal = binary unsafe'c'decQuadCompareSignal

-- | Compares using an IEEE 754 total ordering, which takes into
-- account the exponent.  IEEE 754 says that this function might
-- return different results depending upon whether the operands are
-- canonical; 'Quad' are always canonical so you don't need to worry
-- about that here.
compareTotal :: Quad -> Quad -> Quad
compareTotal = binaryCtxFree unsafe'c'decQuadCompareTotal

-- | Same as 'compareTotal' but compares the absolute value of the
-- two arguments.
compareTotalMag :: Quad -> Quad -> Quad
compareTotalMag = binaryCtxFree unsafe'c'decQuadCompareTotalMag

-- | @copySign x y@ returns @z@, which is a copy of @x@ but has the
-- sign of @y@.  Unlike @decQuadCopySign@, the result is always
-- canonical.  This function never raises any signals.
copySign :: Quad -> Quad -> Quad
copySign fr to = unsafePerformIO $
  newQuad >>= \r ->
  withForeignPtr (unDec r) $ \pR ->
  withForeignPtr (unDec fr) $ \pF ->
  withForeignPtr (unDec to) $ \pT ->
  unsafe'c'decQuadCopySign pR pF pT >>
  unsafe'c'decQuadCanonical pR pR >>
  return r

digits :: Quad -> Int
digits = fromIntegral . unaryGet unsafe'c'decQuadDigits

divide :: Quad -> Quad -> Ctx Quad
divide = binary unsafe'c'decQuadDivide

divideInteger :: Quad -> Quad -> Ctx Quad
divideInteger = binary unsafe'c'decQuadDivideInteger

-- | fused multiply add.
fma :: Quad -> Quad -> Quad -> Ctx Quad
fma = ternary unsafe'c'decQuadFMA

fromInt32 :: C'int32_t -> Quad
fromInt32 i = unsafePerformIO $
  newQuad >>= \r ->
  withForeignPtr (unDec r) $ \pR ->
  unsafe'c'decQuadFromInt32 pR i
  >> return r

-- | Reads a ByteString, which can be in scientific, engineering, or
-- \"regular\" decimal notation.  Also reads NaN, Infinity, etc.
-- Will return a signaling NaN and set the Invalid flag if the
-- string given is invalid.
--
-- In the decNumber C library, this function was called
-- @fromString@; the name was changed here because it doesn't take a
-- regular Haskell 'String'.
fromByteString :: BS8.ByteString -> Ctx Quad
fromByteString s = Ctx $ \pC ->
  newQuad >>= \r ->
  withForeignPtr (unDec r) $ \pR ->
  BS8.useAsCString s $ \pS ->
  unsafe'c'decQuadFromString pR pS pC >>
  return r

fromUInt32 :: C'uint32_t -> Quad
fromUInt32 i = unsafePerformIO $
  newQuad >>= \r ->
  withForeignPtr (unDec r) $ \pR ->
  unsafe'c'decQuadFromUInt32 pR i >>
  return r

invert :: Quad -> Ctx Quad
invert = unary unsafe'c'decQuadInvert

isFinite :: Quad -> Bool
isFinite = boolean unsafe'c'decQuadIsFinite

isInfinite :: Quad -> Bool
isInfinite = boolean unsafe'c'decQuadIsInfinite

-- | True if @x@ is finite and has exponent of @0@; False otherwise.
-- This can lead to unexpected results; for instance, 3 x 10 ^ 2 is
-- 300, but this function will return False.
isInteger :: Quad -> Bool
isInteger = boolean unsafe'c'decQuadIsInteger

-- | True only if @x@ is zero or positive, an integer (finite with
-- exponent of 0), and the coefficient is only zeroes and/or ones.
isLogical :: Quad -> Bool
isLogical = boolean unsafe'c'decQuadIsLogical

isNaN :: Quad -> Bool
isNaN = boolean unsafe'c'decQuadIsNaN

-- | True only if @x@ is less than zero and is not an NaN.
isNegative :: Quad -> Bool
isNegative = boolean unsafe'c'decQuadIsNegative

-- | True only if @x@ is finite, non-zero, and not subnormal.
isNormal :: Quad -> Bool
isNormal = boolean unsafe'c'decQuadIsNormal

-- | True only if @x@ is greater than zero and is not an NaN.
isPositive :: Quad -> Bool
isPositive = boolean unsafe'c'decQuadIsPositive

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

-- | True only if @x@ is a zero.
isZero :: Quad -> Bool
isZero = boolean unsafe'c'decQuadIsZero

logB :: Quad -> Ctx Quad
logB = unary unsafe'c'decQuadLogB

max :: Quad -> Quad -> Ctx Quad
max = binary unsafe'c'decQuadMax

maxMag :: Quad -> Quad -> Ctx Quad
maxMag = binary unsafe'c'decQuadMaxMag

min :: Quad -> Quad -> Ctx Quad
min = binary unsafe'c'decQuadMin

minMag :: Quad -> Quad -> Ctx Quad
minMag = binary unsafe'c'decQuadMinMag

minus :: Quad -> Ctx Quad
minus = unary unsafe'c'decQuadMinus

multiply :: Quad -> Quad -> Ctx Quad
multiply = binary unsafe'c'decQuadMultiply

nextMinus :: Quad -> Ctx Quad
nextMinus = unary unsafe'c'decQuadNextMinus

nextPlus :: Quad -> Ctx Quad
nextPlus = unary unsafe'c'decQuadNextPlus

nextToward :: Quad -> Quad -> Ctx Quad
nextToward = binary unsafe'c'decQuadNextToward

-- | Digit wise logical inclusive Or.  Operands must be:
--
-- * zero or positive
-- * integers
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

reduce :: Quad -> Ctx Quad
reduce = unary unsafe'c'decQuadReduce

remainder :: Quad -> Quad -> Ctx Quad
remainder = binary unsafe'c'decQuadRemainder

remainderNear :: Quad -> Quad -> Ctx Quad
remainderNear = binary unsafe'c'decQuadRemainderNear

rotate :: Quad -> Quad -> Ctx Quad
rotate = binary unsafe'c'decQuadRotate

sameQuantum :: Quad -> Quad -> Bool
sameQuantum x y = unsafePerformIO $
  withForeignPtr (unDec x) $ \pX ->
  withForeignPtr (unDec y) $ \pY ->
  unsafe'c'decQuadSameQuantum pX pY >>= \r ->
  return $ case r of
    1 -> True
    0 -> False
    _ -> error "sameQuantum: error: invalid result"

scaleB :: Quad -> Quad -> Ctx Quad
scaleB = binary unsafe'c'decQuadScaleB

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

toInt32 :: Round -> Quad -> Ctx C'int32_t
toInt32 = getRounded unsafe'c'decQuadToInt32

toInt32Exact :: Round -> Quad -> Ctx C'int32_t
toInt32Exact = getRounded unsafe'c'decQuadToInt32Exact

toIntegralExact :: Quad -> Ctx Quad
toIntegralExact = unary unsafe'c'decQuadToIntegralExact

toIntegralValue :: Round -> Quad -> Ctx Quad
toIntegralValue (Round rnd) d = Ctx $ \pC ->
  withForeignPtr (unDec d) $ \pD ->
  newQuad >>= \r ->
  withForeignPtr (unDec r) $ \pR ->
  unsafe'c'decQuadToIntegralValue pR pD pC rnd >>
  return r

-- | Converts a 'Quad' to a string.  May use non-scientific
-- notation, but only if that's unambiguous; otherwise, uses
-- scientific notation.
--
-- In the decNumber C library, this is called @toString@; the name
-- was changed here because this function doesn't return a Haskell
-- 'String'.
toByteString :: Quad -> BS8.ByteString
toByteString = mkString unsafe'c'decQuadToString

toUInt32 :: Round -> Quad -> Ctx C'uint32_t
toUInt32 = getRounded unsafe'c'decQuadToUInt32

toUInt32Exact :: Round -> Quad -> Ctx C'uint32_t
toUInt32Exact = getRounded unsafe'c'decQuadToUInt32Exact

version :: BS8.ByteString
version = unsafePerformIO $
  unsafe'c'decQuadVersion >>= BS8.packCString

xor :: Quad -> Quad -> Ctx Quad
xor = binary unsafe'c'decQuadXor

zero :: Quad
zero = unsafePerformIO $
  newQuad >>= \d ->
  withForeignPtr (unDec d) $ \pD ->
  unsafe'c'decQuadZero pD >>
  return d

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

-- | Ensures that the exponent is within the range allowed by
-- 'minMaxExp'.
exponent :: Int -> Maybe Exponent
exponent i
  | i < l = Nothing
  | i > h = Nothing
  | otherwise = Just . Exponent $ i
  where
    (l, h) = minMaxExp


zeroExponent :: Exponent
zeroExponent = Exponent 0

data Value
  = Finite Coefficient Exponent
  | Infinite
  | NaN NaN Payload
  deriving (Eq, Ord, Show)

data Decoded = Decoded
  { dSign :: Sign
  , dValue :: Value
  } deriving (Eq, Ord, Show)


toBCD :: Quad -> Decoded
toBCD d = unsafePerformIO $
  withForeignPtr (unDec d) $ \pD ->
  allocaBytes c'DECQUAD_Pmax $ \pArr ->
  alloca $ \pExp ->
  unsafe'c'decQuadToBCD pD pExp pArr >>= \sgn ->
  peek pExp >>= \ex ->
  peekArray c'DECQUAD_Pmax pArr >>= \coef ->
  return (getDecoded sgn ex coef)

-- | Encodes a new 'Quad'.  The result is always canonical.  However,
-- the function does not signal if the result is an sNaN.
fromBCD :: Decoded -> Quad
fromBCD dcd = unsafePerformIO $
  newQuad >>= \d ->
  withForeignPtr (unDec d) $ \pD ->
  let (expn, digs, sgn) = toDecNumberBCD dcd in
  withArray digs $ \pArr ->
  unsafe'c'decQuadFromBCD pD expn pArr sgn >>
  return d


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


-- ## Digits

-- | A single decimal digit.
data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Eq, Ord, Show, Enum, Bounded)

digitToInt :: Integral a => Digit -> a
digitToInt d = case d of
  { D0 -> 0; D1 -> 1; D2 -> 2; D3 -> 3; D4 -> 4; D5 -> 5;
    D6 -> 6; D7 -> 7; D8 -> 8; D9 -> 9 }

intToDigit :: Integral a => a -> Digit
intToDigit i = case i of
  { 0 -> D0; 1 -> D1; 2 -> D2; 3 -> D3; 4 -> D4;
    5 -> D5; 6 -> D6; 7 -> D7; 8 -> D8; 9 -> D9;
    _ -> error "intToDigit: integer out of range" }


-- | A list of digits, less than or equal to 'coefficientLen' long.
-- Corresponds only to finite numbers.
newtype Coefficient = Coefficient { unCoefficient :: [Digit] }
  deriving (Eq, Ord, Show)

coefficient :: [Digit] -> Maybe Coefficient
coefficient ls
  | null ls = Nothing
  | length ls > 1 && head ls == D0 = Nothing
  | length ls > coefficientLen = Nothing
  | otherwise = Just . Coefficient $ ls

-- | A list of digits, less than or equal to 'payloadDigitsLen'
-- long.
newtype Payload = Payload { unPayload :: [Digit] }
  deriving (Eq, Ord, Show)

payloadDigits :: [Digit] -> Maybe Payload
payloadDigits ds
  | null ds = Nothing
  | length ds > 1 && head ds == D0 = Nothing
  | length ds > payloadDigitsLen = Nothing
  | otherwise = Just . Payload $ ds


-- | The most significant digit is at the head of the list.
digitsToIntegral :: [Digit] -> Integer
digitsToIntegral ls = go (length ls - 1) 0 ls
  where
    go c t ds = case ds of
      [] -> t
      x:xs -> let m = digitToInt x * 10 ^ c
                  t' = m + t
                  c' = c - 1
                  _types = c :: Int
              in t' `seq` c' `seq` go c' t' xs

-- | The most significant digit is at
-- the head of the list.  Sign of number is not relevant.
integralToDigits :: Integral a => a -> [Digit]
integralToDigits = reverse . go . Prelude.abs
  where
    go i
      | i == 0 = []
      | otherwise =
          let (d, m) = i `divMod` 10
          in intToDigit m : go d

coefficientLen :: Int
coefficientLen = c'DECQUAD_Pmax

payloadDigitsLen :: Int
payloadDigitsLen = c'DECQUAD_Pmax - 1

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
data AdjustedExp = AdjustedExp { unAdjustedExp :: Int }
  deriving (Eq, Show, Ord)

adjustedExp :: Coefficient -> Exponent -> AdjustedExp
adjustedExp ds e = AdjustedExp $ unExponent e
  + dDigits ds - 1

adjustedToExponent :: Coefficient -> AdjustedExp -> Exponent
adjustedToExponent ds e = Exponent $ unAdjustedExp e -
  dDigits ds + 1

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
