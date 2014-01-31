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
module Data.Deka.IO
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

  -- * Env monad
  , Env
  , runEnvIO

  -- * Ctx monad
  , Ctx
  , getStatus
  , setStatus
  , getRound
  , setRound
  , runCtxIO
  , liftEnv

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
  , Exponent
  , exponent
  , unExponent
  , minMaxExp
  , Payload
  , unPayload
  , payload
  , zeroPayload
  , Sign(..)
  , NaN(..)
  , Value(..)
  , Decoded(..)
  , finiteExponent
  , finiteCoefficient
  , decode
  , encode

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
import Data.Maybe

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


-- | The Env monad
--
-- Since Deka is a binding to a C library, all the work happens in
-- the mutable land of C pointers.  That means that everything
-- happens in the IO monad.  The Env monad is simply a wrapper of
-- the IO monad.  Since the Env data constructor is not exported,
-- you can't do arbitrary IO computations in the monad; you can only
-- do the computations from this module.  Because all the
-- computations in this module do not create observable side
-- effects, it is safe to use 'unsafePerformIO' to perform Env
-- computations in a pure function.  This module does not have such
-- a function so that this module can stay Safe for Safe Haskell
-- purposes; however, the "Data.Deka.Pure" module does have such a
-- function.

newtype Env a = Env { runEnvIO :: IO a }

instance Functor Env where
  fmap = liftM

instance Applicative Env where
  pure = return
  (<*>) = ap

instance Monad Env where
  return = Env . return
  Env a >>= f = Env $ do
    r1 <- a
    runEnvIO $ f r1
  fail = Env . fail


-- | The Ctx monad
--
-- The General Decimal Arithmetic specification states that most
-- computations occur within a @context@, which affects the manner
-- in which computations are done (for instance, the context
-- determines the rounding algorithm).  The context also carries
-- some results from computations (for instance, a computation might
-- set a flag to indicate that the result is rounded or inexact or
-- was a division by zero.) The Ctx monad carries this context.
--
-- Conceptually the Ctx monad includes the Env monad.  Any Env
-- computation can be lifted into a Ctx computation with a
-- 'liftEnv'.  You will do most computations in the Ctx monad;
-- however, on occasion it is useful to do computations entirely in
-- the Env monad.  You know that computations entirely in the Env
-- monad are unaffected by, and cannot affect, the context.
--
-- The Ctx monad captures both the context and the IO.  Because
-- 'Quad' is exposed only as an immutable type, and because there is
-- no way to do arbitrary IO in the Ctx monad (which is why it is
-- not an instance of the 'MonadIO' class), it is safe to put an
-- 'unsafePerformIO' on Ctx computations so they can be done in pure
-- functions.
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

-- | Lifts an Env computation into a Ctx.
liftEnv :: Env a -> Ctx a
liftEnv (Env k) = Ctx $ \_ -> k

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
runCtxIO :: Ctx a -> IO (a, Flags)
runCtxIO (Ctx k) = do
  fp <- mallocForeignPtr
  withForeignPtr fp $ \pCtx -> do
    _ <- c'decContextDefault pCtx c'DEC_INIT_DECQUAD
    res <- k pCtx
    fl' <- fmap Flags . peek . p'decContext'status $ pCtx
    return (res, fl')

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
-- mutable like anything in C.  However, the API exposed in this
-- module does not mutate a Quad after a function returns one.)
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
  -> Env Quad
binaryCtxFree f x y = Env $
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
  -> Env a
unaryGet f d = Env $
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
  -> Env Bool
boolean f d = Env $
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
  -> Env BS8.ByteString
mkString f d = Env $
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
abs = unary c'decQuadAbs

add :: Quad -> Quad -> Ctx Quad
add = binary c'decQuadAdd

-- | Digit-wise logical and.  Operands must be:
--
-- * zero or positive
-- * integers
-- * comprise only zeroes and/or ones
--
-- If not, 'invalidOperation' is set.
and :: Quad -> Quad -> Ctx Quad
and = binary c'decQuadAnd

-- | More information about a particular 'Quad'.
decClass :: Quad -> Env DecClass
decClass = fmap DecClass . unaryGet c'decQuadClass

-- | Compares two 'Quad' numerically.  The result might be @-1@, @0@,
-- @1@, or NaN, where @-1@ means x is less than y, @0@ indicates
-- numerical equality, @1@ means y is greater than x.  NaN is
-- returned only if x or y is an NaN.
--
-- Thus, this function does not return an 'Ordering' because the
-- result might be an NaN.
--
compare :: Quad -> Quad -> Ctx Quad
compare = binary c'decQuadCompare

-- | Same as 'compare', but a quietNaN is treated like a signaling
-- NaN (sets 'invalidOperation').
compareSignal :: Quad -> Quad -> Ctx Quad
compareSignal = binary c'decQuadCompareSignal

-- | Compares using an IEEE 754 total ordering, which takes into
-- account the exponent.  IEEE 754 says that this function might
-- return different results depending upon whether the operands are
-- canonical; 'Quad' are always canonical so you don't need to worry
-- about that here.
compareTotal :: Quad -> Quad -> Env Quad
compareTotal = binaryCtxFree c'decQuadCompareTotal

-- | Same as 'compareTotal' but compares the absolute value of the
-- two arguments.
compareTotalMag :: Quad -> Quad -> Env Quad
compareTotalMag = binaryCtxFree c'decQuadCompareTotalMag

-- | @copySign x y@ returns @z@, which is a copy of @x@ but has the
-- sign of @y@.  Unlike @decQuadCopySign@, the result is always
-- canonical.  This function never raises any signals.
copySign :: Quad -> Quad -> Env Quad
copySign fr to = Env $
  newQuad >>= \r ->
  withForeignPtr (unDec r) $ \pR ->
  withForeignPtr (unDec fr) $ \pF ->
  withForeignPtr (unDec to) $ \pT ->
  c'decQuadCopySign pR pF pT >>
  c'decQuadCanonical pR pR >>
  return r

digits :: Quad -> Env Int
digits = fmap fromIntegral . unaryGet c'decQuadDigits

divide :: Quad -> Quad -> Ctx Quad
divide = binary c'decQuadDivide

divideInteger :: Quad -> Quad -> Ctx Quad
divideInteger = binary c'decQuadDivideInteger

-- | fused multiply add.
fma :: Quad -> Quad -> Quad -> Ctx Quad
fma = ternary c'decQuadFMA

fromInt32 :: C'int32_t -> Env Quad
fromInt32 i = Env $
  newQuad >>= \r ->
  withForeignPtr (unDec r) $ \pR ->
  c'decQuadFromInt32 pR i
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
  c'decQuadFromString pR pS pC >>
  return r

fromUInt32 :: C'uint32_t -> Env Quad
fromUInt32 i = Env $
  newQuad >>= \r ->
  withForeignPtr (unDec r) $ \pR ->
  c'decQuadFromUInt32 pR i >>
  return r

invert :: Quad -> Ctx Quad
invert = unary c'decQuadInvert

isFinite :: Quad -> Env Bool
isFinite = boolean c'decQuadIsFinite

isInfinite :: Quad -> Env Bool
isInfinite = boolean c'decQuadIsInfinite

-- | True if @x@ is finite and has exponent of @0@; False otherwise.
isInteger :: Quad -> Env Bool
isInteger = boolean c'decQuadIsInteger

isLogical :: Quad -> Env Bool
isLogical = boolean c'decQuadIsLogical

isNaN :: Quad -> Env Bool
isNaN = boolean c'decQuadIsNaN

isNegative :: Quad -> Env Bool
isNegative = boolean c'decQuadIsNegative

isNormal :: Quad -> Env Bool
isNormal = boolean c'decQuadIsNormal

isPositive :: Quad -> Env Bool
isPositive = boolean c'decQuadIsPositive

isSignaling :: Quad -> Env Bool
isSignaling = boolean c'decQuadIsSignaling

isSigned :: Quad -> Env Bool
isSigned = boolean c'decQuadIsSigned

isSubnormal :: Quad -> Env Bool
isSubnormal = boolean c'decQuadIsSubnormal

isZero :: Quad -> Env Bool
isZero = boolean c'decQuadIsZero

logB :: Quad -> Ctx Quad
logB = unary c'decQuadLogB

max :: Quad -> Quad -> Ctx Quad
max = binary c'decQuadMax

maxMag :: Quad -> Quad -> Ctx Quad
maxMag = binary c'decQuadMaxMag

min :: Quad -> Quad -> Ctx Quad
min = binary c'decQuadMin

minMag :: Quad -> Quad -> Ctx Quad
minMag = binary c'decQuadMinMag

minus :: Quad -> Ctx Quad
minus = unary c'decQuadMinus

multiply :: Quad -> Quad -> Ctx Quad
multiply = binary c'decQuadMultiply

nextMinus :: Quad -> Ctx Quad
nextMinus = unary c'decQuadNextMinus

nextPlus :: Quad -> Ctx Quad
nextPlus = unary c'decQuadNextPlus

nextToward :: Quad -> Quad -> Ctx Quad
nextToward = binary c'decQuadNextToward

-- | Digit wise logical inclusive Or.  Operands must be:
--
-- * zero or positive
-- * integers
-- * comprise only zeroes and/or ones
--
-- If not, 'invalidOperation' is set.
or :: Quad -> Quad -> Ctx Quad
or = binary c'decQuadOr

-- | Same effect as @0 + x@ where the exponent of the zero is the
-- same as that of @x@ if @x@ is finite).  NaNs are handled as for
-- arithmetic operations.
plus :: Quad -> Ctx Quad
plus = unary c'decQuadPlus

-- | @quantize x y@ returns @z@ which is @x@ set to have the same
-- quantum as @y@; that is, numerically the same value but rounded
-- or padded if necessary to have the same exponent as @y@.  Useful
-- for rounding monetary quantities.
quantize :: Quad -> Quad -> Ctx Quad
quantize = binary c'decQuadQuantize

reduce :: Quad -> Ctx Quad
reduce = unary c'decQuadReduce

remainder :: Quad -> Quad -> Ctx Quad
remainder = binary c'decQuadRemainder

remainderNear :: Quad -> Quad -> Ctx Quad
remainderNear = binary c'decQuadRemainderNear

rotate :: Quad -> Quad -> Ctx Quad
rotate = binary c'decQuadRotate

sameQuantum :: Quad -> Quad -> Env Bool
sameQuantum x y = Env $
  withForeignPtr (unDec x) $ \pX ->
  withForeignPtr (unDec y) $ \pY ->
  c'decQuadSameQuantum pX pY >>= \r ->
  return $ case r of
    1 -> True
    0 -> False
    _ -> error "sameQuantum: error: invalid result"

scaleB :: Quad -> Quad -> Ctx Quad
scaleB = binary c'decQuadScaleB

-- skipped: SetCoefficient
-- skipped : SetExponent

shift :: Quad -> Quad -> Ctx Quad
shift = binary c'decQuadShift

-- omitted: Show

subtract :: Quad -> Quad -> Ctx Quad
subtract = binary c'decQuadSubtract

-- | Returns a string in engineering notation.
--
-- In the decNumber C library, this is called @toEngString@; the
-- name is changed here because the function does not return a
-- regular Haskell 'String'.
toEngByteString :: Quad -> Env BS8.ByteString
toEngByteString = mkString c'decQuadToEngString

toInt32 :: Round -> Quad -> Ctx C'int32_t
toInt32 = getRounded c'decQuadToInt32

toInt32Exact :: Round -> Quad -> Ctx C'int32_t
toInt32Exact = getRounded c'decQuadToInt32Exact

toIntegralExact :: Quad -> Ctx Quad
toIntegralExact = unary c'decQuadToIntegralExact

toIntegralValue :: Round -> Quad -> Ctx Quad
toIntegralValue (Round rnd) d = Ctx $ \pC ->
  withForeignPtr (unDec d) $ \pD ->
  newQuad >>= \r ->
  withForeignPtr (unDec r) $ \pR ->
  c'decQuadToIntegralValue pR pD pC rnd >>
  return r

-- | Converts a 'Quad' to a string.  May use non-scientific
-- notation, but only if that's unambiguous; otherwise, uses
-- scientific notation.
--
-- In the decNumber C library, this is called @toString@; the name
-- was changed here because this function doesn't return a Haskell
-- 'String'.
toByteString :: Quad -> Env BS8.ByteString
toByteString = mkString c'decQuadToString

toUInt32 :: Round -> Quad -> Ctx C'uint32_t
toUInt32 = getRounded c'decQuadToUInt32

toUInt32Exact :: Round -> Quad -> Ctx C'uint32_t
toUInt32Exact = getRounded c'decQuadToUInt32Exact

version :: Env BS8.ByteString
version = Env $
  c'decQuadVersion >>= BS8.packCString

xor :: Quad -> Quad -> Ctx Quad
xor = binary c'decQuadXor

zero :: Env Quad
zero = Env $
  newQuad >>= \d ->
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

-- | The minimum and maximum possible exponent.
-- If the coefficient has c digits, and Emax is x, the exponent e
-- is within the closed-ended range
--
-- @-x - (c - 1) + 1@ and @x - (c - 1)@
--
-- See Decimal Arithmetic Specification version 1.70, page 10.
minMaxExp :: (Int, Int)
minMaxExp = (l, h)
  where
    l = negate x - (c - 1) + 1
    h = x - (c - 1)
    x = c'DECQUAD_Emax
    c = c'DECQUAD_Pmax

newtype Exponent = Exponent { unExponent :: Int }
  deriving (Eq, Ord, Show)

-- | Ensures the exponent falls within the correct range.
exponent :: Int -> Either String Exponent
exponent i
  | i < l = Left "exponent too small"
  | i > h = Left "exponent too large"
  | otherwise = Right . Exponent $ i
  where
    (l, h) = minMaxExp

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
  = Finite Coefficient Exponent
  | Infinite
  | NaN NaN Payload
  deriving (Eq, Ord, Show)

data Decoded = Decoded
  { dSign :: Sign
  , dValue :: Value
  } deriving (Eq, Ord, Show)

-- | Gets the exponent of a finite number, if the Decoded is finite.
finiteExponent :: Decoded -> Maybe Int
finiteExponent d = case dValue d of
  Finite _ e -> Just $ unExponent e
  _ -> Nothing

-- | Gets the coefficient of a finite number, if the Decoded is
-- finite.
finiteCoefficient :: Decoded -> Maybe Integer
finiteCoefficient d = case dValue d of
  Finite c _ -> Just $ unCoefficient c
  _ -> Nothing


decode :: Quad -> Env Decoded
decode d = Env $
  withForeignPtr (unDec d) $ \pD ->
  allocaBytes c'DECQUAD_Pmax $ \pArr ->
  alloca $ \pExp ->
  c'decQuadToBCD pD pExp pArr >>= \sgn ->
  peek pExp >>= \ex ->
  peekArray c'DECQUAD_Pmax pArr >>= \coef ->
  return (getDecoded sgn ex coef)

-- | Encodes a new 'Quad'.  The result is always canonical.  However,
-- the function does not signal if the result is an sNaN.
encode :: Decoded -> Env Quad
encode dcd = Env $
  newQuad >>= \d ->
  withForeignPtr (unDec d) $ \pD ->
  let (expn, arr) = packBCD dcd in
  withArray arr $ \pArr ->
  c'decQuadFromPackedChecked pD expn pArr >>= \r ->
  let iPtr = ptrToIntPtr r
  in if iPtr == c'NULL
      then error ("Deka.IO: encode failed. Decoded: " ++ show dcd)
      else return () >>
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
      | otherwise = Finite coe (Exponent $ fromIntegral ex)
      where
        pld = Payload $ fromBCD (c'DECQUAD_Pmax - 1) (tail coef)
        coe = Coefficient $ fromBCD c'DECQUAD_Pmax coef

packNibbles
  :: C'uint8_t
  -> C'uint8_t
  -> C'uint8_t
packNibbles a b =
  let r = a
      r' = shiftL r 4
  in r' + b

lastByte :: Sign -> C'uint8_t -> C'uint8_t
lastByte s u =
  shiftL u 4 .|. case s of
    Positive -> c'DECPPLUS
    Negative -> c'DECPMINUS

firstByte :: C'uint8_t -> C'uint8_t
firstByte = id

-- | List must be DECQUAD_Pmax long
packList :: Sign -> [C'uint8_t] -> [C'uint8_t]
packList s ls = case ls of
  x:xs ->
    firstByte x : packRest xs
  [] -> error "packList: empty list"
  where
    packRest rs = case rs of
      x:y:xs -> packNibbles x y : packRest xs
      x:[] -> [lastByte s x]
      [] -> error "packRest: empty list"

packInfinite :: Sign -> [C'uint8_t]
packInfinite s = packList s (replicate c'DECQUAD_Pmax 0)

packNaN :: Sign -> Payload -> [C'uint8_t]
packNaN s p =
  packList s (0 : toBCD (c'DECQUAD_Pmax - 1) (unPayload p))

packFinite :: Sign -> Coefficient -> [C'uint8_t]
packFinite s c =
  packList s (toBCD c'DECQUAD_Pmax (unCoefficient c))


packBCD :: Decoded -> (C'int32_t, [C'uint8_t])
packBCD (Decoded s v) = (expt, ls)
  where
    expt = case v of
      Infinite -> c'DECFLOAT_Inf
      NaN n _ -> case n of
        Signaling -> c'DECFLOAT_sNaN
        Quiet -> c'DECFLOAT_qNaN
      Finite _ i -> fromIntegral . unExponent $ i
    ls = case v of
      Infinite -> packInfinite s
      NaN _ p -> packNaN s p
      Finite c _ -> packFinite s c

-- ## Decoded predicates

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

dIsLogical :: Decoded -> Bool
dIsLogical (Decoded s v) = fromMaybe False $ do
  guard $ s == Positive
  (c, e) <- case v of
    Finite co ex -> return (co, ex)
    _ -> Nothing
  guard $ unExponent e == 0
  return . all isZeroOrOne . show . unCoefficient $ c
  where
    isZeroOrOne c = c == '0' || c == '1'

dIsNaN :: Decoded -> Bool
dIsNaN (Decoded _ v) = case v of
  NaN _ _ -> True
  _ -> False

-- | True if @x@ is less than zero and is not an NaN.  Returns False
-- for the negative zero.
dIsNegative :: Decoded -> Bool
dIsNegative (Decoded s v)
  | s == Positive = False
  | otherwise = case v of
      Finite c _ -> unCoefficient c /= 0
      Infinite -> True
      _ -> False

dIsPositive :: Decoded -> Bool
dIsPositive (Decoded s v)
  | s == Negative = False
  | otherwise = case v of
      Finite c _ -> unCoefficient c /= 0
      Infinite -> True
      _ -> False

dIsSignaling :: Decoded -> Bool
dIsSignaling (Decoded _ v) = case v of
  NaN n _ -> n == Signaling
  _ -> False

dIsSigned :: Decoded -> Bool
dIsSigned (Decoded s _) = s == Negative

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
