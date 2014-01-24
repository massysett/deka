{-# LANGUAGE Safe #-}
module Data.Deka.Env
  ( -- * Rounding
    Round
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

  -- * Env monad
  , Env
  , getStatus
  , setStatus
  , getRound
  , setRound
  , runEnv

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

  -- * Dec
  , Dec

  -- * Arithmetic
  , abs
  , add
  , and
  , canonical
  , decClass
  -- skipped: classString - not needed
  , compare
  , compareSignal
  , compareTotal
  , compareTotalMag
  -- skipped: copy - not needed
  , copyAbs
  , copyNegate
  , copySign
  , digits
  , divide
  , divideInteger
  , fma
  -- skipped: fromBCD - use encode function instead
  , fromInt32
  -- skipped: fromNumber - not needed
  -- skipped: fromPacked - use encode function instead
  -- skipped: fromPackedChecked - use encode function instead
  , fromString
  , fromUInt32
  -- skipped: fromWider - not needed
  -- skipped: getCoefficient - use decode function instead
  -- to be removed: getExponent - use decode function instead
  , getExponent
  , invert
  , isCanonical
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
  , logB
  , max
  , maxMag
  , min
  , minMag
  , minus
  , multiply
  , nextMinus
  , nextPlus
  , nextToward
  , or
  , plus
  , quantize
  -- skipped: radix - not needed
  , reduce
  , remainder
  , remainderNear
  , rotate
  , sameQuantum
  , scaleB
  -- skipped: setCoefficient - use encode function instead
  -- skipped: setExponent - use encode function instead
  , shift
  , subtract
  -- skipped: toBCD - use decode function instead
  , toEngString
  , toInt32
  , toInt32Exact
  , toIntegralExact
  , toIntegralValue
  -- skipped: toNumber - not needed
  -- skipped: toPacked - use decode function instead
  , toString
  , toUInt32
  , toUInt32Exact
  -- skipped: toWider - not needed
  , version
  , xor
  , zero

  -- * Conversions
  , Sign(..)
  , Significand
  , unSignificand
  , significand
  , Exponent
  , unExponent
  , exponent
  , Decoded(..)

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

newtype Flags = Flags { unFlags :: C'uint32_t }
  deriving (Eq, Ord, Show)

setFlag :: Flag -> Flags -> Flags
setFlag (Flag f1) (Flags fA) = Flags (f1 .|. fA)

clearFlag :: Flag -> Flags -> Flags
clearFlag (Flag f1) (Flags fA) = Flags (complement f1 .&. fA)

checkFlag :: Flag -> Flags -> Bool
checkFlag (Flag f1) (Flags fA) = (f1 .&. fA) /= 0

emptyFlags :: Flags
emptyFlags = Flags 0

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

getStatus :: Env Flags
getStatus = Env $ \cPtr -> do
  let pSt = p'decContext'status cPtr
  fmap Flags . peek $ pSt

setStatus :: Flags -> Env ()
setStatus f = Env $ \cPtr -> do
  let pSt = p'decContext'status cPtr
  poke pSt . unFlags $ f

getRound :: Env Round
getRound = Env $ \cPtr -> do
  let pR = p'decContext'round cPtr
  fmap Round . peek $ pR

setRound :: Round -> Env ()
setRound r = Env $ \cPtr -> do
  let pR = p'decContext'round cPtr
  poke pR . unRound $ r

-- | By default, rounding is half even.  No status flags are set
-- initially.  Returns the final status flags along with the result
-- of the computation.
runEnv :: Env a -> IO (a, Flags)
runEnv (Env k) = do
  fp <- mallocForeignPtr
  withForeignPtr fp $ \pCtx -> do
    _ <- c'decContextDefault pCtx c'DEC_INIT_DECQUAD
    res <- k pCtx
    fl' <- fmap Flags . peek . p'decContext'status $ pCtx
    return (res, fl')

-- # Class

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
-- would ordinarily work with.
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
  void (f ptrR ptrX ptrC)
  >> return r

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
  void (f pR pX pY pC)
  >> return r

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
  void (f pR pX pY) >> return r

type UnaryGet a
  = Ptr C'decQuad
  -> IO a

unaryGet
  :: UnaryGet a
  -> Dec
  -> Env a
unaryGet f d = Env $ \_ ->
  withForeignPtr (unDec d) $ \pD -> f pD

type UnaryCtxFree
  = Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO (Ptr C'decQuad)

unaryCtxFree
  :: UnaryCtxFree
  -> Dec
  -> Env Dec
unaryCtxFree f d = Env $ \_ ->
  newDec >>= \n -> 
  withForeignPtr (unDec n) $ \pN ->
  withForeignPtr (unDec d) $ \pD -> f pN pD
  >> return n

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
  void (f pR pX pY pZ pC)
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
  void (f pD pS)
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

-- # Functions

abs :: Dec -> Env Dec
abs = unary c'decQuadAbs

add :: Dec -> Dec -> Env Dec
add = binary c'decQuadAdd

and :: Dec -> Dec -> Env Dec
and = binary c'decQuadAnd

canonical :: Dec -> Env Dec
canonical = unaryCtxFree c'decQuadCanonical

decClass :: Dec -> Env DecClass
decClass = fmap DecClass . unaryGet c'decQuadClass

compare :: Dec -> Dec -> Env Dec
compare = binary c'decQuadCompare

compareSignal :: Dec -> Dec -> Env Dec
compareSignal = binary c'decQuadCompareSignal

compareTotal :: Dec -> Dec -> Env Dec
compareTotal = binaryCtxFree c'decQuadCompareTotal

compareTotalMag :: Dec -> Dec -> Env Dec
compareTotalMag = binaryCtxFree c'decQuadCompareTotalMag

copyAbs :: Dec -> Env Dec
copyAbs = unaryCtxFree c'decQuadCopyAbs

copyNegate :: Dec -> Env Dec
copyNegate = unaryCtxFree c'decQuadCopyNegate

copySign :: Dec -> Env Dec
copySign = unaryCtxFree c'decQuadCopySign

digits :: Dec -> Env Int
digits = fmap fromIntegral . unaryGet c'decQuadDigits

divide :: Dec -> Dec -> Env Dec
divide = binary c'decQuadDivide

divideInteger :: Dec -> Dec -> Env Dec
divideInteger = binary c'decQuadDivideInteger

fma :: Dec -> Dec -> Dec -> Env Dec
fma = ternary c'decQuadFMA

fromInt32 :: C'int32_t -> Env Dec
fromInt32 i = Env $ \_ ->
  newDec >>= \r ->
  withForeignPtr (unDec r) $ \pR ->
  void (c'decQuadFromInt32 pR i)
  >> return r

fromString :: BS8.ByteString -> Env Dec
fromString s = Env $ \pC ->
  newDec >>= \r ->
  withForeignPtr (unDec r) $ \pR ->
  BS8.useAsCString s $ \pS ->
  void (c'decQuadFromString pR pS pC)
  >> return r

fromUInt32 :: C'uint32_t -> Env Dec
fromUInt32 i = Env $ \_ ->
  newDec >>= \r ->
  withForeignPtr (unDec r) $ \pR ->
  void (c'decQuadFromUInt32 pR i)
  >> return r

getExponent :: Dec -> Env C'int32_t
getExponent = unaryGet c'decQuadGetExponent

invert :: Dec -> Env Dec
invert = unary c'decQuadInvert

isCanonical :: Dec -> Env Bool
isCanonical = boolean c'decQuadIsCanonical

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

or :: Dec -> Dec -> Env Dec
or = binary c'decQuadOr

plus :: Dec -> Env Dec
plus = unary c'decQuadPlus

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
  void (c'decQuadToIntegralValue pR pD pC rnd)
  >> return r

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
  void (c'decQuadZero pD)
  >> return d

-- # Conversions

data Sign
  = Positive
  -- ^ The number is positive or is zero
  | Negative
  -- ^ The number is negative or the negative zero
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | This is always zero or positive.
data Significand = Significand { unSignificand :: Integer }
  deriving (Eq, Ord, Show)

significand :: Integer -> Maybe Significand
significand i
  | i < 0 = Nothing
  | otherwise = Just . Significand $ i

-- | This is restricted to the same size as a C uint32_t.  This does
-- not check to make sure the exponent is within Emax or Emin
-- because those limits are for adjusted exponents.  However, encode
-- does ensure that its results are canonical, which adjusts the
-- exponent; therefore, if Exponent is out of range, an Underflow or
-- Overflow will result.
data Exponent = Exponent { unExponent :: C'uint32_t }
  deriving (Eq, Ord, Show)

exponent :: Integer -> Maybe Exponent
exponent i
  | i < (fromIntegral (minBound :: C'uint32_t)) = Nothing
  | i > (fromIntegral (maxBound :: C'uint32_t)) = Nothing
  | otherwise = Just . Exponent $ fromIntegral i

data Decoded
  = Finite Sign Significand Exponent
  | Infinite Sign
  | NonSignaling
  | Signaling
  deriving (Eq, Ord, Show)
