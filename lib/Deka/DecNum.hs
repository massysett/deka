{-# LANGUAGE EmptyDataDecls, Trustworthy #-}
module Deka.DecNum
  ( DecNum

  -- * Conversions
  , C'int32_t
  , fromInt32
  , toInt32
  , C'uint32_t
  , fromUInt32
  , toUInt32
  , fromByteString
  , toByteString
  , toEngByteString

  -- * Arithmetic and logical functions
  , abs
  , add
  , and
  , compare
  , compareSignal
  , compareTotal
  , compareTotalMag
  , divide
  , divideInteger
  , exp
  , fma
  , invert
  , ln
  , logB
  , log10
  , max
  , maxMag
  , min
  , minMag
  , minus
  , multiply
  , normalize
  , or
  , plus
  , power
  , quantize
  , reduce
  , remainder
  , remainderNear
  , rescale
  , rotate
  , sameQuantum
  , scaleB
  , shift
  , squareRoot
  , subtract
  , toIntegralExact
  , toIntegralValue
  , xor
  , nextMinus
  , nextPlus
  , nextToward

  -- * Utility functions
  , numClass
  , copyAbs
  , negate
  , copySign
  , trim
  , version
  , zero
  , isNormal
  , isSubnormal
  , isCanonical
  , isFinite
  , isInfinite
  , isNaN
  , isNegative
  , isQNaN
  , isSNaN
  , isSpecial
  , isZero

  -- * Decoding and encoding

  -- | /Encoding/ takes Haskell types and converts them to a C
  -- decNumber type so you can perform arithmetic on them.
  -- /Decoding/ takes a C decNumber type and converts it to Haskell
  -- types.

  -- ** Number components
  , Coefficient
  , coefficient
  , unCoefficient
  , zeroCoefficient
  , oneCoefficient
  , Exponent(..)
  , Sign(..)
  , NaNtype(..)
  , Payload(..)
  , Decoded(..)

  -- ** Decoding
  , decode

  -- ** Encoding
  , infinity
  , notANumber
  , nonSpecialCtxFree
  , nonSpecial

  -- * Adjusted exponents
  , AdjExponent
  , unAdjExponent
  , adjExponent
  ) where

import Prelude hiding (abs, and, or, max, min, compare, exp,
  subtract, negate, isNaN, isInfinite, exponent)
import qualified Prelude as P
import qualified Data.ByteString.Char8 as BS8
import Foreign.Safe hiding (rotate, shift, xor)
import Deka.DecNum.Internal
  ( DecNum(..)
  , newDecNum
  , unsafe0
  , unsafe1
  , unsafe2
  , unsafe3
  , unsafe4
  , toByteString
  , Coefficient
  , coefficient
  , unCoefficient
  , zeroCoefficient
  , oneCoefficient
  , Exponent(..)
  , Sign(..)
  , NaNtype(..)
  , Payload(..)
  , Decoded(..)
  , AdjExponent
  , unAdjExponent
  , adjExponent
  )

import qualified Deka.DecNum.Internal as I
import Deka.Decnumber.DecNumber
import Deka.Decnumber.Types
import Deka.Decnumber.Context
import Deka.Context
import Deka.Context.Internal
import Deka.Class.Internal

fromByteString :: BS8.ByteString -> Ctx DecNum
fromByteString bs = Ctx $ \pCtx ->
  newDecNum pCtx >>= \dn ->
  withForeignPtr (unDecNum dn) $ \pDn ->
  BS8.useAsCString bs $ \cstr ->
  c'decNumberFromString (castPtr pDn) cstr pCtx >>
  return dn

fromInt32 :: C'int32_t -> DecNum
fromInt32 = unsafe1 I.fromInt32

fromUInt32 :: C'uint32_t -> DecNum
fromUInt32 = unsafe1 I.fromUInt32

toEngByteString :: DecNum -> BS8.ByteString
toEngByteString = unsafe1 I.toEngByteString

toUInt32 :: DecNum -> Ctx C'uint32_t
toUInt32 dn = Ctx $ \pCtx ->
  withForeignPtr (unDecNum dn) $ \pDn ->
  c'decNumberToUInt32 (castPtr pDn) pCtx

toInt32 :: DecNum -> Ctx C'int32_t
toInt32 dn = Ctx $ \pCtx ->
  withForeignPtr (unDecNum dn) $ \pDn ->
  c'decNumberToInt32 (castPtr pDn) pCtx

-- skipped: getBCD, setBCD

type Unary
  = Ptr C'decNumber
  -- ^ Result
  -> Ptr C'decNumber
  -- ^ Input
  -> Ptr C'decContext
  -> IO (Ptr C'decNumber)

unary :: Unary -> DecNum -> Ctx DecNum
unary f x = Ctx $ \pCtx ->
  newDecNum pCtx >>= \res ->
  withForeignPtr (unDecNum res) $ \pRes ->
  withForeignPtr (unDecNum x) $ \pX ->
  f (castPtr pRes) (castPtr pX) pCtx >>
  return res

type Binary
   = Ptr C'decNumber
   -- ^ Result
   -> Ptr C'decNumber
   -- ^ Input X
   -> Ptr C'decNumber
   -- ^ Input Y
   -> Ptr C'decContext
   -> IO (Ptr C'decNumber)

binary :: Binary -> DecNum -> DecNum -> Ctx DecNum
binary f x y = Ctx $ \pCtx ->
  newDecNum pCtx >>= \res ->
  withForeignPtr (unDecNum res) $ \pRes ->
  withForeignPtr (unDecNum x) $ \pX ->
  withForeignPtr (unDecNum y) $ \pY ->
  f (castPtr pRes) (castPtr pX) (castPtr pY) pCtx >>
  return res

type Ternary
   = Ptr C'decNumber
   -- ^ Result
   -> Ptr C'decNumber
   -- ^ Input X
   -> Ptr C'decNumber
   -- ^ Input Y
   -> Ptr C'decNumber
   -- ^ Input Z
   -> Ptr C'decContext
   -> IO (Ptr C'decNumber)

ternary :: Ternary -> DecNum -> DecNum -> DecNum -> Ctx DecNum
ternary f x y z = Ctx $ \pCtx ->
  newDecNum pCtx >>= \res ->
  withForeignPtr (unDecNum res) $ \pRes ->
  withForeignPtr (unDecNum x) $ \pX ->
  withForeignPtr (unDecNum y) $ \pY ->
  withForeignPtr (unDecNum z) $ \pZ ->
  f (castPtr pRes) (castPtr pX) (castPtr pY) (castPtr pZ) pCtx >>
  return res

abs :: DecNum -> Ctx DecNum
abs = unary c'decNumberAbs

add :: DecNum -> DecNum -> Ctx DecNum
add = binary c'decNumberAdd

and :: DecNum -> DecNum -> Ctx DecNum
and = binary c'decNumberAnd

compare :: DecNum -> DecNum -> Ctx DecNum
compare = binary c'decNumberCompare

compareSignal :: DecNum -> DecNum -> Ctx DecNum
compareSignal = binary c'decNumberCompareSignal

compareTotal :: DecNum -> DecNum -> Ctx DecNum
compareTotal = binary c'decNumberCompareTotal

compareTotalMag :: DecNum -> DecNum -> Ctx DecNum
compareTotalMag = binary c'decNumberCompareTotalMag

divide :: DecNum -> DecNum -> Ctx DecNum
divide = binary c'decNumberDivide

divideInteger :: DecNum -> DecNum -> Ctx DecNum
divideInteger = binary c'decNumberDivideInteger

exp :: DecNum -> Ctx DecNum
exp = unary c'decNumberExp

fma :: DecNum -> DecNum -> DecNum -> Ctx DecNum
fma = ternary c'decNumberFMA

invert :: DecNum -> Ctx DecNum
invert = unary c'decNumberInvert

ln :: DecNum -> Ctx DecNum
ln = unary c'decNumberLn

logB :: DecNum -> Ctx DecNum
logB = unary c'decNumberLogB

log10 :: DecNum -> Ctx DecNum
log10 = unary c'decNumberLog10

max :: DecNum -> DecNum -> Ctx DecNum
max = binary c'decNumberMax

maxMag :: DecNum -> DecNum -> Ctx DecNum
maxMag = binary c'decNumberMaxMag

min :: DecNum -> DecNum -> Ctx DecNum
min = binary c'decNumberMin

minMag :: DecNum -> DecNum -> Ctx DecNum
minMag = binary c'decNumberMinMag

minus :: DecNum -> Ctx DecNum
minus = unary c'decNumberMinus

multiply :: DecNum -> DecNum -> Ctx DecNum
multiply = binary c'decNumberMultiply

normalize :: DecNum -> Ctx DecNum
normalize = unary c'decNumberNormalize

or :: DecNum -> DecNum -> Ctx DecNum
or = binary c'decNumberOr

plus :: DecNum -> Ctx DecNum
plus = unary c'decNumberPlus

power :: DecNum -> DecNum -> Ctx DecNum
power = binary c'decNumberPower

quantize :: DecNum -> DecNum -> Ctx DecNum
quantize = binary c'decNumberQuantize

reduce :: DecNum -> Ctx DecNum
reduce = unary c'decNumberReduce

remainder :: DecNum -> DecNum -> Ctx DecNum
remainder = binary c'decNumberRemainder

remainderNear :: DecNum -> DecNum -> Ctx DecNum
remainderNear = binary c'decNumberRemainderNear

rescale :: DecNum -> DecNum -> Ctx DecNum
rescale = binary c'decNumberRescale

rotate :: DecNum -> DecNum -> Ctx DecNum
rotate = binary c'decNumberRotate

sameQuantum :: DecNum -> DecNum -> DecNum
sameQuantum = unsafe2 I.sameQuantum

scaleB :: DecNum -> DecNum -> Ctx DecNum
scaleB = binary c'decNumberScaleB

shift :: DecNum -> DecNum -> Ctx DecNum
shift = binary c'decNumberShift

squareRoot :: DecNum -> Ctx DecNum
squareRoot = unary c'decNumberSquareRoot

subtract :: DecNum -> DecNum -> Ctx DecNum
subtract = binary c'decNumberSubtract

toIntegralExact :: DecNum -> Ctx DecNum
toIntegralExact = unary c'decNumberToIntegralExact

toIntegralValue :: DecNum -> Ctx DecNum
toIntegralValue = unary c'decNumberToIntegralValue

xor :: DecNum -> DecNum -> Ctx DecNum
xor = binary c'decNumberXor

nextMinus :: DecNum -> Ctx DecNum
nextMinus = unary c'decNumberNextMinus

nextPlus :: DecNum -> Ctx DecNum
nextPlus = unary c'decNumberNextPlus

nextToward :: DecNum -> DecNum -> Ctx DecNum
nextToward = binary c'decNumberNextToward

numClass :: DecNum -> Ctx Class
numClass (DecNum fp) = Ctx $ \pCtx ->
  withForeignPtr fp $ \pd ->
  c'decNumberClass (castPtr pd) pCtx >>= \cl ->
  return (Class cl)

-- skipped: ClassToString, Copy

copyAbs
  :: DecNum
  -- ^ Source of sign
  -> DecNum
  -- ^ Copy sign to this destination
  -> DecNum
  -- ^ Result
copyAbs = unsafe2 I.copyAbs

negate :: DecNum -> DecNum
negate = unsafe1 I.negate

copySign
  :: DecNum
  -- ^ Source of content (except sign)
  -> DecNum
  -- ^ Source of sign
  -> DecNum
copySign = unsafe2 I.copySign

trim :: DecNum -> DecNum
trim = unsafe1 I.trim

version :: BS8.ByteString
version = unsafe0 I.version

zero :: DecNum
zero = unsafe0 I.zero

isNormal :: DecNum -> Ctx Bool
isNormal (DecNum d) = Ctx $ \pCtx ->
  withForeignPtr d $ \pd ->
  c'decNumberIsNormal (castPtr pd) pCtx >>= \int ->
  return (toBool int)

isSubnormal :: DecNum -> Ctx Bool
isSubnormal (DecNum d) = Ctx $ \pCtx ->
  withForeignPtr d $ \pd ->
  c'decNumberIsSubnormal (castPtr pd) pCtx >>= \int ->
  return (toBool int)

isCanonical :: DecNum -> Bool
isCanonical = unsafe1 I.isCanonical

isFinite :: DecNum -> Bool
isFinite = unsafe1 I.isFinite

isInfinite :: DecNum -> Bool
isInfinite = unsafe1 I.isInfinite

isNaN :: DecNum -> Bool
isNaN = unsafe1 I.isNaN

isNegative :: DecNum -> Bool
isNegative = unsafe1 I.isNegative

isQNaN :: DecNum -> Bool
isQNaN = unsafe1 I.isQNaN

isSNaN :: DecNum -> Bool
isSNaN = unsafe1 I.isSNaN

isSpecial :: DecNum -> Bool
isSpecial = unsafe1 I.isSpecial

isZero :: DecNum -> Bool
isZero = unsafe1 I.isZero

-- skipped: radix

-- | Take a C 'DecNum' and convert it to Haskell types.
decode :: DecNum -> Decoded
decode = unsafe1 I.decode

-- | Encodes positive or negative infinities.
infinity :: Sign -> DecNum
infinity = unsafe1 I.infinity

-- | Encodes quiet or signaling NaNs.
notANumber :: Sign -> NaNtype -> Coefficient -> DecNum
notANumber = unsafe3 I.notANumber

-- | Encodes non-special numbers (also known as finite numbers.)
-- Does not need the context; however, you will have to supply
-- information about whether subnormal values are allowed.
nonSpecialCtxFree
  :: Maybe Precision
  -- ^ If Just, allow subnormal values.  In that case, the maximum
  -- number of digits is needed in order to compute the lower limit
  -- for the exponent.  If Nothing, do not allow subnormal values.

  -> Sign
  -> Coefficient
  -> Exponent
  -> Maybe DecNum
  -- ^ Fails if the exponent is out of range.
nonSpecialCtxFree = unsafe4 I.nonSpecialCtxFree

-- | Like 'nonSpecialCtxFree' but gets information about allowed
-- subnormal values from the 'Ctx'.
nonSpecial
  :: Sign
  -> Coefficient
  -> Exponent
  -> Ctx (Maybe DecNum)
  -- ^ Fails if the exponent is out of range
nonSpecial sgn coe rawEx = do
  ext <- getExtended
  let getPc | ext = fmap Just getPrecision
            | otherwise = return Nothing
  pc <- getPc
  return $ nonSpecialCtxFree pc sgn coe rawEx
