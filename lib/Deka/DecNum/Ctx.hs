{-# LANGUAGE Safe #-}
module Deka.DecNum.Ctx where

import qualified Data.ByteString.Char8 as BS8
import Deka.Context.Internal
import Deka.DecNum.Util
import Deka.DecNum.CtxFree
import Deka.Decnumber.DecNumber
import Deka.Decnumber.Context
import Deka.DecNum.DecNum
import Deka.Context
import Deka.Class.Internal
import Foreign.Safe
import Deka.Decnumber.Types

fromByteString :: BS8.ByteString -> Ctx DecNum
fromByteString bs = Ctx $ \pCtx ->
  newDecNum pCtx >>= \dn ->
  withForeignPtr (unDecNum dn) $ \pDn ->
  BS8.useAsCString bs $ \cstr ->
  c'decNumberFromString (castPtr pDn) cstr pCtx >>
  return dn

toUInt32 :: DecNum -> Ctx C'uint32_t
toUInt32 dn = Ctx $ \pCtx ->
  withForeignPtr (unDecNum dn) $ \pDn ->
  c'decNumberToUInt32 (castPtr pDn) pCtx

toInt32 :: DecNum -> Ctx C'int32_t
toInt32 dn = Ctx $ \pCtx ->
  withForeignPtr (unDecNum dn) $ \pDn ->
  c'decNumberToInt32 (castPtr pDn) pCtx

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

-- | Like 'nonSpecialCtxFree' but gets information about allowed
-- subnormal values from the 'Ctx'.
nonSpecial
  :: Sign
  -> Coefficient
  -> Exponent
  -> Ctx (Maybe DecNum)
  -- ^ Fails if the exponent is out of range
nonSpecial sgn coe rawEx = Ctx $ \pCtx -> do
  ext <- fmap toBool . peek . p'decContext'extended $ pCtx
  let getPc | ext = fmap (Just . Precision) . peek
                . p'decContext'digits $ pCtx
            | otherwise = return Nothing
  pc <- getPc
  nonSpecialCtxFree pc sgn coe rawEx
