{-# LANGUAGE Safe #-}
module Deka.Internal.Dec.Ctx where

import qualified Data.ByteString.Char8 as BS8
import Deka.Internal.Context
import Deka.Internal.Dec.Util
import Deka.Internal.Dec.CtxFree
import Deka.Internal.Decnumber.DecNumber
import Deka.Internal.Decnumber.Context
import Deka.Internal.Dec.Dec
import Deka.Internal.Class
import Foreign.Safe
import Deka.Decoded

fromByteString :: BS8.ByteString -> Ctx Dec
fromByteString bs = Ctx $ \pCtx ->
  newDec pCtx >>= \dn ->
  withForeignPtr (unDec dn) $ \pDn ->
  BS8.useAsCString bs $ \cstr ->
  c'decNumberFromString pDn cstr pCtx >>
  return dn

toUInt32 :: Dec -> Ctx Word32
toUInt32 dn = Ctx $ \pCtx ->
  withForeignPtr (unDec dn) $ \pDn ->
  c'decNumberToUInt32 pDn pCtx

toInt32 :: Dec -> Ctx Int32
toInt32 dn = Ctx $ \pCtx ->
  withForeignPtr (unDec dn) $ \pDn ->
  c'decNumberToInt32 pDn pCtx

type Unary
  = Ptr C'decNumber
  -- ^ Result
  -> Ptr C'decNumber
  -- ^ Input
  -> Ptr C'decContext
  -> IO (Ptr C'decNumber)

unary :: Unary -> Dec -> Ctx Dec
unary f x = Ctx $ \pCtx ->
  newDec pCtx >>= \res ->
  withForeignPtr (unDec res) $ \pRes ->
  withForeignPtr (unDec x) $ \pX ->
  f pRes pX pCtx >>
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

binary :: Binary -> Dec -> Dec -> Ctx Dec
binary f x y = Ctx $ \pCtx ->
  newDec pCtx >>= \res ->
  withForeignPtr (unDec res) $ \pRes ->
  withForeignPtr (unDec x) $ \pX ->
  withForeignPtr (unDec y) $ \pY ->
  f pRes pX pY pCtx >>
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

ternary :: Ternary -> Dec -> Dec -> Dec -> Ctx Dec
ternary f x y z = Ctx $ \pCtx ->
  newDec pCtx >>= \res ->
  withForeignPtr (unDec res) $ \pRes ->
  withForeignPtr (unDec x) $ \pX ->
  withForeignPtr (unDec y) $ \pY ->
  withForeignPtr (unDec z) $ \pZ ->
  f pRes pX pY pZ pCtx >>
  return res

abs :: Dec -> Ctx Dec
abs = unary c'decNumberAbs

add :: Dec -> Dec -> Ctx Dec
add = binary c'decNumberAdd

and :: Dec -> Dec -> Ctx Dec
and = binary c'decNumberAnd

compare :: Dec -> Dec -> Ctx Dec
compare = binary c'decNumberCompare

compareSignal :: Dec -> Dec -> Ctx Dec
compareSignal = binary c'decNumberCompareSignal

compareTotal :: Dec -> Dec -> Ctx Dec
compareTotal = binary c'decNumberCompareTotal

compareTotalMag :: Dec -> Dec -> Ctx Dec
compareTotalMag = binary c'decNumberCompareTotalMag

divide :: Dec -> Dec -> Ctx Dec
divide = binary c'decNumberDivide

divideInteger :: Dec -> Dec -> Ctx Dec
divideInteger = binary c'decNumberDivideInteger

exp :: Dec -> Ctx Dec
exp = unary c'decNumberExp

fma :: Dec -> Dec -> Dec -> Ctx Dec
fma = ternary c'decNumberFMA

invert :: Dec -> Ctx Dec
invert = unary c'decNumberInvert

ln :: Dec -> Ctx Dec
ln = unary c'decNumberLn

logB :: Dec -> Ctx Dec
logB = unary c'decNumberLogB

log10 :: Dec -> Ctx Dec
log10 = unary c'decNumberLog10

max :: Dec -> Dec -> Ctx Dec
max = binary c'decNumberMax

maxMag :: Dec -> Dec -> Ctx Dec
maxMag = binary c'decNumberMaxMag

min :: Dec -> Dec -> Ctx Dec
min = binary c'decNumberMin

minMag :: Dec -> Dec -> Ctx Dec
minMag = binary c'decNumberMinMag

minus :: Dec -> Ctx Dec
minus = unary c'decNumberMinus

multiply :: Dec -> Dec -> Ctx Dec
multiply = binary c'decNumberMultiply

normalize :: Dec -> Ctx Dec
normalize = unary c'decNumberNormalize

or :: Dec -> Dec -> Ctx Dec
or = binary c'decNumberOr

plus :: Dec -> Ctx Dec
plus = unary c'decNumberPlus

power :: Dec -> Dec -> Ctx Dec
power = binary c'decNumberPower

quantize :: Dec -> Dec -> Ctx Dec
quantize = binary c'decNumberQuantize

reduce :: Dec -> Ctx Dec
reduce = unary c'decNumberReduce

remainder :: Dec -> Dec -> Ctx Dec
remainder = binary c'decNumberRemainder

remainderNear :: Dec -> Dec -> Ctx Dec
remainderNear = binary c'decNumberRemainderNear

rescale :: Dec -> Dec -> Ctx Dec
rescale = binary c'decNumberRescale

rotate :: Dec -> Dec -> Ctx Dec
rotate = binary c'decNumberRotate

scaleB :: Dec -> Dec -> Ctx Dec
scaleB = binary c'decNumberScaleB

shift :: Dec -> Dec -> Ctx Dec
shift = binary c'decNumberShift

squareRoot :: Dec -> Ctx Dec
squareRoot = unary c'decNumberSquareRoot

subtract :: Dec -> Dec -> Ctx Dec
subtract = binary c'decNumberSubtract

toIntegralExact :: Dec -> Ctx Dec
toIntegralExact = unary c'decNumberToIntegralExact

toIntegralValue :: Dec -> Ctx Dec
toIntegralValue = unary c'decNumberToIntegralValue

xor :: Dec -> Dec -> Ctx Dec
xor = binary c'decNumberXor

nextMinus :: Dec -> Ctx Dec
nextMinus = unary c'decNumberNextMinus

nextPlus :: Dec -> Ctx Dec
nextPlus = unary c'decNumberNextPlus

nextToward :: Dec -> Dec -> Ctx Dec
nextToward = binary c'decNumberNextToward

numClass :: Dec -> Ctx Class
numClass (Dec fp) = Ctx $ \pCtx ->
  withForeignPtr fp $ \pd ->
  c'decNumberClass pd pCtx >>= \cl ->
  return (Class cl)

isNormal :: Dec -> Ctx Bool
isNormal (Dec d) = Ctx $ \pCtx ->
  withForeignPtr d $ \pd ->
  c'decNumberIsNormal pd pCtx >>= \int ->
  return (toBool int)

isSubnormal :: Dec -> Ctx Bool
isSubnormal (Dec d) = Ctx $ \pCtx ->
  withForeignPtr d $ \pd ->
  c'decNumberIsSubnormal pd pCtx >>= \int ->
  return (toBool int)

-- | Encodes non-special numbers (also known as finite numbers.)
-- Uses information from the context to determine whether subnormal
-- values are allowed.
nonSpecial
  :: Sign
  -> Coefficient
  -> Exponent
  -> Ctx (Maybe Dec)
  -- ^ Fails if the exponent is out of range
nonSpecial sgn coe rawEx = Ctx $ \pCtx -> do
  ext <- fmap toBool . peek . p'decContext'extended $ pCtx
  let getPc | ext = fmap (Just . Precision) . peek
                . p'decContext'digits $ pCtx
            | otherwise = return Nothing
  pc <- getPc
  nonSpecialCtxFree pc sgn coe rawEx
