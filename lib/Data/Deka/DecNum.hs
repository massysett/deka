{-# LANGUAGE EmptyDataDecls, Trustworthy #-}
module Data.Deka.DecNum
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
  ) where

import Prelude hiding (abs, and, or, max, min, compare, exp,
  subtract, negate, isNaN, isInfinite)
import qualified Prelude as P
import qualified Data.ByteString.Char8 as BS8
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Safe hiding (rotate, shift, xor)
import Data.Deka.Decnumber.DecNumber
import Data.Deka.Decnumber.Types
import Data.Deka.Decnumber.Context
import Data.Deka.Context.Internal
import Data.Deka.Class.Internal
import Foreign.C

-- | How many bytes must be malloc'ed to hold this many
-- digits?

mallocAmount
  :: C'int32_t
  -- ^ Number of digits
  -> Int
  -- ^ Malloc this many bytes total
mallocAmount s = base + extra
  where
    base = sizeOf (undefined :: C'decNumber)
    baseUnits = c'DECNUMUNITS
    totUnits = (s + c'DECDPUN - 1) `quot` c'DECDPUN
    extraUnits = P.max 0 $ totUnits - baseUnits
    extra = sizeOf (undefined :: C'decNumberUnit) * fromIntegral extraUnits

data DPtr

newtype DecNum = DecNum { unDecNum :: ForeignPtr DPtr }

instance Show DecNum where
  show = BS8.unpack . toByteString

oneDigitDecNum :: IO DecNum
oneDigitDecNum = do
  fp <- mallocForeignPtrBytes (sizeOf (undefined :: C'decNumber))
  return $ DecNum (castForeignPtr fp)

newDecNum :: Ptr C'decContext -> IO DecNum
newDecNum p = do
  digits <- peek (p'decContext'digits p)
  let sz = mallocAmount digits
  fp <- mallocForeignPtrBytes sz
  return $ DecNum fp

copyDecNum :: DecNum -> IO DecNum
copyDecNum (DecNum p) = withForeignPtr p $ \dp ->
  peek (p'decNumber'digits (castPtr dp)) >>= \dgts ->
  newDecNumSize dgts >>= \dn' ->
  withForeignPtr (unDecNum dn') $ \dp' ->
  c'decNumberCopy (castPtr dp') (castPtr dp) >>
  return dn'

newDecNumSize :: C'int32_t -> IO DecNum
newDecNumSize i = do
  let sz = mallocAmount i
  fp <- mallocForeignPtrBytes sz
  return $ DecNum fp

fromInt32 :: C'int32_t -> DecNum
fromInt32 i = unsafePerformIO $ do
  dn <- newDecNumSize 10
  withForeignPtr (unDecNum dn) $ \ptr -> do
    _ <- c'decNumberFromInt32 (castPtr ptr) i
    return dn

fromUInt32 :: C'uint32_t -> DecNum
fromUInt32 i = unsafePerformIO $ do
  dn <- newDecNumSize 10
  withForeignPtr (unDecNum dn) $ \ptr -> do
    _ <- c'decNumberFromUInt32 (castPtr ptr) i
    return dn

fromByteString :: BS8.ByteString -> Ctx DecNum
fromByteString bs = Ctx $ \pCtx ->
  newDecNum pCtx >>= \dn ->
  withForeignPtr (unDecNum dn) $ \pDn ->
  BS8.useAsCString bs $ \cstr ->
  c'decNumberFromString (castPtr pDn) cstr pCtx >>
  return dn

toByteString :: DecNum -> BS8.ByteString
toByteString dn = unsafePerformIO $
  withForeignPtr (unDecNum dn) $ \pDn ->
  peek (p'decNumber'digits (castPtr pDn)) >>= \digs ->
  let digsTot = fromIntegral digs + 14 in
  allocaBytes digsTot $ \pStr ->
  c'decNumberToString (castPtr pDn) pStr >>
  BS8.packCString pStr

toEngByteString :: DecNum -> BS8.ByteString
toEngByteString dn = unsafePerformIO $
  withForeignPtr (unDecNum dn) $ \pDn ->
  peek (p'decNumber'digits (castPtr pDn)) >>= \digs ->
  let digsTot = fromIntegral digs + 14 in
  allocaBytes digsTot $ \pStr ->
  c'decNumberToEngString (castPtr pDn) pStr >>
  BS8.packCString pStr

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
sameQuantum (DecNum x) (DecNum y) = unsafePerformIO $
  withForeignPtr x $ \px ->
  withForeignPtr y $ \py ->
  oneDigitDecNum >>= \o ->
  withForeignPtr (unDecNum o) $ \po ->
  c'decNumberSameQuantum (castPtr po) (castPtr px) (castPtr py) >>
  return o

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
copyAbs src dest = unsafePerformIO $
  copyDecNum dest >>= \r ->
  withForeignPtr (unDecNum r) $ \pr ->
  withForeignPtr (unDecNum src) $ \ps ->
  c'decNumberCopyAbs (castPtr pr) (castPtr ps) >>
  return r

-- CopyNegate, CopySign

negate :: DecNum -> DecNum
negate src = unsafePerformIO $
  copyDecNum src >>= \r ->
  withForeignPtr (unDecNum r) $ \pr ->
  withForeignPtr (unDecNum src) $ \ps ->
  c'decNumberCopyNegate (castPtr pr) (castPtr ps) >>
  return r

copySign
  :: DecNum
  -- ^ Source of content (except sign)
  -> DecNum
  -- ^ Source of sign
  -> DecNum
copySign src sgn = unsafePerformIO $
  withForeignPtr (unDecNum src) $ \pc ->
  peek (p'decNumber'digits (castPtr pc)) >>= \dgts ->
  newDecNumSize dgts >>= \dn' ->
  withForeignPtr (unDecNum dn') $ \dp' ->
  withForeignPtr (unDecNum sgn) $ \pn ->
  c'decNumberCopySign (castPtr dp') (castPtr pc) (castPtr pn) >>
  return dn'

trim :: DecNum -> DecNum
trim src = unsafePerformIO $
  copyDecNum src >>= \dest ->
  withForeignPtr (unDecNum dest) $ \pd ->
  c'decNumberTrim (castPtr pd) >>
  return dest

version :: BS8.ByteString
version = unsafePerformIO $
  c'decNumberVersion >>= \pv ->
  BS8.packCString pv

zero :: DecNum
zero = unsafePerformIO $
  oneDigitDecNum >>= \od ->
  withForeignPtr (unDecNum od) $ \pod ->
  c'decNumberZero (castPtr pod) >>
  return od

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

testBool
  :: (Ptr C'decNumber -> IO CInt)
  -> DecNum
  -> Bool
testBool f (DecNum dn) = unsafePerformIO $
  withForeignPtr dn $ \pn ->
  f (castPtr pn) >>= \bl ->
  return (toBool bl)

isCanonical :: DecNum -> Bool
isCanonical = testBool c'decNumberIsCanonical

isFinite :: DecNum -> Bool
isFinite = testBool c'decNumberIsFinite

isInfinite :: DecNum -> Bool
isInfinite = testBool c'decNumberIsInfinite

isNaN :: DecNum -> Bool
isNaN = testBool c'decNumberIsNaN

isNegative :: DecNum -> Bool
isNegative = testBool c'decNumberIsNegative

isQNaN :: DecNum -> Bool
isQNaN = testBool c'decNumberIsQNaN

isSNaN :: DecNum -> Bool
isSNaN = testBool c'decNumberIsSNaN

isSpecial :: DecNum -> Bool
isSpecial = testBool c'decNumberIsSpecial

isZero :: DecNum -> Bool
isZero = testBool c'decNumberIsZero

-- skipped: radix
