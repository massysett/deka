{-# LANGUAGE ForeignFunctionInterface, Safe, EmptyDataDecls #-}

#include <decContext.h>
#include <decDouble.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- | Low-level bindings to the decNumber library.
module Deka.Internal.Decnumber.DecDouble where

import Foreign.Safe
import Foreign.C
import Deka.Internal.Decnumber.Context

c'NULL :: Num a => a
c'NULL = #const NULL

c'DECDOUBLE_Bytes :: Num a => a
c'DECDOUBLE_Bytes = #const DECDOUBLE_Bytes

c'DECDOUBLE_Pmax :: Num a => a
c'DECDOUBLE_Pmax = #const DECDOUBLE_Pmax

c'DECDOUBLE_Emin :: Num a => a
c'DECDOUBLE_Emin = #const DECDOUBLE_Emin

c'DECDOUBLE_Emax :: Num a => a
c'DECDOUBLE_Emax = #const DECDOUBLE_Emax

c'DECDOUBLE_EmaxD :: Num a => a
c'DECDOUBLE_EmaxD = #const DECDOUBLE_EmaxD

c'DECDOUBLE_Bias :: Num a => a
c'DECDOUBLE_Bias = #const DECDOUBLE_Bias

c'DECDOUBLE_String :: Num a => a
c'DECDOUBLE_String = #const DECDOUBLE_String

c'DECDOUBLE_EconL :: Num a => a
c'DECDOUBLE_EconL = #const DECDOUBLE_EconL

c'DECDOUBLE_Declets :: Num a => a
c'DECDOUBLE_Declets = #const DECDOUBLE_Declets

c'DECDOUBLE_Ehigh :: Num a => a
c'DECDOUBLE_Ehigh = #const DECDOUBLE_Ehigh

c'decDouble'sizeOf :: Int
c'decDouble'sizeOf = #size decDouble

data C'decDouble

p'decDouble'bytes :: Ptr C'decDouble -> Ptr c'decDouble'bytes
p'decDouble'bytes = #ptr decDouble, bytes

c'DECFLOAT_Sign :: Num a => a
c'DECFLOAT_Sign = #const DECFLOAT_Sign

c'DECFLOAT_NaN :: Num a => a
c'DECFLOAT_NaN = #const DECFLOAT_NaN

c'DECFLOAT_qNaN :: Num a => a
c'DECFLOAT_qNaN = #const DECFLOAT_qNaN

c'DECFLOAT_sNaN :: Num a => a
c'DECFLOAT_sNaN = #const DECFLOAT_sNaN

c'DECFLOAT_Inf :: Num a => a
c'DECFLOAT_Inf = #const DECFLOAT_Inf

c'DECFLOAT_MinSp :: Num a => a
c'DECFLOAT_MinSp = #const DECFLOAT_MinSp


c'DECPPLUSALT :: Num a => a
c'DECPPLUSALT = #const DECPPLUSALT

c'DECPMINUSALT :: Num a => a
c'DECPMINUSALT = #const DECPMINUSALT

c'DECPPLUS :: Num a => a
c'DECPPLUS = #const DECPPLUS

c'DECPMINUS :: Num a => a
c'DECPMINUS = #const DECPMINUS

c'DECPPLUSALT2 :: Num a => a
c'DECPPLUSALT2 = #const DECPPLUSALT2

c'DECPUNSIGNED :: Num a => a
c'DECPUNSIGNED = #const DECPUNSIGNED


-- Utilities

foreign import ccall unsafe "decDoubleToInt32" c'decDoubleToInt32
  :: Ptr C'decDouble
  -> Ptr C'decContext
  -> C'rounding
  -> IO Int32

foreign import ccall unsafe "decDoubleToInt32Exact" c'decDoubleToInt32Exact
  :: Ptr C'decDouble
  -> Ptr C'decContext
  -> C'rounding
  -> IO Int32

foreign import ccall unsafe "decDoubleFromInt32" c'decDoubleFromInt32
  :: Ptr C'decDouble
  -> Int32
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleFromPacked" c'decDoubleFromPacked
  :: Ptr C'decDouble
  -> Int32
  -> Ptr Word8
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleFromPackedChecked" c'decDoubleFromPackedChecked
  :: Ptr C'decDouble
  -> Int32
  -> Ptr Word8
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleFromUInt32" c'decDoubleFromUInt32
  :: Ptr C'decDouble
  -> Word32
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleFromString" c'decDoubleFromString
  :: Ptr C'decDouble
  -> CString
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleGetCoefficient" c'decDoubleGetCoefficient
  :: Ptr C'decDouble
  -> Ptr Word8
  -> IO Int32

foreign import ccall unsafe "decDoubleGetExponent" c'decDoubleGetExponent
  :: Ptr C'decDouble
  -> IO Int32

foreign import ccall unsafe "decDoubleSetCoefficient" c'decDoubleSetCoefficient
  :: Ptr C'decDouble
  -> Ptr Word8
  -> Int32
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleSetExponent" c'decDoubleSetExponent
  :: Ptr C'decDouble
  -> Ptr C'decContext
  -> Int32
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleShow" c'decDoubleShow
  :: Ptr C'decDouble
  -> CString
  -> IO ()

foreign import ccall unsafe "decDoubleToEngString" c'decDoubleToEngString
  :: Ptr C'decDouble
  -> CString
  -> IO CString

foreign import ccall unsafe "decDoubleToString" c'decDoubleToString
  :: Ptr C'decDouble
  -> CString
  -> IO CString

foreign import ccall unsafe "decDoubleToUInt32" c'decDoubleToUInt32
  :: Ptr C'decDouble
  -> Ptr C'decContext
  -> C'rounding
  -> IO Word32


foreign import ccall unsafe "decDoubleToUInt32Exact" c'decDoubleToUInt32Exact
  :: Ptr C'decDouble
  -> Ptr C'decContext
  -> C'rounding
  -> IO Word32

foreign import ccall unsafe "decDoubleZero" c'decDoubleZero
  :: Ptr C'decDouble
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleAbs" c'decDoubleAbs
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleAdd" c'decDoubleAdd
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleAnd" c'decDoubleAnd
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleDivide" c'decDoubleDivide
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleDivideInteger" c'decDoubleDivideInteger
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleFMA" c'decDoubleFMA
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleFromBCD" c'decDoubleFromBCD
  :: Ptr C'decDouble
  -> Int32
  -> Ptr Word8
  -> Int32
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleInvert" c'decDoubleInvert
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleLogB" c'decDoubleLogB
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleMax" c'decDoubleMax
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleMaxMag" c'decDoubleMaxMag
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleMin" c'decDoubleMin
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleMinMag" c'decDoubleMinMag
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleMinus" c'decDoubleMinus
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleMultiply" c'decDoubleMultiply
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleNextMinus" c'decDoubleNextMinus
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleNextPlus" c'decDoubleNextPlus
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleNextToward" c'decDoubleNextToward
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleOr" c'decDoubleOr
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoublePlus" c'decDoublePlus
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleQuantize" c'decDoubleQuantize
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleReduce" c'decDoubleReduce
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleRemainder" c'decDoubleRemainder
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleRemainderNear" c'decDoubleRemainderNear
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleRotate" c'decDoubleRotate
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleScaleB" c'decDoubleScaleB
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleShift" c'decDoubleShift
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleSubtract" c'decDoubleSubtract
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleToBCD" c'decDoubleToBCD
  :: Ptr C'decDouble
  -> Ptr Int32
  -> Ptr Word8
  -> IO Int32

foreign import ccall unsafe "decDoubleToIntegralValue" c'decDoubleToIntegralValue
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> C'rounding
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleToIntegralExact" c'decDoubleToIntegralExact
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleXor" c'decDoubleXor
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

-- Comparisons

foreign import ccall unsafe "decDoubleCompare" c'decDoubleCompare
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleCompareSignal" c'decDoubleCompareSignal
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleCompareTotal" c'decDoubleCompareTotal
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleCompareTotalMag" c'decDoubleCompareTotalMag
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> IO (Ptr C'decDouble)

-- Copies
foreign import ccall unsafe "decDoubleCanonical" c'decDoubleCanonical
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleCopyAbs" c'decDoubleCopyAbs
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleCopyNegate" c'decDoubleCopyNegate
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleCopySign" c'decDoubleCopySign
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleCopy" c'decDoubleCopy
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> IO (Ptr C'decDouble)

-- Non-computational

foreign import ccall unsafe "decDoubleClass" c'decDoubleClass
  :: Ptr C'decDouble
  -> IO C'decClass

foreign import ccall unsafe "decDoubleClassString" c'decDoubleClassString
  :: Ptr C'decDouble
  -> IO CString

foreign import ccall unsafe "decDoubleDigits" c'decDoubleDigits
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsCanonical" c'decDoubleIsCanonical
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsFinite" c'decDoubleIsFinite
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsInteger" c'decDoubleIsInteger
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsLogical" c'decDoubleIsLogical
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsInfinite" c'decDoubleIsInfinite
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsNaN" c'decDoubleIsNaN
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsNegative" c'decDoubleIsNegative
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsNormal" c'decDoubleIsNormal
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsPositive" c'decDoubleIsPositive
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsSignaling" c'decDoubleIsSignaling
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsSigned" c'decDoubleIsSigned
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsSubnormal" c'decDoubleIsSubnormal
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsZero" c'decDoubleIsZero
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleRadix" c'decDoubleRadix
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleSameQuantum" c'decDoubleSameQuantum
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleVersion" c'decDoubleVersion
  :: IO CString
