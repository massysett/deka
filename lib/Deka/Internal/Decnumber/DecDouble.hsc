{-# LANGUAGE ForeignFunctionInterface, Safe, EmptyDataDecls #-}

#include <decContext.h>
#include <decDouble.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- | Low-level bindings to the decNumber library.
module Deka.Internal.Decnumber.DecDouble where

import Foreign.Safe
import Foreign.C
import Deka.Internal.Decnumber.Types
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

foreign import ccall unsafe "decDoubleToInt32" unsafe'c'decDoubleToInt32
  :: Ptr C'decDouble
  -> Ptr C'decContext
  -> C'rounding
  -> IO Int32

foreign import ccall unsafe "decDoubleToInt32Exact" unsafe'c'decDoubleToInt32Exact
  :: Ptr C'decDouble
  -> Ptr C'decContext
  -> C'rounding
  -> IO Int32

foreign import ccall unsafe "decDoubleFromInt32" unsafe'c'decDoubleFromInt32
  :: Ptr C'decDouble
  -> Int32
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleFromPacked" unsafe'c'decDoubleFromPacked
  :: Ptr C'decDouble
  -> Int32
  -> Ptr Word8
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleFromPackedChecked" unsafe'c'decDoubleFromPackedChecked
  :: Ptr C'decDouble
  -> Int32
  -> Ptr Word8
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleFromUInt32" unsafe'c'decDoubleFromUInt32
  :: Ptr C'decDouble
  -> Word32
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleFromString" unsafe'c'decDoubleFromString
  :: Ptr C'decDouble
  -> CString
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleGetCoefficient" unsafe'c'decDoubleGetCoefficient
  :: Ptr C'decDouble
  -> Ptr Word8
  -> IO Int32

foreign import ccall unsafe "decDoubleGetExponent" unsafe'c'decDoubleGetExponent
  :: Ptr C'decDouble
  -> IO Int32

foreign import ccall unsafe "decDoubleSetCoefficient" unsafe'c'decDoubleSetCoefficient
  :: Ptr C'decDouble
  -> Ptr Word8
  -> Int32
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleSetExponent" unsafe'c'decDoubleSetExponent
  :: Ptr C'decDouble
  -> Ptr C'decContext
  -> Int32
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleShow" unsafe'c'decDoubleShow
  :: Ptr C'decDouble
  -> CString
  -> IO ()

foreign import ccall unsafe "decDoubleToEngString" unsafe'c'decDoubleToEngString
  :: Ptr C'decDouble
  -> CString
  -> IO CString

foreign import ccall unsafe "decDoubleToString" unsafe'c'decDoubleToString
  :: Ptr C'decDouble
  -> CString
  -> IO CString

foreign import ccall unsafe "decDoubleToUInt32" unsafe'c'decDoubleToUInt32
  :: Ptr C'decDouble
  -> Ptr C'decContext
  -> C'rounding
  -> IO Word32


foreign import ccall unsafe "decDoubleToUInt32Exact" unsafe'c'decDoubleToUInt32Exact
  :: Ptr C'decDouble
  -> Ptr C'decContext
  -> C'rounding
  -> IO Word32

foreign import ccall unsafe "decDoubleZero" unsafe'c'decDoubleZero
  :: Ptr C'decDouble
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleAbs" unsafe'c'decDoubleAbs
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleAdd" unsafe'c'decDoubleAdd
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleAnd" unsafe'c'decDoubleAnd
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleDivide" unsafe'c'decDoubleDivide
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleDivideInteger" unsafe'c'decDoubleDivideInteger
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleFMA" unsafe'c'decDoubleFMA
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleFromBCD" unsafe'c'decDoubleFromBCD
  :: Ptr C'decDouble
  -> Int32
  -> Ptr Word8
  -> Int32
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleInvert" unsafe'c'decDoubleInvert
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleLogB" unsafe'c'decDoubleLogB
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleMax" unsafe'c'decDoubleMax
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleMaxMag" unsafe'c'decDoubleMaxMag
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleMin" unsafe'c'decDoubleMin
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleMinMag" unsafe'c'decDoubleMinMag
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleMinus" unsafe'c'decDoubleMinus
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleMultiply" unsafe'c'decDoubleMultiply
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleNextMinus" unsafe'c'decDoubleNextMinus
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleNextPlus" unsafe'c'decDoubleNextPlus
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleNextToward" unsafe'c'decDoubleNextToward
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleOr" unsafe'c'decDoubleOr
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoublePlus" unsafe'c'decDoublePlus
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleQuantize" unsafe'c'decDoubleQuantize
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleReduce" unsafe'c'decDoubleReduce
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleRemainder" unsafe'c'decDoubleRemainder
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleRemainderNear" unsafe'c'decDoubleRemainderNear
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleRotate" unsafe'c'decDoubleRotate
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleScaleB" unsafe'c'decDoubleScaleB
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleShift" unsafe'c'decDoubleShift
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleSubtract" unsafe'c'decDoubleSubtract
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleToBCD" unsafe'c'decDoubleToBCD
  :: Ptr C'decDouble
  -> Ptr Int32
  -> Ptr Word8
  -> IO Int32

foreign import ccall unsafe "decDoubleToIntegralValue" unsafe'c'decDoubleToIntegralValue
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> C'rounding
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleToIntegralExact" unsafe'c'decDoubleToIntegralExact
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleXor" unsafe'c'decDoubleXor
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

-- Comparisons

foreign import ccall unsafe "decDoubleCompare" unsafe'c'decDoubleCompare
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleCompareSignal" unsafe'c'decDoubleCompareSignal
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleCompareTotal" unsafe'c'decDoubleCompareTotal
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleCompareTotalMag" unsafe'c'decDoubleCompareTotalMag
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> IO (Ptr C'decDouble)

-- Copies
foreign import ccall unsafe "decDoubleCanonical" unsafe'c'decDoubleCanonical
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleCopyAbs" unsafe'c'decDoubleCopyAbs
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleCopyNegate" unsafe'c'decDoubleCopyNegate
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleCopySign" unsafe'c'decDoubleCopySign
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> Ptr C'decDouble
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decDoubleCopy" unsafe'c'decDoubleCopy
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> IO (Ptr C'decDouble)

-- Non-computational

foreign import ccall unsafe "decDoubleClass" unsafe'c'decDoubleClass
  :: Ptr C'decDouble
  -> IO C'decClass

foreign import ccall unsafe "decDoubleClassString" unsafe'c'decDoubleClassString
  :: Ptr C'decDouble
  -> IO CString

foreign import ccall unsafe "decDoubleDigits" unsafe'c'decDoubleDigits
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsCanonical" unsafe'c'decDoubleIsCanonical
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsFinite" unsafe'c'decDoubleIsFinite
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsInteger" unsafe'c'decDoubleIsInteger
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsLogical" unsafe'c'decDoubleIsLogical
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsInfinite" unsafe'c'decDoubleIsInfinite
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsNaN" unsafe'c'decDoubleIsNaN
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsNegative" unsafe'c'decDoubleIsNegative
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsNormal" unsafe'c'decDoubleIsNormal
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsPositive" unsafe'c'decDoubleIsPositive
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsSignaling" unsafe'c'decDoubleIsSignaling
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsSigned" unsafe'c'decDoubleIsSigned
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsSubnormal" unsafe'c'decDoubleIsSubnormal
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleIsZero" unsafe'c'decDoubleIsZero
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleRadix" unsafe'c'decDoubleRadix
  :: Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleSameQuantum" unsafe'c'decDoubleSameQuantum
  :: Ptr C'decDouble
  -> Ptr C'decDouble
  -> IO Word32

foreign import ccall unsafe "decDoubleVersion" unsafe'c'decDoubleVersion
  :: IO CString
