{-# LANGUAGE ForeignFunctionInterface, Safe, EmptyDataDecls #-}

#include <decContext.h>
#include <decQuad.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- | Low-level bindings to the decNumber library.
module Deka.Internal.Decnumber.DecQuad where

import Foreign.Safe
import Foreign.C
import Deka.Internal.Decnumber.Context

c'NULL :: Num a => a
c'NULL = #const NULL

c'DECQUAD_Bytes :: Num a => a
c'DECQUAD_Bytes = #const DECQUAD_Bytes

c'DECQUAD_Pmax :: Num a => a
c'DECQUAD_Pmax = #const DECQUAD_Pmax

c'DECQUAD_Emin :: Num a => a
c'DECQUAD_Emin = #const DECQUAD_Emin

c'DECQUAD_Emax :: Num a => a
c'DECQUAD_Emax = #const DECQUAD_Emax

c'DECQUAD_EmaxD :: Num a => a
c'DECQUAD_EmaxD = #const DECQUAD_EmaxD

c'DECQUAD_Bias :: Num a => a
c'DECQUAD_Bias = #const DECQUAD_Bias

c'DECQUAD_String :: Num a => a
c'DECQUAD_String = #const DECQUAD_String

c'DECQUAD_EconL :: Num a => a
c'DECQUAD_EconL = #const DECQUAD_EconL

c'DECQUAD_Declets :: Num a => a
c'DECQUAD_Declets = #const DECQUAD_Declets

c'DECQUAD_Ehigh :: Num a => a
c'DECQUAD_Ehigh = #const DECQUAD_Ehigh

c'decQuad'sizeOf :: Int
c'decQuad'sizeOf = #size decQuad

data C'decQuad

p'decQuad'bytes :: Ptr C'decQuad -> Ptr c'decQuad'bytes
p'decQuad'bytes = #ptr decQuad, bytes

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

foreign import ccall unsafe "decQuadToInt32" c'decQuadToInt32
  :: Ptr C'decQuad
  -> Ptr C'decContext
  -> C'rounding
  -> IO Int32

foreign import ccall unsafe "decQuadToInt32Exact" c'decQuadToInt32Exact
  :: Ptr C'decQuad
  -> Ptr C'decContext
  -> C'rounding
  -> IO Int32

foreign import ccall unsafe "decQuadFromInt32" c'decQuadFromInt32
  :: Ptr C'decQuad
  -> Int32
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadFromPacked" c'decQuadFromPacked
  :: Ptr C'decQuad
  -> Int32
  -> Ptr Word8
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadFromPackedChecked" c'decQuadFromPackedChecked
  :: Ptr C'decQuad
  -> Int32
  -> Ptr Word8
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadFromUInt32" c'decQuadFromUInt32
  :: Ptr C'decQuad
  -> Word32
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadFromString" c'decQuadFromString
  :: Ptr C'decQuad
  -> CString
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadGetCoefficient" c'decQuadGetCoefficient
  :: Ptr C'decQuad
  -> Ptr Word8
  -> IO Int32

foreign import ccall unsafe "decQuadGetExponent" c'decQuadGetExponent
  :: Ptr C'decQuad
  -> IO Int32

foreign import ccall unsafe "decQuadSetCoefficient" c'decQuadSetCoefficient
  :: Ptr C'decQuad
  -> Ptr Word8
  -> Int32
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadSetExponent" c'decQuadSetExponent
  :: Ptr C'decQuad
  -> Ptr C'decContext
  -> Int32
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadShow" c'decQuadShow
  :: Ptr C'decQuad
  -> CString
  -> IO ()

foreign import ccall unsafe "decQuadToEngString" c'decQuadToEngString
  :: Ptr C'decQuad
  -> CString
  -> IO CString

foreign import ccall unsafe "decQuadToString" c'decQuadToString
  :: Ptr C'decQuad
  -> CString
  -> IO CString

foreign import ccall unsafe "decQuadToUInt32" c'decQuadToUInt32
  :: Ptr C'decQuad
  -> Ptr C'decContext
  -> C'rounding
  -> IO Word32


foreign import ccall unsafe "decQuadToUInt32Exact" c'decQuadToUInt32Exact
  :: Ptr C'decQuad
  -> Ptr C'decContext
  -> C'rounding
  -> IO Word32

foreign import ccall unsafe "decQuadZero" c'decQuadZero
  :: Ptr C'decQuad
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadAbs" c'decQuadAbs
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadAdd" c'decQuadAdd
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadAnd" c'decQuadAnd
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadDivide" c'decQuadDivide
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadDivideInteger" c'decQuadDivideInteger
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadFMA" c'decQuadFMA
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadFromBCD" c'decQuadFromBCD
  :: Ptr C'decQuad
  -> Int32
  -> Ptr Word8
  -> Int32
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadInvert" c'decQuadInvert
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadLogB" c'decQuadLogB
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadMax" c'decQuadMax
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadMaxMag" c'decQuadMaxMag
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadMin" c'decQuadMin
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadMinMag" c'decQuadMinMag
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadMinus" c'decQuadMinus
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadMultiply" c'decQuadMultiply
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadNextMinus" c'decQuadNextMinus
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadNextPlus" c'decQuadNextPlus
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadNextToward" c'decQuadNextToward
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadOr" c'decQuadOr
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadPlus" c'decQuadPlus
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadQuantize" c'decQuadQuantize
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadReduce" c'decQuadReduce
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadRemainder" c'decQuadRemainder
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadRemainderNear" c'decQuadRemainderNear
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadRotate" c'decQuadRotate
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadScaleB" c'decQuadScaleB
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadShift" c'decQuadShift
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadSubtract" c'decQuadSubtract
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadToBCD" c'decQuadToBCD
  :: Ptr C'decQuad
  -> Ptr Int32
  -> Ptr Word8
  -> IO Int32

foreign import ccall unsafe "decQuadToIntegralValue" c'decQuadToIntegralValue
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> C'rounding
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadToIntegralExact" c'decQuadToIntegralExact
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadXor" c'decQuadXor
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

-- Comparisons

foreign import ccall unsafe "decQuadCompare" c'decQuadCompare
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadCompareSignal" c'decQuadCompareSignal
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadCompareTotal" c'decQuadCompareTotal
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadCompareTotalMag" c'decQuadCompareTotalMag
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO (Ptr C'decQuad)

-- Copies
foreign import ccall unsafe "decQuadCanonical" c'decQuadCanonical
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadCopyAbs" c'decQuadCopyAbs
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadCopyNegate" c'decQuadCopyNegate
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadCopySign" c'decQuadCopySign
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadCopy" c'decQuadCopy
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO (Ptr C'decQuad)

-- Non-computational

foreign import ccall unsafe "decQuadClass" c'decQuadClass
  :: Ptr C'decQuad
  -> IO C'decClass

foreign import ccall unsafe "decQuadClassString" c'decQuadClassString
  :: Ptr C'decQuad
  -> IO CString

foreign import ccall unsafe "decQuadDigits" c'decQuadDigits
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsCanonical" c'decQuadIsCanonical
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsFinite" c'decQuadIsFinite
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsInteger" c'decQuadIsInteger
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsLogical" c'decQuadIsLogical
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsInfinite" c'decQuadIsInfinite
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsNaN" c'decQuadIsNaN
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsNegative" c'decQuadIsNegative
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsNormal" c'decQuadIsNormal
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsPositive" c'decQuadIsPositive
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsSignaling" c'decQuadIsSignaling
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsSigned" c'decQuadIsSigned
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsSubnormal" c'decQuadIsSubnormal
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsZero" c'decQuadIsZero
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadRadix" c'decQuadRadix
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadSameQuantum" c'decQuadSameQuantum
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadVersion" c'decQuadVersion
  :: IO CString
