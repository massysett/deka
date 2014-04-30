{-# LANGUAGE ForeignFunctionInterface, Safe, EmptyDataDecls #-}

#include <decContext.h>
#include <decQuad.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- | Low-level bindings to the decNumber library.
module Deka.Internal.Decnumber.DecQuad where

import Foreign.Safe
import Foreign.C
import Deka.Internal.Decnumber.Types
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

foreign import ccall unsafe "decQuadToInt32" unsafe'c'decQuadToInt32
  :: Ptr C'decQuad
  -> Ptr C'decContext
  -> C'rounding
  -> IO Int32

foreign import ccall unsafe "decQuadToInt32Exact" unsafe'c'decQuadToInt32Exact
  :: Ptr C'decQuad
  -> Ptr C'decContext
  -> C'rounding
  -> IO Int32

foreign import ccall unsafe "decQuadFromInt32" unsafe'c'decQuadFromInt32
  :: Ptr C'decQuad
  -> Int32
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadFromPacked" unsafe'c'decQuadFromPacked
  :: Ptr C'decQuad
  -> Int32
  -> Ptr Word8
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadFromPackedChecked" unsafe'c'decQuadFromPackedChecked
  :: Ptr C'decQuad
  -> Int32
  -> Ptr Word8
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadFromUInt32" unsafe'c'decQuadFromUInt32
  :: Ptr C'decQuad
  -> Word32
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadFromString" unsafe'c'decQuadFromString
  :: Ptr C'decQuad
  -> CString
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadGetCoefficient" unsafe'c'decQuadGetCoefficient
  :: Ptr C'decQuad
  -> Ptr Word8
  -> IO Int32

foreign import ccall unsafe "decQuadGetExponent" unsafe'c'decQuadGetExponent
  :: Ptr C'decQuad
  -> IO Int32

foreign import ccall unsafe "decQuadSetCoefficient" unsafe'c'decQuadSetCoefficient
  :: Ptr C'decQuad
  -> Ptr Word8
  -> Int32
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadSetExponent" unsafe'c'decQuadSetExponent
  :: Ptr C'decQuad
  -> Ptr C'decContext
  -> Int32
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadShow" unsafe'c'decQuadShow
  :: Ptr C'decQuad
  -> CString
  -> IO ()

foreign import ccall unsafe "decQuadToEngString" unsafe'c'decQuadToEngString
  :: Ptr C'decQuad
  -> CString
  -> IO CString

foreign import ccall unsafe "decQuadToString" unsafe'c'decQuadToString
  :: Ptr C'decQuad
  -> CString
  -> IO CString

foreign import ccall unsafe "decQuadToUInt32" unsafe'c'decQuadToUInt32
  :: Ptr C'decQuad
  -> Ptr C'decContext
  -> C'rounding
  -> IO Word32


foreign import ccall unsafe "decQuadToUInt32Exact" unsafe'c'decQuadToUInt32Exact
  :: Ptr C'decQuad
  -> Ptr C'decContext
  -> C'rounding
  -> IO Word32

foreign import ccall unsafe "decQuadZero" unsafe'c'decQuadZero
  :: Ptr C'decQuad
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadAbs" unsafe'c'decQuadAbs
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadAdd" unsafe'c'decQuadAdd
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadAnd" unsafe'c'decQuadAnd
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadDivide" unsafe'c'decQuadDivide
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadDivideInteger" unsafe'c'decQuadDivideInteger
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadFMA" unsafe'c'decQuadFMA
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadFromBCD" unsafe'c'decQuadFromBCD
  :: Ptr C'decQuad
  -> Int32
  -> Ptr Word8
  -> Int32
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadInvert" unsafe'c'decQuadInvert
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadLogB" unsafe'c'decQuadLogB
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadMax" unsafe'c'decQuadMax
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadMaxMag" unsafe'c'decQuadMaxMag
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadMin" unsafe'c'decQuadMin
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadMinMag" unsafe'c'decQuadMinMag
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadMinus" unsafe'c'decQuadMinus
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadMultiply" unsafe'c'decQuadMultiply
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadNextMinus" unsafe'c'decQuadNextMinus
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadNextPlus" unsafe'c'decQuadNextPlus
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadNextToward" unsafe'c'decQuadNextToward
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadOr" unsafe'c'decQuadOr
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadPlus" unsafe'c'decQuadPlus
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadQuantize" unsafe'c'decQuadQuantize
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadReduce" unsafe'c'decQuadReduce
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadRemainder" unsafe'c'decQuadRemainder
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadRemainderNear" unsafe'c'decQuadRemainderNear
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadRotate" unsafe'c'decQuadRotate
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadScaleB" unsafe'c'decQuadScaleB
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadShift" unsafe'c'decQuadShift
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadSubtract" unsafe'c'decQuadSubtract
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadToBCD" unsafe'c'decQuadToBCD
  :: Ptr C'decQuad
  -> Ptr Int32
  -> Ptr Word8
  -> IO Int32

foreign import ccall unsafe "decQuadToIntegralValue" unsafe'c'decQuadToIntegralValue
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> C'rounding
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadToIntegralExact" unsafe'c'decQuadToIntegralExact
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadXor" unsafe'c'decQuadXor
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

-- Comparisons

foreign import ccall unsafe "decQuadCompare" unsafe'c'decQuadCompare
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadCompareSignal" unsafe'c'decQuadCompareSignal
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadCompareTotal" unsafe'c'decQuadCompareTotal
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadCompareTotalMag" unsafe'c'decQuadCompareTotalMag
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO (Ptr C'decQuad)

-- Copies
foreign import ccall unsafe "decQuadCanonical" unsafe'c'decQuadCanonical
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadCopyAbs" unsafe'c'decQuadCopyAbs
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadCopyNegate" unsafe'c'decQuadCopyNegate
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadCopySign" unsafe'c'decQuadCopySign
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadCopy" unsafe'c'decQuadCopy
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO (Ptr C'decQuad)

-- Non-computational

foreign import ccall unsafe "decQuadClass" unsafe'c'decQuadClass
  :: Ptr C'decQuad
  -> IO C'decClass

foreign import ccall unsafe "decQuadClassString" unsafe'c'decQuadClassString
  :: Ptr C'decQuad
  -> IO CString

foreign import ccall unsafe "decQuadDigits" unsafe'c'decQuadDigits
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsCanonical" unsafe'c'decQuadIsCanonical
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsFinite" unsafe'c'decQuadIsFinite
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsInteger" unsafe'c'decQuadIsInteger
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsLogical" unsafe'c'decQuadIsLogical
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsInfinite" unsafe'c'decQuadIsInfinite
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsNaN" unsafe'c'decQuadIsNaN
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsNegative" unsafe'c'decQuadIsNegative
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsNormal" unsafe'c'decQuadIsNormal
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsPositive" unsafe'c'decQuadIsPositive
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsSignaling" unsafe'c'decQuadIsSignaling
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsSigned" unsafe'c'decQuadIsSigned
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsSubnormal" unsafe'c'decQuadIsSubnormal
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadIsZero" unsafe'c'decQuadIsZero
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadRadix" unsafe'c'decQuadRadix
  :: Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadSameQuantum" unsafe'c'decQuadSameQuantum
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO Word32

foreign import ccall unsafe "decQuadVersion" unsafe'c'decQuadVersion
  :: IO CString
