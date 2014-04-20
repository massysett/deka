{-# LANGUAGE ForeignFunctionInterface #-}

#include <decContext.h>
#include <decQuad.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- | Low-level bindings to the decNumber library.
module Deka.Decnumber.DecQuad where

import Foreign.Safe
import Foreign.C
import Control.Applicative

c'NULL :: Num a => a
c'NULL = #const NULL

type C'rounding = #type enum rounding

c'DEC_ROUND_CEILING :: Num a => a
c'DEC_ROUND_CEILING = #const DEC_ROUND_CEILING

c'DEC_ROUND_UP :: Num a => a
c'DEC_ROUND_UP = #const DEC_ROUND_UP

c'DEC_ROUND_HALF_UP :: Num a => a
c'DEC_ROUND_HALF_UP = #const DEC_ROUND_HALF_UP

c'DEC_ROUND_HALF_EVEN :: Num a => a
c'DEC_ROUND_HALF_EVEN = #const DEC_ROUND_HALF_EVEN

c'DEC_ROUND_HALF_DOWN :: Num a => a
c'DEC_ROUND_HALF_DOWN = #const DEC_ROUND_HALF_DOWN

c'DEC_ROUND_DOWN :: Num a => a
c'DEC_ROUND_DOWN = #const DEC_ROUND_DOWN

c'DEC_ROUND_FLOOR :: Num a => a
c'DEC_ROUND_FLOOR = #const DEC_ROUND_FLOOR

c'DEC_ROUND_05UP :: Num a => a
c'DEC_ROUND_05UP = #const DEC_ROUND_05UP

c'DEC_ROUND_MAX :: Num a => a
c'DEC_ROUND_MAX = #const DEC_ROUND_MAX

type C'int32_t = #type int32_t
type C'uint8_t = #type uint8_t
type C'uint16_t = #type uint16_t
type C'uint32_t = #type uint32_t
type C'uint64_t = #type uint64_t

data C'decContext = C'decContext
  { c'decContext'digits :: C'int32_t
  , c'decContext'emax :: C'int32_t
  , c'decContext'emin :: C'int32_t
  , c'decContext'round :: C'rounding
  , c'decContext'traps :: C'uint32_t
  , c'decContext'status :: C'uint32_t
  , c'decContext'clamp :: C'uint8_t
  } deriving (Eq, Show)

instance Storable C'decContext where
  sizeOf _ = #size decContext
  alignment _ = #alignment decContext
  peek p =
    C'decContext
    <$> #{peek decContext, digits} p
    <*> #{peek decContext, emax} p
    <*> #{peek decContext, emin} p
    <*> #{peek decContext, round} p
    <*> #{peek decContext, traps} p
    <*> #{peek decContext, status} p
    <*> #{peek decContext, clamp} p

  poke p (C'decContext d ex en r t s c) =
    #{poke decContext, digits} p d
    >> #{poke decContext, emax} p ex
    >> #{poke decContext, emin} p en
    >> #{poke decContext, round} p r
    >> #{poke decContext, traps} p t
    >> #{poke decContext, status} p s
    >> #{poke decContext, clamp} p c

p'decContext'status :: Ptr C'decContext -> Ptr C'uint32_t
p'decContext'status = #ptr decContext, status

p'decContext'round :: Ptr C'decContext -> Ptr C'rounding
p'decContext'round = #ptr decContext, round

-- decContext
c'DEC_INIT_DECQUAD :: Num a => a
c'DEC_INIT_DECQUAD = #const DEC_INIT_DECQUAD

foreign import ccall unsafe "decContextDefault" unsafe'c'decContextDefault
  :: Ptr C'decContext
  -> C'int32_t
  -> IO (Ptr C'decContext)

type C'decClass = #type enum decClass

c'DEC_CLASS_SNAN :: Num a => a
c'DEC_CLASS_SNAN = #const DEC_CLASS_SNAN

c'DEC_CLASS_QNAN :: Num a => a
c'DEC_CLASS_QNAN = #const DEC_CLASS_QNAN

c'DEC_CLASS_NEG_INF :: Num a => a
c'DEC_CLASS_NEG_INF = #const DEC_CLASS_NEG_INF

c'DEC_CLASS_NEG_NORMAL :: Num a => a
c'DEC_CLASS_NEG_NORMAL = #const DEC_CLASS_NEG_NORMAL

c'DEC_CLASS_NEG_SUBNORMAL :: Num a => a
c'DEC_CLASS_NEG_SUBNORMAL = #const DEC_CLASS_NEG_SUBNORMAL

c'DEC_CLASS_NEG_ZERO :: Num a => a
c'DEC_CLASS_NEG_ZERO = #const DEC_CLASS_NEG_ZERO

c'DEC_CLASS_POS_ZERO :: Num a => a
c'DEC_CLASS_POS_ZERO = #const DEC_CLASS_POS_ZERO

c'DEC_CLASS_POS_SUBNORMAL :: Num a => a
c'DEC_CLASS_POS_SUBNORMAL = #const DEC_CLASS_POS_SUBNORMAL

c'DEC_CLASS_POS_NORMAL :: Num a => a
c'DEC_CLASS_POS_NORMAL = #const DEC_CLASS_POS_NORMAL

c'DEC_CLASS_POS_INF :: Num a => a
c'DEC_CLASS_POS_INF = #const DEC_CLASS_POS_INF

c'DEC_Conversion_syntax :: Num a => a
c'DEC_Conversion_syntax = #const DEC_Conversion_syntax

c'DEC_Division_by_zero :: Num a => a
c'DEC_Division_by_zero = #const DEC_Division_by_zero

c'DEC_Division_impossible :: Num a => a
c'DEC_Division_impossible = #const DEC_Division_impossible

c'DEC_Division_undefined :: Num a => a
c'DEC_Division_undefined = #const DEC_Division_undefined

c'DEC_Insufficient_storage :: Num a => a
c'DEC_Insufficient_storage = #const DEC_Insufficient_storage

c'DEC_Inexact :: Num a => a
c'DEC_Inexact = #const DEC_Inexact

c'DEC_Invalid_context :: Num a => a
c'DEC_Invalid_context = #const DEC_Invalid_context

c'DEC_Invalid_operation :: Num a => a
c'DEC_Invalid_operation = #const DEC_Invalid_operation

c'DEC_Overflow :: Num a => a
c'DEC_Overflow = #const DEC_Overflow

c'DEC_Clamped :: Num a => a
c'DEC_Clamped = #const DEC_Clamped

c'DEC_Rounded :: Num a => a
c'DEC_Rounded = #const DEC_Rounded

c'DEC_Subnormal :: Num a => a
c'DEC_Subnormal = #const DEC_Subnormal

c'DEC_Underflow :: Num a => a
c'DEC_Underflow = #const DEC_Underflow

c'DEC_IEEE_754_Division_by_zero :: Num a => a
c'DEC_IEEE_754_Division_by_zero = #const DEC_IEEE_754_Division_by_zero

c'DEC_IEEE_754_Inexact :: Num a => a
c'DEC_IEEE_754_Inexact = #const DEC_IEEE_754_Inexact

c'DEC_IEEE_754_Invalid_operation :: Num a => a
c'DEC_IEEE_754_Invalid_operation = #const DEC_IEEE_754_Invalid_operation

c'DEC_IEEE_754_Overflow :: Num a => a
c'DEC_IEEE_754_Overflow = #const DEC_IEEE_754_Overflow

c'DEC_IEEE_754_Underflow :: Num a => a
c'DEC_IEEE_754_Underflow = #const DEC_IEEE_754_Underflow

c'DEC_Errors :: Num a => a
c'DEC_Errors = #const DEC_Errors

c'DEC_NaNs :: Num a => a
c'DEC_NaNs = #const DEC_NaNs

c'DEC_Condition_Length :: Num a => a
c'DEC_Condition_Length = #const DEC_Condition_Length

c'DEC_INIT_BASE :: Num a => a
c'DEC_INIT_BASE = #const DEC_INIT_BASE

c'DEC_INIT_DECIMAL32 :: Num a => a
c'DEC_INIT_DECIMAL32 = #const DEC_INIT_DECIMAL32

c'DEC_INIT_DECIMAL64 :: Num a => a
c'DEC_INIT_DECIMAL64 = #const DEC_INIT_DECIMAL64

c'DEC_INIT_DECIMAL128 :: Num a => a
c'DEC_INIT_DECIMAL128 = #const DEC_INIT_DECIMAL128


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

newtype C'decQuad = C'decQuad
  { c'decQuad'bytes :: [C'uint8_t]
  } deriving (Eq, Show)

p'decQuad'bytes :: Ptr C'decQuad -> Ptr c'decQuad'bytes
p'decQuad'bytes = #ptr decQuad, bytes

instance Storable C'decQuad where
  sizeOf _ = #size decQuad
  alignment _ = #alignment decQuad
  peek p =
    let pArr = p'decQuad'bytes p
    in fmap C'decQuad $ peekArray c'DECQUAD_Bytes pArr
  poke p (C'decQuad bs) = pokeArray (p'decQuad'bytes p) bs

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
  -> IO C'int32_t

foreign import ccall unsafe "decQuadToInt32Exact" unsafe'c'decQuadToInt32Exact
  :: Ptr C'decQuad
  -> Ptr C'decContext
  -> C'rounding
  -> IO C'int32_t

foreign import ccall unsafe "decQuadFromInt32" unsafe'c'decQuadFromInt32
  :: Ptr C'decQuad
  -> C'int32_t
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadFromPacked" unsafe'c'decQuadFromPacked
  :: Ptr C'decQuad
  -> C'int32_t
  -> Ptr C'uint8_t
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadFromPackedChecked" unsafe'c'decQuadFromPackedChecked
  :: Ptr C'decQuad
  -> C'int32_t
  -> Ptr C'uint8_t
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadFromUInt32" unsafe'c'decQuadFromUInt32
  :: Ptr C'decQuad
  -> C'uint32_t
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadFromString" unsafe'c'decQuadFromString
  :: Ptr C'decQuad
  -> CString
  -> Ptr C'decContext
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadGetCoefficient" unsafe'c'decQuadGetCoefficient
  :: Ptr C'decQuad
  -> Ptr C'uint8_t
  -> IO C'int32_t

foreign import ccall unsafe "decQuadGetExponent" unsafe'c'decQuadGetExponent
  :: Ptr C'decQuad
  -> IO C'int32_t

foreign import ccall unsafe "decQuadSetCoefficient" unsafe'c'decQuadSetCoefficient
  :: Ptr C'decQuad
  -> Ptr C'uint8_t
  -> C'int32_t
  -> IO (Ptr C'decQuad)

foreign import ccall unsafe "decQuadSetExponent" unsafe'c'decQuadSetExponent
  :: Ptr C'decQuad
  -> Ptr C'decContext
  -> C'int32_t
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
  -> IO C'uint32_t


foreign import ccall unsafe "decQuadToUInt32Exact" unsafe'c'decQuadToUInt32Exact
  :: Ptr C'decQuad
  -> Ptr C'decContext
  -> C'rounding
  -> IO C'uint32_t

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
  -> C'int32_t
  -> Ptr C'uint8_t
  -> C'int32_t
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
  -> Ptr C'int32_t
  -> Ptr C'uint8_t
  -> IO C'int32_t

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
  -> IO C'uint32_t

foreign import ccall unsafe "decQuadIsCanonical" unsafe'c'decQuadIsCanonical
  :: Ptr C'decQuad
  -> IO C'uint32_t

foreign import ccall unsafe "decQuadIsFinite" unsafe'c'decQuadIsFinite
  :: Ptr C'decQuad
  -> IO C'uint32_t

foreign import ccall unsafe "decQuadIsInteger" unsafe'c'decQuadIsInteger
  :: Ptr C'decQuad
  -> IO C'uint32_t

foreign import ccall unsafe "decQuadIsLogical" unsafe'c'decQuadIsLogical
  :: Ptr C'decQuad
  -> IO C'uint32_t

foreign import ccall unsafe "decQuadIsInfinite" unsafe'c'decQuadIsInfinite
  :: Ptr C'decQuad
  -> IO C'uint32_t

foreign import ccall unsafe "decQuadIsNaN" unsafe'c'decQuadIsNaN
  :: Ptr C'decQuad
  -> IO C'uint32_t

foreign import ccall unsafe "decQuadIsNegative" unsafe'c'decQuadIsNegative
  :: Ptr C'decQuad
  -> IO C'uint32_t

foreign import ccall unsafe "decQuadIsNormal" unsafe'c'decQuadIsNormal
  :: Ptr C'decQuad
  -> IO C'uint32_t

foreign import ccall unsafe "decQuadIsPositive" unsafe'c'decQuadIsPositive
  :: Ptr C'decQuad
  -> IO C'uint32_t

foreign import ccall unsafe "decQuadIsSignaling" unsafe'c'decQuadIsSignaling
  :: Ptr C'decQuad
  -> IO C'uint32_t

foreign import ccall unsafe "decQuadIsSigned" unsafe'c'decQuadIsSigned
  :: Ptr C'decQuad
  -> IO C'uint32_t

foreign import ccall unsafe "decQuadIsSubnormal" unsafe'c'decQuadIsSubnormal
  :: Ptr C'decQuad
  -> IO C'uint32_t

foreign import ccall unsafe "decQuadIsZero" unsafe'c'decQuadIsZero
  :: Ptr C'decQuad
  -> IO C'uint32_t

foreign import ccall unsafe "decQuadRadix" unsafe'c'decQuadRadix
  :: Ptr C'decQuad
  -> IO C'uint32_t

foreign import ccall unsafe "decQuadSameQuantum" unsafe'c'decQuadSameQuantum
  :: Ptr C'decQuad
  -> Ptr C'decQuad
  -> IO C'uint32_t

foreign import ccall unsafe "decQuadVersion" unsafe'c'decQuadVersion
  :: IO CString
