{-# LANGUAGE ForeignFunctionInterface #-}
-- Bindings-dsl sometimes shadows in do notation
-- Bindings-dsl imports unused things

{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-imports #-}

#include <bindings.dsl.h>
#include <decContext.h>
#include <decQuad.h>

-- | Low-level bindings to the decNumber library.
module Data.Deka.Decnumber where

#strict_import

#num NULL

#integral_t enum rounding
#num DEC_ROUND_CEILING
#num DEC_ROUND_UP
#num DEC_ROUND_HALF_UP
#num DEC_ROUND_HALF_EVEN
#num DEC_ROUND_HALF_DOWN
#num DEC_ROUND_DOWN
#num DEC_ROUND_FLOOR
#num DEC_ROUND_05UP
#num DEC_ROUND_MAX

#integral_t int32_t
#integral_t uint8_t
#integral_t uint16_t
#integral_t uint32_t
#integral_t uint64_t


#starttype decContext
#field digits , <int32_t>
#field emax , <int32_t>
#field emin , <int32_t>
#field round , <enum rounding>
#field traps , <uint32_t>
#field status , <uint32_t>
#field clamp , <uint8_t>
#stoptype

-- decContext
#num DEC_INIT_DECQUAD
#ccall_unsafe decContextDefault , Ptr <decContext> -> <int32_t> -> IO (Ptr <decContext>)

#integral_t enum decClass
#num DEC_CLASS_SNAN
#num DEC_CLASS_QNAN
#num DEC_CLASS_NEG_INF
#num DEC_CLASS_NEG_NORMAL
#num DEC_CLASS_NEG_SUBNORMAL
#num DEC_CLASS_NEG_ZERO
#num DEC_CLASS_POS_ZERO
#num DEC_CLASS_POS_SUBNORMAL
#num DEC_CLASS_POS_NORMAL
#num DEC_CLASS_POS_INF

#num DEC_Conversion_syntax
#num DEC_Division_by_zero
#num DEC_Division_impossible
#num DEC_Division_undefined
#num DEC_Insufficient_storage
#num DEC_Inexact
#num DEC_Invalid_context
#num DEC_Invalid_operation
#num DEC_Overflow
#num DEC_Clamped
#num DEC_Rounded
#num DEC_Subnormal
#num DEC_Underflow

#num DEC_IEEE_754_Division_by_zero
#num DEC_IEEE_754_Inexact
#num DEC_IEEE_754_Invalid_operation
#num DEC_IEEE_754_Overflow
#num DEC_IEEE_754_Underflow
#num DEC_Errors
#num DEC_NaNs

#num DEC_Condition_Length

#num DEC_INIT_BASE
#num DEC_INIT_DECIMAL32
#num DEC_INIT_DECIMAL64
#num DEC_INIT_DECIMAL128


#num DECQUAD_Bytes
#num DECQUAD_Pmax
#num DECQUAD_Emin
#num DECQUAD_Emax
#num DECQUAD_EmaxD
#num DECQUAD_Bias
#num DECQUAD_String
#num DECQUAD_EconL
#num DECQUAD_Declets
#num DECQUAD_Ehigh

#starttype decQuad
#array_field bytes , <uint8_t>
#array_field shorts , <uint16_t>
#array_field words , <uint32_t>
#stoptype


#num DECFLOAT_Sign
#num DECFLOAT_NaN
#num DECFLOAT_qNaN
#num DECFLOAT_sNaN
#num DECFLOAT_Inf
#num DECFLOAT_MinSp

#num DECPPLUSALT
#num DECPMINUSALT
#num DECPPLUS
#num DECPMINUS
#num DECPPLUSALT2
#num DECPUNSIGNED

-- Utilities

#ccall_unsafe decQuadToInt32 , Ptr <decQuad> -> Ptr <decContext> -> <enum rounding> -> IO <int32_t>
#ccall_unsafe decQuadToInt32Exact , Ptr <decQuad> -> Ptr <decContext> -> <enum rounding> -> IO <int32_t>

#ccall_unsafe decQuadFromInt32 , Ptr <decQuad> -> <int32_t> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadFromPacked , Ptr <decQuad> -> <int32_t> -> Ptr <uint8_t> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadFromPackedChecked , Ptr <decQuad> -> <int32_t> -> Ptr <uint8_t> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadFromUInt32 , Ptr <decQuad> -> <uint32_t> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadFromString , Ptr <decQuad> -> CString -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadGetCoefficient , Ptr <decQuad> -> Ptr <uint8_t> -> IO <int32_t>
#ccall_unsafe decQuadGetExponent , Ptr <decQuad> -> IO <int32_t>
#ccall_unsafe decQuadSetCoefficient , Ptr <decQuad> -> Ptr <uint8_t> -> <int32_t> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadSetExponent , Ptr <decQuad> -> Ptr <decContext> -> <int32_t> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadShow , Ptr <decQuad> -> CString -> IO ()
#ccall_unsafe decQuadToEngString , Ptr <decQuad> -> CString -> IO CString
#ccall_unsafe decQuadToString , Ptr <decQuad> -> CString -> IO CString
#ccall_unsafe decQuadToUInt32 , Ptr <decQuad> -> Ptr <decContext> -> <enum rounding> -> IO <uint32_t>
#ccall_unsafe decQuadToUInt32Exact , Ptr <decQuad> -> Ptr <decContext> -> <enum rounding> -> IO <uint32_t>
#ccall_unsafe decQuadZero , Ptr <decQuad> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadAbs , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadAdd , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadAnd , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadDivide , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadDivideInteger , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadFMA , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadFromBCD , Ptr <decQuad> -> <int32_t> -> Ptr <uint8_t> -> <int32_t> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadInvert , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadLogB , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadMax , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadMaxMag , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadMin , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadMinMag , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadMinus , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadMultiply , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadNextMinus , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadNextPlus , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadNextToward , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadOr , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadPlus , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadQuantize , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadReduce , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadRemainder , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadRemainderNear , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadRotate , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadScaleB , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadShift , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadSubtract , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadToBCD , Ptr <decQuad> -> Ptr <int32_t> -> Ptr <uint8_t> -> IO <int32_t>
#ccall_unsafe decQuadToIntegralValue , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> <enum rounding> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadToIntegralExact , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadXor , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)

-- Comparisons

#ccall_unsafe decQuadCompare , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadCompareSignal , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadCompareTotal , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadCompareTotalMag , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> IO (Ptr <decQuad>)

-- Copies
#ccall_unsafe decQuadCanonical , Ptr <decQuad> -> Ptr <decQuad> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadCopyAbs , Ptr <decQuad> -> Ptr <decQuad> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadCopyNegate , Ptr <decQuad> -> Ptr <decQuad> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadCopySign , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> IO (Ptr <decQuad>)
#ccall_unsafe decQuadCopy , Ptr <decQuad> -> Ptr <decQuad> -> IO (Ptr <decQuad>)

-- Non-computational

#ccall_unsafe decQuadClass , Ptr <decQuad> -> IO <decClass>
#ccall_unsafe decQuadClassString , Ptr <decQuad> -> IO CString
#ccall_unsafe decQuadDigits , Ptr <decQuad> -> IO <uint32_t>
#ccall_unsafe decQuadIsCanonical , Ptr <decQuad> -> IO <uint32_t>
#ccall_unsafe decQuadIsFinite , Ptr <decQuad> -> IO <uint32_t>
#ccall_unsafe decQuadIsInteger , Ptr <decQuad> -> IO <uint32_t>
#ccall_unsafe decQuadIsLogical , Ptr <decQuad> -> IO <uint32_t>
#ccall_unsafe decQuadIsInfinite , Ptr <decQuad> -> IO <uint32_t>
#ccall_unsafe decQuadIsNaN , Ptr <decQuad> -> IO <uint32_t>
#ccall_unsafe decQuadIsNegative , Ptr <decQuad> -> IO <uint32_t>
#ccall_unsafe decQuadIsNormal , Ptr <decQuad> -> IO <uint32_t>
#ccall_unsafe decQuadIsPositive , Ptr <decQuad> -> IO <uint32_t>
#ccall_unsafe decQuadIsSignaling , Ptr <decQuad> -> IO <uint32_t>
#ccall_unsafe decQuadIsSigned , Ptr <decQuad> -> IO <uint32_t>
#ccall_unsafe decQuadIsSubnormal , Ptr <decQuad> -> IO <uint32_t>
#ccall_unsafe decQuadIsZero , Ptr <decQuad> -> IO <uint32_t>
#ccall_unsafe decQuadRadix , Ptr <decQuad> -> IO <uint32_t>
#ccall_unsafe decQuadSameQuantum , Ptr <decQuad> -> Ptr <decQuad> -> IO <uint32_t>
#ccall_unsafe decQuadVersion , IO CString
