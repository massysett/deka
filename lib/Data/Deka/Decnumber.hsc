{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Safe #-}
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
#ccall decContextDefault , Ptr <decContext> -> <int32_t> -> IO (Ptr <decContext>)

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

#ccall decQuadToInt32 , Ptr <decQuad> -> Ptr <decContext> -> <enum rounding> -> IO <int32_t>
#ccall decQuadToInt32Exact , Ptr <decQuad> -> Ptr <decContext> -> <enum rounding> -> IO <int32_t>

#ccall decQuadFromInt32 , Ptr <decQuad> -> <int32_t> -> IO (Ptr <decQuad>)
#ccall decQuadFromPacked , Ptr <decQuad> -> <int32_t> -> Ptr <uint8_t> -> IO (Ptr <decQuad>)
#ccall decQuadFromPackedChecked , Ptr <decQuad> -> <int32_t> -> Ptr <uint8_t> -> IO (Ptr <decQuad>)
#ccall decQuadFromUInt32 , Ptr <decQuad> -> <uint32_t> -> IO (Ptr <decQuad>)
#ccall decQuadFromString , Ptr <decQuad> -> CString -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadGetCoefficient , Ptr <decQuad> -> Ptr <uint8_t> -> IO <int32_t>
#ccall decQuadGetExponent , Ptr <decQuad> -> IO <int32_t>
#ccall decQuadSetCoefficient , Ptr <decQuad> -> Ptr <uint8_t> -> <int32_t> -> IO (Ptr <decQuad>)
#ccall decQuadSetExponent , Ptr <decQuad> -> Ptr <decContext> -> <int32_t> -> IO (Ptr <decQuad>)
#ccall decQuadShow , Ptr <decQuad> -> CString -> IO ()
#ccall decQuadToEngString , Ptr <decQuad> -> CString -> IO CString
#ccall decQuadToString , Ptr <decQuad> -> CString -> IO CString
#ccall decQuadToUInt32 , Ptr <decQuad> -> Ptr <decContext> -> <enum rounding> -> IO <uint32_t>
#ccall decQuadToUInt32Exact , Ptr <decQuad> -> Ptr <decContext> -> <enum rounding> -> IO <uint32_t>
#ccall decQuadZero , Ptr <decQuad> -> IO (Ptr <decQuad>)
#ccall decQuadAbs , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadAdd , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadAnd , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadDivide , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadDivideInteger , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadFMA , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadFromBCD , Ptr <decQuad> -> <int32_t> -> Ptr <uint8_t> -> <int32_t> -> IO (Ptr <decQuad>)
#ccall decQuadInvert , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadLogB , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadMax , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadMaxMag , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadMin , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadMinMag , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadMinus , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadMultiply , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadNextMinus , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadNextPlus , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadNextToward , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadOr , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadPlus , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadQuantize , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadReduce , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadRemainder , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadRemainderNear , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadRotate , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadScaleB , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadShift , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadSubtract , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadToBCD , Ptr <decQuad> -> Ptr <int32_t> -> Ptr <uint8_t> -> IO <int32_t>
#ccall decQuadToIntegralValue , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> <enum rounding> -> IO (Ptr <decQuad>)
#ccall decQuadToIntegralExact , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadXor , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)

-- Comparisons

#ccall decQuadCompare , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadCompareSignal , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadCompareTotal , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> IO (Ptr <decQuad>)
#ccall decQuadCompareTotalMag , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> IO (Ptr <decQuad>)

-- Copies
#ccall decQuadCanonical , Ptr <decQuad> -> Ptr <decQuad> -> IO (Ptr <decQuad>)
#ccall decQuadCopyAbs , Ptr <decQuad> -> Ptr <decQuad> -> IO (Ptr <decQuad>)
#ccall decQuadCopyNegate , Ptr <decQuad> -> Ptr <decQuad> -> IO (Ptr <decQuad>)
#ccall decQuadCopySign , Ptr <decQuad> -> Ptr <decQuad> -> Ptr <decQuad> -> IO (Ptr <decQuad>)
#ccall decQuadCopy , Ptr <decQuad> -> Ptr <decQuad> -> IO (Ptr <decQuad>)

-- Non-computational

#ccall decQuadClass , Ptr <decQuad> -> IO <decClass>
#ccall decQuadClassString , Ptr <decQuad> -> IO CString
#ccall decQuadDigits , Ptr <decQuad> -> IO <uint32_t>
#ccall decQuadIsCanonical , Ptr <decQuad> -> IO <uint32_t>
#ccall decQuadIsFinite , Ptr <decQuad> -> IO <uint32_t>
#ccall decQuadIsInteger , Ptr <decQuad> -> IO <uint32_t>
#ccall decQuadIsLogical , Ptr <decQuad> -> IO <uint32_t>
#ccall decQuadIsInfinite , Ptr <decQuad> -> IO <uint32_t>
#ccall decQuadIsNaN , Ptr <decQuad> -> IO <uint32_t>
#ccall decQuadIsNegative , Ptr <decQuad> -> IO <uint32_t>
#ccall decQuadIsNormal , Ptr <decQuad> -> IO <uint32_t>
#ccall decQuadIsPositive , Ptr <decQuad> -> IO <uint32_t>
#ccall decQuadIsSignaling , Ptr <decQuad> -> IO <uint32_t>
#ccall decQuadIsSigned , Ptr <decQuad> -> IO <uint32_t>
#ccall decQuadIsSubnormal , Ptr <decQuad> -> IO <uint32_t>
#ccall decQuadIsZero , Ptr <decQuad> -> IO <uint32_t>
#ccall decQuadRadix , Ptr <decQuad> -> IO <uint32_t>
#ccall decQuadSameQuantum , Ptr <decQuad> -> Ptr <decQuad> -> IO <uint32_t>
#ccall decQuadVersion , IO CString
