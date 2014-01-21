{-# LANGUAGE ForeignFunctionInterface #-}
-- Bindings-dsl sometimes shadows in do notation
-- Bindings-dsl imports unused things

{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-imports #-}

#include <bindings.dsl.h>
#include <c/decContext.h>
#include <c/decQuad.h>

module Data.Deka.Decnumber where

#strict_import

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
#field round , CInt
#field traps , <uint32_t>
#field status , <uint32_t>
#field clamp , <uint8_t>
#stoptype

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

#globalarray DEC_Condition_CS , CChar
#globalarray DEC_Condition_DZ , CChar
#globalarray DEC_Condition_DI , CChar
#globalarray DEC_Condition_DU , CChar
#globalarray DEC_Condition_IE , CChar
#globalarray DEC_Condition_IS , CChar
#globalarray DEC_Condition_IC , CChar
#globalarray DEC_Condition_IO , CChar
#globalarray DEC_Condition_OV , CChar
#globalarray DEC_Condition_PA , CChar
#globalarray DEC_Condition_RO , CChar
#globalarray DEC_Condition_SU , CChar
#globalarray DEC_Condition_UN , CChar
#globalarray DEC_Condition_ZE , CChar
#globalarray DEC_Condition_MU , CChar

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

#ccall decQuadFromString , Ptr <decQuad> -> CString -> Ptr <decContext> -> IO (Ptr <decQuad>)
#ccall decQuadGetCoefficient , Ptr <decQuad> -> Ptr <uint8_t> -> IO <int32_t>
#ccall decQuadGetExponent , Ptr <decQuad> -> IO <int32_t>
#ccall decQuadSetCoefficient , Ptr <decQuad> -> Ptr <uint8_t> -> <int32_t> -> IO (Ptr <decQuad>)
#ccall decQuadSetExponent , Ptr <decQuad> -> Ptr <decContext> -> <int32_t> -> IO (Ptr <decQuad>)
