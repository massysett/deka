{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings, Safe #-}
{-# LANGUAGE EmptyDataDecls #-}
#define DECSUBSET 1
#include <decContext.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

module Deka.Internal.Decnumber.Context where

import Foreign.Safe
import Foreign.C
import Deka.Internal.Decnumber.Types
import Data.String

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

-- | All rounding constants.

allRounds :: Num a => [a]
allRounds =
  [ c'DEC_ROUND_CEILING
  , c'DEC_ROUND_UP
  , c'DEC_ROUND_HALF_UP
  , c'DEC_ROUND_HALF_EVEN
  , c'DEC_ROUND_HALF_DOWN
  , c'DEC_ROUND_DOWN
  , c'DEC_ROUND_FLOOR
  , c'DEC_ROUND_05UP
  , c'DEC_ROUND_MAX
  ]

data C'decContext

c'decContext'sizeOf :: Int
c'decContext'sizeOf = #size decContext

-- | Precision to be used.  Must be from 1 to 999,999,999.
p'decContext'digits :: Ptr C'decContext -> Ptr C'int32_t
p'decContext'digits = #ptr decContext, digits

-- | Largest adjusted exponent.  Must be from 0 to 999,999,999.
p'decContext'emax :: Ptr C'decContext -> Ptr C'int32_t
p'decContext'emax = #ptr decContext, emax

-- | Smallest adjusted exponent.  Must be in range -999,999,999 to 0.
p'decContext'emin :: Ptr C'decContext -> Ptr C'int32_t
p'decContext'emin = #ptr decContext, emin

-- | Rounding.  Must be one of the rounding enums.
p'decContext'round :: Ptr C'decContext -> Ptr C'rounding
p'decContext'round = #ptr decContext, round

-- | Which exceptions should cause a trap.
p'decContext'traps :: Ptr C'decContext -> Ptr C'uint32_t
p'decContext'traps = #ptr decContext, traps

-- | Status flags.  User should not set bits in here; only clear them.
p'decContext'status :: Ptr C'decContext -> Ptr C'uint32_t
p'decContext'status = #ptr decContext, status

-- | Zero for no clamping; 1 for clamping.
p'decContext'clamp :: Ptr C'decContext -> Ptr C'uint32_t
p'decContext'clamp = #ptr decContext, clamp

-- | Zero: no special values.  1: special values allowed.
p'decContext'extended :: Ptr C'decContext -> Ptr C'uint32_t
p'decContext'extended = #ptr decContext, extended

c'DEC_MAX_DIGITS :: Num a => a
c'DEC_MAX_DIGITS = #const DEC_MAX_DIGITS

c'DEC_MIN_DIGITS :: Num a => a
c'DEC_MIN_DIGITS = #const DEC_MIN_DIGITS

c'DEC_MAX_EMAX :: Num a => a
c'DEC_MAX_EMAX = #const DEC_MAX_EMAX

c'DEC_MIN_EMAX :: Num a => a
c'DEC_MIN_EMAX = #const DEC_MIN_EMAX

c'DEC_MAX_EMIN :: Num a => a
c'DEC_MAX_EMIN = #const DEC_MAX_EMIN

c'DEC_MIN_EMIN :: Num a => a
c'DEC_MIN_EMIN = #const DEC_MIN_EMIN

c'DEC_MAX_MATH :: Num a => a
c'DEC_MAX_MATH = #const DEC_MAX_MATH

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

-- | All decClass constants.

allDecClass :: Num a => [a]
allDecClass =
  [ c'DEC_CLASS_SNAN
  , c'DEC_CLASS_QNAN
  , c'DEC_CLASS_NEG_INF
  , c'DEC_CLASS_NEG_NORMAL
  , c'DEC_CLASS_NEG_SUBNORMAL
  , c'DEC_CLASS_NEG_ZERO
  , c'DEC_CLASS_POS_ZERO
  , c'DEC_CLASS_POS_SUBNORMAL
  , c'DEC_CLASS_POS_NORMAL
  , c'DEC_CLASS_POS_INF
  ]

c'DEC_ClassString_SN :: IsString a => a
c'DEC_ClassString_SN = #const_str DEC_ClassString_SN

c'DEC_ClassString_QN :: IsString a => a
c'DEC_ClassString_QN = #const_str DEC_ClassString_QN

c'DEC_ClassString_NI :: IsString a => a
c'DEC_ClassString_NI = #const_str DEC_ClassString_NI

c'DEC_ClassString_NN :: IsString a => a
c'DEC_ClassString_NN = #const_str DEC_ClassString_NN

c'DEC_ClassString_NS :: IsString a => a
c'DEC_ClassString_NS = #const_str DEC_ClassString_NS

c'DEC_ClassString_NZ :: IsString a => a
c'DEC_ClassString_NZ = #const_str DEC_ClassString_NZ

c'DEC_ClassString_PZ :: IsString a => a
c'DEC_ClassString_PZ = #const_str DEC_ClassString_PZ

c'DEC_ClassString_PS :: IsString a => a
c'DEC_ClassString_PS = #const_str DEC_ClassString_PS

c'DEC_ClassString_PN :: IsString a => a
c'DEC_ClassString_PN = #const_str DEC_ClassString_PN

c'DEC_ClassString_PI :: IsString a => a
c'DEC_ClassString_PI = #const_str DEC_ClassString_PI

c'DEC_ClassString_UN :: IsString a => a
c'DEC_ClassString_UN = #const_str DEC_ClassString_UN

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

c'DEC_Lost_digits :: Num a => a
c'DEC_Lost_digits = #const DEC_Lost_digits

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

-- | All extended flags
extFlags :: Num a => [a]
extFlags =
  [ c'DEC_Conversion_syntax
  , c'DEC_Division_by_zero
  , c'DEC_Division_impossible
  , c'DEC_Division_undefined
  , c'DEC_Insufficient_storage
  , c'DEC_Inexact
  , c'DEC_Invalid_context
  , c'DEC_Invalid_operation
  , c'DEC_Lost_digits
  , c'DEC_Overflow
  , c'DEC_Clamped
  , c'DEC_Rounded
  , c'DEC_Subnormal
  , c'DEC_Underflow
  ]

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

c'DEC_Information :: Num a => a
c'DEC_Information = #const DEC_Information

c'DEC_IEEE_854_Division_by_zero :: Num a => a
c'DEC_IEEE_854_Division_by_zero = #const DEC_IEEE_854_Division_by_zero

c'DEC_IEEE_854_Inexact :: Num a => a
c'DEC_IEEE_854_Inexact = #const DEC_IEEE_854_Inexact

c'DEC_IEEE_854_Invalid_operation :: Num a => a
c'DEC_IEEE_854_Invalid_operation = #const DEC_IEEE_854_Invalid_operation

c'DEC_IEEE_854_Overflow :: Num a => a
c'DEC_IEEE_854_Overflow = #const DEC_IEEE_854_Overflow

c'DEC_IEEE_854_Underflow :: Num a => a
c'DEC_IEEE_854_Underflow = #const DEC_IEEE_854_Underflow

c'DEC_Condition_CS :: IsString a => a
c'DEC_Condition_CS = #const_str DEC_Condition_CS

c'DEC_Condition_DZ :: IsString a => a
c'DEC_Condition_DZ = #const_str DEC_Condition_DZ

c'DEC_Condition_DI :: IsString a => a
c'DEC_Condition_DI = #const_str DEC_Condition_DI

c'DEC_Condition_DU :: IsString a => a
c'DEC_Condition_DU = #const_str DEC_Condition_DU

c'DEC_Condition_IE :: IsString a => a
c'DEC_Condition_IE = #const_str DEC_Condition_IE

c'DEC_Condition_IS :: IsString a => a
c'DEC_Condition_IS = #const_str DEC_Condition_IS

c'DEC_Condition_IC :: IsString a => a
c'DEC_Condition_IC = #const_str DEC_Condition_IC

c'DEC_Condition_IO :: IsString a => a
c'DEC_Condition_IO = #const_str DEC_Condition_IO

c'DEC_Condition_LD :: IsString a => a
c'DEC_Condition_LD = #const_str DEC_Condition_LD

c'DEC_Condition_OV :: IsString a => a
c'DEC_Condition_OV = #const_str DEC_Condition_OV

c'DEC_Condition_PA :: IsString a => a
c'DEC_Condition_PA = #const_str DEC_Condition_PA

c'DEC_Condition_RO :: IsString a => a
c'DEC_Condition_RO = #const_str DEC_Condition_RO

c'DEC_Condition_SU :: IsString a => a
c'DEC_Condition_SU = #const_str DEC_Condition_SU

c'DEC_Condition_UN :: IsString a => a
c'DEC_Condition_UN = #const_str DEC_Condition_UN

c'DEC_Condition_ZE :: IsString a => a
c'DEC_Condition_ZE = #const_str DEC_Condition_ZE

c'DEC_Condition_MU :: IsString a => a
c'DEC_Condition_MU = #const_str DEC_Condition_MU

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

-- | All initializers
allInitializers :: Num a => [a]
allInitializers =
  [ c'DEC_INIT_BASE
  , c'DEC_INIT_DECIMAL32
  , c'DEC_INIT_DECIMAL64
  , c'DEC_INIT_DECIMAL128
  ]

c'DEC_INIT_DECSINGLE :: Num a => a
c'DEC_INIT_DECSINGLE = #const DEC_INIT_DECSINGLE

c'DEC_INIT_DECDOUBLE :: Num a => a
c'DEC_INIT_DECDOUBLE = #const DEC_INIT_DECDOUBLE

c'DEC_INIT_DECQUAD :: Num a => a
c'DEC_INIT_DECQUAD = #const DEC_INIT_DECQUAD

-- # Functions

foreign import ccall unsafe "decContextClearStatus" c'decContextClearStatus
  :: Ptr C'decContext
  -> C'uint32_t
  -> IO (Ptr C'decContext)

foreign import ccall unsafe "decContextDefault" c'decContextDefault
  :: Ptr C'decContext
  -> C'int32_t
  -> IO (Ptr C'decContext)

foreign import ccall unsafe "decContextGetRounding" c'decContextGetRounding
  :: Ptr C'decContext
  -> IO C'rounding

foreign import ccall unsafe "decContextGetStatus" c'decContextGetStatus
  :: Ptr C'decContext
  -> IO C'uint32_t

foreign import ccall unsafe "decContextRestoreStatus" c'decContextRestoreStatus
  :: Ptr C'decContext
  -> C'uint32_t
  -> C'uint32_t
  -> IO (Ptr C'decContext)

foreign import ccall unsafe "decContextSaveStatus" c'decContextSaveStatus
  :: Ptr C'decContext
  -> C'uint32_t
  -> IO C'uint32_t

foreign import ccall unsafe "decContextSetRounding" c'decContextSetRounding
  :: Ptr C'decContext
  -> C'rounding
  -> IO (Ptr C'decContext)

foreign import ccall unsafe "decContextSetStatus" c'decContextSetStatus
  :: Ptr C'decContext
  -> C'uint32_t
  -> IO (Ptr C'decContext)

foreign import ccall unsafe "decContextSetStatusFromString" c'decContextSetStatusFromString
  :: Ptr C'decContext
  -> CString
  -> IO (Ptr C'decContext)

foreign import ccall unsafe "decContextSetStatusFromStringQuiet" c'decContextSetStatusFromStringQuiet
  :: Ptr C'decContext
  -> CString
  -> IO (Ptr C'decContext)

foreign import ccall unsafe "decContextStatusToString" c'decContextStatusToString
  :: Ptr C'decContext
  -> IO CString

foreign import ccall unsafe "decContextTestEndian" c'decContextTestEndian
  :: C'uint8_t
  -> IO C'int32_t

foreign import ccall unsafe "decContextTestSavedStatus" c'decContextTestSavedStatus
  :: C'uint32_t
  -> C'uint32_t
  -> IO C'uint32_t

foreign import ccall unsafe "decContextTestStatus" c'decContextTestStatus
  :: Ptr C'decContext
  -> C'uint32_t
  -> IO C'uint32_t

foreign import ccall unsafe "decContextZeroStatus" c'decContextZeroStatus
  :: Ptr C'decContext
  -> IO (Ptr C'decContext)

