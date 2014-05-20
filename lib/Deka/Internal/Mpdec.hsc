{-# LANGUAGE EmptyDataDecls, Safe #-}
{-# LANGUAGE OverloadedStrings #-}
#include <mpdecimal.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

module Deka.Internal.Mpdec
  ( 
   -- * Context
    Signed
  , Unsigned
  , C'mpd_context_t
  , c'MPD_VERSION
  , c'MPD_SSIZE_MAX
  , c'MPD_SSIZE_MIN
  , c'MPD_MAX_PREC
  , c'MPD_MAX_EMAX
  , c'MPD_MIN_EMIN
  , c'MPD_ROUND_UP
  , c'MPD_ROUND_DOWN
  , c'MPD_ROUND_CEILING
  , c'MPD_ROUND_FLOOR
  , c'MPD_ROUND_HALF_UP
  , c'MPD_ROUND_HALF_DOWN
  , c'MPD_ROUND_HALF_EVEN
  , c'MPD_ROUND_05UP
  , c'MPD_ROUND_TRUNC
  , c'mpd_context_t'sizeOf
  , p'mpd_context_t'prec
  , p'mpd_context_t'emax
  , p'mpd_context_t'emin
  , p'mpd_context_t'traps
  , p'mpd_context_t'status
  , p'mpd_context_t'newtrap
  , p'mpd_context_t'round
  , p'mpd_context_t'clamp
  , p'mpd_context_t'allcr
  , c'MPD_Clamped
  , c'MPD_Conversion_syntax
  , c'MPD_Division_by_zero
  , c'MPD_Division_impossible
  , c'MPD_Division_undefined
  , c'MPD_Fpu_error
  , c'MPD_Inexact
  , c'MPD_Invalid_context
  , c'MPD_Invalid_operation
  , c'MPD_Malloc_error
  , c'MPD_Not_implemented
  , c'MPD_Overflow
  , c'MPD_Rounded
  , c'MPD_Subnormal
  , c'MPD_Underflow
  , c'mpd_maxcontext
  , c'mpd_defaultcontext
  , c'mpd_basiccontext
  , c'mpd_ieee_context

  -- * Mpdec
  , CMpd
  , Mpd
  , Dec
  , withDec
  , newDec
  , newDec2
  , c'divmod
  , c'mpd_fma
  , c'mpd_powmod
  , c'mpd_adjexp
  , capitalize
  , c'mpd_to_sci
  , c'mpd_to_eng
  , c'mpd_set_string
  , c'mpd_compare_total
  , c'mpd_cmp_total
  , c'mpd_compare_total_mag
  , c'mpd_cmp_total_mag
  , c'mpd_same_quantum
  , c'mpd_class
  , c'mpd_isnormal
  , c'mpd_issubnormal
  , c'mpd_sign
  , c'mpd_arith_sign
  , c'mpd_trail_zeros
  , c'mpd_del
  , c'mpd_copy
  , c'mpd_copy_abs
  , c'mpd_copy_negate
  , c'mpd_invert
  , c'mpd_logb
  , c'mpd_abs
  , c'mpd_exp
  , c'mpd_ln
  , c'mpd_log10
  , c'mpd_minus
  , c'mpd_next_minus
  , c'mpd_next_plus
  , c'mpd_plus
  , c'mpd_reduce
  , c'mpd_round_to_intx
  , c'mpd_round_to_int
  , c'mpd_trunc
  , c'mpd_floor
  , c'mpd_ceil
  , c'mpd_sqrt
  , c'mpd_invroot
  , c'mpd_and
  , c'mpd_copy_sign
  , c'mpd_or
  , c'mpd_rotate
  , c'mpd_scaleb
  , c'mpd_shift
  , c'mpd_xor
  , c'mpd_compare
  , c'mpd_compare_signal
  , c'mpd_add
  , c'mpd_sub
  , c'mpd_div
  , c'mpd_divint
  , c'mpd_max
  , c'mpd_max_mag
  , c'mpd_min
  , c'mpd_min_mag
  , c'mpd_mul
  , c'mpd_next_toward
  , c'mpd_pow
  , c'mpd_quantize
  , c'mpd_rescale
  , c'mpd_rem
  , c'mpd_rem_near
  , c'mpd_isfinite
  , c'mpd_isinfinite
  , c'mpd_isinteger
  , c'mpd_isnan
  , c'mpd_isnegative
  , c'mpd_ispositive
  , c'mpd_isqnan
  , c'mpd_issnan
  , c'mpd_issigned
  , c'mpd_isspecial
  , c'mpd_iszero
  , c'mpd_iszerocoeff
  , c'mpd_isoddcoeff
  , c'mpd_isodd
  , c'mpd_iseven
  ) where

import Foreign.Safe
import Foreign.C
import Control.Monad
import Data.String

c'MPD_VERSION :: IsString a => a
c'MPD_VERSION = #const_str MPD_VERSION

-- | An unsigned integer.  Its size is platform dependent.
type Unsigned = #type mpd_size_t

-- | A signed integer.  Its size is platform dependent.
type Signed = #type mpd_ssize_t

c'MPD_SSIZE_MAX :: Signed
c'MPD_SSIZE_MAX = #const MPD_SSIZE_MAX

-- Must convert the constant to an Integer first; it will overflow
-- otherwise.  GHC's NegativeLiterals extension solves this problem,
-- but it is not available on GHC < 7.8.
c'MPD_SSIZE_MIN :: Signed
c'MPD_SSIZE_MIN = fromInteger $ #const MPD_SSIZE_MIN

c'MPD_MAX_PREC :: Signed
c'MPD_MAX_PREC = #const MPD_MAX_PREC

c'MPD_MAX_EMAX :: Signed
c'MPD_MAX_EMAX = #const MPD_MAX_EMAX

c'MPD_MIN_EMIN :: Signed
c'MPD_MIN_EMIN = #const MPD_MIN_EMIN

c'MPD_ROUND_UP :: CInt
c'MPD_ROUND_UP = #const MPD_ROUND_UP

c'MPD_ROUND_DOWN :: CInt
c'MPD_ROUND_DOWN = #const MPD_ROUND_DOWN

c'MPD_ROUND_CEILING :: CInt
c'MPD_ROUND_CEILING = #const MPD_ROUND_CEILING

c'MPD_ROUND_FLOOR :: CInt
c'MPD_ROUND_FLOOR = #const MPD_ROUND_FLOOR

c'MPD_ROUND_HALF_UP :: CInt
c'MPD_ROUND_HALF_UP = #const MPD_ROUND_HALF_UP

c'MPD_ROUND_HALF_DOWN :: CInt
c'MPD_ROUND_HALF_DOWN = #const MPD_ROUND_HALF_DOWN

c'MPD_ROUND_HALF_EVEN :: CInt
c'MPD_ROUND_HALF_EVEN = #const MPD_ROUND_HALF_EVEN

c'MPD_ROUND_05UP :: CInt
c'MPD_ROUND_05UP = #const MPD_ROUND_05UP

c'MPD_ROUND_TRUNC :: CInt
c'MPD_ROUND_TRUNC = #const MPD_ROUND_TRUNC

data C'mpd_context_t

c'mpd_context_t'sizeOf :: Int
c'mpd_context_t'sizeOf = #size mpd_context_t

p'mpd_context_t'prec :: Ptr C'mpd_context_t -> Ptr Signed
p'mpd_context_t'prec = #ptr mpd_context_t, prec

p'mpd_context_t'emax :: Ptr C'mpd_context_t -> Ptr Signed
p'mpd_context_t'emax = #ptr mpd_context_t, emax

p'mpd_context_t'emin :: Ptr C'mpd_context_t -> Ptr Signed
p'mpd_context_t'emin = #ptr mpd_context_t, emin

p'mpd_context_t'traps :: Ptr C'mpd_context_t -> Ptr Word32
p'mpd_context_t'traps = #ptr mpd_context_t, traps

p'mpd_context_t'status :: Ptr C'mpd_context_t -> Ptr Word32
p'mpd_context_t'status = #ptr mpd_context_t, status

p'mpd_context_t'newtrap :: Ptr C'mpd_context_t -> Ptr Word32
p'mpd_context_t'newtrap = #ptr mpd_context_t, newtrap

p'mpd_context_t'round :: Ptr C'mpd_context_t -> Ptr CInt
p'mpd_context_t'round = #ptr mpd_context_t, round

p'mpd_context_t'clamp :: Ptr C'mpd_context_t -> Ptr CInt
p'mpd_context_t'clamp = #ptr mpd_context_t, clamp

p'mpd_context_t'allcr :: Ptr C'mpd_context_t -> Ptr CInt
p'mpd_context_t'allcr = #ptr mpd_context_t, allcr

c'MPD_Clamped :: Word32
c'MPD_Clamped = #const MPD_Clamped

c'MPD_Conversion_syntax :: Word32
c'MPD_Conversion_syntax = #const MPD_Conversion_syntax

c'MPD_Division_by_zero :: Word32
c'MPD_Division_by_zero = #const MPD_Division_by_zero

c'MPD_Division_impossible :: Word32
c'MPD_Division_impossible = #const MPD_Division_impossible

c'MPD_Division_undefined :: Word32
c'MPD_Division_undefined = #const MPD_Division_undefined

c'MPD_Fpu_error :: Word32
c'MPD_Fpu_error = #const MPD_Fpu_error

c'MPD_Inexact :: Word32
c'MPD_Inexact = #const MPD_Inexact

c'MPD_Invalid_context :: Word32
c'MPD_Invalid_context = #const MPD_Invalid_context

c'MPD_Invalid_operation :: Word32
c'MPD_Invalid_operation = #const MPD_Invalid_operation

c'MPD_Malloc_error :: Word32
c'MPD_Malloc_error = #const MPD_Malloc_error

c'MPD_Not_implemented :: Word32
c'MPD_Not_implemented = #const MPD_Not_implemented

c'MPD_Overflow :: Word32
c'MPD_Overflow = #const MPD_Overflow

c'MPD_Rounded :: Word32
c'MPD_Rounded = #const MPD_Rounded

c'MPD_Subnormal :: Word32
c'MPD_Subnormal = #const MPD_Subnormal

c'MPD_Underflow :: Word32
c'MPD_Underflow = #const MPD_Underflow

foreign import ccall unsafe "mpd_maxcontext" c'mpd_maxcontext
  :: Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_defaultcontext" c'mpd_defaultcontext
  :: Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_basiccontext" c'mpd_basiccontext
  :: Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_ieee_context" c'mpd_ieee_context
  :: Ptr C'mpd_context_t
  -> CInt
  -> IO CInt

--
-- mpd_t
--

data C'mpd_t

newtype CMpd = CMpd { _unCMpd :: Ptr C'mpd_t }
newtype Mpd = Mpd { unMpd :: Ptr C'mpd_t }

-- | A decimal value.  A decimal consists of:
--
-- * an integral /coefficient/,
--
-- * an /exponent/, and
--
-- * a /sign/.
--
-- A decimal may also be a /special value/, which can be:
--
-- * /NaN/ (Not a Number), which may be either /quiet/
-- (propagates quietly through operations) or /signaling/ (raises
-- the /Invalid operation/ condition when encountered), or
--
-- * /Infinity/, either positive or negative.

newtype Dec = Dec { _unDec :: ForeignPtr C'mpd_t }

withDec :: Dec -> (CMpd -> IO a) -> IO a
withDec (Dec fp) f =
  withForeignPtr fp $ \ptr ->
  f (CMpd ptr)

-- Irregular arithmetics

foreign import ccall unsafe "mpd_divmod" c'divmod
  :: Mpd
  -> Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_fma" c'mpd_fma
  :: Mpd
  -> CMpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_powmod" c'mpd_powmod
  :: Mpd
  -> CMpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_adjexp" c'mpd_adjexp
  :: CMpd
  -> IO Signed

-- Output to string

foreign import ccall unsafe "mpd_to_sci" c'mpd_to_sci
  :: CMpd
  -> CInt
  -> IO (Ptr CChar)

-- | Set to 1 to capitalize the exponent character; otherwise, if it
-- is 0, the exponent character is lower case.
capitalize :: CInt
capitalize = 1

foreign import ccall unsafe "mpd_to_eng" c'mpd_to_eng
  :: CMpd
  -> CInt
  -> IO (Ptr CChar)

foreign import ccall unsafe "mpd_set_string" c'mpd_set_string
  :: Mpd
  -> Ptr CChar
  -> Ptr C'mpd_context_t
  -> IO ()

-- comparisons

-- compare_total is context free
foreign import ccall unsafe "mpd_compare_total" c'mpd_compare_total
  :: Mpd
  -> CMpd
  -> CMpd
  -> IO CInt

foreign import ccall unsafe "mpd_cmp_total" c'mpd_cmp_total
  :: CMpd
  -> CMpd
  -> IO CInt

-- total_mag is context free
foreign import ccall unsafe "mpd_compare_total_mag" c'mpd_compare_total_mag
  :: Mpd
  -> CMpd
  -> CMpd
  -> IO CInt

foreign import ccall unsafe "mpd_cmp_total_mag" c'mpd_cmp_total_mag
  :: CMpd
  -> CMpd
  -> IO CInt

foreign import ccall unsafe "mpd_same_quantum" c'mpd_same_quantum
  :: CMpd
  -> CMpd
  -> IO CInt

-- Tests

foreign import ccall unsafe "mpd_class" c'mpd_class
  :: CMpd
  -> Ptr C'mpd_context_t
  -> IO (Ptr CChar)

foreign import ccall unsafe "mpd_isnormal" c'mpd_isnormal
  :: CMpd
  -> Ptr C'mpd_context_t
  -> IO CInt

foreign import ccall unsafe "mpd_issubnormal" c'mpd_issubnormal
  :: CMpd
  -> Ptr C'mpd_context_t
  -> IO CInt

foreign import ccall unsafe "mpd_sign" c'mpd_sign
  :: CMpd
  -> IO Word8

foreign import ccall unsafe "mpd_arith_sign" c'mpd_arith_sign
  :: CMpd
  -> IO CInt

foreign import ccall unsafe "mpd_trail_zeros" c'mpd_trail_zeros
  :: CMpd
  -> IO Signed

-- Memory handling
foreign import ccall unsafe "mpd_qnew" c'mpd_qnew
  :: IO (Mpd)

newDec :: (Mpd -> IO ()) -> IO Dec
newDec f = do
  p <- c'mpd_qnew
  when (unMpd p == nullPtr) $ error "newMpd: failure"
  fp <- newForeignPtr fp'mpd_del (unMpd p)
  withForeignPtr fp $ \x1 ->
    f (Mpd x1)
  return $ Dec fp

newDec2 :: (Mpd -> Mpd -> IO ()) -> IO (Dec, Dec)
newDec2 f = do
  p1 <- c'mpd_qnew
  when (unMpd p1 == nullPtr) $ error "newMpd: failure"
  p2 <- c'mpd_qnew
  when (unMpd p2 == nullPtr) $ error "newMpd: failure"
  fp1 <- newForeignPtr fp'mpd_del (unMpd p1)
  fp2 <- newForeignPtr fp'mpd_del (unMpd p2)
  withForeignPtr fp1 $ \x1 ->
    withForeignPtr fp2 $ \x2 ->
    f (Mpd x1) (Mpd x2)
  return (Dec fp1, Dec fp2)


foreign import ccall unsafe "mpd_del" c'mpd_del
  :: Mpd
  -> IO ()

foreign import ccall unsafe "&mpd_del" fp'mpd_del
  :: FunPtr (Ptr C'mpd_t -> IO ())

-- Imported from mkmpd

foreign import ccall unsafe "mpd_copy" c'mpd_copy
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_copy_abs" c'mpd_copy_abs
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_copy_negate" c'mpd_copy_negate
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_invert" c'mpd_invert
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_logb" c'mpd_logb
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_abs" c'mpd_abs
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_exp" c'mpd_exp
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_ln" c'mpd_ln
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_log10" c'mpd_log10
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_minus" c'mpd_minus
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_next_minus" c'mpd_next_minus
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_next_plus" c'mpd_next_plus
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_plus" c'mpd_plus
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_reduce" c'mpd_reduce
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_round_to_intx" c'mpd_round_to_intx
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_round_to_int" c'mpd_round_to_int
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_trunc" c'mpd_trunc
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_floor" c'mpd_floor
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_ceil" c'mpd_ceil
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_sqrt" c'mpd_sqrt
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_invroot" c'mpd_invroot
  :: Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()


foreign import ccall unsafe "mpd_and" c'mpd_and
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_copy_sign" c'mpd_copy_sign
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_or" c'mpd_or
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_rotate" c'mpd_rotate
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_scaleb" c'mpd_scaleb
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_shift" c'mpd_shift
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_xor" c'mpd_xor
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_compare" c'mpd_compare
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_compare_signal" c'mpd_compare_signal
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_add" c'mpd_add
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_sub" c'mpd_sub
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_div" c'mpd_div
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_divint" c'mpd_divint
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_max" c'mpd_max
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_max_mag" c'mpd_max_mag
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_min" c'mpd_min
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_min_mag" c'mpd_min_mag
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_mul" c'mpd_mul
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_next_toward" c'mpd_next_toward
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_pow" c'mpd_pow
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_quantize" c'mpd_quantize
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_rescale" c'mpd_rescale
  :: Mpd
  -> CMpd
  -> Signed
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_rem" c'mpd_rem
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_rem_near" c'mpd_rem_near
  :: Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

foreign import ccall unsafe "mpd_isfinite" c'mpd_isfinite
  :: CMpd
  -> IO CInt

foreign import ccall unsafe "mpd_isinfinite" c'mpd_isinfinite
  :: CMpd
  -> IO CInt

foreign import ccall unsafe "mpd_isinteger" c'mpd_isinteger
  :: CMpd
  -> IO CInt

foreign import ccall unsafe "mpd_isnan" c'mpd_isnan
  :: CMpd
  -> IO CInt

foreign import ccall unsafe "mpd_isnegative" c'mpd_isnegative
  :: CMpd
  -> IO CInt

foreign import ccall unsafe "mpd_ispositive" c'mpd_ispositive
  :: CMpd
  -> IO CInt

foreign import ccall unsafe "mpd_isqnan" c'mpd_isqnan
  :: CMpd
  -> IO CInt

foreign import ccall unsafe "mpd_issnan" c'mpd_issnan
  :: CMpd
  -> IO CInt

foreign import ccall unsafe "mpd_issigned" c'mpd_issigned
  :: CMpd
  -> IO CInt

foreign import ccall unsafe "mpd_isspecial" c'mpd_isspecial
  :: CMpd
  -> IO CInt

foreign import ccall unsafe "mpd_iszero" c'mpd_iszero
  :: CMpd
  -> IO CInt

foreign import ccall unsafe "mpd_iszerocoeff" c'mpd_iszerocoeff
  :: CMpd
  -> IO CInt

foreign import ccall unsafe "mpd_isoddcoeff" c'mpd_isoddcoeff
  :: CMpd
  -> IO CInt

foreign import ccall unsafe "mpd_isodd" c'mpd_isodd
  :: CMpd
  -> IO CInt

foreign import ccall unsafe "mpd_iseven" c'mpd_iseven
  :: CMpd
  -> IO CInt

-- Handlers


