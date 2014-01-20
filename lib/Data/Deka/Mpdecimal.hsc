{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <mpdecimal.h>
module Data.Deka.Mpdecimal where
import Foreign.Ptr
#strict_import

--  Version
#ccall mpd_version , IO CString

#num MPD_MAJOR_VERSION
#num MPD_MINOR_VERSION
#num MPD_MICRO_VERSION
#num NULL

#num MPD_VERSION

--  Sizes and integral types
#integral_t mpd_uint_t
#integral_t mpd_size_t
#integral_t mpd_ssize_t
#integral_t uint32_t
#integral_t size_t
#integral_t uint8_t
#integral_t uint16_t
#integral_t int32_t
#integral_t uint64_t
#integral_t int64_t
#integral_t uchar

--  Configuration

--  Modular and base arithmetic
#num MPD_UINT_MAX
#num MPD_BITS_PER_UINT
#num MPD_SIZE_MAX

--  Type for exp, digits, len, prec
#num MPD_SSIZE_MAX
#num MPD_SSIZE_MIN

--  Decimal arithmetic
#num MPD_RADIX
#num MPD_RDIGITS
#num MPD_MAX_POW10
#num MPD_EXPDIGITS

#num MPD_MAXTRANSFORM_2N
#num MPD_MAX_PREC
#num MPD_MAX_PREC_LOG2
#num MPD_ELIMIT
#num MPD_MAX_EMAX
#num MPD_MIN_EMIN
#num MPD_MIN_ETINY
#num MPD_EXP_INF
#num MPD_EXP_CLAMP
#num MPD_MAXIMPORT

--  Conversion specifiers
#num PRI_mpd_uint_t
#num PRI_mpd_ssize_t


--  Context

{- enum {
    MPD_ROUND_UP,
    MPD_ROUND_DOWN,
    MPD_ROUND_CEILING,
    MPD_ROUND_FLOOR,
    MPD_ROUND_HALF_UP,
    MPD_ROUND_HALF_DOWN,
    MPD_ROUND_HALF_EVEN,
    MPD_ROUND_05UP,
    MPD_ROUND_TRUNC,
    MPD_ROUND_GUARD
}; -}
#num MPD_ROUND_UP
#num MPD_ROUND_DOWN
#num MPD_ROUND_CEILING
#num MPD_ROUND_FLOOR
#num MPD_ROUND_HALF_UP
#num MPD_ROUND_HALF_DOWN
#num MPD_ROUND_HALF_EVEN
#num MPD_ROUND_05UP
#num MPD_ROUND_TRUNC
#num MPD_ROUND_GUARD
{- enum {
    MPD_CLAMP_DEFAULT, MPD_CLAMP_IEEE_754, MPD_CLAMP_GUARD
}; -}
#num MPD_CLAMP_DEFAULT
#num MPD_CLAMP_IEEE_754
#num MPD_CLAMP_GUARD

#globalvar mpd_round_string , CChar
#globalvar mpd_clamp_string , CChar

{- typedef struct mpd_context_t {
            mpd_ssize_t prec;
            mpd_ssize_t emax;
            mpd_ssize_t emin;
            uint32_t traps;
            uint32_t status;
            uint32_t newtrap;
            int round;
            int clamp;
            int allcr;
        } mpd_context_t; -}

#starttype mpd_context_t
#field prec , <mpd_ssize_t>
#field emax , <mpd_ssize_t>
#field emin , <mpd_ssize_t>
#field traps , <uint32_t>
#field status , <uint32_t>
#field newtrap , <uint32_t>
#field round , CInt
#field clamp , CInt
#field allcr , CInt
#stoptype

--  Status flags

#num MPD_Clamped
#num MPD_Conversion_syntax
#num MPD_Division_by_zero
#num MPD_Division_impossible
#num MPD_Division_undefined
#num MPD_Fpu_error
#num MPD_Inexact
#num MPD_Invalid_context
#num MPD_Invalid_operation
#num MPD_Malloc_error
#num MPD_Not_implemented
#num MPD_Overflow
#num MPD_Rounded
#num MPD_Subnormal
#num MPD_Underflow

--  Conditions that result in an IEEE 754 exception
#num MPD_IEEE_Invalid_operation

--  Errors that require the result of an operation to be set to NaN
#num MPD_Errors

--  Default traps
#num MPD_Traps

--  Official name
#num MPD_Insufficient_storage

--  IEEE 754 interchange format contexts
#num MPD_IEEE_CONTEXT_MAX_BITS
#num MPD_DECIMAL32
#num MPD_DECIMAL64
#num MPD_DECIMAL128

--  minalloc

#num MPD_MINALLOC_MIN
#num MPD_MINALLOC_MAX
#globalvar MPD_MINALLOC , CInt

--  Traphandler

#callback_t funptr_mpd_traphandler , Ptr <mpd_context_t> -> IO ()
#ccall mpd_dflt_traphandler , Ptr <mpd_context_t> -> IO ()
#globalvar mpd_traphandler , <funptr_mpd_traphandler>

--  Context functions
#ccall mpd_setminalloc , <mpd_ssize_t> -> IO ()
#ccall mpd_init , Ptr <mpd_context_t> -> <mpd_ssize_t> -> IO ()
#ccall mpd_maxcontext , Ptr <mpd_context_t> -> IO ()
#ccall mpd_defaultcontext , Ptr <mpd_context_t> -> IO ()
#ccall mpd_basiccontext , Ptr <mpd_context_t> -> IO ()
#ccall mpd_ieee_context , Ptr <mpd_context_t> -> CInt -> IO CInt
#ccall mpd_getprec , Ptr <mpd_context_t> -> IO <mpd_ssize_t>
#ccall mpd_getemax , Ptr <mpd_context_t> -> IO <mpd_ssize_t>
#ccall mpd_getemin , Ptr <mpd_context_t> -> IO <mpd_ssize_t>
#ccall mpd_getround , Ptr <mpd_context_t> -> IO CInt
#ccall mpd_gettraps , Ptr <mpd_context_t> -> IO <uint32_t>
#ccall mpd_getstatus , Ptr <mpd_context_t> -> IO <uint32_t>
#ccall mpd_getclamp , Ptr <mpd_context_t> -> IO CInt
#ccall mpd_getcr , Ptr <mpd_context_t> -> IO CInt
#ccall mpd_qsetprec , Ptr <mpd_context_t> -> <mpd_ssize_t> -> IO CInt
#ccall mpd_qsetemax , Ptr <mpd_context_t> -> <mpd_ssize_t> -> IO CInt
#ccall mpd_qsetemin , Ptr <mpd_context_t> -> <mpd_ssize_t> -> IO CInt
#ccall mpd_qsetround , Ptr <mpd_context_t> -> CInt -> IO CInt
#ccall mpd_qsettraps , Ptr <mpd_context_t> -> <uint32_t> -> IO CInt
#ccall mpd_qsetstatus , Ptr <mpd_context_t> -> <uint32_t> -> IO CInt
#ccall mpd_qsetclamp , Ptr <mpd_context_t> -> CInt -> IO CInt
#ccall mpd_qsetcr , Ptr <mpd_context_t> -> CInt -> IO CInt
#ccall mpd_addstatus_raise , Ptr <mpd_context_t> -> <uint32_t> -> IO ()

--  Decimal arithmetic

--  mpd_t flags
#num MPD_POS
#num MPD_NEG
#num MPD_INF
#num MPD_NAN
#num MPD_SNAN
#num MPD_SPECIAL
#num MPD_STATIC
#num MPD_STATIC_DATA
#num MPD_SHARED_DATA
#num MPD_CONST_DATA
#num MPD_DATAFLAGS


--  mpd_t

{- typedef struct mpd_t {
            uint8_t flags;
            mpd_ssize_t exp;
            mpd_ssize_t digits;
            mpd_ssize_t len;
            mpd_ssize_t alloc;
            mpd_uint_t * data;
        } mpd_t; -}
#starttype mpd_t
#field flags , <uint8_t>
#field exp , <mpd_ssize_t>
#field digits , <mpd_ssize_t>
#field len , <mpd_ssize_t>
#field alloc , <mpd_ssize_t>
#field data , Ptr <mpd_uint_t>
#stoptype
{- typedef unsigned char uchar; -}

--  Quiet, thread-safe functions

--  Format specification
{- typedef struct mpd_spec_t {
            mpd_ssize_t min_width;
            mpd_ssize_t prec;
            char type;
            char align;
            char sign;
            char fill[5];
            const char * dot;
            const char * sep;
            const char * grouping;
        } mpd_spec_t; -}
#starttype mpd_spec_t
#field min_width , <mpd_ssize_t>
#field prec , <mpd_ssize_t>
#field type , CChar
#field align , CChar
#field sign , CChar
#array_field fill , CChar
#field dot , CString
#field sep , CString
#field grouping , CString
#stoptype

--  Output to a string
#ccall mpd_to_sci , Ptr <mpd_t> -> CInt -> IO CString
#ccall mpd_to_eng , Ptr <mpd_t> -> CInt -> IO CString
#ccall mpd_to_sci_size , Ptr CString -> Ptr <mpd_t> -> CInt -> IO <mpd_ssize_t>
#ccall mpd_to_eng_size , Ptr CString -> Ptr <mpd_t> -> CInt -> IO <mpd_ssize_t>
#ccall mpd_qformat , Ptr <mpd_t> -> CString -> Ptr <mpd_context_t> -> Ptr <uint32_t> -> IO CString

#num MPD_NUM_FLAGS
#num MPD_MAX_FLAG_STRING
#num MPD_MAX_FLAG_LIST
#num MPD_MAX_SIGNAL_LIST


#ccall mpd_snprint_flags , CString -> CInt -> <uint32_t> -> IO CInt
#ccall mpd_lsnprint_flags , CString -> CInt -> <uint32_t> -> Ptr CString -> IO CInt
#ccall mpd_lsnprint_signals , CString -> CInt -> <uint32_t> -> Ptr CString -> IO CInt

--  Output to a file

#ccall mpd_fprint , Ptr CFile -> Ptr <mpd_t> -> IO ()
#ccall mpd_print , Ptr <mpd_t> -> IO ()


--  Assignment from a string

#ccall mpd_qset_string , Ptr <mpd_t> -> CString -> Ptr <mpd_context_t> -> Ptr <uint32_t> -> IO ()

--  Set to NaN with error flags

#ccall mpd_seterror , Ptr <mpd_t> -> <uint32_t> -> Ptr <uint32_t> -> IO ()

--  Set a special with sign and type

#ccall mpd_setspecial , Ptr <mpd_t> -> <uint8_t> -> <uint8_t> -> IO ()

--  Set a coefficient to zero or all nines

#ccall mpd_zerocoeff , Ptr <mpd_t> -> IO ()
#ccall mpd_qmaxcoeff , Ptr <mpd_t> -> Ptr <mpd_context_t> -> Ptr <uint32_t> -> IO ()

--  Quiet functions

#ccall mpd_same_quantum , Ptr <mpd_t> -> Ptr <mpd_t> -> IO CInt
#ccall mpd_qcmp , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <uint32_t> -> IO CInt
#ccall mpd_qcompare_signal , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> Ptr <uint32_t> -> IO CInt
#ccall mpd_cmp_total , Ptr <mpd_t> -> Ptr <mpd_t> -> IO CInt
#ccall mpd_cmp_total_mag , Ptr <mpd_t> -> Ptr <mpd_t> -> IO CInt
#ccall mpd_compare_total , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> IO CInt
#ccall mpd_compare_total_mag , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> IO CInt
#ccall mpd_qround_to_intx , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> Ptr <uint32_t> -> IO ()
#ccall mpd_qround_to_int , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> Ptr <uint32_t> -> IO ()

--  Signalling functions

#ccall mpd_format , Ptr <mpd_t> -> CString -> Ptr <mpd_context_t> -> IO CString
#ccall mpd_import_u16 , Ptr <mpd_t> -> Ptr CUShort -> <size_t> -> CUChar -> <uint32_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_export_u16 , Ptr (Ptr CUShort) -> <size_t> -> <uint32_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO <size_t>
#ccall mpd_finalize , Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_check_nan , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO CInt
#ccall mpd_check_nans , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO CInt
#ccall mpd_set_string , Ptr <mpd_t> -> CString -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_maxcoeff , Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_and , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_copy , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_canonical , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_copy_abs , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_copy_negate , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_copy_sign , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_invert , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_logb , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_or , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_rotate , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_scaleb , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_shiftl , Ptr <mpd_t> -> Ptr <mpd_t> -> <mpd_ssize_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_shiftr , Ptr <mpd_t> -> Ptr <mpd_t> -> <mpd_ssize_t> -> Ptr <mpd_context_t> -> IO <mpd_uint_t>
#ccall mpd_shiftn , Ptr <mpd_t> -> Ptr <mpd_t> -> <mpd_ssize_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_shift , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_xor , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_abs , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_cmp , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO CInt
#ccall mpd_compare , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO CInt
#ccall mpd_compare_signal , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO CInt
#ccall mpd_add , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_sub , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_div , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_divmod , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_divint , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_exp , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_fma , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_ln , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_log10 , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_max , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_max_mag , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_min , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_min_mag , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_minus , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_mul , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_next_minus , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_next_plus , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_next_toward , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_plus , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_pow , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_powmod , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_quantize , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_rescale , Ptr <mpd_t> -> Ptr <mpd_t> -> <mpd_ssize_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_reduce , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_rem , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_rem_near , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_round_to_intx , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_round_to_int , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_trunc , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_floor , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_ceil , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_sqrt , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()
#ccall mpd_invroot , Ptr <mpd_t> -> Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO ()

--  Get attributes of a decimal

#ccall mpd_adjexp , Ptr <mpd_t> -> IO CInt
#ccall mpd_etiny , Ptr <mpd_context_t> -> IO <mpd_ssize_t>
#ccall mpd_etop , Ptr <mpd_context_t> -> IO <mpd_ssize_t>
#ccall mpd_msword , Ptr <mpd_t> -> IO <mpd_uint_t>
#ccall mpd_word_digits , <mpd_uint_t> -> IO CInt

#ccall mpd_msd , <mpd_uint_t> -> IO <mpd_uint_t>

#ccall mpd_lsd , <mpd_uint_t> -> IO <mpd_uint_t>

#ccall mpd_digits_to_size , CInt -> IO CInt

#ccall mpd_exp_digits , <mpd_ssize_t> -> IO CInt
#ccall mpd_iscanonical , Ptr <mpd_t> -> IO CInt
#ccall mpd_isfinite , Ptr <mpd_t> -> IO CInt
#ccall mpd_isinfinite , Ptr <mpd_t> -> IO CInt
#ccall mpd_isinteger , Ptr <mpd_t> -> IO CInt
#ccall mpd_isnan , Ptr <mpd_t> -> IO CInt
#ccall mpd_isnegative , Ptr <mpd_t> -> IO CInt
#ccall mpd_ispositive , Ptr <mpd_t> -> IO CInt
#ccall mpd_isqnan , Ptr <mpd_t> -> IO CInt
#ccall mpd_issigned , Ptr <mpd_t> -> IO CInt
#ccall mpd_issnan , Ptr <mpd_t> -> IO CInt
#ccall mpd_isspecial , Ptr <mpd_t> -> IO CInt
#ccall mpd_iszero , Ptr <mpd_t> -> IO CInt

#ccall mpd_iszerocoeff , Ptr <mpd_t> -> IO CInt
#ccall mpd_isnormal , Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO CInt
#ccall mpd_issubnormal , Ptr <mpd_t> -> Ptr <mpd_context_t> -> IO CInt

#ccall mpd_isoddword , <mpd_uint_t> -> IO CInt

#ccall mpd_isoddcoeff , Ptr <mpd_t> -> IO CInt

#ccall mpd_isodd , Ptr <mpd_t> -> IO CInt

#ccall mpd_iseven , Ptr <mpd_t> -> IO CInt

#ccall mpd_sign , Ptr <mpd_t> -> IO CUChar

#ccall mpd_arith_sign , Ptr <mpd_t> -> IO CInt
#ccall mpd_radix , IO CLong
#ccall mpd_isdynamic , Ptr <mpd_t> -> IO CInt
#ccall mpd_isstatic , Ptr <mpd_t> -> IO CInt
#ccall mpd_isdynamic_data , Ptr <mpd_t> -> IO CInt
#ccall mpd_isstatic_data , Ptr <mpd_t> -> IO CInt
#ccall mpd_isshared_data , Ptr <mpd_t> -> IO CInt
#ccall mpd_isconst_data , Ptr <mpd_t> -> IO CInt
#ccall mpd_trail_zeros , Ptr <mpd_t> -> IO CInt

--  Set attributes of a decimal

#ccall mpd_setdigits , Ptr <mpd_t> -> IO ()
#ccall mpd_set_sign , Ptr <mpd_t> -> <uint8_t> -> IO ()

#ccall mpd_signcpy , Ptr <mpd_t> -> Ptr <mpd_t> -> IO ()
#ccall mpd_set_infinity , Ptr <mpd_t> -> IO ()
#ccall mpd_set_qnan , Ptr <mpd_t> -> IO ()
#ccall mpd_set_snan , Ptr <mpd_t> -> IO ()
#ccall mpd_set_negative , Ptr <mpd_t> -> IO ()
#ccall mpd_set_positive , Ptr <mpd_t> -> IO ()
#ccall mpd_set_dynamic , Ptr <mpd_t> -> IO ()
#ccall mpd_set_static , Ptr <mpd_t> -> IO ()
#ccall mpd_set_dynamic_data , Ptr <mpd_t> -> IO ()
#ccall mpd_set_static_data , Ptr <mpd_t> -> IO ()
#ccall mpd_set_shared_data , Ptr <mpd_t> -> IO ()
#ccall mpd_set_const_data , Ptr <mpd_t> -> IO ()
#ccall mpd_clear_flags , Ptr <mpd_t> -> IO ()
#ccall mpd_set_flags , Ptr <mpd_t> -> <uint8_t> -> IO ()
#ccall mpd_copy_flags , Ptr <mpd_t> -> Ptr <mpd_t> -> IO ()

-- Memory handling
-- I skipped many functions.

#ccall mpd_qnew , IO (Ptr <mpd_t>)
#ccall mpd_new , Ptr <mpd_context_t> -> IO (Ptr <mpd_t>)
#ccall mpd_del , Ptr <mpd_t> -> IO ()
