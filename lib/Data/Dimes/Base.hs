{-# LINE 1 "Base.hsc" #-}
module Data.Dimes.Base where
{-# LINE 2 "Base.hsc" #-}

import Foreign
import Foreign.Storable


{-# LINE 7 "Base.hsc" #-}


{-# LINE 9 "Base.hsc" #-}

data Mpd_context_t = Mpd_context_t
  { ctPrec :: Word64
{-# LINE 12 "Base.hsc" #-}
  , emax :: Word64
{-# LINE 13 "Base.hsc" #-}
  , emin :: Word64
{-# LINE 14 "Base.hsc" #-}
  , traps :: Word32
{-# LINE 15 "Base.hsc" #-}
  , status :: Word32
{-# LINE 16 "Base.hsc" #-}
  , newtrap :: Word32
{-# LINE 17 "Base.hsc" #-}
  , round :: Int32
{-# LINE 18 "Base.hsc" #-}
  , clamp :: Int32
{-# LINE 19 "Base.hsc" #-}
  , allcr :: Int32
{-# LINE 20 "Base.hsc" #-}
  }

instance Storable Mpd_context_t where
  alignment _ = 8
{-# LINE 24 "Base.hsc" #-}
  sizeOf _ = (48)
{-# LINE 25 "Base.hsc" #-}
  peek ptr = do
    p <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 27 "Base.hsc" #-}
    ex <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 28 "Base.hsc" #-}
    em <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 29 "Base.hsc" #-}
    t <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 30 "Base.hsc" #-}
    s <- (\hsc_ptr -> peekByteOff hsc_ptr 28) ptr
{-# LINE 31 "Base.hsc" #-}
    n <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 32 "Base.hsc" #-}
    r <- (\hsc_ptr -> peekByteOff hsc_ptr 36) ptr
{-# LINE 33 "Base.hsc" #-}
    c <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 34 "Base.hsc" #-}
    a <- (\hsc_ptr -> peekByteOff hsc_ptr 44) ptr
{-# LINE 35 "Base.hsc" #-}
    return $ Mpd_context_t p ex em t s n r c a

  poke ptr (Mpd_context_t p ex em t s n r c a) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr p
{-# LINE 39 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr ex
{-# LINE 40 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr em
{-# LINE 41 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr t
{-# LINE 42 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 28) ptr s
{-# LINE 43 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr n
{-# LINE 44 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 36) ptr r
{-# LINE 45 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 40) ptr c
{-# LINE 46 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 44) ptr a
{-# LINE 47 "Base.hsc" #-}

data Mpd_t = Mpd_t
  { flags :: Word8
{-# LINE 50 "Base.hsc" #-}
  , exp :: Int64
{-# LINE 51 "Base.hsc" #-}
  , digits :: Int64
{-# LINE 52 "Base.hsc" #-}
  , len :: Int64
{-# LINE 53 "Base.hsc" #-}
  , alloc :: Int64
{-# LINE 54 "Base.hsc" #-}
  , mpdData :: Ptr Word64
{-# LINE 55 "Base.hsc" #-}
  }

instance Storable Mpd_t where
  alignment _ = 8
{-# LINE 59 "Base.hsc" #-}
  sizeOf _ = (48)
{-# LINE 60 "Base.hsc" #-}
  peek ptr = do
    f <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 62 "Base.hsc" #-}
    e <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 63 "Base.hsc" #-}
    di <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 64 "Base.hsc" #-}
    l <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 65 "Base.hsc" #-}
    a <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 66 "Base.hsc" #-}
    dt <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 67 "Base.hsc" #-}
    return $ Mpd_t f e di l a dt

  poke ptr (Mpd_t f e di l a dt) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr f
{-# LINE 71 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr e
{-# LINE 72 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr di
{-# LINE 73 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr l
{-# LINE 74 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr a
{-# LINE 75 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 40) ptr dt
{-# LINE 76 "Base.hsc" #-}

data Mpd_spec_t = Mpd_spec_t
  { min_width :: Int64
{-# LINE 79 "Base.hsc" #-}
  , spPrec :: Int64
{-# LINE 80 "Base.hsc" #-}
  , spType :: Int8
{-# LINE 81 "Base.hsc" #-}
  , spAlign :: Int8
{-# LINE 82 "Base.hsc" #-}
  , spSign :: Int8
{-# LINE 83 "Base.hsc" #-}
  , spFill :: Ptr Int8
{-# LINE 84 "Base.hsc" #-}
  , spDot :: Ptr Int8
{-# LINE 85 "Base.hsc" #-}
  , spSep :: Ptr Int8
{-# LINE 86 "Base.hsc" #-}
  , spGrouping :: Ptr Int8
{-# LINE 87 "Base.hsc" #-}
  }

instance Storable Mpd_spec_t where
  alignment _ = 8
{-# LINE 91 "Base.hsc" #-}
  sizeOf _ = (48)
{-# LINE 92 "Base.hsc" #-}
  peek p = do
    wd <- (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 94 "Base.hsc" #-}
    pc <- (\hsc_ptr -> peekByteOff hsc_ptr 8) p
{-# LINE 95 "Base.hsc" #-}
    ty <- (\hsc_ptr -> peekByteOff hsc_ptr 16) p
{-# LINE 96 "Base.hsc" #-}
    al <- (\hsc_ptr -> peekByteOff hsc_ptr 17) p
{-# LINE 97 "Base.hsc" #-}
    si <- (\hsc_ptr -> peekByteOff hsc_ptr 18) p
{-# LINE 98 "Base.hsc" #-}
    fi <- (\hsc_ptr -> peekByteOff hsc_ptr 19) p
{-# LINE 99 "Base.hsc" #-}
    dt <- (\hsc_ptr -> peekByteOff hsc_ptr 24) p
{-# LINE 100 "Base.hsc" #-}
    sp <- (\hsc_ptr -> peekByteOff hsc_ptr 32) p
{-# LINE 101 "Base.hsc" #-}
    gr <- (\hsc_ptr -> peekByteOff hsc_ptr 40) p
{-# LINE 102 "Base.hsc" #-}
    return $ Mpd_spec_t wd pc ty al si fi dt sp gr

  poke p (Mpd_spec_t wd pc ty al si fi dt sp gr) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) p wd
{-# LINE 106 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 8) p pc
{-# LINE 107 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 16) p ty
{-# LINE 108 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 17) p al
{-# LINE 109 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 18) p si
{-# LINE 110 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 19) p fi
{-# LINE 111 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 24) p dt
{-# LINE 112 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 32) p sp
{-# LINE 113 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 40) p gr
{-# LINE 114 "Base.hsc" #-}


-- Line 1: /*
-- Line 2:  * Copyright (c) 2008-2016 Stefan Krah. All rights reserved.
-- Line 3:  *
-- Line 4:  * Redistribution and use in source and binary forms, with or without
-- Line 5:  * modification, are permitted provided that the following conditions
-- Line 6:  * are met:
-- Line 7:  *
-- Line 8:  * 1. Redistributions of source code must retain the above copyright
-- Line 9:  *    notice, this list of conditions and the following disclaimer.
-- Line 10:  *
-- Line 11:  * 2. Redistributions in binary form must reproduce the above copyright
-- Line 12:  *    notice, this list of conditions and the following disclaimer in the
-- Line 13:  *    documentation and/or other materials provided with the distribution.
-- Line 14:  *
-- Line 15:  * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS "AS IS" AND
-- Line 16:  * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- Line 17:  * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- Line 18:  * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
-- Line 19:  * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- Line 20:  * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- Line 21:  * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- Line 22:  * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- Line 23:  * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- Line 24:  * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- Line 25:  * SUCH DAMAGE.
-- Line 26:  */
-- Line 27: 
-- Line 28: 
-- Line 29: #ifndef MPDECIMAL_H
-- Line 30: #define MPDECIMAL_H
-- Line 31: 
-- Line 32: 
-- Line 33: #ifdef __cplusplus
-- Line 34: extern "C" {
-- Line 35:   #ifndef __STDC_LIMIT_MACROS
-- Line 36:     #define __STDC_LIMIT_MACROS
-- Line 37:     #define MPD_CLEAR_STDC_LIMIT_MACROS
-- Line 38:   #endif
-- Line 39: #endif
-- Line 40: 
-- Line 41: 
-- Line 42: #include <stdio.h>
-- Line 43: #include <stdlib.h>
-- Line 44: #include <string.h>
-- Line 45: #include <limits.h>
-- Line 46: #include <assert.h>
-- Line 47: #include <stdint.h>
-- Line 48: #include <inttypes.h>
-- Line 49: 
-- Line 50: 
-- Line 51: #ifndef __GNUC_STDC_INLINE__
-- Line 52:   #define __GNUC_STDC_INLINE__ 1
-- Line 53: #endif
-- Line 54: #if defined(__GNUC__) && !defined(__INTEL_COMPILER)
-- Line 55:   #define UNUSED __attribute__((unused))
-- Line 56: #else
-- Line 57:   #define UNUSED
-- Line 58: #endif
-- Line 59: #if (defined(__linux__) || defined(__FreeBSD__) || defined(__APPLE__)) && \
-- Line 60:     defined(__GNUC__) && __GNUC__ >= 4 && !defined(__INTEL_COMPILER)
-- Line 61:   #define MPD_PRAGMA(x) _Pragma(x)
-- Line 62:   #define MPD_HIDE_SYMBOLS_START "GCC visibility push(hidden)"
-- Line 63:   #define MPD_HIDE_SYMBOLS_END "GCC visibility pop"
-- Line 64: #else
-- Line 65:   #define MPD_PRAGMA(x)
-- Line 66:   #define MPD_HIDE_SYMBOLS_START
-- Line 67:   #define MPD_HIDE_SYMBOLS_END
-- Line 68: #endif
-- Line 69: 
-- Line 70: 
-- Line 71: #if !defined(LEGACY_COMPILER)
-- Line 72:   #if !defined(UINT64_MAX)
-- Line 73:     /* The following #error is just a warning. If the compiler indeed does
-- Line 74:      * not have uint64_t, it is perfectly safe to comment out the #error. */
-- Line 75:     #error "Warning: Compiler without uint64_t. Comment out this line."
-- Line 76:     #define LEGACY_COMPILER
-- Line 77:   #endif
-- Line 78: #endif
-- Line 79: 
-- Line 80: 
-- Line 81: /******************************************************************************/
-- Line 82: /*                                  Version                                   */
-- Line 83: /******************************************************************************/
-- Line 84: 
-- Line 85: #define MPD_MAJOR_VERSION 2
-- Line 86: #define MPD_MINOR_VERSION 4
-- Line 87: #define MPD_MICRO_VERSION 0
-- Line 88: 
-- Line 89: #define MPD_VERSION "2.4.0"
-- Line 90: 
-- Line 91: const char *mpd_version(void);
-- Line 92: 
-- Line 93: 
-- Line 94: /******************************************************************************/
-- Line 95: /*                              Configuration                                 */
-- Line 96: /******************************************************************************/
-- Line 97: 
-- Line 98: /* ABI: 64-bit */
-- Line 99: #ifdef CONFIG_32
-- Line 100:   #error "cannot use CONFIG_32 with 64-bit header."
-- Line 101: #endif
-- Line 102: 
-- Line 103: #ifndef CONFIG_64
-- Line 104:   #define CONFIG_64
-- Line 105: #endif
-- Line 106: 
-- Line 107: 
-- Line 108: /* BEGIN CONFIG_64 */
-- Line 109: #if defined(CONFIG_64)
-- Line 110: /* types for modular and base arithmetic */
-- Line 111: #define MPD_UINT_MAX UINT64_MAX
-- Line 112: #define MPD_BITS_PER_UINT 64
-- Line 113: typedef uint64_t mpd_uint_t;  /* unsigned mod type */
-- Line 114: 
-- Line 115: #define MPD_SIZE_MAX SIZE_MAX
-- Line 116: typedef size_t mpd_size_t; /* unsigned size type */
-- Line 117: 
-- Line 118: /* type for exp, digits, len, prec */
-- Line 119: #define MPD_SSIZE_MAX INT64_MAX
-- Line 120: #define MPD_SSIZE_MIN INT64_MIN
-- Line 121: typedef int64_t mpd_ssize_t;
-- Line 122: #define _mpd_strtossize strtoll
-- Line 123: 
-- Line 124: /* decimal arithmetic */
-- Line 125: #define MPD_RADIX 10000000000000000000ULL  /* 10**19 */
-- Line 126: #define MPD_RDIGITS 19
-- Line 127: #define MPD_MAX_POW10 19
-- Line 128: #define MPD_EXPDIGITS 19  /* MPD_EXPDIGITS <= MPD_RDIGITS+1 */
-- Line 129: 
-- Line 130: #define MPD_MAXTRANSFORM_2N 4294967296ULL      /* 2**32 */
-- Line 131: #define MPD_MAX_PREC 999999999999999999LL
-- Line 132: #define MPD_MAX_PREC_LOG2 64
-- Line 133: #define MPD_ELIMIT  1000000000000000000LL
-- Line 134: #define MPD_MAX_EMAX   999999999999999999LL    /* ELIMIT-1 */
-- Line 135: #define MPD_MIN_EMIN  (-999999999999999999LL)  /* -EMAX */
-- Line 136: #define MPD_MIN_ETINY (MPD_MIN_EMIN-(MPD_MAX_PREC-1))
-- Line 137: #define MPD_EXP_INF 2000000000000000001LL
-- Line 138: #define MPD_EXP_CLAMP (-4000000000000000001LL)
-- Line 139: #define MPD_MAXIMPORT 105263157894736842L /* ceil((2*MPD_MAX_PREC)/MPD_RDIGITS) */
-- Line 140: 
-- Line 141: /* conversion specifiers */
-- Line 142: #define PRI_mpd_uint_t PRIu64
-- Line 143: #define PRI_mpd_ssize_t PRIi64
-- Line 144: /* END CONFIG_64 */
-- Line 145: 
-- Line 146: 
-- Line 147: /* BEGIN CONFIG_32 */
-- Line 148: #elif defined(CONFIG_32)
-- Line 149: /* types for modular and base arithmetic */
-- Line 150: #define MPD_UINT_MAX UINT32_MAX
-- Line 151: #define MPD_BITS_PER_UINT 32
-- Line 152: typedef uint32_t mpd_uint_t;  /* unsigned mod type */
-- Line 153: 
-- Line 154: #ifndef LEGACY_COMPILER
-- Line 155: #define MPD_UUINT_MAX UINT64_MAX
-- Line 156: typedef uint64_t mpd_uuint_t; /* double width unsigned mod type */
-- Line 157: #endif
-- Line 158: 
-- Line 159: #define MPD_SIZE_MAX SIZE_MAX
-- Line 160: typedef size_t mpd_size_t; /* unsigned size type */
-- Line 161: 
-- Line 162: /* type for dec->len, dec->exp, ctx->prec */
-- Line 163: #define MPD_SSIZE_MAX INT32_MAX
-- Line 164: #define MPD_SSIZE_MIN INT32_MIN
-- Line 165: typedef int32_t mpd_ssize_t;
-- Line 166: #define _mpd_strtossize strtol
-- Line 167: 
-- Line 168: /* decimal arithmetic */
-- Line 169: #define MPD_RADIX 1000000000UL  /* 10**9 */
-- Line 170: #define MPD_RDIGITS 9
-- Line 171: #define MPD_MAX_POW10 9
-- Line 172: #define MPD_EXPDIGITS 10 /* MPD_EXPDIGITS <= MPD_RDIGITS+1 */
-- Line 173: 
-- Line 174: #define MPD_MAXTRANSFORM_2N 33554432UL /* 2**25 */
-- Line 175: #define MPD_MAX_PREC 425000000L
-- Line 176: #define MPD_MAX_PREC_LOG2 32
-- Line 177: #define MPD_ELIMIT 425000001L
-- Line 178: #define MPD_MAX_EMAX 425000000L        /* ELIMIT-1 */
-- Line 179: #define MPD_MIN_EMIN (-425000000L)     /* -EMAX */
-- Line 180: #define MPD_MIN_ETINY (MPD_MIN_EMIN-(MPD_MAX_PREC-1))
-- Line 181: #define MPD_EXP_INF 1000000001L      /* allows for emax=999999999 in the tests */
-- Line 182: #define MPD_EXP_CLAMP (-2000000001L) /* allows for emin=-999999999 in the tests */
-- Line 183: #define MPD_MAXIMPORT 94444445L      /* ceil((2*MPD_MAX_PREC)/MPD_RDIGITS) */
-- Line 184: 
-- Line 185: /* conversion specifiers */
-- Line 186: #define PRI_mpd_uint_t PRIu32
-- Line 187: #define PRI_mpd_ssize_t PRIi32
-- Line 188: /* END CONFIG_32 */
-- Line 189: 
-- Line 190: #else
-- Line 191:   #error "define CONFIG_64 or CONFIG_32"
-- Line 192: #endif
-- Line 193: /* END CONFIG */
-- Line 194: 
-- Line 195: 
-- Line 196: #if MPD_SIZE_MAX != MPD_UINT_MAX
-- Line 197:   #error "unsupported platform: need mpd_size_t == mpd_uint_t"
-- Line 198: #endif
-- Line 199: 
-- Line 200: 
-- Line 201: /******************************************************************************/
-- Line 202: /*                                Context                                     */
-- Line 203: /******************************************************************************/
-- Line 204: 
-- Line 205: enum {
-- Line 206:     MPD_ROUND_UP,          /* round away from 0               */
-- Line 207:     MPD_ROUND_DOWN,        /* round toward 0 (truncate)       */
-- Line 208:     MPD_ROUND_CEILING,     /* round toward +infinity          */
-- Line 209:     MPD_ROUND_FLOOR,       /* round toward -infinity          */
-- Line 210:     MPD_ROUND_HALF_UP,     /* 0.5 is rounded up               */
-- Line 211:     MPD_ROUND_HALF_DOWN,   /* 0.5 is rounded down             */
-- Line 212:     MPD_ROUND_HALF_EVEN,   /* 0.5 is rounded to even          */
-- Line 213:     MPD_ROUND_05UP,        /* round zero or five away from 0  */
-- Line 214:     MPD_ROUND_TRUNC,       /* truncate, but set infinity      */
-- Line 215:     MPD_ROUND_GUARD
-- Line 216: };
-- Line 217: 
-- Line 218: enum { MPD_CLAMP_DEFAULT, MPD_CLAMP_IEEE_754, MPD_CLAMP_GUARD };
-- Line 219: 
-- Line 220: extern const char *mpd_round_string[MPD_ROUND_GUARD];
-- Line 221: extern const char *mpd_clamp_string[MPD_CLAMP_GUARD];
-- Line 222: 
-- Line 223: 
-- Line 224: typedef struct mpd_context_t {
-- Line 225:     mpd_ssize_t prec;   /* precision */
-- Line 226:     mpd_ssize_t emax;   /* max positive exp */
-- Line 227:     mpd_ssize_t emin;   /* min negative exp */
-- Line 228:     uint32_t traps;     /* status events that should be trapped */
-- Line 229:     uint32_t status;    /* status flags */
-- Line 230:     uint32_t newtrap;   /* set by mpd_addstatus_raise() */
-- Line 231:     int      round;     /* rounding mode */
-- Line 232:     int      clamp;     /* clamp mode */
-- Line 233:     int      allcr;     /* all functions correctly rounded */
-- Line 234: } mpd_context_t;
-- Line 235: 
-- Line 236: 
-- Line 237: /* Status flags */
-- Line 238: #define MPD_Clamped             0x00000001U
-- Line 239: #define MPD_Conversion_syntax   0x00000002U
-- Line 240: #define MPD_Division_by_zero    0x00000004U
-- Line 241: #define MPD_Division_impossible 0x00000008U
-- Line 242: #define MPD_Division_undefined  0x00000010U
-- Line 243: #define MPD_Fpu_error           0x00000020U
-- Line 244: #define MPD_Inexact             0x00000040U
-- Line 245: #define MPD_Invalid_context     0x00000080U
-- Line 246: #define MPD_Invalid_operation   0x00000100U
-- Line 247: #define MPD_Malloc_error        0x00000200U
-- Line 248: #define MPD_Not_implemented     0x00000400U
-- Line 249: #define MPD_Overflow            0x00000800U
-- Line 250: #define MPD_Rounded             0x00001000U
-- Line 251: #define MPD_Subnormal           0x00002000U
-- Line 252: #define MPD_Underflow           0x00004000U
-- Line 253: #define MPD_Max_status         (0x00008000U-1U)
-- Line 254: 
-- Line 255: /* Conditions that result in an IEEE 754 exception */
-- Line 256: #define MPD_IEEE_Invalid_operation (MPD_Conversion_syntax |   \
-- Line 257:                                     MPD_Division_impossible | \
-- Line 258:                                     MPD_Division_undefined |  \
-- Line 259:                                     MPD_Fpu_error |           \
-- Line 260:                                     MPD_Invalid_context |     \
-- Line 261:                                     MPD_Invalid_operation |   \
-- Line 262:                                     MPD_Malloc_error)         \
-- Line 263: 
-- Line 264: /* Errors that require the result of an operation to be set to NaN */
-- Line 265: #define MPD_Errors (MPD_IEEE_Invalid_operation | \
-- Line 266:                     MPD_Division_by_zero)
-- Line 267: 
-- Line 268: /* Default traps */
-- Line 269: #define MPD_Traps (MPD_IEEE_Invalid_operation | \
-- Line 270:                    MPD_Division_by_zero |       \
-- Line 271:                    MPD_Overflow |               \
-- Line 272:                    MPD_Underflow)
-- Line 273: 
-- Line 274: /* Official name */
-- Line 275: #define MPD_Insufficient_storage MPD_Malloc_error
-- Line 276: 
-- Line 277: /* IEEE 754 interchange format contexts */
-- Line 278: #define MPD_IEEE_CONTEXT_MAX_BITS 512 /* 16*(log2(MPD_MAX_EMAX / 3)-3) */
-- Line 279: #define MPD_DECIMAL32 32
-- Line 280: #define MPD_DECIMAL64 64
-- Line 281: #define MPD_DECIMAL128 128
-- Line 282: 
-- Line 283: 
-- Line 284: #define MPD_MINALLOC_MIN 2
-- Line 285: #define MPD_MINALLOC_MAX 64
-- Line 286: extern mpd_ssize_t MPD_MINALLOC;
-- Line 287: extern void (* mpd_traphandler)(mpd_context_t *);
-- Line 288
foreign import ccall mpd_dflt_traphandler
  :: Ptr Mpd_context_t
  -> IO ()


-- Line 289: 
-- Line 290
foreign import ccall mpd_setminalloc
  :: Int64
{-# LINE 413 "Base.hsc" #-}
  -- ^ n
  -> IO ()


-- Line 291
foreign import ccall mpd_init
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> Int64
{-# LINE 422 "Base.hsc" #-}
  -- ^ prec
  -> IO ()


-- Line 292: 
-- Line 293
foreign import ccall mpd_maxcontext
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 294
foreign import ccall mpd_defaultcontext
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 295
foreign import ccall mpd_basiccontext
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 296
foreign import ccall mpd_ieee_context
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> Int32
{-# LINE 453 "Base.hsc" #-}
  -- ^ bits
  -> IO (Int32)
{-# LINE 455 "Base.hsc" #-}


-- Line 297: 
-- Line 298
foreign import ccall mpd_getprec
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int64)
{-# LINE 463 "Base.hsc" #-}


-- Line 299
foreign import ccall mpd_getemax
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int64)
{-# LINE 470 "Base.hsc" #-}


-- Line 300
foreign import ccall mpd_getemin
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int64)
{-# LINE 477 "Base.hsc" #-}


-- Line 301
foreign import ccall mpd_getround
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int32)
{-# LINE 484 "Base.hsc" #-}


-- Line 302
foreign import ccall mpd_gettraps
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Word32)
{-# LINE 491 "Base.hsc" #-}


-- Line 303
foreign import ccall mpd_getstatus
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Word32)
{-# LINE 498 "Base.hsc" #-}


-- Line 304
foreign import ccall mpd_getclamp
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int32)
{-# LINE 505 "Base.hsc" #-}


-- Line 305
foreign import ccall mpd_getcr
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int32)
{-# LINE 512 "Base.hsc" #-}


-- Line 306: 
-- Line 307
foreign import ccall mpd_qsetprec
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> Int64
{-# LINE 520 "Base.hsc" #-}
  -- ^ prec
  -> IO (Int32)
{-# LINE 522 "Base.hsc" #-}


-- Line 308
foreign import ccall mpd_qsetemax
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> Int64
{-# LINE 529 "Base.hsc" #-}
  -- ^ emax
  -> IO (Int32)
{-# LINE 531 "Base.hsc" #-}


-- Line 309
foreign import ccall mpd_qsetemin
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> Int64
{-# LINE 538 "Base.hsc" #-}
  -- ^ emin
  -> IO (Int32)
{-# LINE 540 "Base.hsc" #-}


-- Line 310
foreign import ccall mpd_qsetround
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> Int32
{-# LINE 547 "Base.hsc" #-}
  -- ^ newround
  -> IO (Int32)
{-# LINE 549 "Base.hsc" #-}


-- Line 311
foreign import ccall mpd_qsettraps
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> Word32
{-# LINE 556 "Base.hsc" #-}
  -- ^ flags
  -> IO (Int32)
{-# LINE 558 "Base.hsc" #-}


-- Line 312
foreign import ccall mpd_qsetstatus
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> Word32
{-# LINE 565 "Base.hsc" #-}
  -- ^ flags
  -> IO (Int32)
{-# LINE 567 "Base.hsc" #-}


-- Line 313
foreign import ccall mpd_qsetclamp
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> Int32
{-# LINE 574 "Base.hsc" #-}
  -- ^ c
  -> IO (Int32)
{-# LINE 576 "Base.hsc" #-}


-- Line 314
foreign import ccall mpd_qsetcr
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> Int32
{-# LINE 583 "Base.hsc" #-}
  -- ^ c
  -> IO (Int32)
{-# LINE 585 "Base.hsc" #-}


-- Line 315
foreign import ccall mpd_addstatus_raise
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> Word32
{-# LINE 592 "Base.hsc" #-}
  -- ^ flags
  -> IO ()


-- Line 316: 
-- Line 317: 
-- Line 318: /******************************************************************************/
-- Line 319: /*                           Decimal Arithmetic                               */
-- Line 320: /******************************************************************************/
-- Line 321: 
-- Line 322: /* mpd_t flags */
-- Line 323: #define MPD_POS                 ((uint8_t)0)
-- Line 324: #define MPD_NEG                 ((uint8_t)1)
-- Line 325: #define MPD_INF                 ((uint8_t)2)
-- Line 326: #define MPD_NAN                 ((uint8_t)4)
-- Line 327: #define MPD_SNAN                ((uint8_t)8)
-- Line 328: #define MPD_SPECIAL (MPD_INF|MPD_NAN|MPD_SNAN)
-- Line 329: #define MPD_STATIC              ((uint8_t)16)
-- Line 330: #define MPD_STATIC_DATA         ((uint8_t)32)
-- Line 331: #define MPD_SHARED_DATA         ((uint8_t)64)
-- Line 332: #define MPD_CONST_DATA          ((uint8_t)128)
-- Line 333: #define MPD_DATAFLAGS (MPD_STATIC_DATA|MPD_SHARED_DATA|MPD_CONST_DATA)
-- Line 334: 
-- Line 335: /* mpd_t */
-- Line 336: typedef struct mpd_t {
-- Line 337:     uint8_t flags;
-- Line 338:     mpd_ssize_t exp;
-- Line 339:     mpd_ssize_t digits;
-- Line 340:     mpd_ssize_t len;
-- Line 341:     mpd_ssize_t alloc;
-- Line 342:     mpd_uint_t *data;
-- Line 343: } mpd_t;
-- Line 344: 
-- Line 345: 
-- Line 346: typedef unsigned char uchar;
-- Line 347: 
-- Line 348: 
-- Line 349: /******************************************************************************/
-- Line 350: /*                       Quiet, thread-safe functions                         */
-- Line 351: /******************************************************************************/
-- Line 352: 
-- Line 353: /* format specification */
-- Line 354: typedef struct mpd_spec_t {
-- Line 355:     mpd_ssize_t min_width; /* minimum field width */
-- Line 356:     mpd_ssize_t prec;      /* fraction digits or significant digits */
-- Line 357:     char type;             /* conversion specifier */
-- Line 358:     char align;            /* alignment */
-- Line 359:     char sign;             /* sign printing/alignment */
-- Line 360:     char fill[5];          /* fill character */
-- Line 361:     const char *dot;       /* decimal point */
-- Line 362:     const char *sep;       /* thousands separator */
-- Line 363:     const char *grouping;  /* grouping of digits */
-- Line 364: } mpd_spec_t;
-- Line 365: 
-- Line 366: /* output to a string */
-- Line 367
foreign import ccall mpd_to_sci
  :: Ptr Mpd_t
  -- ^ dec
  -> Int32
{-# LINE 652 "Base.hsc" #-}
  -- ^ fmt
  -> IO (Int8)
{-# LINE 654 "Base.hsc" #-}


-- Line 368
foreign import ccall mpd_to_eng
  :: Ptr Mpd_t
  -- ^ dec
  -> Int32
{-# LINE 661 "Base.hsc" #-}
  -- ^ fmt
  -> IO (Int8)
{-# LINE 663 "Base.hsc" #-}


-- Line 369
foreign import ccall mpd_to_sci_size
  :: Ptr (Ptr Int8)
{-# LINE 668 "Base.hsc" #-}
  -- ^ res
  -> Ptr Mpd_t
  -- ^ dec
  -> Int32
{-# LINE 672 "Base.hsc" #-}
  -- ^ fmt
  -> IO (Int64)
{-# LINE 674 "Base.hsc" #-}


-- Line 370
foreign import ccall mpd_to_eng_size
  :: Ptr (Ptr Int8)
{-# LINE 679 "Base.hsc" #-}
  -- ^ res
  -> Ptr Mpd_t
  -- ^ dec
  -> Int32
{-# LINE 683 "Base.hsc" #-}
  -- ^ fmt
  -> IO (Int64)
{-# LINE 685 "Base.hsc" #-}


-- Line 371
foreign import ccall mpd_validate_lconv
  :: Ptr Mpd_spec_t
  -- ^ spec
  -> IO (Int32)
{-# LINE 692 "Base.hsc" #-}


-- Line 372
foreign import ccall mpd_parse_fmt_str
  :: Ptr Mpd_spec_t
  -- ^ spec
  -> Ptr Int8
{-# LINE 699 "Base.hsc" #-}
  -- ^ fmt
  -> Int32
{-# LINE 701 "Base.hsc" #-}
  -- ^ caps
  -> IO (Int32)
{-# LINE 703 "Base.hsc" #-}


-- Line 373
foreign import ccall mpd_qformat_spec
  :: Ptr Mpd_t
  -- ^ dec
  -> Ptr Mpd_spec_t
  -- ^ spec
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 714 "Base.hsc" #-}
  -- ^ status
  -> IO (Int8)
{-# LINE 716 "Base.hsc" #-}


-- Line 374
foreign import ccall mpd_qformat
  :: Ptr Mpd_t
  -- ^ dec
  -> Ptr Int8
{-# LINE 723 "Base.hsc" #-}
  -- ^ fmt
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 727 "Base.hsc" #-}
  -- ^ status
  -> IO (Int8)
{-# LINE 729 "Base.hsc" #-}


-- Line 375: 
-- Line 376: #define MPD_NUM_FLAGS 15
-- Line 377: #define MPD_MAX_FLAG_STRING 208
-- Line 378: #define MPD_MAX_FLAG_LIST (MPD_MAX_FLAG_STRING+18)
-- Line 379: #define MPD_MAX_SIGNAL_LIST 121
-- Line 380
foreign import ccall mpd_snprint_flags
  :: Ptr Int8
{-# LINE 739 "Base.hsc" #-}
  -- ^ dest
  -> Int32
{-# LINE 741 "Base.hsc" #-}
  -- ^ nmemb
  -> Word32
{-# LINE 743 "Base.hsc" #-}
  -- ^ flags
  -> IO (Int32)
{-# LINE 745 "Base.hsc" #-}


-- Line 381: int mpd_lsnprint_flags(char *dest, int nmemb, uint32_t flags, const char *flag_string[]);
-- Line 382: int mpd_lsnprint_signals(char *dest, int nmemb, uint32_t flags, const char *signal_string[]);
-- Line 383: 
-- Line 384: /* output to a file */
-- Line 385
{-
foreign import ccall mpd_fprint
  :: Ptr #{type FILE}
  -- ^ file
  -> Ptr Mpd_t
  -- ^ dec
  -> IO ()
-}


-- Line 386
foreign import ccall mpd_print
  :: Ptr Mpd_t
  -- ^ dec
  -> IO ()


-- Line 387: 
-- Line 388: /* assignment from a string */
-- Line 389
foreign import ccall mpd_qset_string
  :: Ptr Mpd_t
  -- ^ dec
  -> Ptr Int8
{-# LINE 776 "Base.hsc" #-}
  -- ^ s
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 780 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 390: 
-- Line 391: /* set to NaN with error flags */
-- Line 392
foreign import ccall mpd_seterror
  :: Ptr Mpd_t
  -- ^ result
  -> Word32
{-# LINE 791 "Base.hsc" #-}
  -- ^ flags
  -> Ptr Word32
{-# LINE 793 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 393: /* set a special with sign and type */
-- Line 394
foreign import ccall mpd_setspecial
  :: Ptr Mpd_t
  -- ^ dec
  -> Word8
{-# LINE 803 "Base.hsc" #-}
  -- ^ sign
  -> Word8
{-# LINE 805 "Base.hsc" #-}
  -- ^ type
  -> IO ()


-- Line 395: /* set coefficient to zero or all nines */
-- Line 396
foreign import ccall mpd_zerocoeff
  :: Ptr Mpd_t
  -- ^ result
  -> IO ()


-- Line 397
foreign import ccall mpd_qmaxcoeff
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 824 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 398: 
-- Line 399: /* quietly assign a C integer type to an mpd_t */
-- Line 400
foreign import ccall mpd_qset_ssize
  :: Ptr Mpd_t
  -- ^ result
  -> Int64
{-# LINE 835 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 839 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 401
foreign import ccall mpd_qset_i32
  :: Ptr Mpd_t
  -- ^ result
  -> Int32
{-# LINE 848 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 852 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 402
foreign import ccall mpd_qset_uint
  :: Ptr Mpd_t
  -- ^ result
  -> Word64
{-# LINE 861 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 865 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 403
foreign import ccall mpd_qset_u32
  :: Ptr Mpd_t
  -- ^ result
  -> Word32
{-# LINE 874 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 878 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 404: #ifndef LEGACY_COMPILER
-- Line 405
foreign import ccall mpd_qset_i64
  :: Ptr Mpd_t
  -- ^ result
  -> Int64
{-# LINE 888 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 892 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 406
foreign import ccall mpd_qset_u64
  :: Ptr Mpd_t
  -- ^ result
  -> Word64
{-# LINE 901 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 905 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 407: #endif
-- Line 408: 
-- Line 409: /* quietly assign a C integer type to an mpd_t with a static coefficient */
-- Line 410
foreign import ccall mpd_qsset_ssize
  :: Ptr Mpd_t
  -- ^ result
  -> Int64
{-# LINE 917 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 921 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 411
foreign import ccall mpd_qsset_i32
  :: Ptr Mpd_t
  -- ^ result
  -> Int32
{-# LINE 930 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 934 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 412
foreign import ccall mpd_qsset_uint
  :: Ptr Mpd_t
  -- ^ result
  -> Word64
{-# LINE 943 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 947 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 413
foreign import ccall mpd_qsset_u32
  :: Ptr Mpd_t
  -- ^ result
  -> Word32
{-# LINE 956 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 960 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 414: 
-- Line 415: /* quietly get a C integer type from an mpd_t */
-- Line 416
foreign import ccall mpd_qget_ssize
  :: Ptr Mpd_t
  -- ^ dec
  -> Ptr Word32
{-# LINE 971 "Base.hsc" #-}
  -- ^ status
  -> IO (Int64)
{-# LINE 973 "Base.hsc" #-}


-- Line 417
foreign import ccall mpd_qget_uint
  :: Ptr Mpd_t
  -- ^ dec
  -> Ptr Word32
{-# LINE 980 "Base.hsc" #-}
  -- ^ status
  -> IO (Word64)
{-# LINE 982 "Base.hsc" #-}


-- Line 418
foreign import ccall mpd_qabs_uint
  :: Ptr Mpd_t
  -- ^ dec
  -> Ptr Word32
{-# LINE 989 "Base.hsc" #-}
  -- ^ status
  -> IO (Word64)
{-# LINE 991 "Base.hsc" #-}


-- Line 419: 
-- Line 420
foreign import ccall mpd_qget_i32
  :: Ptr Mpd_t
  -- ^ dec
  -> Ptr Word32
{-# LINE 999 "Base.hsc" #-}
  -- ^ status
  -> IO (Int32)
{-# LINE 1001 "Base.hsc" #-}


-- Line 421
foreign import ccall mpd_qget_u32
  :: Ptr Mpd_t
  -- ^ dec
  -> Ptr Word32
{-# LINE 1008 "Base.hsc" #-}
  -- ^ status
  -> IO (Word32)
{-# LINE 1010 "Base.hsc" #-}


-- Line 422: #ifndef LEGACY_COMPILER
-- Line 423
foreign import ccall mpd_qget_i64
  :: Ptr Mpd_t
  -- ^ dec
  -> Ptr Word32
{-# LINE 1018 "Base.hsc" #-}
  -- ^ status
  -> IO (Int64)
{-# LINE 1020 "Base.hsc" #-}


-- Line 424
foreign import ccall mpd_qget_u64
  :: Ptr Mpd_t
  -- ^ dec
  -> Ptr Word32
{-# LINE 1027 "Base.hsc" #-}
  -- ^ status
  -> IO (Word64)
{-# LINE 1029 "Base.hsc" #-}


-- Line 425: #endif
-- Line 426: 
-- Line 427: /* quiet functions */
-- Line 428
foreign import ccall mpd_qcheck_nan
  :: Ptr Mpd_t
  -- ^ nanresult
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1043 "Base.hsc" #-}
  -- ^ status
  -> IO (Int32)
{-# LINE 1045 "Base.hsc" #-}


-- Line 429
foreign import ccall mpd_qcheck_nans
  :: Ptr Mpd_t
  -- ^ nanresult
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1058 "Base.hsc" #-}
  -- ^ status
  -> IO (Int32)
{-# LINE 1060 "Base.hsc" #-}


-- Line 430
foreign import ccall mpd_qfinalize
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1069 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 431: 
-- Line 432
foreign import ccall mpd_class
  :: Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int8)
{-# LINE 1081 "Base.hsc" #-}


-- Line 433: 
-- Line 434: int mpd_qcopy(mpd_t *result, const mpd_t *a,  uint32_t *status);
-- Line 435
foreign import ccall mpd_qncopy
  :: Ptr Mpd_t
  -- ^ a
  -> IO (Ptr Mpd_t)


-- Line 436
foreign import ccall mpd_qcopy_abs
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Word32
{-# LINE 1099 "Base.hsc" #-}
  -- ^ status
  -> IO (Int32)
{-# LINE 1101 "Base.hsc" #-}


-- Line 437
foreign import ccall mpd_qcopy_negate
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Word32
{-# LINE 1110 "Base.hsc" #-}
  -- ^ status
  -> IO (Int32)
{-# LINE 1112 "Base.hsc" #-}


-- Line 438
foreign import ccall mpd_qcopy_sign
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Word32
{-# LINE 1123 "Base.hsc" #-}
  -- ^ status
  -> IO (Int32)
{-# LINE 1125 "Base.hsc" #-}


-- Line 439: 
-- Line 440
foreign import ccall mpd_qand
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1139 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 441
foreign import ccall mpd_qinvert
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1152 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 442
foreign import ccall mpd_qlogb
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1165 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 443
foreign import ccall mpd_qor
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1180 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 444
foreign import ccall mpd_qscaleb
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1195 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 445
foreign import ccall mpd_qxor
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1210 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 446
foreign import ccall mpd_same_quantum
  :: Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> IO (Int32)
{-# LINE 1221 "Base.hsc" #-}


-- Line 447: 
-- Line 448
foreign import ccall mpd_qrotate
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1235 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 449
foreign import ccall mpd_qshiftl
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 1246 "Base.hsc" #-}
  -- ^ n
  -> Ptr Word32
{-# LINE 1248 "Base.hsc" #-}
  -- ^ status
  -> IO (Int32)
{-# LINE 1250 "Base.hsc" #-}


-- Line 450
foreign import ccall mpd_qshiftr
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 1259 "Base.hsc" #-}
  -- ^ n
  -> Ptr Word32
{-# LINE 1261 "Base.hsc" #-}
  -- ^ status
  -> IO (Word64)
{-# LINE 1263 "Base.hsc" #-}


-- Line 451
foreign import ccall mpd_qshiftr_inplace
  :: Ptr Mpd_t
  -- ^ result
  -> Int64
{-# LINE 1270 "Base.hsc" #-}
  -- ^ n
  -> IO (Word64)
{-# LINE 1272 "Base.hsc" #-}


-- Line 452
foreign import ccall mpd_qshift
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1285 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 453
foreign import ccall mpd_qshiftn
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 1296 "Base.hsc" #-}
  -- ^ n
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1300 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 454: 
-- Line 455
foreign import ccall mpd_qcmp
  :: Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Word32
{-# LINE 1312 "Base.hsc" #-}
  -- ^ status
  -> IO (Int32)
{-# LINE 1314 "Base.hsc" #-}


-- Line 456
foreign import ccall mpd_qcompare
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1327 "Base.hsc" #-}
  -- ^ status
  -> IO (Int32)
{-# LINE 1329 "Base.hsc" #-}


-- Line 457
foreign import ccall mpd_qcompare_signal
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1342 "Base.hsc" #-}
  -- ^ status
  -> IO (Int32)
{-# LINE 1344 "Base.hsc" #-}


-- Line 458
foreign import ccall mpd_cmp_total
  :: Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> IO (Int32)
{-# LINE 1353 "Base.hsc" #-}


-- Line 459
foreign import ccall mpd_cmp_total_mag
  :: Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> IO (Int32)
{-# LINE 1362 "Base.hsc" #-}


-- Line 460
foreign import ccall mpd_compare_total
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> IO (Int32)
{-# LINE 1373 "Base.hsc" #-}


-- Line 461
foreign import ccall mpd_compare_total_mag
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> IO (Int32)
{-# LINE 1384 "Base.hsc" #-}


-- Line 462: 
-- Line 463
foreign import ccall mpd_qround_to_intx
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1396 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 464
foreign import ccall mpd_qround_to_int
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1409 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 465
foreign import ccall mpd_qtrunc
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1422 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 466
foreign import ccall mpd_qfloor
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1435 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 467
foreign import ccall mpd_qceil
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1448 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 468: 
-- Line 469
foreign import ccall mpd_qabs
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1462 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 470
foreign import ccall mpd_qmax
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1477 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 471
foreign import ccall mpd_qmax_mag
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1492 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 472
foreign import ccall mpd_qmin
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1507 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 473
foreign import ccall mpd_qmin_mag
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1522 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 474
foreign import ccall mpd_qminus
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1535 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 475
foreign import ccall mpd_qplus
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1548 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 476
foreign import ccall mpd_qnext_minus
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1561 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 477
foreign import ccall mpd_qnext_plus
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1574 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 478
foreign import ccall mpd_qnext_toward
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1589 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 479
foreign import ccall mpd_qquantize
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1604 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 480
foreign import ccall mpd_qrescale
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 1615 "Base.hsc" #-}
  -- ^ exp
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1619 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 481
foreign import ccall mpd_qrescale_fmt
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 1630 "Base.hsc" #-}
  -- ^ exp
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1634 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 482
foreign import ccall mpd_qreduce
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1647 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 483
foreign import ccall mpd_qadd
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1662 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 484
foreign import ccall mpd_qadd_ssize
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 1673 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1677 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 485
foreign import ccall mpd_qadd_i32
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int32
{-# LINE 1688 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1692 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 486
foreign import ccall mpd_qadd_uint
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word64
{-# LINE 1703 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1707 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 487
foreign import ccall mpd_qadd_u32
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word32
{-# LINE 1718 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1722 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 488
foreign import ccall mpd_qsub
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1737 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 489
foreign import ccall mpd_qsub_ssize
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 1748 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1752 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 490
foreign import ccall mpd_qsub_i32
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int32
{-# LINE 1763 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1767 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 491
foreign import ccall mpd_qsub_uint
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word64
{-# LINE 1778 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1782 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 492
foreign import ccall mpd_qsub_u32
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word32
{-# LINE 1793 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1797 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 493
foreign import ccall mpd_qmul
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1812 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 494
foreign import ccall mpd_qmul_ssize
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 1823 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1827 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 495
foreign import ccall mpd_qmul_i32
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int32
{-# LINE 1838 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1842 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 496
foreign import ccall mpd_qmul_uint
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word64
{-# LINE 1853 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1857 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 497
foreign import ccall mpd_qmul_u32
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word32
{-# LINE 1868 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1872 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 498
foreign import ccall mpd_qfma
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_t
  -- ^ c
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1889 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 499
foreign import ccall mpd_qdiv
  :: Ptr Mpd_t
  -- ^ q
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1904 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 500
foreign import ccall mpd_qdiv_ssize
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 1915 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1919 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 501
foreign import ccall mpd_qdiv_i32
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int32
{-# LINE 1930 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1934 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 502
foreign import ccall mpd_qdiv_uint
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word64
{-# LINE 1945 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1949 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 503
foreign import ccall mpd_qdiv_u32
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word32
{-# LINE 1960 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1964 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 504
foreign import ccall mpd_qdivint
  :: Ptr Mpd_t
  -- ^ q
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1979 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 505
foreign import ccall mpd_qrem
  :: Ptr Mpd_t
  -- ^ r
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 1994 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 506
foreign import ccall mpd_qrem_near
  :: Ptr Mpd_t
  -- ^ r
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 2009 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 507
foreign import ccall mpd_qdivmod
  :: Ptr Mpd_t
  -- ^ q
  -> Ptr Mpd_t
  -- ^ r
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 2026 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 508
foreign import ccall mpd_qpow
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ base
  -> Ptr Mpd_t
  -- ^ exp
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 2041 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 509
foreign import ccall mpd_qpowmod
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ base
  -> Ptr Mpd_t
  -- ^ exp
  -> Ptr Mpd_t
  -- ^ mod
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 2058 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 510
foreign import ccall mpd_qexp
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 2071 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 511
foreign import ccall mpd_qln10
  :: Ptr Mpd_t
  -- ^ result
  -> Int64
{-# LINE 2080 "Base.hsc" #-}
  -- ^ prec
  -> Ptr Word32
{-# LINE 2082 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 512
foreign import ccall mpd_qln
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 2095 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 513
foreign import ccall mpd_qlog10
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 2108 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 514
foreign import ccall mpd_qsqrt
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 2121 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 515
foreign import ccall mpd_qinvroot
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 2134 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 516: 
-- Line 517: #ifndef LEGACY_COMPILER
-- Line 518
foreign import ccall mpd_qadd_i64
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 2147 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 2151 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 519
foreign import ccall mpd_qadd_u64
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word64
{-# LINE 2162 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 2166 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 520
foreign import ccall mpd_qsub_i64
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 2177 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 2181 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 521
foreign import ccall mpd_qsub_u64
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word64
{-# LINE 2192 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 2196 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 522
foreign import ccall mpd_qmul_i64
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 2207 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 2211 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 523
foreign import ccall mpd_qmul_u64
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word64
{-# LINE 2222 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 2226 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 524
foreign import ccall mpd_qdiv_i64
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 2237 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 2241 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 525
foreign import ccall mpd_qdiv_u64
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word64
{-# LINE 2252 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 2256 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 526: #endif
-- Line 527: 
-- Line 528: 
-- Line 529
foreign import ccall mpd_sizeinbase
  :: Ptr Mpd_t
  -- ^ a
  -> Word32
{-# LINE 2268 "Base.hsc" #-}
  -- ^ base
  -> IO (Word64)
{-# LINE 2270 "Base.hsc" #-}


-- Line 530: void mpd_qimport_u16(mpd_t *result, const uint16_t *srcdata, size_t srclen,
-- Line 531:                      uint8_t srcsign, uint32_t srcbase,
-- Line 532:                      const mpd_context_t *ctx, uint32_t *status);
-- Line 533: void mpd_qimport_u32(mpd_t *result, const uint32_t *srcdata, size_t srclen,
-- Line 534:                      uint8_t srcsign, uint32_t srcbase,
-- Line 535:                      const mpd_context_t *ctx, uint32_t *status);
-- Line 536: size_t mpd_qexport_u16(uint16_t **rdata, size_t rlen, uint32_t base,
-- Line 537:                        const mpd_t *src, uint32_t *status);
-- Line 538: size_t mpd_qexport_u32(uint32_t **rdata, size_t rlen, uint32_t base,
-- Line 539:                        const mpd_t *src, uint32_t *status);
-- Line 540: 
-- Line 541: 
-- Line 542: /******************************************************************************/
-- Line 543: /*                           Signalling functions                             */
-- Line 544: /******************************************************************************/
-- Line 545: 
-- Line 546
foreign import ccall mpd_format
  :: Ptr Mpd_t
  -- ^ dec
  -> Ptr Int8
{-# LINE 2293 "Base.hsc" #-}
  -- ^ fmt
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int8)
{-# LINE 2297 "Base.hsc" #-}


-- Line 547
foreign import ccall mpd_import_u16
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Word16
{-# LINE 2304 "Base.hsc" #-}
  -- ^ srcdata
  -> Word64
{-# LINE 2306 "Base.hsc" #-}
  -- ^ srclen
  -> Word8
{-# LINE 2308 "Base.hsc" #-}
  -- ^ srcsign
  -> Word32
{-# LINE 2310 "Base.hsc" #-}
  -- ^ base
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 548
foreign import ccall mpd_import_u32
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Word32
{-# LINE 2321 "Base.hsc" #-}
  -- ^ srcdata
  -> Word64
{-# LINE 2323 "Base.hsc" #-}
  -- ^ srclen
  -> Word8
{-# LINE 2325 "Base.hsc" #-}
  -- ^ srcsign
  -> Word32
{-# LINE 2327 "Base.hsc" #-}
  -- ^ base
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 549
foreign import ccall mpd_export_u16
  :: Ptr (Ptr Word16)
{-# LINE 2336 "Base.hsc" #-}
  -- ^ rdata
  -> Word64
{-# LINE 2338 "Base.hsc" #-}
  -- ^ rlen
  -> Word32
{-# LINE 2340 "Base.hsc" #-}
  -- ^ base
  -> Ptr Mpd_t
  -- ^ src
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Word64)
{-# LINE 2346 "Base.hsc" #-}


-- Line 550
foreign import ccall mpd_export_u32
  :: Ptr (Ptr Word32)
{-# LINE 2351 "Base.hsc" #-}
  -- ^ rdata
  -> Word64
{-# LINE 2353 "Base.hsc" #-}
  -- ^ rlen
  -> Word32
{-# LINE 2355 "Base.hsc" #-}
  -- ^ base
  -> Ptr Mpd_t
  -- ^ src
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Word64)
{-# LINE 2361 "Base.hsc" #-}


-- Line 551
foreign import ccall mpd_finalize
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 552
foreign import ccall mpd_check_nan
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int32)
{-# LINE 2381 "Base.hsc" #-}


-- Line 553
foreign import ccall mpd_check_nans
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int32)
{-# LINE 2394 "Base.hsc" #-}


-- Line 554
foreign import ccall mpd_set_string
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Int8
{-# LINE 2401 "Base.hsc" #-}
  -- ^ s
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 555
foreign import ccall mpd_maxcoeff
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 556
foreign import ccall mpd_sset_ssize
  :: Ptr Mpd_t
  -- ^ result
  -> Int64
{-# LINE 2421 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 557
foreign import ccall mpd_sset_i32
  :: Ptr Mpd_t
  -- ^ result
  -> Int32
{-# LINE 2432 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 558
foreign import ccall mpd_sset_uint
  :: Ptr Mpd_t
  -- ^ result
  -> Word64
{-# LINE 2443 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 559
foreign import ccall mpd_sset_u32
  :: Ptr Mpd_t
  -- ^ result
  -> Word32
{-# LINE 2454 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 560
foreign import ccall mpd_set_ssize
  :: Ptr Mpd_t
  -- ^ result
  -> Int64
{-# LINE 2465 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 561
foreign import ccall mpd_set_i32
  :: Ptr Mpd_t
  -- ^ result
  -> Int32
{-# LINE 2476 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 562
foreign import ccall mpd_set_uint
  :: Ptr Mpd_t
  -- ^ result
  -> Word64
{-# LINE 2487 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 563
foreign import ccall mpd_set_u32
  :: Ptr Mpd_t
  -- ^ result
  -> Word32
{-# LINE 2498 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 564: #ifndef LEGACY_COMPILER
-- Line 565
foreign import ccall mpd_set_i64
  :: Ptr Mpd_t
  -- ^ result
  -> Int64
{-# LINE 2510 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 566
foreign import ccall mpd_set_u64
  :: Ptr Mpd_t
  -- ^ result
  -> Word64
{-# LINE 2521 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 567: #endif
-- Line 568
foreign import ccall mpd_get_ssize
  :: Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int64)
{-# LINE 2535 "Base.hsc" #-}


-- Line 569
foreign import ccall mpd_get_uint
  :: Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Word64)
{-# LINE 2544 "Base.hsc" #-}


-- Line 570
foreign import ccall mpd_abs_uint
  :: Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Word64)
{-# LINE 2553 "Base.hsc" #-}


-- Line 571
foreign import ccall mpd_get_i32
  :: Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int32)
{-# LINE 2562 "Base.hsc" #-}


-- Line 572
foreign import ccall mpd_get_u32
  :: Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Word32)
{-# LINE 2571 "Base.hsc" #-}


-- Line 573: #ifndef LEGACY_COMPILER
-- Line 574
foreign import ccall mpd_get_i64
  :: Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int64)
{-# LINE 2581 "Base.hsc" #-}


-- Line 575
foreign import ccall mpd_get_u64
  :: Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Word64)
{-# LINE 2590 "Base.hsc" #-}


-- Line 576: #endif
-- Line 577
foreign import ccall mpd_and
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 578
foreign import ccall mpd_copy
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 579
foreign import ccall mpd_canonical
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 580
foreign import ccall mpd_copy_abs
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 581
foreign import ccall mpd_copy_negate
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 582
foreign import ccall mpd_copy_sign
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 583
foreign import ccall mpd_invert
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 584
foreign import ccall mpd_logb
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 585
foreign import ccall mpd_or
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 586
foreign import ccall mpd_rotate
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 587
foreign import ccall mpd_scaleb
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 588
foreign import ccall mpd_shiftl
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 2731 "Base.hsc" #-}
  -- ^ n
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 589
foreign import ccall mpd_shiftr
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 2744 "Base.hsc" #-}
  -- ^ n
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Word64)
{-# LINE 2748 "Base.hsc" #-}


-- Line 590
foreign import ccall mpd_shiftn
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 2757 "Base.hsc" #-}
  -- ^ n
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 591
foreign import ccall mpd_shift
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 592
foreign import ccall mpd_xor
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 593
foreign import ccall mpd_abs
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 594
foreign import ccall mpd_cmp
  :: Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int32)
{-# LINE 2809 "Base.hsc" #-}


-- Line 595
foreign import ccall mpd_compare
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int32)
{-# LINE 2822 "Base.hsc" #-}


-- Line 596
foreign import ccall mpd_compare_signal
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int32)
{-# LINE 2835 "Base.hsc" #-}


-- Line 597
foreign import ccall mpd_add
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 598
foreign import ccall mpd_add_ssize
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 2857 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 599
foreign import ccall mpd_add_i32
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int32
{-# LINE 2870 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 600
foreign import ccall mpd_add_uint
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word64
{-# LINE 2883 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 601
foreign import ccall mpd_add_u32
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word32
{-# LINE 2896 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 602
foreign import ccall mpd_sub
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 603
foreign import ccall mpd_sub_ssize
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 2922 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 604
foreign import ccall mpd_sub_i32
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int32
{-# LINE 2935 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 605
foreign import ccall mpd_sub_uint
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word64
{-# LINE 2948 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 606
foreign import ccall mpd_sub_u32
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word32
{-# LINE 2961 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 607
foreign import ccall mpd_div
  :: Ptr Mpd_t
  -- ^ q
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 608
foreign import ccall mpd_div_ssize
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 2987 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 609
foreign import ccall mpd_div_i32
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int32
{-# LINE 3000 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 610
foreign import ccall mpd_div_uint
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word64
{-# LINE 3013 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 611
foreign import ccall mpd_div_u32
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word32
{-# LINE 3026 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 612
foreign import ccall mpd_divmod
  :: Ptr Mpd_t
  -- ^ q
  -> Ptr Mpd_t
  -- ^ r
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 613
foreign import ccall mpd_divint
  :: Ptr Mpd_t
  -- ^ q
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 614
foreign import ccall mpd_exp
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 615
foreign import ccall mpd_fma
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_t
  -- ^ c
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 616
foreign import ccall mpd_ln
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 617
foreign import ccall mpd_log10
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 618
foreign import ccall mpd_max
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 619
foreign import ccall mpd_max_mag
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 620
foreign import ccall mpd_min
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 621
foreign import ccall mpd_min_mag
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 622
foreign import ccall mpd_minus
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 623
foreign import ccall mpd_mul
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 624
foreign import ccall mpd_mul_ssize
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 3191 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 625
foreign import ccall mpd_mul_i32
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int32
{-# LINE 3204 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 626
foreign import ccall mpd_mul_uint
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word64
{-# LINE 3217 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 627
foreign import ccall mpd_mul_u32
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word32
{-# LINE 3230 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 628
foreign import ccall mpd_next_minus
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 629
foreign import ccall mpd_next_plus
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 630
foreign import ccall mpd_next_toward
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 631
foreign import ccall mpd_plus
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 632
foreign import ccall mpd_pow
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ base
  -> Ptr Mpd_t
  -- ^ exp
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 633
foreign import ccall mpd_powmod
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ base
  -> Ptr Mpd_t
  -- ^ exp
  -> Ptr Mpd_t
  -- ^ mod
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 634
foreign import ccall mpd_quantize
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 635
foreign import ccall mpd_rescale
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 3330 "Base.hsc" #-}
  -- ^ exp
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 636
foreign import ccall mpd_reduce
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 637
foreign import ccall mpd_rem
  :: Ptr Mpd_t
  -- ^ r
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 638
foreign import ccall mpd_rem_near
  :: Ptr Mpd_t
  -- ^ r
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_t
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 639
foreign import ccall mpd_round_to_intx
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 640
foreign import ccall mpd_round_to_int
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 641
foreign import ccall mpd_trunc
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 642
foreign import ccall mpd_floor
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 643
foreign import ccall mpd_ceil
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 644
foreign import ccall mpd_sqrt
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 645
foreign import ccall mpd_invroot
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 646: 
-- Line 647: #ifndef LEGACY_COMPILER
-- Line 648
foreign import ccall mpd_add_i64
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 3459 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 649
foreign import ccall mpd_add_u64
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word64
{-# LINE 3472 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 650
foreign import ccall mpd_sub_i64
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 3485 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 651
foreign import ccall mpd_sub_u64
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word64
{-# LINE 3498 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 652
foreign import ccall mpd_div_i64
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 3511 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 653
foreign import ccall mpd_div_u64
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word64
{-# LINE 3524 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 654
foreign import ccall mpd_mul_i64
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Int64
{-# LINE 3537 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 655
foreign import ccall mpd_mul_u64
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> Word64
{-# LINE 3550 "Base.hsc" #-}
  -- ^ b
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 656: #endif
-- Line 657: 
-- Line 658: 
-- Line 659: /******************************************************************************/
-- Line 660: /*                          Configuration specific                            */
-- Line 661: /******************************************************************************/
-- Line 662: 
-- Line 663: #ifdef CONFIG_64
-- Line 664
foreign import ccall mpd_qsset_i64
  :: Ptr Mpd_t
  -- ^ result
  -> Int64
{-# LINE 3569 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 3573 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 665
foreign import ccall mpd_qsset_u64
  :: Ptr Mpd_t
  -- ^ result
  -> Word64
{-# LINE 3582 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> Ptr Word32
{-# LINE 3586 "Base.hsc" #-}
  -- ^ status
  -> IO ()


-- Line 666
foreign import ccall mpd_sset_i64
  :: Ptr Mpd_t
  -- ^ result
  -> Int64
{-# LINE 3595 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 667
foreign import ccall mpd_sset_u64
  :: Ptr Mpd_t
  -- ^ result
  -> Word64
{-# LINE 3606 "Base.hsc" #-}
  -- ^ a
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO ()


-- Line 668: #endif
-- Line 669: 
-- Line 670: 
-- Line 671: /******************************************************************************/
-- Line 672: /*                       Get attributes of a decimal                          */
-- Line 673: /******************************************************************************/
-- Line 674: 
-- Line 675
foreign import ccall mpd_adjexp
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int64)
{-# LINE 3624 "Base.hsc" #-}


-- Line 676
foreign import ccall mpd_etiny
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int64)
{-# LINE 3631 "Base.hsc" #-}


-- Line 677
foreign import ccall mpd_etop
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int64)
{-# LINE 3638 "Base.hsc" #-}


-- Line 678
foreign import ccall mpd_msword
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Word64)
{-# LINE 3645 "Base.hsc" #-}


-- Line 679
foreign import ccall mpd_word_digits
  :: Word64
{-# LINE 3650 "Base.hsc" #-}
  -- ^ word
  -> IO (Int32)
{-# LINE 3652 "Base.hsc" #-}


-- Line 680: /* most significant digit of a word */
-- Line 681
foreign import ccall mpd_msd
  :: Word64
{-# LINE 3658 "Base.hsc" #-}
  -- ^ word
  -> IO (Word64)
{-# LINE 3660 "Base.hsc" #-}


-- Line 682: /* least significant digit of a word */
-- Line 683
foreign import ccall mpd_lsd
  :: Word64
{-# LINE 3666 "Base.hsc" #-}
  -- ^ word
  -> IO (Word64)
{-# LINE 3668 "Base.hsc" #-}


-- Line 684: /* coefficient size needed to store 'digits' */
-- Line 685
foreign import ccall mpd_digits_to_size
  :: Int64
{-# LINE 3674 "Base.hsc" #-}
  -- ^ digits
  -> IO (Int64)
{-# LINE 3676 "Base.hsc" #-}


-- Line 686: /* number of digits in the exponent, undefined for MPD_SSIZE_MIN */
-- Line 687
foreign import ccall mpd_exp_digits
  :: Int64
{-# LINE 3682 "Base.hsc" #-}
  -- ^ exp
  -> IO (Int32)
{-# LINE 3684 "Base.hsc" #-}


-- Line 688: int mpd_iscanonical(const mpd_t *dec UNUSED);
-- Line 689
foreign import ccall mpd_isfinite
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3692 "Base.hsc" #-}


-- Line 690
foreign import ccall mpd_isinfinite
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3699 "Base.hsc" #-}


-- Line 691
foreign import ccall mpd_isinteger
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3706 "Base.hsc" #-}


-- Line 692
foreign import ccall mpd_isnan
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3713 "Base.hsc" #-}


-- Line 693
foreign import ccall mpd_isnegative
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3720 "Base.hsc" #-}


-- Line 694
foreign import ccall mpd_ispositive
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3727 "Base.hsc" #-}


-- Line 695
foreign import ccall mpd_isqnan
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3734 "Base.hsc" #-}


-- Line 696
foreign import ccall mpd_issigned
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3741 "Base.hsc" #-}


-- Line 697
foreign import ccall mpd_issnan
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3748 "Base.hsc" #-}


-- Line 698
foreign import ccall mpd_isspecial
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3755 "Base.hsc" #-}


-- Line 699
foreign import ccall mpd_iszero
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3762 "Base.hsc" #-}


-- Line 700: /* undefined for special numbers */
-- Line 701
foreign import ccall mpd_iszerocoeff
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3770 "Base.hsc" #-}


-- Line 702
foreign import ccall mpd_isnormal
  :: Ptr Mpd_t
  -- ^ dec
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int32)
{-# LINE 3779 "Base.hsc" #-}


-- Line 703
foreign import ccall mpd_issubnormal
  :: Ptr Mpd_t
  -- ^ dec
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int32)
{-# LINE 3788 "Base.hsc" #-}


-- Line 704: /* odd word */
-- Line 705
foreign import ccall mpd_isoddword
  :: Word64
{-# LINE 3794 "Base.hsc" #-}
  -- ^ word
  -> IO (Int32)
{-# LINE 3796 "Base.hsc" #-}


-- Line 706: /* odd coefficient */
-- Line 707
foreign import ccall mpd_isoddcoeff
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3804 "Base.hsc" #-}


-- Line 708: /* odd decimal, only defined for integers */
-- Line 709
foreign import ccall mpd_isodd
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3812 "Base.hsc" #-}


-- Line 710: /* even decimal, only defined for integers */
-- Line 711
foreign import ccall mpd_iseven
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3820 "Base.hsc" #-}


-- Line 712: /* 0 if dec is positive, 1 if dec is negative */
-- Line 713
foreign import ccall mpd_sign
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Word8)
{-# LINE 3828 "Base.hsc" #-}


-- Line 714: /* 1 if dec is positive, -1 if dec is negative */
-- Line 715
foreign import ccall mpd_arith_sign
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3836 "Base.hsc" #-}


-- Line 716: long mpd_radix(void);
-- Line 717
foreign import ccall mpd_isdynamic
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3844 "Base.hsc" #-}


-- Line 718
foreign import ccall mpd_isstatic
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3851 "Base.hsc" #-}


-- Line 719
foreign import ccall mpd_isdynamic_data
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3858 "Base.hsc" #-}


-- Line 720
foreign import ccall mpd_isstatic_data
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3865 "Base.hsc" #-}


-- Line 721
foreign import ccall mpd_isshared_data
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3872 "Base.hsc" #-}


-- Line 722
foreign import ccall mpd_isconst_data
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int32)
{-# LINE 3879 "Base.hsc" #-}


-- Line 723
foreign import ccall mpd_trail_zeros
  :: Ptr Mpd_t
  -- ^ dec
  -> IO (Int64)
{-# LINE 3886 "Base.hsc" #-}


-- Line 724: 
-- Line 725: 
-- Line 726: /******************************************************************************/
-- Line 727: /*                       Set attributes of a decimal                          */
-- Line 728: /******************************************************************************/
-- Line 729: 
-- Line 730: /* set number of decimal digits in the coefficient */
-- Line 731
foreign import ccall mpd_setdigits
  :: Ptr Mpd_t
  -- ^ result
  -> IO ()


-- Line 732
foreign import ccall mpd_set_sign
  :: Ptr Mpd_t
  -- ^ result
  -> Word8
{-# LINE 3907 "Base.hsc" #-}
  -- ^ sign
  -> IO ()


-- Line 733: /* copy sign from another decimal */
-- Line 734
foreign import ccall mpd_signcpy
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> IO ()


-- Line 735
foreign import ccall mpd_set_infinity
  :: Ptr Mpd_t
  -- ^ result
  -> IO ()


-- Line 736
foreign import ccall mpd_set_qnan
  :: Ptr Mpd_t
  -- ^ result
  -> IO ()


-- Line 737
foreign import ccall mpd_set_snan
  :: Ptr Mpd_t
  -- ^ result
  -> IO ()


-- Line 738
foreign import ccall mpd_set_negative
  :: Ptr Mpd_t
  -- ^ result
  -> IO ()


-- Line 739
foreign import ccall mpd_set_positive
  :: Ptr Mpd_t
  -- ^ result
  -> IO ()


-- Line 740
foreign import ccall mpd_set_dynamic
  :: Ptr Mpd_t
  -- ^ result
  -> IO ()


-- Line 741
foreign import ccall mpd_set_static
  :: Ptr Mpd_t
  -- ^ result
  -> IO ()


-- Line 742
foreign import ccall mpd_set_dynamic_data
  :: Ptr Mpd_t
  -- ^ result
  -> IO ()


-- Line 743
foreign import ccall mpd_set_static_data
  :: Ptr Mpd_t
  -- ^ result
  -> IO ()


-- Line 744
foreign import ccall mpd_set_shared_data
  :: Ptr Mpd_t
  -- ^ result
  -> IO ()


-- Line 745
foreign import ccall mpd_set_const_data
  :: Ptr Mpd_t
  -- ^ result
  -> IO ()


-- Line 746
foreign import ccall mpd_clear_flags
  :: Ptr Mpd_t
  -- ^ result
  -> IO ()


-- Line 747
foreign import ccall mpd_set_flags
  :: Ptr Mpd_t
  -- ^ result
  -> Word8
{-# LINE 4010 "Base.hsc" #-}
  -- ^ flags
  -> IO ()


-- Line 748
foreign import ccall mpd_copy_flags
  :: Ptr Mpd_t
  -- ^ result
  -> Ptr Mpd_t
  -- ^ a
  -> IO ()


-- Line 749: 
-- Line 750: 
-- Line 751: /******************************************************************************/
-- Line 752: /*                              Error Macros                                  */
-- Line 753: /******************************************************************************/
-- Line 754: 
-- Line 755: #define mpd_err_fatal(...) \
-- Line 756:     do {fprintf(stderr, "%s:%d: error: ", __FILE__, __LINE__); \
-- Line 757:         fprintf(stderr, __VA_ARGS__);  fputc('\n', stderr);    \
-- Line 758:         abort();                                               \
-- Line 759:     } while (0)
-- Line 760: #define mpd_err_warn(...) \
-- Line 761:     do {fprintf(stderr, "%s:%d: warning: ", __FILE__, __LINE__); \
-- Line 762:         fprintf(stderr, __VA_ARGS__); fputc('\n', stderr);       \
-- Line 763:     } while (0)
-- Line 764: 
-- Line 765: 
-- Line 766: /******************************************************************************/
-- Line 767: /*                            Memory handling                                 */
-- Line 768: /******************************************************************************/
-- Line 769: 
-- Line 770: extern void *(* mpd_mallocfunc)(size_t size);
-- Line 771: extern void *(* mpd_callocfunc)(size_t nmemb, size_t size);
-- Line 772: extern void *(* mpd_reallocfunc)(void *ptr, size_t size);
-- Line 773: extern void (* mpd_free)(void *ptr);
-- Line 774: 
-- Line 775
foreign import ccall mpd_callocfunc_em
  :: Word64
{-# LINE 4052 "Base.hsc" #-}
  -- ^ nmemb
  -> Word64
{-# LINE 4054 "Base.hsc" #-}
  -- ^ size
  -> IO ()


-- Line 776: 
-- Line 777
foreign import ccall mpd_alloc
  :: Word64
{-# LINE 4062 "Base.hsc" #-}
  -- ^ nmemb
  -> Word64
{-# LINE 4064 "Base.hsc" #-}
  -- ^ size
  -> IO ()


-- Line 778
foreign import ccall mpd_calloc
  :: Word64
{-# LINE 4071 "Base.hsc" #-}
  -- ^ nmemb
  -> Word64
{-# LINE 4073 "Base.hsc" #-}
  -- ^ size
  -> IO ()


-- Line 779
foreign import ccall mpd_realloc
  :: Ptr a
  -- ^ ptr
  -> Word64
{-# LINE 4082 "Base.hsc" #-}
  -- ^ nmemb
  -> Word64
{-# LINE 4084 "Base.hsc" #-}
  -- ^ size
  -> Ptr Word8
{-# LINE 4086 "Base.hsc" #-}
  -- ^ err
  -> IO ()


-- Line 780
foreign import ccall mpd_sh_alloc
  :: Word64
{-# LINE 4093 "Base.hsc" #-}
  -- ^ struct_size
  -> Word64
{-# LINE 4095 "Base.hsc" #-}
  -- ^ nmemb
  -> Word64
{-# LINE 4097 "Base.hsc" #-}
  -- ^ size
  -> IO ()


-- Line 781: 
-- Line 782: mpd_t *mpd_qnew(void);
-- Line 783
foreign import ccall mpd_new
  :: Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Ptr Mpd_t)


-- Line 784
foreign import ccall mpd_qnew_size
  :: Int64
{-# LINE 4113 "Base.hsc" #-}
  -- ^ size
  -> IO (Ptr Mpd_t)


-- Line 785
foreign import ccall mpd_del
  :: Ptr Mpd_t
  -- ^ dec
  -> IO ()


-- Line 786: 
-- Line 787
foreign import ccall mpd_uint_zero
  :: Ptr Word64
{-# LINE 4128 "Base.hsc" #-}
  -- ^ dest
  -> Word64
{-# LINE 4130 "Base.hsc" #-}
  -- ^ len
  -> IO ()


-- Line 788
foreign import ccall mpd_qresize
  :: Ptr Mpd_t
  -- ^ result
  -> Int64
{-# LINE 4139 "Base.hsc" #-}
  -- ^ size
  -> Ptr Word32
{-# LINE 4141 "Base.hsc" #-}
  -- ^ status
  -> IO (Int32)
{-# LINE 4143 "Base.hsc" #-}


-- Line 789
foreign import ccall mpd_qresize_zero
  :: Ptr Mpd_t
  -- ^ result
  -> Int64
{-# LINE 4150 "Base.hsc" #-}
  -- ^ size
  -> Ptr Word32
{-# LINE 4152 "Base.hsc" #-}
  -- ^ status
  -> IO (Int32)
{-# LINE 4154 "Base.hsc" #-}


-- Line 790
foreign import ccall mpd_minalloc
  :: Ptr Mpd_t
  -- ^ result
  -> IO ()


-- Line 791: 
-- Line 792
foreign import ccall mpd_resize
  :: Ptr Mpd_t
  -- ^ result
  -> Int64
{-# LINE 4169 "Base.hsc" #-}
  -- ^ size
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int32)
{-# LINE 4173 "Base.hsc" #-}


-- Line 793
foreign import ccall mpd_resize_zero
  :: Ptr Mpd_t
  -- ^ result
  -> Int64
{-# LINE 4180 "Base.hsc" #-}
  -- ^ size
  -> Ptr Mpd_context_t
  -- ^ ctx
  -> IO (Int32)
{-# LINE 4184 "Base.hsc" #-}


-- Line 794: 
-- Line 795: 
-- Line 796: #ifdef __cplusplus
-- Line 797:   #ifdef MPD_CLEAR_STDC_LIMIT_MACROS
-- Line 798:     #undef MPD_CLEAR_STDC_LIMIT_MACROS
-- Line 799:     #undef __STDC_LIMIT_MACROS
-- Line 800:   #endif
-- Line 801: } /* END extern "C" */
-- Line 802: #endif
-- Line 803: 
-- Line 804: 
-- Line 805: #endif /* MPDECIMAL_H */
-- Line 806: 
-- Line 807: 
-- Line 808: 
