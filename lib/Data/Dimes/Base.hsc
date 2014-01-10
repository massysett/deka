module Data.Dimes.Base where

import Foreign
import Foreign.Storable
import Foreign.C.Types

#include <mpdecimal.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data Mpd_context_t = Mpd_context_t
  { prec :: #type mpd_size_t
  , emax :: #type mpd_size_t
  , emin :: #type mpd_size_t
  , traps :: #type uint32_t
  , status :: #type uint32_t
  , newtrap :: #type uint32_t
  , round :: #type int
  , clamp :: #type int
  , allcr :: #type int
  }

instance Storable Mpd_context_t where
  alignment _ = #{alignment mpd_context_t}
  sizeOf _ = #{size mpd_context_t}
  peek ptr = do
    p <- #{peek mpd_context_t, prec} ptr
    ex <- #{peek mpd_context_t, emax} ptr
    em <- #{peek mpd_context_t, emin} ptr
    t <- #{peek mpd_context_t, traps} ptr
    s <- #{peek mpd_context_t, status} ptr
    n <- #{peek mpd_context_t, newtrap} ptr
    r <- #{peek mpd_context_t, round} ptr
    c <- #{peek mpd_context_t, clamp} ptr
    a <- #{peek mpd_context_t, allcr} ptr
    return $ Mpd_context_t p ex em t s n r c a

  poke ptr (Mpd_context_t p ex em t s n r c a) = do
    #{poke mpd_context_t, prec} ptr p
    #{poke mpd_context_t, emax} ptr ex
    #{poke mpd_context_t, emin} ptr em
    #{poke mpd_context_t, traps} ptr t
    #{poke mpd_context_t, status} ptr s
    #{poke mpd_context_t, newtrap} ptr n
    #{poke mpd_context_t, round} ptr r
    #{poke mpd_context_t, clamp} ptr c
    #{poke mpd_context_t, allcr} ptr a

foreign import ccall unsafe "mpdecimal.h mpd_init"
  mpd_init
    :: Ptr Mpd_context_t
    -> #type mpd_ssize_t
    -> IO ()

foreign import ccall unsafe "mpdecimal.h mpd_maxcontext"
  mpd_maxcontext
    :: Ptr Mpd_context_t
    -> IO ()

foreign import ccall unsafe "mpdecimal.h mpd_defaultcontext"
  mpd_defaultcontext
    :: Ptr Mpd_context_t
    -> IO ()

foreign import ccall unsafe "mpdecimal.h mpd_basiccontext"
  mpd_basiccontext
    :: Ptr Mpd_context_t
    -> IO ()

foreign import ccall unsafe "mpdecimal.h mpd_getprec"
  mpd_getprec
    :: Ptr Mpd_context_t
    -> IO #type size_t

foreign import ccall unsafe "mpdecimal.h mpd_getemax"
  mpd_getemax
    :: Ptr Mpd_context_t
    -> IO #type mpd_ssize_t

foreign import ccall unsafe "mpdecimal.h mpd_getemin"
  mpd_getemin
    :: Ptr Mpd_context_t
    -> IO #type mpd_ssize_t

foreign import ccall unsafe "mpdecimal.h mpd_getround"
  mpd_getround
    :: Ptr Mpd_context_t
    -> IO #type int

foreign import ccall unsafe "mpdecimal.h mpd_gettraps"
  mpd_gettraps
    :: Ptr Mpd_context_t
    -> IO #type uint32_t

foreign import ccall unsafe "mpdecimal.h mpd_getstatus"
  mpd_getstatus
    :: Ptr Mpd_context_t
    -> IO #type uint32_t

foreign import ccall unsafe "mpdecimal.h mpd_getclamp"
  mpd_getcclamp
    :: Ptr Mpd_context_t
    -> IO #type int

foreign import ccall unsafe "mpdecimal.h mpd_getcr"
  mpd_getcr
    :: Ptr Mpd_context_t
    -> IO #type int

foreign import ccall unsafe "mpdecimal.h mpd_etiny"
  mpd_etiny
    :: Ptr Mpd_context_t
    -> IO #type mpd_ssize_t

foreign import ccall unsafe "mpdecimal.h mpd_etop"
  mpd_etop
    :: Ptr Mpd_context_t
    -> IO #type mpd_ssize_t

foreign import ccall unsafe "mpdecimal.h mpd_qsetprec"
  mpd_qsetprec
    :: Ptr Mpd_context_t
    -> #type mpd_ssize_t
    -> IO #type int

foreign import ccall unsafe "mpdecimal.h mpd_qsetemax"
  mpd_qsetemax
    :: Ptr Mpd_context_t
    -> #type mpd_ssize_t
    -> IO #type int

foreign import ccall unsafe "mpdecimal.h mpd_qsetemin"
  mpd_qsetemin
    :: Ptr Mpd_context_t
    -> #type mpd_ssize_t
    -> IO #type int

foreign import ccall unsafe "mpdecimal.h mpd_qsetround"
  mpd_qsetround
    :: Ptr Mpd_context_t
    -> #type int
    -> IO #type int

foreign import ccall unsafe "mpdecimal.h mpd_qsettraps"
  mpd_qsettraps
    :: Ptr Mpd_context_t
    -> #type uint32_t
    -> IO #type int

foreign import ccall unsafe "mpdecimal.h mpd_qsetstatus"
  mpd_qsetstatus
    :: Ptr Mpd_context_t
    -> #type uint32_t
    -> IO #type int

foreign import ccall unsafe "mpdecimal.h mpd_qsetclamp"
  mpd_qsetclamp
    :: Ptr Mpd_context_t
    -> #type int
    -> IO #type int

foreign import ccall unsafe "mpdecimal.h mpd_qsetcr"
  mpd_qsetcr
    :: Ptr Mpd_context_t
    -> #type int
    -> IO #type int

foreign import ccall "mpdecimal.h &mpd_traphandler"
  mpd_traphandler
    :: Ptr (FunPtr (Ptr Mpd_context_t -> IO ()))

foreign import ccall unsafe mpd_dflt_traphandler
  :: Ptr Mpd_context_t -> IO ()

foreign import ccall safe mpd_addstatus_raise
  :: Ptr Mpd_context_t
  -> #type uint32_t
  -> IO ()

data Mpd_t = Mpd_t
  { flags :: #type uint8_t
  , exp :: #type mpd_ssize_t
  , digits :: #type mpd_ssize_t
  , len :: #type mpd_ssize_t
  , alloc :: #type mpd_ssize_t
  , mpdData :: Ptr #type mpd_uint_t
  }

instance Storable Mpd_t where
  alignment _ = #{alignment mpd_t}
  sizeOf _ = #{size mpd_t}
  peek ptr = do
    f <- #{peek mpd_t, flags} ptr
    e <- #{peek mpd_t, exp} ptr
    di <- #{peek mpd_t, digits} ptr
    l <- #{peek mpd_t, len} ptr
    a <- #{peek mpd_t, alloc} ptr
    dt <- #{peek mpd_t, data} ptr
    return $ Mpd_t f e di l a dt

  poke ptr (Mpd_t f e di l a dt) = do
    #{poke mpd_t, flags} ptr f
    #{poke mpd_t, exp} ptr e
    #{poke mpd_t, digits} ptr di
    #{poke mpd_t, len} ptr l
    #{poke mpd_t, alloc} ptr a
    #{poke mpd_t, data} ptr dt

foreign import ccall unsafe mpd_qnew
  :: IO (Ptr Mpd_t)

foreign import ccall unsafe mpd_new
  :: Ptr Mpd_context_t -> IO (Ptr Mpd_t)

foreign import ccall unsafe mpd_qnew_size
  :: #type mpd_ssize_t
  -> IO (Ptr Mpd_t)

foreign import ccall unsafe mpd_del
  :: Ptr Mpd_t -> IO ()

foreign import ccall unsafe mpd_qset_string
  :: Ptr Mpd_t
  -> Ptr CChar
  -> Ptr Mpd_context_t
  -> #type uint32_t
  -> IO ()

foreign import ccall unsafe mpd_set_string
  :: Ptr Mpd_t
  -> Ptr CChar
  -> Ptr Mpd_context_t
  -> IO ()

foreign import ccall unsafe mpd_qset_ssize
  :: Ptr Mpd_t
  -> #type mpd_ssize_t
  -> Ptr Mpd_context_t
  -> #type uint32_t
  -> IO ()

foreign import ccall unsafe mpd_qset_i32
  :: Ptr Mpd_t
  -> #type int32_t
  -> Ptr Mpd_context_t
  -> #type uint32_t
  -> IO ()

foreign import ccall unsafe mpd_qset_i64
  :: Ptr Mpd_t
  -> #type int64_t
  -> Ptr Mpd_context_t
  -> #type uint32_t
  -> IO ()

foreign import ccall unsafe mpd_qset_uint
  :: Ptr Mpd_t
  -> #type mpd_uint_t
  -> Ptr Mpd_context_t
  -> #type uint32_t
  -> IO ()

foreign import ccall unsafe mpd_qset_u32
  :: Ptr Mpd_t
  -> #type uint32_t
  -> Ptr Mpd_context_t
  -> #type uint32_t
  -> IO ()

foreign import ccall unsafe mpd_qset_u64
  :: Ptr Mpd_t
  -> #type uint64_t
  -> Ptr Mpd_context_t
  -> #type uint32_t
  -> IO ()

foreign import ccall unsafe mpd_set_ssize
  :: Ptr Mpd_t
  -> #type mpd_ssize_t
  -> Ptr Mpd_context_t
  -> IO ()

foreign import ccall unsafe mpd_set_i32
  :: Ptr Mpd_t
  -> #type int32_t
  -> Ptr Mpd_context_t
  -> IO ()

foreign import ccall unsafe mpd_set_i64
  :: Ptr Mpd_t
  -> #type int64_t
  -> Ptr Mpd_context_t
  -> IO ()

foreign import ccall unsafe mpd_set_uint
  :: Ptr Mpd_t
  -> #type mpd_uint_t
  -> Ptr Mpd_context_t
  -> IO ()

foreign import ccall unsafe mpd_set_u32
  :: Ptr Mpd_t
  -> Ptr Mpd_context_t
  -> IO ()

foreign import ccall unsafe mpd_set_u64
  :: Ptr Mpd_t
  -> #type uint64_t
  -> Ptr Mpd_context_t
  -> IO ()

foreign import ccall unsafe mpd_seterror
  :: Ptr Mpd_t
  -> #type uint32_t
  -> #type uint32_t
  -> IO ()

foreign import ccall unsafe mpd_setspecial
  :: Ptr Mpd_t
  -> #type uint8_t
  -> #type uint8_t
  -> IO ()

foreign import ccall unsafe mpd_zerocoeff
  :: Ptr Mpd_t
  -> IO ()

foreign import ccall unsafe mpd_qmaxcoeff
  :: Ptr Mpd_t
  -> Ptr Mpd_context_t
  -> #type uint32_t
  -> IO ()

foreign import ccall unsafe mpd_maxcoeff
  :: Ptr Mpd_t
  -> Ptr Mpd_context_t
  -> IO ()
  
-- Start with "Create Static Decimal"


