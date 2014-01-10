{-# LINE 1 "Base.hsc" #-}
module Data.Dimes.Base where
{-# LINE 2 "Base.hsc" #-}

import Foreign
import Foreign.Storable
import Foreign.C.Types


{-# LINE 8 "Base.hsc" #-}


{-# LINE 10 "Base.hsc" #-}

data Mpd_context_t = Mpd_context_t
  { prec :: Word64
{-# LINE 13 "Base.hsc" #-}
  , emax :: Word64
{-# LINE 14 "Base.hsc" #-}
  , emin :: Word64
{-# LINE 15 "Base.hsc" #-}
  , traps :: Word32
{-# LINE 16 "Base.hsc" #-}
  , status :: Word32
{-# LINE 17 "Base.hsc" #-}
  , newtrap :: Word32
{-# LINE 18 "Base.hsc" #-}
  , round :: Int32
{-# LINE 19 "Base.hsc" #-}
  , clamp :: Int32
{-# LINE 20 "Base.hsc" #-}
  , allcr :: Int32
{-# LINE 21 "Base.hsc" #-}
  }

instance Storable Mpd_context_t where
  alignment _ = 8
{-# LINE 25 "Base.hsc" #-}
  sizeOf _ = (48)
{-# LINE 26 "Base.hsc" #-}
  peek ptr = do
    p <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 28 "Base.hsc" #-}
    ex <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 29 "Base.hsc" #-}
    em <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 30 "Base.hsc" #-}
    t <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 31 "Base.hsc" #-}
    s <- (\hsc_ptr -> peekByteOff hsc_ptr 28) ptr
{-# LINE 32 "Base.hsc" #-}
    n <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 33 "Base.hsc" #-}
    r <- (\hsc_ptr -> peekByteOff hsc_ptr 36) ptr
{-# LINE 34 "Base.hsc" #-}
    c <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 35 "Base.hsc" #-}
    a <- (\hsc_ptr -> peekByteOff hsc_ptr 44) ptr
{-# LINE 36 "Base.hsc" #-}
    return $ Mpd_context_t p ex em t s n r c a

  poke ptr (Mpd_context_t p ex em t s n r c a) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr p
{-# LINE 40 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr ex
{-# LINE 41 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr em
{-# LINE 42 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr t
{-# LINE 43 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 28) ptr s
{-# LINE 44 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr n
{-# LINE 45 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 36) ptr r
{-# LINE 46 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 40) ptr c
{-# LINE 47 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 44) ptr a
{-# LINE 48 "Base.hsc" #-}

foreign import ccall unsafe "mpdecimal.h mpd_init"
  mpd_init
    :: Ptr Mpd_context_t
    -> Int64
{-# LINE 53 "Base.hsc" #-}
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
    -> IO Word64
{-# LINE 74 "Base.hsc" #-}

foreign import ccall unsafe "mpdecimal.h mpd_getemax"
  mpd_getemax
    :: Ptr Mpd_context_t
    -> IO Int64
{-# LINE 79 "Base.hsc" #-}

foreign import ccall unsafe "mpdecimal.h mpd_getemin"
  mpd_getemin
    :: Ptr Mpd_context_t
    -> IO Int64
{-# LINE 84 "Base.hsc" #-}

foreign import ccall unsafe "mpdecimal.h mpd_getround"
  mpd_getround
    :: Ptr Mpd_context_t
    -> IO Int32
{-# LINE 89 "Base.hsc" #-}

foreign import ccall unsafe "mpdecimal.h mpd_gettraps"
  mpd_gettraps
    :: Ptr Mpd_context_t
    -> IO Word32
{-# LINE 94 "Base.hsc" #-}

foreign import ccall unsafe "mpdecimal.h mpd_getstatus"
  mpd_getstatus
    :: Ptr Mpd_context_t
    -> IO Word32
{-# LINE 99 "Base.hsc" #-}

foreign import ccall unsafe "mpdecimal.h mpd_getclamp"
  mpd_getcclamp
    :: Ptr Mpd_context_t
    -> IO Int32
{-# LINE 104 "Base.hsc" #-}

foreign import ccall unsafe "mpdecimal.h mpd_getcr"
  mpd_getcr
    :: Ptr Mpd_context_t
    -> IO Int32
{-# LINE 109 "Base.hsc" #-}

foreign import ccall unsafe "mpdecimal.h mpd_etiny"
  mpd_etiny
    :: Ptr Mpd_context_t
    -> IO Int64
{-# LINE 114 "Base.hsc" #-}

foreign import ccall unsafe "mpdecimal.h mpd_etop"
  mpd_etop
    :: Ptr Mpd_context_t
    -> IO Int64
{-# LINE 119 "Base.hsc" #-}

foreign import ccall unsafe "mpdecimal.h mpd_qsetprec"
  mpd_qsetprec
    :: Ptr Mpd_context_t
    -> Int64
{-# LINE 124 "Base.hsc" #-}
    -> IO Int32
{-# LINE 125 "Base.hsc" #-}

foreign import ccall unsafe "mpdecimal.h mpd_qsetemax"
  mpd_qsetemax
    :: Ptr Mpd_context_t
    -> Int64
{-# LINE 130 "Base.hsc" #-}
    -> IO Int32
{-# LINE 131 "Base.hsc" #-}

foreign import ccall unsafe "mpdecimal.h mpd_qsetemin"
  mpd_qsetemin
    :: Ptr Mpd_context_t
    -> Int64
{-# LINE 136 "Base.hsc" #-}
    -> IO Int32
{-# LINE 137 "Base.hsc" #-}

foreign import ccall unsafe "mpdecimal.h mpd_qsetround"
  mpd_qsetround
    :: Ptr Mpd_context_t
    -> Int32
{-# LINE 142 "Base.hsc" #-}
    -> IO Int32
{-# LINE 143 "Base.hsc" #-}

foreign import ccall unsafe "mpdecimal.h mpd_qsettraps"
  mpd_qsettraps
    :: Ptr Mpd_context_t
    -> Word32
{-# LINE 148 "Base.hsc" #-}
    -> IO Int32
{-# LINE 149 "Base.hsc" #-}

foreign import ccall unsafe "mpdecimal.h mpd_qsetstatus"
  mpd_qsetstatus
    :: Ptr Mpd_context_t
    -> Word32
{-# LINE 154 "Base.hsc" #-}
    -> IO Int32
{-# LINE 155 "Base.hsc" #-}

foreign import ccall unsafe "mpdecimal.h mpd_qsetclamp"
  mpd_qsetclamp
    :: Ptr Mpd_context_t
    -> Int32
{-# LINE 160 "Base.hsc" #-}
    -> IO Int32
{-# LINE 161 "Base.hsc" #-}

foreign import ccall unsafe "mpdecimal.h mpd_qsetcr"
  mpd_qsetcr
    :: Ptr Mpd_context_t
    -> Int32
{-# LINE 166 "Base.hsc" #-}
    -> IO Int32
{-# LINE 167 "Base.hsc" #-}

foreign import ccall "mpdecimal.h &mpd_traphandler"
  mpd_traphandler
    :: Ptr (FunPtr (Ptr Mpd_context_t -> IO ()))

foreign import ccall unsafe mpd_dflt_traphandler
  :: Ptr Mpd_context_t -> IO ()

foreign import ccall safe mpd_addstatus_raise
  :: Ptr Mpd_context_t
  -> Word32
{-# LINE 178 "Base.hsc" #-}
  -> IO ()

data Mpd_t = Mpd_t
  { flags :: Word8
{-# LINE 182 "Base.hsc" #-}
  , exp :: Int64
{-# LINE 183 "Base.hsc" #-}
  , digits :: Int64
{-# LINE 184 "Base.hsc" #-}
  , len :: Int64
{-# LINE 185 "Base.hsc" #-}
  , alloc :: Int64
{-# LINE 186 "Base.hsc" #-}
  , mpdData :: Ptr Word64
{-# LINE 187 "Base.hsc" #-}
  }

instance Storable Mpd_t where
  alignment _ = 8
{-# LINE 191 "Base.hsc" #-}
  sizeOf _ = (48)
{-# LINE 192 "Base.hsc" #-}
  peek ptr = do
    f <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 194 "Base.hsc" #-}
    e <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 195 "Base.hsc" #-}
    di <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 196 "Base.hsc" #-}
    l <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 197 "Base.hsc" #-}
    a <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 198 "Base.hsc" #-}
    dt <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 199 "Base.hsc" #-}
    return $ Mpd_t f e di l a dt

  poke ptr (Mpd_t f e di l a dt) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr f
{-# LINE 203 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr e
{-# LINE 204 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr di
{-# LINE 205 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr l
{-# LINE 206 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr a
{-# LINE 207 "Base.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 40) ptr dt
{-# LINE 208 "Base.hsc" #-}

foreign import ccall unsafe mpd_qnew
  :: IO (Ptr Mpd_t)

foreign import ccall unsafe mpd_new
  :: Ptr Mpd_context_t -> IO (Ptr Mpd_t)

foreign import ccall unsafe mpd_qnew_size
  :: Int64
{-# LINE 217 "Base.hsc" #-}
  -> IO (Ptr Mpd_t)

foreign import ccall unsafe mpd_del
  :: Ptr Mpd_t -> IO ()

foreign import ccall unsafe mpd_qset_string
  :: Ptr Mpd_t
  -> Ptr CChar
  -> Ptr Mpd_context_t
  -> Word32
{-# LINE 227 "Base.hsc" #-}
  -> IO ()

foreign import ccall unsafe mpd_set_string
  :: Ptr Mpd_t
  -> Ptr CChar
  -> Ptr Mpd_context_t
  -> IO ()

foreign import ccall unsafe mpd_qset_ssize
  :: Ptr Mpd_t
  -> Int64
{-# LINE 238 "Base.hsc" #-}
  -> Ptr Mpd_context_t
  -> Word32
{-# LINE 240 "Base.hsc" #-}
  -> IO ()

foreign import ccall unsafe mpd_qset_i32
  :: Ptr Mpd_t
  -> Int32
{-# LINE 245 "Base.hsc" #-}
  -> Ptr Mpd_context_t
  -> Word32
{-# LINE 247 "Base.hsc" #-}
  -> IO ()

foreign import ccall unsafe mpd_qset_i64
  :: Ptr Mpd_t
  -> Int64
{-# LINE 252 "Base.hsc" #-}
  -> Ptr Mpd_context_t
  -> Word32
{-# LINE 254 "Base.hsc" #-}
  -> IO ()

foreign import ccall unsafe mpd_qset_uint
  :: Ptr Mpd_t
  -> Word64
{-# LINE 259 "Base.hsc" #-}
  -> Ptr Mpd_context_t
  -> Word32
{-# LINE 261 "Base.hsc" #-}
  -> IO ()

foreign import ccall unsafe mpd_qset_u32
  :: Ptr Mpd_t
  -> Word32
{-# LINE 266 "Base.hsc" #-}
  -> Ptr Mpd_context_t
  -> Word32
{-# LINE 268 "Base.hsc" #-}
  -> IO ()

foreign import ccall unsafe mpd_qset_u64
  :: Ptr Mpd_t
  -> Word64
{-# LINE 273 "Base.hsc" #-}
  -> Ptr Mpd_context_t
  -> Word32
{-# LINE 275 "Base.hsc" #-}
  -> IO ()

foreign import ccall unsafe mpd_set_ssize
  :: Ptr Mpd_t
  -> Int64
{-# LINE 280 "Base.hsc" #-}
  -> Ptr Mpd_context_t
  -> IO ()

foreign import ccall unsafe mpd_set_i32
  :: Ptr Mpd_t
  -> Int32
{-# LINE 286 "Base.hsc" #-}
  -> Ptr Mpd_context_t
  -> IO ()

foreign import ccall unsafe mpd_set_i64
  :: Ptr Mpd_t
  -> Int64
{-# LINE 292 "Base.hsc" #-}
  -> Ptr Mpd_context_t
  -> IO ()

foreign import ccall unsafe mpd_set_uint
  :: Ptr Mpd_t
  -> Word64
{-# LINE 298 "Base.hsc" #-}
  -> Ptr Mpd_context_t
  -> IO ()

foreign import ccall unsafe mpd_set_u32
  :: Ptr Mpd_t
  -> Ptr Mpd_context_t
  -> IO ()

foreign import ccall unsafe mpd_set_u64
  :: Ptr Mpd_t
  -> Word64
{-# LINE 309 "Base.hsc" #-}
  -> Ptr Mpd_context_t
  -> IO ()

foreign import ccall unsafe mpd_seterror
  :: Ptr Mpd_t
  -> Word32
{-# LINE 315 "Base.hsc" #-}
  -> Word32
{-# LINE 316 "Base.hsc" #-}
  -> IO ()

foreign import ccall unsafe mpd_setspecial
  :: Ptr Mpd_t
  -> Word8
{-# LINE 321 "Base.hsc" #-}
  -> Word8
{-# LINE 322 "Base.hsc" #-}
  -> IO ()

foreign import ccall unsafe mpd_zerocoeff
  :: Ptr Mpd_t
  -> IO ()

foreign import ccall unsafe mpd_qmaxcoeff
  :: Ptr Mpd_t
  -> Ptr Mpd_context_t
  -> Word32
{-# LINE 332 "Base.hsc" #-}
  -> IO ()

foreign import ccall unsafe mpd_maxcoeff
  :: Ptr Mpd_t
  -> Ptr Mpd_context_t
  -> IO ()


