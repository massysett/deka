{-# LINE 1 "Base.hsc" #-}
module Data.Dimes.Base where
{-# LINE 2 "Base.hsc" #-}

import Foreign
import Foreign.Storable


{-# LINE 7 "Base.hsc" #-}


{-# LINE 9 "Base.hsc" #-}

data Mpd_context_t = Mpd_context_t
  { prec :: Word64
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

foreign import ccall unsafe "mpdecimal.h mpd_init"
  mpd_init
    :: Ptr Mpd_context_t
    -> Int64
{-# LINE 52 "Base.hsc" #-}
    -> IO ()
