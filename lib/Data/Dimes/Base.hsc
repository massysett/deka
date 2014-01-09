module Data.Dimes.Base where

import Foreign
import Foreign.Storable

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
