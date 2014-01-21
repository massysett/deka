module Main where

import Bindings.Mpdecimal
import Foreign
import qualified Data.ByteString.Char8 as BS8
import System.Environment

main = do
  a1:a2:[] <- getArgs
  let (contents, format) = (BS8.pack a1, BS8.pack a2)
  cPtr <- malloc
  c'mpd_maxcontext cPtr
  mpd <- c'mpd_qnew
  BS8.useAsCString contents $ \ptrStr ->
    c'mpd_set_string mpd ptrStr cPtr
  r <- BS8.useAsCString format $ \ptrFmt ->
    c'mpd_format mpd ptrFmt cPtr
  bs <- BS8.packCString r
  print bs
