-- | The decimal32 type.  For testing use only.

{-# LANGUAGE EmptyDataDecls #-}

module Deka.Internal.Decnumber.Decimal32 where

#include <decimal32.h>
#include <decContext.h>
#include <decNumber.h>

import Deka.Internal.Decnumber.Types
import Deka.Internal.Decnumber.Context
import Deka.Internal.Decnumber.DecNumber
import Foreign.C.Types
import Foreign.Safe

data C'decimal32

c'DECIMAL32_String :: Num a => a
c'DECIMAL32_String = #const DECIMAL32_String

c'decimal32'sizeOf :: Int
c'decimal32'sizeOf = #size decimal32

foreign import ccall unsafe "decimal32FromString" c'decimal32FromString
  :: Ptr C'decimal32
  -> Ptr CChar
  -> Ptr C'decContext
  -> IO (Ptr C'decimal32)

foreign import ccall unsafe "decimal32ToString" c'decimal32ToString
  :: Ptr C'decimal32
  -> Ptr CChar
  -- ^ Must be at least c'DECIMAL32_String long
  -> IO (Ptr CChar)

foreign import ccall unsafe "decimal32ToEngString" c'decimal32ToEngString
  :: Ptr C'decimal32
  -> Ptr CChar
  -- ^ Must be at least c'DECIMAL32_String long
  -> IO (Ptr CChar)

foreign import ccall unsafe "decimal32FromNumber" c'decimal32FromNumber
  :: Ptr C'decimal32
  -> Ptr C'decNumber
  -> Ptr C'decContext
  -> IO (Ptr C'decimal32)

foreign import ccall unsafe "decimal32ToNumber" c'decimal32ToNumber
  :: Ptr C'decimal32
  -> Ptr C'decNumber
  -- ^ Must have space for 7 digits of precision
  -> IO (Ptr C'decNumber)

foreign import ccall unsafe "decimal32Canonical" c'decimal32Canonical
  :: Ptr C'decimal32
  -- ^ Receives a copy of the source, with canonical encoding
  -> Ptr C'decimal32
  -- ^ Source
  -> IO (Ptr C'decimal32)

foreign import ccall unsafe "decimal32IsCanonical" c'decimal32IsCanonical
  :: Ptr C'decimal32
  -> IO C'uint32_t
