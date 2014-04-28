-- | The decimal64 type.  For testing use only.

{-# LANGUAGE EmptyDataDecls #-}

module Deka.Internal.Decnumber.Decimal64 where

#include <decimal64.h>
#include <decContext.h>
#include <decNumber.h>

import Deka.Internal.Decnumber.Types
import Deka.Internal.Decnumber.Context
import Deka.Internal.Decnumber.DecNumber
import Foreign.C.Types
import Foreign.Safe

data C'decimal64

c'decimal64'sizeOf :: Int
c'decimal64'sizeOf = #size decimal64

c'DECIMAL64_String :: Num a => a
c'DECIMAL64_String = #const DECIMAL64_String

foreign import ccall unsafe "decimal64FromString" c'decimal64FromString
  :: Ptr C'decimal64
  -> Ptr CChar
  -> Ptr C'decContext
  -> IO (Ptr C'decimal64)

foreign import ccall unsafe "decimal64ToString" c'decimal64ToString
  :: Ptr C'decimal64
  -> Ptr CChar
  -- ^ Must be at least c'DECIMAL64_String long
  -> IO (Ptr CChar)

foreign import ccall unsafe "decimal64ToEngString" c'decimal64ToEngString
  :: Ptr C'decimal64
  -> Ptr CChar
  -- ^ Must be at least c'DECIMAL64_String long
  -> IO (Ptr CChar)

foreign import ccall unsafe "decimal64FromNumber" c'decimal64FromNumber
  :: Ptr C'decimal64
  -> Ptr C'decNumber
  -> Ptr C'decContext
  -> IO (Ptr C'decimal64)

foreign import ccall unsafe "decimal64ToNumber" c'decimal64ToNumber
  :: Ptr C'decimal64
  -> Ptr C'decNumber
  -- ^ Must have space for 16 digits of precision
  -> IO (Ptr C'decNumber)

foreign import ccall unsafe "decimal64Canonical" c'decimal64Canonical
  :: Ptr C'decimal64
  -- ^ Receives a copy of the source, with canonical encoding
  -> Ptr C'decimal64
  -- ^ Source
  -> IO (Ptr C'decimal64)

foreign import ccall unsafe "decimal64IsCanonical" c'decimal64IsCanonical
  :: Ptr C'decimal64
  -> IO C'uint32_t
