-- | The decimal128 type.  For testing use only.

{-# LANGUAGE EmptyDataDecls #-}

module Deka.Internal.Decnumber.Decimal128 where

#include <decimal128.h>
#include <decContext.h>
#include <decNumber.h>

import Deka.Internal.Decnumber.Types
import Deka.Internal.Decnumber.Context
import Deka.Internal.Decnumber.DecNumber
import Foreign.C.Types
import Foreign.Safe

data C'decimal128

c'decimal128'sizeOf :: Int
c'decimal128'sizeOf = #size decimal128

c'DECIMAL128_String :: Num a => a
c'DECIMAL128_String = #const DECIMAL128_String

foreign import ccall unsafe "decimal128FromString" c'decimal128FromString
  :: Ptr C'decimal128
  -> Ptr CChar
  -> Ptr C'decContext
  -> IO (Ptr C'decimal128)

foreign import ccall unsafe "decimal128ToString" c'decimal128ToString
  :: Ptr C'decimal128
  -> Ptr CChar
  -- ^ Must be at least c'DECIMAL128_String long
  -> IO (Ptr CChar)

foreign import ccall unsafe "decimal128ToEngString" c'decimal128ToEngString
  :: Ptr C'decimal128
  -> Ptr CChar
  -- ^ Must be at least c'DECIMAL128_String long
  -> IO (Ptr CChar)

foreign import ccall unsafe "decimal128FromNumber" c'decimal128FromNumber
  :: Ptr C'decimal128
  -> Ptr C'decNumber
  -> Ptr C'decContext
  -> IO (Ptr C'decimal128)

foreign import ccall unsafe "decimal128ToNumber" c'decimal128ToNumber
  :: Ptr C'decimal128
  -> Ptr C'decNumber
  -- ^ Must have space for 16 digits of precision
  -> IO (Ptr C'decNumber)

foreign import ccall unsafe "decimal128Canonical" c'decimal128Canonical
  :: Ptr C'decimal128
  -- ^ Receives a copy of the source, with canonical encoding
  -> Ptr C'decimal128
  -- ^ Source
  -> IO (Ptr C'decimal128)

foreign import ccall unsafe "decimal128IsCanonical" c'decimal128IsCanonical
  :: Ptr C'decimal128
  -> IO C'uint32_t
