{-# LANGUAGE ForeignFunctionInterface, Safe, EmptyDataDecls #-}

#include <decContext.h>
#include <decSingle.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- | Low-level bindings to the decNumber library.
module Deka.Internal.Decnumber.DecSingle where

import Foreign.Safe
import Foreign.C
import Deka.Internal.Decnumber.Context
import Deka.Internal.Decnumber.DecDouble

c'NULL :: Num a => a
c'NULL = #const NULL

c'DECSINGLE_Bytes :: Num a => a
c'DECSINGLE_Bytes = #const DECSINGLE_Bytes

c'DECSINGLE_Pmax :: Num a => a
c'DECSINGLE_Pmax = #const DECSINGLE_Pmax

c'DECSINGLE_Emin :: Num a => a
c'DECSINGLE_Emin = #const DECSINGLE_Emin

c'DECSINGLE_Emax :: Num a => a
c'DECSINGLE_Emax = #const DECSINGLE_Emax

c'DECSINGLE_EmaxD :: Num a => a
c'DECSINGLE_EmaxD = #const DECSINGLE_EmaxD

c'DECSINGLE_Bias :: Num a => a
c'DECSINGLE_Bias = #const DECSINGLE_Bias

c'DECSINGLE_String :: Num a => a
c'DECSINGLE_String = #const DECSINGLE_String

c'DECSINGLE_EconL :: Num a => a
c'DECSINGLE_EconL = #const DECSINGLE_EconL

c'DECSINGLE_Declets :: Num a => a
c'DECSINGLE_Declets = #const DECSINGLE_Declets

c'DECSINGLE_Ehigh :: Num a => a
c'DECSINGLE_Ehigh = #const DECSINGLE_Ehigh

c'decSingle'sizeOf :: Int
c'decSingle'sizeOf = #size decSingle

data C'decSingle

p'decSingle'bytes :: Ptr C'decSingle -> Ptr c'decSingle'bytes
p'decSingle'bytes = #ptr decSingle, bytes

c'DECFLOAT_Sign :: Num a => a
c'DECFLOAT_Sign = #const DECFLOAT_Sign

c'DECFLOAT_NaN :: Num a => a
c'DECFLOAT_NaN = #const DECFLOAT_NaN

c'DECFLOAT_qNaN :: Num a => a
c'DECFLOAT_qNaN = #const DECFLOAT_qNaN

c'DECFLOAT_sNaN :: Num a => a
c'DECFLOAT_sNaN = #const DECFLOAT_sNaN

c'DECFLOAT_Inf :: Num a => a
c'DECFLOAT_Inf = #const DECFLOAT_Inf

c'DECFLOAT_MinSp :: Num a => a
c'DECFLOAT_MinSp = #const DECFLOAT_MinSp


c'DECPPLUSALT :: Num a => a
c'DECPPLUSALT = #const DECPPLUSALT

c'DECPMINUSALT :: Num a => a
c'DECPMINUSALT = #const DECPMINUSALT

c'DECPPLUS :: Num a => a
c'DECPPLUS = #const DECPPLUS

c'DECPMINUS :: Num a => a
c'DECPMINUS = #const DECPMINUS

c'DECPPLUSALT2 :: Num a => a
c'DECPPLUSALT2 = #const DECPPLUSALT2

c'DECPUNSIGNED :: Num a => a
c'DECPUNSIGNED = #const DECPUNSIGNED


-- Utilities

foreign import ccall unsafe "decSingleFromPacked" c'decSingleFromPacked
  :: Ptr C'decSingle
  -> Int32
  -> Ptr Word8
  -> IO (Ptr C'decSingle)

foreign import ccall unsafe "decSingleFromPackedChecked" c'decSingleFromPackedChecked
  :: Ptr C'decSingle
  -> Int32
  -> Ptr Word8
  -> IO (Ptr C'decSingle)

foreign import ccall unsafe "decSingleFromString" c'decSingleFromString
  :: Ptr C'decSingle
  -> CString
  -> Ptr C'decContext
  -> IO (Ptr C'decSingle)

foreign import ccall unsafe "decSingleFromWider" c'decSingleFromWider
  :: Ptr C'decSingle
  -> Ptr C'decDouble
  -> Ptr C'decContext
  -> IO (Ptr C'decSingle)

foreign import ccall unsafe "decSingleGetCoefficient" c'decSingleGetCoefficient
  :: Ptr C'decSingle
  -> Ptr Word8
  -> IO Int32

foreign import ccall unsafe "decSingleGetExponent" c'decSingleGetExponent
  :: Ptr C'decSingle
  -> IO Int32

foreign import ccall unsafe "decSingleSetCoefficient" c'decSingleSetCoefficient
  :: Ptr C'decSingle
  -> Ptr Word8
  -> Int32
  -> IO (Ptr C'decSingle)

foreign import ccall unsafe "decSingleSetExponent" c'decSingleSetExponent
  :: Ptr C'decSingle
  -> Ptr C'decContext
  -> Int32
  -> IO (Ptr C'decSingle)

foreign import ccall unsafe "decSingleShow" c'decSingleShow
  :: Ptr C'decSingle
  -> CString
  -> IO ()

foreign import ccall unsafe "decSingleToEngString" c'decSingleToEngString
  :: Ptr C'decSingle
  -> CString
  -> IO CString

foreign import ccall unsafe "decSingleToString" c'decSingleToString
  :: Ptr C'decSingle
  -> CString
  -> IO CString

foreign import ccall unsafe "decSingleToWider" c'decSingleToWider
  :: Ptr C'decSingle
  -> Ptr C'decDouble
  -> IO (Ptr C'decDouble)

foreign import ccall unsafe "decSingleZero" c'decSingleZero
  :: Ptr C'decSingle
  -> IO (Ptr C'decSingle)

foreign import ccall unsafe "decSingleToBCD" c'decSingleToBCD
  :: Ptr C'decSingle
  -> Ptr Int32
  -> Ptr Word8
  -> IO Int32

foreign import ccall unsafe "decSingleRadix" c'decSingleRadix
  :: Ptr C'decSingle
  -> IO Word32

foreign import ccall unsafe "decSingleVersion" c'decSingleVersion
  :: IO CString
