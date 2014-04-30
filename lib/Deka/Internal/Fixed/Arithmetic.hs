module Deka.Internal.Fixed.Arithmetic where

import Foreign.Safe
import Prelude (IO)
import Deka.Internal.Decnumber.Context

type Unary a =
  Ptr a -> Ptr a -> Ptr C'decContext -> IO (Ptr a)

type Binary a = 
  Ptr a -> Ptr a -> Ptr a -> Ptr C'decContext -> IO (Ptr a)

type Ternary a = 
  Ptr a -> Ptr a -> Ptr a -> Ptr a -> Ptr C'decContext -> IO (Ptr a)

class Arith a where
  abs :: Unary a
  add :: Binary a
  and :: Binary a
  compare :: Binary a
  compareSignal :: Binary a
  compareTotal :: Binary a
  compareTotalMag :: Binary a
  divide :: Binary a
  divideInteger :: Binary a
  exp :: Unary a
  fma :: Ternary a
  invert :: Unary a
  ln :: Unary a
  logb :: Unary a
  log10 :: Unary a
  max :: Unary a

