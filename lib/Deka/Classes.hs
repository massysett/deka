{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Deka.Classes where

import Foreign.Safe
import Foreign.C.Types
import Prelude (IO, Bool)
import Deka.Internal.Decnumber.DecNumber
import Deka.Internal.Decnumber.Context

type Unary a =
  Ptr a -> Ptr a -> Ptr C'decContext -> IO (Ptr a)

type Binary a = 
  Ptr a -> Ptr a -> Ptr a -> Ptr C'decContext -> IO (Ptr a)

type Ternary a = 
  Ptr a -> Ptr a -> Ptr a -> Ptr a -> Ptr C'decContext -> IO (Ptr a)

type Predicate a =
  Ptr a -> IO Word32

class Unwrap w a | w -> a, a -> w where
  unwrap :: w -> ForeignPtr a
  rewrap :: ForeignPtr a -> w

class Create a where
  create :: Int32 -> IO a

class (Create w, Unwrap w a) => Arith w a | w -> a, a -> w where
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
  max :: Unary a
  maxMag :: Binary a
  min :: Binary a
  minMag :: Binary a
  minus :: Unary a
  multiply :: Binary a
  nextMinus :: Unary a
  nextPlus :: Unary a
  nextToward :: Binary a
  or :: Binary a
  plus :: Unary a
  quantize :: Binary a
  remainder :: Binary a
  remainderNear :: Binary a
  rotate :: Binary a
  sameQuantum :: Ptr a -> Ptr a -> IO Bool
  scaleB :: Binary a
  shift :: Binary a
  subtract :: Binary a
  toIntegralExact :: Unary a
  xor :: Binary a
  copyAbs :: Ptr a -> Ptr a -> IO (Ptr a)
  copyNegate :: Ptr a -> Ptr a -> IO (Ptr a)
  copySign :: Ptr a -> Ptr a -> Ptr a -> IO (Ptr a)
  fromInt32 :: Ptr a -> Int32 -> IO (Ptr a)
  fromUInt32 :: Ptr a -> Word32 -> IO (Ptr a)
  isFinite :: Predicate a
  isInfinite :: Predicate a
  isNaN :: Predicate a
  isNegative :: Predicate a
  isNormal :: Predicate a
  isSubnormal :: Predicate a
  zero :: Ptr a -> IO (Ptr a)

class Basic b where
  baseFromString
    :: Ptr b -> Ptr CChar -> Ptr C'decContext -> IO (Ptr b)
  stringSize :: b -> Int
  toString
    :: Ptr b -> Ptr CChar -> IO (Ptr CChar)
  toEngString
    :: Ptr b -> Ptr CChar -> IO (Ptr CChar)
  fromNumber
    :: Ptr b -> Ptr C'decNumber -> Ptr C'decContext -> IO (Ptr b)

  toNumber
    :: Ptr b -> Ptr C'decNumber -> IO (Ptr C'decNumber)

  baseCanonical
    :: Ptr b -> Ptr b -> IO (Ptr b)

  isCanonical
    :: Predicate b



{-
class Transcendental a where
  ln :: Unary a
  logb :: Unary a
  log10 :: Unary a
  power :: Binary a
  squareRoot :: Unary a
-}
