{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
module Deka.Class
  ( Class
  , sNaN
  , qNaN
  , negInf
  , negNormal
  , negSubnormal
  , negZero
  , posZero
  , posSubnormal
  , posNormal
  , posInf
  , classes
  ) where

import Deka.Internal.Class
import Data.String

-- | A list of all classes and their corresponding abstract
-- representations.  The classes are specified using the same case
-- (uppercase and lowercase letters) that are used in the General
-- Decimal Arithmetic Specification.

classes :: IsString a => [(a, Class)]
classes =
  [ ("sNaN", sNaN)
  , ("NaN", qNaN)
  , ("-Infinity", negInf)
  , ("-Normal", negNormal)
  , ("-Subnormal", negSubnormal)
  , ("-Zero", negZero)
  , ("+Zero", posZero)
  , ("+Subnormal", posSubnormal)
  , ("+Normal", posNormal)
  , ("+Infinity", posInf)
  ]
