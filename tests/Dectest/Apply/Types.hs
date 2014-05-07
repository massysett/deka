module Dectest.Apply.Types where

import qualified Data.ByteString.Char8 as BS8
import qualified Deka.Context as C
import Dectest.Interp.Octothorpe (WhichPrecision)

data Bypass
  = Skip
  -- ^ No suitable test for this operation; skip the test
  | Null
  -- ^ One of the operands was null; skip the test
  | Fail BS8.ByteString [C.Flag]
  -- ^ Comes with the string representation of the test result
  | OperandMismatch
  -- ^ Test did not come with right number of operands

data Directives a = Directives
  { precision :: a C.Precision
  , rounding :: a C.Round
  , emax :: a C.Emax
  , emin :: a C.Emin
  , extended :: Bool
  , clamp :: Bool
  }

type ApplyTest a
  = [WhichPrecision -> C.Ctx a]
  -- ^ Operands
  -> Maybe (C.Ctx a)
  -- ^ Return Nothing if there is an operand type mismatch;
  -- otherwise, return the result of the function

