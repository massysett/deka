module Deka.Dec.Coarbitrary where

import qualified Deka.Dec as D
import Test.QuickCheck
import Deka.Tests.Util

posNeg :: D.PosNeg -> Gen a -> Gen a
posNeg a = case a of
  D.Pos -> varInt 0
  D.Neg -> varInt 1

sign :: D.Sign -> Gen a -> Gen a
sign a = case a of
  D.Sign0 -> varInt 0
  D.Sign1 -> varInt 1
