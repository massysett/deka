module Deka.Dec.Generators where

import Test.QuickCheck
import qualified Deka.Dec as D

posNeg :: Gen D.PosNeg
posNeg = elements [ D.Pos, D.Neg ]

sign :: Gen D.Sign
sign = elements [ D.Sign0, D.Sign1 ]
