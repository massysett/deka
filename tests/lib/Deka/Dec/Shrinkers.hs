module Deka.Dec.Shrinkers where

import qualified Deka.Dec as D

posNeg :: D.PosNeg -> [D.PosNeg]
posNeg x = case x of
  D.Neg -> [D.Pos]
  _ -> []

sign :: D.Sign -> [D.Sign]
sign x = case x of
  D.Sign1 -> [D.Sign0]
  _ -> []
