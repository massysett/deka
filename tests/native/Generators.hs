-- | Generators of native data types.

module Generators where

import Control.Applicative
import Test.QuickCheck
import qualified Deka.Native as D
import qualified Deka.Dec as Dec
import Prelude hiding (exponent)

novem :: Gen D.Novem
novem = elements [minBound..maxBound]

decem :: Gen D.Decem
decem = frequency [(1, return D.D0), (9, fmap D.Nonem novem)]

decuple :: Gen D.Decuple
decuple = D.Decuple <$> novem <*> listOf decem

aut :: Gen D.Aut
aut = frequency [(1, return D.Nil), (4, fmap D.Plenus decuple)]

posNeg :: Gen Dec.PosNeg
posNeg = elements [Dec.Pos, Dec.Neg]

firmado :: Gen D.Firmado
firmado = frequency [(1, return D.Cero)
  , (4, D.Completo <$> posNeg <*> decuple)]

coefficient :: Gen D.Coefficient
coefficient = fmap D.Coefficient aut

exponent :: Gen D.Exponent
exponent = fmap D.Exponent firmado

diagnostic :: Gen D.Diagnostic
diagnostic = fmap D.Diagnostic decuple

noisy :: Gen D.Noisy
noisy = elements [D.Quiet, D.Signaling]

nonNum :: Gen D.NonNum
nonNum = D.NonNum
  <$> noisy
  <*> frequency [(1, return Nothing), (3, fmap Just diagnostic)]

value :: Gen D.Value
value = frequency
  [ (4, D.Finite <$> coefficient <*> exponent)
  , (1, return D.Infinite)
  , (1, fmap D.NotANumber nonNum)
  ]

sign :: Gen Dec.Sign
sign = elements [ Dec.Sign1, Dec.Sign0 ]

abstract :: Gen D.Abstract
abstract = D.Abstract <$> sign <*> value
