module Deka.Native.Abstract.Generators where

import Test.QuickCheck
import Deka.Dec.Generators
import qualified Deka.Native.Abstract as A
import Control.Monad
import Prelude hiding (exponent)

novem :: Gen A.Novem
novem = elements
  [ A.D1, A.D2, A.D3, A.D4, A.D5, A.D6, A.D7, A.D8, A.D9 ]

decem :: Gen A.Decem
decem = frequency [(1, return A.D0), (5, fmap A.Nonem novem)]

decuple :: Gen A.Decuple
decuple = liftM2 A.Decuple novem (listOf decem)

aut :: Gen A.Aut
aut = frequency [(1, return A.Nil), (5, fmap A.Plenus decuple)]

firmado :: Gen A.Firmado
firmado = frequency [ (1, return A.Cero),
                      (5, liftM2 A.Completo posNeg decuple) ]

coefficient :: Gen A.Coefficient
coefficient = fmap A.Coefficient aut

exponent :: Gen A.Exponent
exponent = fmap A.Exponent firmado

diagnostic :: Gen A.Diagnostic
diagnostic = fmap A.Diagnostic decuple

noisy :: Gen A.Noisy
noisy = elements [ A.Quiet, A.Signaling ]

nonNum :: Gen A.NonNum
nonNum = liftM2 A.NonNum noisy
  (frequency [(1, return Nothing), (5, fmap Just diagnostic)])

value :: Gen A.Value
value = frequency
  [ (4, liftM2 A.Finite coefficient exponent)
  , (1, return A.Infinite)
  , (1, fmap A.NotANumber nonNum)
  ]

abstract :: Gen A.Abstract
abstract = liftM2 A.Abstract sign value

adjustedExp :: Gen A.AdjustedExp
adjustedExp = fmap A.AdjustedExp arbitrarySizedIntegral
