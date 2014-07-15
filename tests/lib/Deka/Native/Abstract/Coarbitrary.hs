module Deka.Native.Abstract.Coarbitrary where

import qualified Deka.Native.Abstract as D
import Test.QuickCheck
import Deka.Tests.Util
import Deka.Dec.Coarbitrary
import Prelude hiding (exponent)

novem :: D.Novem -> Gen a -> Gen a
novem d = case d of
  D.D1 -> v 0
  D.D2 -> v 1
  D.D3 -> v 2
  D.D4 -> v 3
  D.D5 -> v 4
  D.D6 -> v 5
  D.D7 -> v 6
  D.D8 -> v 7
  D.D9 -> v 8
  where
    v = varInt

decem :: D.Decem -> Gen a -> Gen a
decem d = case d of
  D.D0 -> varInt 0
  D.Nonem n -> varInt 1 . novem n

decuple :: D.Decuple -> Gen a -> Gen a
decuple (D.Decuple n ds) = novem n . coarbitraryList decem ds

aut :: D.Aut -> Gen a -> Gen a
aut a = case a of
  D.Nil -> varInt 0
  D.Plenus d -> varInt 1 . decuple d

firmado :: D.Firmado -> Gen a -> Gen a
firmado a = case a of
  D.Cero -> varInt 0
  D.Completo p d -> varInt 1 . posNeg p . decuple d

coefficient :: D.Coefficient -> Gen a -> Gen a
coefficient (D.Coefficient c) = aut c

exponent :: D.Exponent -> Gen a -> Gen a
exponent (D.Exponent c) = firmado c

diagnostic :: D.Diagnostic -> Gen a -> Gen a
diagnostic (D.Diagnostic d) = decuple d

noisy :: D.Noisy -> Gen a -> Gen a
noisy d = case d of
  D.Quiet -> varInt 0
  D.Signaling -> varInt 1

nonNum :: D.NonNum -> Gen a -> Gen a
nonNum (D.NonNum n d) = noisy n . varD d
  where
    varD x = case x of
      Nothing -> varInt 0
      Just j -> varInt 1 . diagnostic j

value :: D.Value -> Gen a -> Gen a
value v = case v of
  D.Finite c e -> varInt 0 . coefficient c . exponent e
  D.Infinite -> varInt 1
  D.NotANumber n -> varInt 2 . nonNum n

abstract :: D.Abstract -> Gen a -> Gen a
abstract (D.Abstract s v) = sign s . value v

adjustedExp :: D.AdjustedExp -> Gen a -> Gen a
adjustedExp (D.AdjustedExp i) = variant i
