module Deka.Native.Abstract.Shrinkers where

import Deka.Dec.Shrinkers
import qualified Deka.Native.Abstract as D
import Test.QuickCheck
import Prelude hiding (exponent)

novem :: D.Novem -> [D.Novem]
novem d = case d of
  D.D1 -> []
  D.D2 -> [D.D1]
  D.D3 -> [D.D1 .. D.D2]
  D.D4 -> [D.D1 .. D.D3]
  D.D5 -> [D.D1 .. D.D4]
  D.D6 -> [D.D1 .. D.D5]
  D.D7 -> [D.D1 .. D.D6]
  D.D8 -> [D.D1 .. D.D7]
  D.D9 -> [D.D1 .. D.D8]

decem :: D.Decem -> [D.Decem]
decem d = case d of
  D.D0 -> []
  D.Nonem nv -> D.D0 : map D.Nonem (novem nv)

decuple :: D.Decuple -> [D.Decuple]
decuple (D.Decuple n d) =
  [ D.Decuple n' d' | n' <- novem n,
                      d' <- shrinkList decem d ]

aut :: D.Aut -> [D.Aut]
aut a = case a of
  D.Nil -> []
  D.Plenus d -> D.Nil : map D.Plenus (decuple d)

firmado :: D.Firmado -> [D.Firmado]
firmado a = case a of
  D.Cero -> []
  D.Completo p d ->
    [ D.Completo p' d' | p' <- posNeg p, d' <- decuple d ]

coefficient :: D.Coefficient -> [D.Coefficient]
coefficient = map D.Coefficient . aut . D.unCoefficient

exponent :: D.Exponent -> [D.Exponent]
exponent = map D.Exponent . firmado . D.unExponent

diagnostic :: D.Diagnostic -> [D.Diagnostic]
diagnostic = map D.Diagnostic . decuple . D.unDiagnostic

noisy :: D.Noisy -> [D.Noisy]
noisy a = case a of
  D.Quiet -> []
  D.Signaling -> [D.Quiet]

nonNum :: D.NonNum -> [D.NonNum]
nonNum (D.NonNum n d) = [ D.NonNum n' d'
  | n' <- noisy n, d' <- shrinkMaybe d ]
  where
    shrinkMaybe m = case m of
      Nothing -> []
      Just x -> Nothing : map Just (diagnostic x)

value :: D.Value -> [D.Value]
value a = case a of
  D.Finite c e -> [ D.Finite c' e' | c' <- coefficient c,
                                     e' <- exponent e ]
  D.Infinite -> []
  D.NotANumber n -> [ D.NotANumber n' | n' <- nonNum n ]

abstract :: D.Abstract -> [D.Abstract]
abstract (D.Abstract s v) =
  [ D.Abstract s' v' | s' <- sign s, v' <- value v ]

adjustedExp :: D.AdjustedExp -> [D.AdjustedExp]
adjustedExp (D.AdjustedExp i) = map D.AdjustedExp $ shrinkIntegral i
