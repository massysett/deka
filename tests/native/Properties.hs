{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Properties where

import qualified Deka.Native.Abstract.Generators as G
import qualified Deka.Native as N
import Test.QuickCheck
import qualified Deka.Dec as D

prop_abstractToStringToAbstract =
  forAll G.abstract $ \a ->
  case N.stringToAbstract . N.abstractToString $ a of
    Left _ -> property False
    Right a' -> a === a'

prop_decToAbstractToDec =
  forAll G.abstract $ \a ->
  let (d, flgs) = N.abstractToDec a
      a' = N.decToAbstract d
      (d'', flgs') = N.abstractToDec a'
  in flgs == D.emptyFlags && flgs' == D.emptyFlags
     ==> D.compareTotal d d'' == EQ
