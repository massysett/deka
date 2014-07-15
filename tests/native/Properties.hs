module Properties where

import qualified Generators as G
import qualified Deka.Native as N
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty (TestTree, testGroup)
import Test.QuickCheck
import qualified Deka.Dec as D

tests :: TestTree
tests = testGroup "Native"
  [ testProperty "abstract -> string -> abstract" $
    forAll G.abstract $ \a ->
      case N.stringToAbstract . N.abstractToString $ a of
        Left _ -> property False
        Right a' -> a === a'

  , testProperty "Dec -> Abstract -> Dec" $
    forAll G.abstract $ \a ->
    let (d, flgs) = N.abstractToDec a
        a' = N.decToAbstract d
        (d'', flgs') = N.abstractToDec a'
    in flgs == D.emptyFlags && flgs' == D.emptyFlags
        ==> D.compareTotal d d'' == EQ
  ]
