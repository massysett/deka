{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module DataDir.DekaTest where

import Control.Exception
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import DataDir.DekaDir.QuadTest

tests = testGroup "Deka"
  [
  ]
