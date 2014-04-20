{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module DekaDir where

import Test.Tasty
import qualified DekaDir.QuadTest
import qualified DekaDir.DecNumTest

tests = testGroup "DekaDir"
  [ DekaDir.QuadTest.tests
  , DekaDir.DecNumTest.tests
  ]
