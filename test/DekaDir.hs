{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module DekaDir where

import Test.Tasty
import qualified DekaDir.QuadTest

tests = testGroup "DekaDir"
  [ DekaDir.QuadTest.tests
  ]
