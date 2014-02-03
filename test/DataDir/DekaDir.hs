{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module DataDir.DekaDir where

import Test.Tasty
import qualified DataDir.DekaDir.QuadTest

tests = testGroup "DekaDir"
  [ DataDir.DekaDir.QuadTest.tests
  ]
