{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module DataDir.DekaDir where

import Test.Tasty
import qualified DataDir.DekaDir.EnvTest
import qualified DataDir.DekaDir.PureTest

tests = testGroup "DekaDir"
  [ DataDir.DekaDir.EnvTest.tests
  , DataDir.DekaDir.PureTest.tests
  ]
