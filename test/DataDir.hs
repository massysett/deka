{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module DataDir where

import Test.Tasty
import qualified DataDir.DekaDir
import qualified DataDir.DekaTest

tests = testGroup "DataDir"
  [ DataDir.DekaDir.tests
  , DataDir.DekaTest.tests
  ]
