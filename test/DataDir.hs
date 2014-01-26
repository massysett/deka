{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module DataDir where

import Test.Tasty
import qualified DataDir.DekaDir
import qualified DataDir.Deka

tests = testGroup "DataDir"
  [ DataDir.DekaDir.tests
  , DataDir.Deka.tests
  ]
