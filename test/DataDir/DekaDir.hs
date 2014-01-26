{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module DataDir.DekaDir where

import Test.Tasty
import qualified DataDir.DekaDir.Env
import qualified DataDir.DekaDir.Pure

tests = testGroup "DekaDir"
  [ DataDir.DekaDir.Env.tests
  , DataDir.DekaDir.Pure.tests
  ]
