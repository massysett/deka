{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main where

import Test.Tasty

import qualified DekaDir

tests :: TestTree
tests = testGroup "tasty-test"
  [ DekaDir.tests
  ]

main = defaultMain tests
