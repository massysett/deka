{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main where

import Test.Tasty

import qualified DataDir

tests :: TestTree
tests = testGroup "tasty-test"
  [ DataDir.tests
  ]

main = defaultMain tests
