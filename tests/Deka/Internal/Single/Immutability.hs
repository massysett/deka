{-# LANGUAGE NoImplicitPrelude #-}
module Deka.Internal.Single.Immutability where

import Deka.Internal.Single.Tests.Util
import Deka.Internal.Single.CtxFree
import Test.Tasty

tests :: TestTree
tests = testGroup "immutability - Single"
  [ testGroup "toEngByteString" [unaryCF toEngByteString]
  , testGroup "toNumber" [unaryCF toNumber]
  ]
