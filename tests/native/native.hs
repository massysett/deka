{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import AllModules
import Properties (tests)
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain tests
