module Main where

import Runner
import System.Environment

main :: IO ()
main = getArgs >>= runAndExit
