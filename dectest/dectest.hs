module Main where

import Runner
import System.Environment
import qualified Data.ByteString.Char8 as BS8

main :: IO ()
main = fmap (map BS8.pack) getArgs >>= runAndExit
