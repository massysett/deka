module Main where

import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import System.Environment
import Text.Show.Pretty

main :: IO ()
main = do
  name:_ <- getArgs
  pd <- readPackageDescription normal name
  putStr . ppShow $ pd
