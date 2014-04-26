module Main where

import Dectest.Parse
import System.Environment

main :: IO ()
main = do
  a:_ <- getArgs
  is <- parseFile a
  mapM_ putStrLn . map show $ is
