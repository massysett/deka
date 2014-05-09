module Main where

import Dectest.Parse
import System.Environment
import qualified Data.ByteString.Char8 as BS8
import Text.Show.Pretty

main :: IO ()
main = do
  a:_ <- getArgs
  is <- parseFile . BS8.pack $ a
  putStrLn . ppShow $ is
