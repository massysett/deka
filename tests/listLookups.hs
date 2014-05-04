-- | Shows all Dectest lookup lists.
module Main where

import Dectest.Lookup.Types
import Data.List (sort)
import Data.Maybe (catMaybes)
import qualified Dectest.Lookup.Double as D
import qualified Dectest.Lookup.Quad as Q

sortRecords :: [Record a] -> [String]
sortRecords
  = sort
  . catMaybes
  . map recTestName

showRecord :: String -> IO ()
showRecord s = putStrLn $ "    " ++ s

showRecords :: String -> [Record a] -> IO ()
showRecords n rs = do
  putStrLn $ n ++ ":"
  mapM_ showRecord . sortRecords $ rs
  putStrLn ""

main :: IO ()
main =
  showRecords "Double" D.functions
  >> showRecords "Quad" Q.functions
