module Main where

import Data.Maybe

readLine :: [String] -> Maybe String
readLine ss = case ss of
  [] -> Nothing
  x:y:xs
    | y == "::" -> Just x
    | x == "foreign" -> Just $ last ss
    | otherwise -> Nothing
  _ -> Nothing

printTokens :: [String] -> String
printTokens ss = unlines $ case ss of
  [] -> []
  x:xs -> line1 x : map lineRest xs ++ [lastLine]
  where
    line1 x = "  ( " ++ x
    lineRest x = "  , " ++ x
    lastLine = "  ) where"

process :: String -> String
process = printTokens . mapMaybe readLine . map words . lines

main :: IO ()
main = interact process
