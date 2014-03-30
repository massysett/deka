module Main where

string :: String -> String
string n = unlines
  [ "c'" ++ n ++ " :: IsString a => a"
  , "c'" ++ n ++ " = #const_str " ++ n
  , ""
  ]

number :: String -> String
number n = unlines
  [ "c'" ++ n ++ " :: Num a => a"
  , "c'" ++ n ++ " = #const " ++ n
  , ""
  ]

synonym :: String -> String -> String
synonym a b = "type C'" ++ a ++ " = C'" ++ b ++ "\n"

decls :: [String]
decls =
  [ string "DECNAME"
  , string "DECFULLNAME"
  , string "DECAUTHOR"
  , number "DECNEG"
  , number "DECINF"
  , number "DECNAN"
  , number "DECSNAN"
  , number "DECSPECIAL"
  , number "DECDPUN"
  , number "DECNUMDIGITS"
  , number "DECNUMUNITS"
  ]

main :: IO ()
main = putStr . concat $ decls
