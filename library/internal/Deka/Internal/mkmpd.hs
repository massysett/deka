module Main where

unaries :: [String]
unaries =
  [ "copy"
  , "copy_abs"
  , "copy_negate"
  , "invert"
  , "logb"
  , "abs"
  , "exp"
  , "ln"
  , "log10"
  , "minus"
  , "next_minus"
  , "next_plus"
  , "plus"
  , "reduce"
  , "round_to_intx"
  , "round_to_int"
  , "trunc"
  , "floor"
  , "ceil"
  , "sqrt"
  , "invroot"
  ]

binaries :: [String]
binaries =
  [ "and"
  , "copy_sign"
  , "or"
  , "rotate"
  , "scaleb"
  , "shift"
  , "xor"
  , "compare"
  , "compare_signal"
  , "add"
  , "sub"
  , "div"
  , "divint"
  , "max"
  , "max_mag"
  , "min"
  , "min_mag"
  , "mul"
  , "next_toward"
  , "pow"
  , "quantize"
  , "rem"
  , "rem_near"
  ]

tests :: [String]
tests =
  [ "finite"
  , "infinite"
  , "integer"
  , "nan"
  , "negative"
  , "positive"
  , "qnan"
  , "signed"
  , "zero"
  , "zerocoeff"
  , "oddcoeff"
  , "odd"
  , "even"
  ]

mpd :: String -> String
mpd = ("mpd_" ++)

cprime :: String -> String
cprime = ("c'mpd_" ++)

quote :: String -> String
quote s = "\"" ++ s ++ "\""

indent :: String -> String
indent = ("  " ++)

signature :: [String] -> [String]
signature ss = case ss of
  [] -> []
  x:[] -> line1 x : [""]
  x:xs -> line1 x : map lineRest xs ++ [""]
  where
    line1 x = indent ("::" <+> x)
    lineRest x = indent ("->" <+> x)

ptr :: String -> String
ptr = ("Ptr" <+>)

(<+>) :: String -> String -> String
l <+> r
  | null l && null r = ""
  | not (null l) && not (null r) = l ++ " " ++ r
  | otherwise = l ++ r


fimport :: String -> String
fimport n = "foreign import ccall unsafe"
  <+> quote (mpd n) <+> cprime n

mkunary :: String -> String
mkunary s = unlines $
  fimport s
  : signature [ ptr "C'mpd_t", ptr "C'mpd_t", ptr "C'mpd_context_t",
                "IO ()" ]

mkbinary :: String -> String
mkbinary s = unlines $
  fimport s
  : signature [ ptr "C'mpd_t", ptr "C'mpd_t",
                ptr "C'mpd_t",
                ptr "C'mpd_context_t",
                "IO ()" ]

mktest :: String -> String
mktest s = unlines $
  fimport ("is" ++ s)
  : signature [ ptr "C'mpd_t", "IO CInt" ]

main :: IO ()
main = do
  mapM_ putStr . map mkunary $ unaries
  mapM_ putStr . map mkbinary $ binaries
  mapM_ putStr . map mktest $ tests

-- skipped: shiftl, shiftn, cmp, divmod, fma, powmod, rescale
-- adjexp, etiny, etop, msword, word_digits, exp_digits
-- isnormal, issubnormal, sign, arith_sign, radix
