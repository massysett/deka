module Main where

binary
  :: String
  -- ^ Last part of C name
  -> String
  -- ^ Haskell name
  -> String

binary c h = unlines
  [ h ++ " :: DecNum -> DecNum -> Ctx DecNum"
  , h ++ " = binary c'decNumber" ++ c
  , ""
  ]

ternary :: String -> String -> String
ternary c h = unlines
  [ h ++ " :: DecNum -> DecNum -> DecNum -> Ctx DecNum"
  , h ++ " = ternary c'decNumber" ++ c
  , ""
  ]

unary :: String -> String -> String
unary c h = unlines
  [ h ++ " :: DecNum -> Ctx DecNum"
  , h ++ " = unary c'decNumber" ++ c
  , ""
  ]

fns :: [String]
fns =
  [ binary "And" "and"
  , binary "Compare" "compare"
  , binary "CompareSignal" "compareSignal"
  , binary "CompareTotal" "compareTotal"
  , binary "CompareTotalMag" "compareTotalMag"
  , binary "Divide" "divide"
  , binary "DivideInteger" "divideInteger"
  , unary "Exp" "exp"
  , ternary "FMA" "fma"
  , unary "Invert" "invert"
  , unary "Ln" "ln"
  , unary "LogB" "logB"
  , unary "Log10" "log10"
  , binary "Max" "max"
  , binary "MaxMag" "maxMag"
  , binary "Min" "min"
  , binary "MinMag" "minMag"
  , unary "Minus" "minus"
  , binary "Multiply" "multiply"
  , unary "Normalize" "normalize"
  , binary "Or" "or"
  , unary "Plus" "plus"
  , binary "Power" "power"
  , binary "Quantize" "quantize"
  , unary "Reduce" "reduce"
  , binary "Remainder" "remainder"
  , binary "RemainderNear" "remainderNear"
  , binary "Rescale" "rescale"
  , binary "Rotate" "rotate"
  -- skip: sameQuantum
  , binary "ScaleB" "scaleB"
  , binary "Shift" "shift"
  , unary "SquareRoot" "squareRoot"
  , binary "Subtract" "subtract"
  , unary "ToIntegralExact" "toIntegralExact"
  , unary "ToIntegralValue" "toIntegralValue"
  , binary "Xor" "xor"
  -- need: Class, ClassToString, Copy, CopyAbs, CopyNegate, CopySign
  , unary "NextMinus" "nextMinus"
  , unary "NextPlus" "nextPlus"
  , binary "NextToward" "nextToward"
  -- skip: trim?
  -- need: version, zero, everything after isNormal
  ]

main :: IO ()
main = mapM_ putStr fns
