module Main where

data NArgs = One | Two | Three | Round

data Function = Function NArgs String

type WithCtx = Bool

un :: String -> Function
un = Function One

bi :: String -> Function
bi = Function Two

tri :: String -> Function
tri = Function Three

rnd :: String -> Function
rnd = Function Round

ctxFree :: [Function]
ctxFree =
  [ un "decClass"
  , un "isNaN"
  , bi "compareOrd"
  , bi "compareTotalMag"
  , bi "copySign"
  , un "digits"
  ] ++ map un (words $ "isFinite isInfinite isInteger isLogical "
                ++ "isNormal isSignaling isSigned isSubnormal") ++
  [ bi "sameQuantum"
  , un "toEngByteString"
  ]

withCtx :: [Function]
withCtx =
  [ un "abs" ]
  ++ map bi (words $ "add and compare compareSignal divide "
    ++ "divideInteger")
  ++
  [ tri "fma"
  , un "invert"
  , un "logB"
  , bi "max"
  , bi "maxMag"
  , bi "min"
  , bi "minMag"
  , un "minus"
  , bi "multiply"
  , un "nextMinus"
  , un "nextPlus"
  , bi "nextToward"
  , bi "or"
  , un "plus"
  , bi "quantize"
  , un "reduce"
  , bi "remainder"
  , bi "remainderNear"
  , bi "rotate"
  , bi "scaleB"
  , bi "shift"
  , bi "subtract"
  , rnd "toInt32"
  , rnd "toInt32Exact"
  , un "toIntegralExact"
  , rnd "toIntegralValue"
  , rnd "toUInt32"
  , rnd "toUInt32Exact"
  , bi "xor"
  ]

makeTest :: (WithCtx, Function) -> String
makeTest (ctx, fn) =
  "testGroup \"" ++ name ++ "\" [" ++ fnName ++ " " ++ name ++ "]"
  where
    Function nas name = fn
    fnName = baseName ++ suf
    suf | ctx = ""
        | otherwise = "CF"
    baseName = case nas of
      One -> "unary"
      Two -> "binary"
      Three -> "ternary"
      Round -> "round"

allTests :: String
allTests = unlines $
  [ "tests :: TestTree"
  , "tests = testGroup \"immutability\""
  , "  [ " ++ ln1
  ] ++ (map ("  , " ++) lns) ++ ["  ]"]
  where
    ln1:lns = map makeTest allLines
    allLines = zip (repeat True) withCtx
      ++ zip (repeat False) ctxFree

main :: IO ()
main = putStr allTests
