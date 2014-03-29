module Main where

ccall
  :: String
  -- ^ Return type
  -> String
  -- ^ C function name
  -> [String]
  -- ^ Argument types
  -> String
ccall rtn fn as =
  line1 ++ args ++ rslt ++ "\n"
  where
    line1 = "foreign import ccall unsafe \"" ++
      fn ++ "\" c'" ++ fn ++ "\n"
    args = concat $ zipWith mkArg as ("::" : repeat "->")
    mkArg a start = "  " ++ start ++ " " ++ a ++ "\n"
    rslt = "  " ++ start ++ " " ++ rtn ++ "\n"
      where
        start | null as = "::"
              | otherwise = "->"

ctxt = "Ptr C'decContext"
ui32 = "C'uint32_t"
i32 = "C'int32_t"
dc n = "decContext" ++ n

functions =
  [ (ctxt, dc "ClearStatus", [ctxt, ui32])
  , (ctxt, dc "Default", [ctxt, i32])
  , ("C'rounding", dc "GetRounding", [ctxt])
  , (ui32, dc "GetStatus", [ctxt])
  , (ctxt, dc "RestoreStatus", [ctxt, ui32, ui32])
  , (ui32, dc "SaveStatus", [ctxt, ui32])
  , (ctxt, dc "SetRounding", [ctxt, "C'rounding"])
  , (ctxt, dc "SetStatus", [ctxt, ui32])
  , (ctxt, dc "SetStatusFromString", [ctxt, "CString"])
  , (ctxt, dc "SetStatusFromStringQuiet", [ctxt, "CString"])
  , ("CString", dc "SetStatusToString", [ctxt])
  , (i32, dc "TestEndian", ["C'uint8_t"])
  , (ui32, dc "TestSavedStatus", [ui32, ui32])
  , (ui32, dc "TestStatus", [ctxt, ui32])
  , (ctxt, dc "ZeroStatus", [ctxt])
  ]

main :: IO ()
main = putStr . concat . map mkCall $ functions
  where
    mkCall (rtn, fn, as) = ccall rtn fn as
