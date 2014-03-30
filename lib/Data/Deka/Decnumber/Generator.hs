module Main where

import Text.Parsec hiding (optional, space)
import Control.Applicative (optional)
import Text.Parsec.String

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

-- # Parsec

prototype :: Parser (String, String, [String])
prototype = do
  _ <- many space
  rtn <- typeDecl
  _ <- many space
  fn <- name
  _ <- char '('
  args <- typeDecl `sepBy` (string ", ")
  _ <- string ");\n"
  return (rtn, fn, args)

typeDecl :: Parser String
typeDecl = try enumType <|> otherType

space :: Parser ()
space = char ' ' >> return ()

name :: Parser String
name = many (letter <|> digit <|> (char '_'))

enumType :: Parser String
enumType = do
  _ <- try $ string "enum"
  _ <- space
  n <- name
  return $ "C'" ++ n

otherType :: Parser String
otherType = do
  _ <- optional (try (string "const") >> space)
  n <- name
  _ <- many space
  ptr <- optional (char '*')
  let r = ptrStr ++ "C'" ++ n
      ptrStr = maybe "" (const "Ptr ") ptr
  return r

main :: IO ()
main = do
  c <- readFile "decNumPrototypes.txt"
  parsed <- case parse (many prototype) "" c of
    Left e -> putStrLn (show e) >> fail "parse failed"
    Right g -> return g
  let mkCall (rtn, fn, as) = ccall rtn fn as
  putStr . concat . map mkCall $ parsed
