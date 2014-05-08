{-# LANGUAGE OverloadedStrings #-}
module Dectest.Parse.Tokens
  ( Raw(unRaw)
  , Line(..)
  , Token(..)
  , splitLines
  , processLine
  , raw
  ) where

import qualified Data.ByteString.Char8 as BS8
import Prelude hiding (Double)

-- | Raw file, parsed in from disk.
newtype Raw = Raw { unRaw :: BS8.ByteString }
  deriving Show

raw :: FilePath -> IO Raw
raw = fmap Raw . BS8.readFile

-- | A line from a Raw.  Does not contain any newlines.
newtype Line = Line { unLine :: BS8.ByteString }
  deriving Show

-- | Splits a Raw into a list of Line.  First, eliminates any
-- MS-DOS carriage returns (ASCII character 0d).  Then, uses the
-- ByteString lines function.
splitLines :: Raw -> [Line]
splitLines = map Line . BS8.lines . BS8.filter (/= '\r') . unRaw

--
-- Parsing a Line into Tokens
--

-- | Set to True when a single close quote character has been
-- parsed.  Since double quote characters indicates an enclosed
-- quote, we don't know until the next character whether to close
-- the quote or just include a quote in the token.
type Pending = Bool

data QuoteType = Single | Double
  deriving (Eq, Show)

toQuot :: QuoteType -> Char
toQuot Single = '\''
toQuot Double = '"'

data InTok
  = PlainWord
  | Quoted QuoteType Pending
  deriving Show

data LowLevelLine
  = InTok InTok BS8.ByteString
  | BetweenToks
  deriving Show

data Token = Token
  { unToken :: BS8.ByteString
  , quoted :: Bool
  } deriving (Eq, Ord, Show)

lowLevelProc :: Char -> LowLevelLine -> (LowLevelLine, Maybe Token)
lowLevelProc c s = case s of
  InTok tokType curr -> case tokType of

    PlainWord -> case c of
      '"' -> (InTok (Quoted Double False) BS8.empty, tok False)
      '\'' -> (InTok (Quoted Single False) BS8.empty, tok False)
      ' ' -> (BetweenToks, tok False)
      x -> (InTok PlainWord (curr `BS8.snoc` x), Nothing)

    Quoted qType pend
      | pend -> case () of
          _ | c == qt ->
                ( InTok (Quoted qType False) (curr `BS8.snoc` qt),
                  Nothing)
            | otherwise -> case c of
                ' ' -> (BetweenToks, tok True)
                '"' -> (InTok (Quoted Double False) BS8.empty, tok True)
                '\'' -> (InTok (Quoted Single False) BS8.empty, tok True)
                _ -> (InTok PlainWord (BS8.singleton c), tok True)
      | otherwise -> case () of
          _ | c == qt -> (InTok (Quoted qType True) curr, Nothing)
            | otherwise ->
                ( InTok (Quoted qType False) (curr `BS8.snoc` c),
                  Nothing)
      where
        qt = toQuot qType
    where
      tok isQuoted = Just (Token curr isQuoted)

  BetweenToks -> case c of
    ' ' -> (BetweenToks, Nothing)
    '"' -> (InTok (Quoted Double False) BS8.empty, Nothing)
    '\'' -> (InTok (Quoted Single False) BS8.empty, Nothing)
    _ -> (InTok PlainWord (BS8.singleton c), Nothing)


data HighLevelLine = HighLevelLine
  { llState :: LowLevelLine
  , llToks :: [Token]
  } deriving Show

highLevelProc :: HighLevelLine -> Char -> HighLevelLine
highLevelProc h c = HighLevelLine l' ts'
  where
    (l', mayT) = lowLevelProc c (llState h)
    ts' = case mayT of
      Nothing -> llToks h
      Just t' -> t' : llToks h

eject :: HighLevelLine -> [Token]
eject h = case llState h of
  BetweenToks -> llToks h
  InTok t bs -> case t of
    PlainWord -> Token bs False : llToks h
    Quoted _ pnd
      | pnd -> Token bs True : llToks h
      | otherwise -> error "unterminated quote"

processLine :: Line -> [Token]
processLine
  = reverse
  . eject
  . BS8.foldl highLevelProc z
  . unLine
  where
    z = HighLevelLine BetweenToks []

_testProcessLine :: String -> IO ()
_testProcessLine
  = mapM_ BS8.putStrLn
  . map unToken
  . processLine
  . Line
  . BS8.pack

