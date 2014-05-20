{-# LANGUAGE OverloadedStrings #-}
module Parse.Tokens
  ( Raw(unRaw)
  , Line(..)
  , T.Token(..)
  , splitLines
  , processLine
  , raw
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Parse.Tokenizer as T

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

data HighLevelLine = HighLevelLine
  { llState :: T.Tokenizer
  , llToks :: [T.Token]
  } deriving Show

highLevelProc :: HighLevelLine -> Char -> HighLevelLine
highLevelProc h c = HighLevelLine l' ts'
  where
    (l', mayT) = T.feed c (llState h)
    ts' = case mayT of
      Nothing -> llToks h
      Just t' -> t' : llToks h

eject :: HighLevelLine -> [T.Token]
eject h = reverse toks
  where
    toks = case T.finish (llState h) of
      Nothing -> llToks h
      Just t -> t : llToks h

processLine :: Line -> [T.Token]
processLine
  = eject
  . BS8.foldl highLevelProc z
  . unLine
  where
    z = HighLevelLine T.start []

_testProcessLine :: String -> IO ()
_testProcessLine
  = mapM_ BS8.putStrLn
  . map T.unToken
  . processLine
  . Line
  . BS8.pack

