{-# LANGUAGE OverloadedStrings #-}
module Dectest.Parse where

import Dectest.Parse.Tokens
import qualified Data.ByteString.Char8 as BS8

-- | Remove the comments from a line of tokens.  Any unquoted token that
-- starts with two dashes is removed.  Also, any token that comes
-- after such a token is also removed.
removeComments :: [Token] -> [Token]
removeComments toks = go toks
  where
    go [] = []
    go (t:ts)
      | quoted t = t : go ts
      | BS8.take 2 (unToken t) == "--" = []
      | otherwise = t : go ts

newtype Keyword = Keyword { unKeyword :: BS8.ByteString }
  deriving (Eq, Show)

newtype Value = Value { unValue :: BS8.ByteString }
  deriving (Eq, Show)

data TestSpec = TestSpec
  { testId :: BS8.ByteString
  , testOperation :: BS8.ByteString
  , testOperands :: [BS8.ByteString]
  , testResult :: BS8.ByteString
  , testConditions :: [BS8.ByteString]
  } deriving Show

data Instruction
  = Blank
  | Directive Keyword Value
  | Test TestSpec
  deriving Show

lineToInstruction :: [Token] -> Instruction
lineToInstruction ts
  | null ts = Blank
  | length ts == 2 = Directive (Keyword (unToken (head ts)))
                               (Value (unToken (last ts)))
  | otherwise = Test $ mkTestSpec ts

mkTestSpec :: [Token] -> TestSpec
mkTestSpec ts
  | length ts < 5 = error "mkTestSpec: list too short"
  | otherwise = TestSpec
      { testId = unToken . head $ ts
      , testOperation = unToken . head . drop 1 $ ts
      , testOperands = map unToken
          . takeWhile (not . resultsIn)
          . drop 2
          $ ts
      , testResult = unToken
          . safeHead
          . drop 1
          . dropWhile (not . resultsIn)
          . drop 2
          $ ts
      , testConditions = map unToken
          . drop 2
          . dropWhile (not . resultsIn)
          . drop 2
          $ ts
      }
  where
    resultsIn t = unToken t == "->" && not (quoted t)
    safeHead x = case x of
      [] -> error "mkTestSpec: list too short"
      y:_ -> y

parseFile :: FilePath -> IO [Instruction]
parseFile = fmap parse . BS8.readFile
  where
    parse
      = map lineToInstruction
      . map removeComments
      . map processLine
      . splitLines
      . File
