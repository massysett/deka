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

data File = File
  { fileName :: BS8.ByteString
  , fileContents :: [Either File Instruction]
  } deriving Show

directive :: [Token] -> (Keyword, Value)
directive ts = case ts of
  x:y:[] -> ( Keyword . BS8.init . unToken $ x,
              Value (unToken y) )
  _ -> error "directive: bad token count"

lineToContent :: [Token] -> IO (Either File Instruction)
lineToContent ts
  | null ts = return . Right $ Blank
  | length ts == 2 && kw == "dectest" = fmap Left $ parseFile fn
  | length ts == 2 = return . Right $ Directive akw avl
  | otherwise = return . Right . Test . mkTestSpec $ ts
  where
    (akw@(Keyword kw), avl@(Value val)) = directive ts
    fn = val `BS8.append` ".dectest"
  
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

rawToContent :: Raw -> IO [Either File Instruction]
rawToContent
  = mapM lineToContent
  . map removeComments
  . map processLine
  . splitLines

parseFile :: BS8.ByteString -> IO File
parseFile fn = do
  rw <- raw (BS8.unpack fn)
  ctnt <- rawToContent rw
  return $ File fn ctnt
