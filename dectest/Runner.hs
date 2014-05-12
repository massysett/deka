{-# LANGUAGE OverloadedStrings #-}
module Runner where

import qualified Parse as P
import qualified Data.ByteString.Char8 as BS8
import Data.Monoid
import Data.List (intersperse)
import System.Exit
import Data.Foldable (toList)
import qualified Data.Sequence as S

data Counts = Counts
  { nPass :: !Int
  , nFail :: !Int
  , nSkip :: !Int
  } deriving Show

showCounts :: Counts -> String
showCounts (Counts p f s) = unlines
  [ "pass: " ++ show p
  , "fail: " ++ show f
  , "skip: " ++ show s
  , "total: " ++ show (p + f + s)
  ]

exit :: Counts -> IO ()
exit c
  | nFail c > 0 = exitFailure
  | otherwise = exitSuccess

showResult
  :: P.TestSpec
  -> (Maybe Bool, S.Seq BS8.ByteString)
  -> BS8.ByteString
showResult t (r, sq) = BS8.unlines $ l1:lr
  where
    l1 = pf <+> showSpec t
    pf = case r of
      Nothing -> "[skip]"
      Just True -> "[pass]"
      Just False -> "[FAIL]"
    lr = map ("    " <>) . toList $ sq

showSpec :: P.TestSpec -> BS8.ByteString
showSpec t =
  P.testId t
  <+> P.testOperation t
  <+> BS8.concat (intersperse " " . P.testOperands $ t)
  <+> "->"
  <+> P.testResult t
  <+> BS8.concat (intersperse " " . P.testConditions $ t)

(<+>) :: BS8.ByteString -> BS8.ByteString -> BS8.ByteString
l <+> r
  | BS8.null l && BS8.null r = ""
  | BS8.null l || BS8.null r = l <> r
  | otherwise = l <> " " <> r

