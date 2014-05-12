{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Runner (runAndExit) where

import qualified Parse as P
import qualified Data.ByteString.Char8 as BS8
import Data.Monoid
import Data.List (intersperse)
import System.Exit
import Data.Foldable (toList)
import qualified Data.Sequence as S
import Data.Sequence ((|>))
import Pipes
import Pipes.Prelude (fold)
import NumTests (testLookups)
import Types

produceFile :: MonadIO m => Pipe BS8.ByteString P.File m ()
produceFile = do
  bs <- await
  f <- liftIO $ P.parseFile bs
  yield f

order :: Monad m => Pipe P.File Order m ()
order = do
  f <- await
  go S.empty (P.fileContents f)
  where
    go !sq ls = case ls of
      [] -> return ()
      x:xs -> case x of
        Left inner -> go S.empty (P.fileContents inner)
        Right i -> case i of
          P.Blank -> go sq xs
          P.Directive k v ->
            let sq' = sq |> (k, v)
            in go sq' xs
          P.Test spec -> do
            yield (Order sq spec)
            go sq xs

output :: Monad m => Pipe Order TestOutput m ()
output = do
  o <- await
  case lookup (P.testOperation . ordSpec $ o) testLookups of
    Nothing -> yield (noOperation . ordSpec $ o)
    Just t -> yield (runTest o t)
  output


printTest
  :: MonadIO m
  => Pipe TestOutput (Maybe Bool) m ()
printTest = do
  o <- await
  liftIO . BS8.putStr . showResult $ o
  yield (outResult o)
  printTest

pipeline :: MonadIO m => Pipe BS8.ByteString (Maybe Bool) m ()
pipeline =
  produceFile
  >-> order
  >-> output
  >-> printTest

totals :: Monad m => Producer (Maybe Bool) m () -> m Counts
totals = fold tally mempty id

runAndExit :: [String] -> IO ()
runAndExit ss = do
  let bs = map BS8.pack ss
      pip = each bs >-> pipeline
  tot <- totals pip
  putStr . showCounts $ tot
  exit tot

noOperation :: P.TestSpec -> TestOutput
noOperation ts = TestOutput
  { outSpec = ts
  , outResult = Nothing
  , outLog = S.singleton "no matching operation; skipping"
  }

data Order = Order
  { _ordDirectives :: S.Seq (P.Keyword, P.Value)
  , ordSpec :: P.TestSpec
  }

runTest
  :: Order
  -> Test
  -> TestOutput
runTest (Order ds ts) t = TestOutput
  { outSpec = ts
  , outResult = r
  , outLog = l
  }
  where
    (r, l) = t ds (P.testOperands ts) (P.testResult ts)
      (P.testConditions ts)

data TestOutput = TestOutput
  { outSpec :: P.TestSpec
  , outResult :: Maybe Bool
  , outLog :: S.Seq BS8.ByteString
  }

data Counts = Counts
  { _nPass :: !Int
  , nFail :: !Int
  , _nSkip :: !Int
  } deriving Show

tally :: Counts -> Maybe Bool -> Counts
tally (Counts p f s) mb = case mb of
  Nothing -> Counts p f (s + 1)
  Just True -> Counts (p + 1) f s
  Just False -> Counts p (f + 1) s

instance Monoid Counts where
  mempty = Counts 0 0 0
  (Counts x1 y1 z1) `mappend` (Counts x2 y2 z2) =
    Counts (x1 + x2) (y1 + y2) (z1 + z2)

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
  :: TestOutput
  -> BS8.ByteString
showResult (TestOutput t r sq) = BS8.unlines $ l1:lr
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

