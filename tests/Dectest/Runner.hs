{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Dectest.Runner where

import qualified Deka.Context as C
import qualified Dectest.Parse as P
import qualified Dectest.Apply.Types as Y
import qualified Dectest.Apply.Apply as A
import qualified Data.ByteString.Char8 as BS8
import Data.Functor.Identity
import Data.Char (toLower)
import Data.Maybe
import qualified Dectest.Interp.Operand as O
import qualified Dectest.Interp.Result as R
import Data.Monoid
import Data.List (intersperse)
import System.Exit
import qualified Dectest.Lookup.Single as LS
import qualified Dectest.Lookup.Double as LD
import qualified Dectest.Lookup.Quad as LQ
import qualified Dectest.Lookup.Dec as LN
import Dectest.Log
import Control.Monad.Trans.Class
import Control.Monad


data Item = Item
  { itemFile :: BS8.ByteString
  , itemDesc :: ItemDesc
  }

data TestResult = TestResult
  { trSpec :: P.TestSpec
  , trDirectives :: Y.Directives Identity
  -- ^ Directives in force when test was run
  , trResult :: Maybe Y.Bypass
  -- ^ Nothing if passed; Just if not passed.
  }

data ItemDesc
  = SwitchFiles
  -- ^ Switch files
  | Directive P.Keyword P.Value
  -- ^ Process directive; gives keyword and value of directive
  | Result TestResult
  -- ^ Gives the result of a test

runTests
  :: (O.Operand a, R.Result a, R.ToByteString a)
  => Bool
  -- ^ If True, remove clamped, lost_digits, rounded, subnormal from
  -- list of expxected flags.
  -> [(BS8.ByteString, Y.ApplyTest a)]
  -> P.File
  -> [Item]
runTests rmve lkp f
  = Item (P.fileName f) SwitchFiles
  : go Y.blankDirectives (P.fileContents f)
  where
    go _ [] = []
    go d (x:xs) = case x of
      Left file -> runTests rmve lkp file ++ go d xs
      Right ixn -> case ixn of
        P.Blank -> go d xs
        P.Directive k v ->
          (Item (P.fileName f) (Directive k v))
          : go (addDirective k v d) xs
        P.Test ts -> (Item (P.fileName f) (Result tr)) : go d xs
          where
            tr = TestResult ts dirs res
            res = A.applyTest lkp dirs (specToInputs rmve ts)
            dirs = Y.setDirectives d

data Counts = Counts
  { nPass :: !Int
  , nFail :: !Int
  , nSkip :: !Int
  } deriving Show

showCounts :: Counts -> IO ()
showCounts (Counts p f s) = do
  putStrLn ""
  putStrLn $ "pass: " ++ show p
  putStrLn $ "fail: " ++ show f
  putStrLn $ "skip: " ++ show s
  putStrLn $ "total: " ++ show (p + f + s)

exit :: Counts -> IO ()
exit c
  | nFail c > 0 = exitFailure
  | otherwise = exitSuccess

singleTests :: BS8.ByteString
singleTests = "decSingle.decTest"

doubleTests :: BS8.ByteString
doubleTests = "decDouble.decTest"

quadTests :: BS8.ByteString
quadTests = "decQuad.decTest"

numTests :: [BS8.ByteString]
numTests = ["testall.decTest", "testall0.decTest"]

testList :: [Counts -> Log IO Counts]
testList =
  [ parseAndTest True LS.testLookups singleTests
  , parseAndTest True LD.testLookups doubleTests
  , parseAndTest True LQ.testLookups quadTests
  ] ++ zipWith (parseAndTest False) (repeat LN.testLookups) numTests

runAllTests :: IO ()
runAllTests = do
  (cnts, _) <- runLog $ runTestList testList
  showCounts cnts
  exit cnts

runTestList :: Monad m => [Counts -> m Counts] -> m Counts
runTestList ls = go ls (Counts 0 0 0)
  where
    go list !c = case list of
      [] -> return c
      x:xs -> do
        c' <- x c
        go xs c'

parseAndTest
  :: (O.Operand a, R.Result a, R.ToByteString a)
  => Bool
  -- ^ If True, remove clamped, lost_digits, rounded, and subnormal
  -- from the list of expected flags.
  -> [(BS8.ByteString, Y.ApplyTest a)]
  -> BS8.ByteString
  -- ^ File name
  -> Counts
  -> Log IO Counts
parseAndTest rmve lkp n c = do
  f <- P.parseFile n
  lift $ printItems c (runTests rmve lkp f)

printItems :: Counts -> [Item] -> IO Counts
printItems cs is = go cs is
  where
    go !c [] = return c
    go !c (x:xs) =
      BS8.putStrLn (showItem x) 
      >> go (increment x c) xs

increment :: Item -> Counts -> Counts
increment i c = case itemDesc i of
  SwitchFiles -> c
  Directive _ _ -> c
  Result r -> case trResult r of
    Nothing -> c { nPass = succ (nPass c) }
    Just y -> case y of
      Y.Skip -> c { nSkip = succ (nSkip c) }
      Y.Null -> c { nSkip = succ (nSkip c) }
      Y.Fail _ _ -> c { nFail = succ (nFail c) }
      Y.OperandMismatch -> c { nFail = succ (nFail c) }


showItem :: Item -> BS8.ByteString
showItem i = case itemDesc i of
  SwitchFiles -> "load file: " <> itemFile i
  Directive k v -> "directive: " <> P.unKeyword k
    <> " value: " <> P.unValue v
  Result r -> showResult r

showResult :: TestResult -> BS8.ByteString
showResult r = showTestResult (trResult r) <> " "
  <> showSpec (trSpec r)

showTestResult :: Maybe Y.Bypass -> BS8.ByteString
showTestResult m = case m of
  Nothing -> "pass"
  Just y -> case y of
    Y.Skip -> "skip"
    Y.Null -> "null input; skip"
    Y.Fail d fs -> "FAIL actual result:" <+> d
      <+> showFlags fs
    Y.OperandMismatch -> "OPERAND MISMATCH"

showFlags :: [C.Flag] -> BS8.ByteString
showFlags = BS8.pack . concat . intersperse ", " . map show

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

-- | Converts a TestSpec to TestInputs.  Removes informational flags
-- if requested - see documentation for the decContext module, which
-- states that clamped, lost_digits, rounded, and subnormal are NOT
-- reported for any of the decFloats modules.

specToInputs
  :: Bool
  -- ^ If True, remove clamped, lost_digits, rounded, and subnormal
  -- from the list of expected flags.
  -> P.TestSpec
  -> A.TestInputs
specToInputs rmve t = A.TestInputs
  { A.inName = P.testOperation t
  , A.inOperands = P.testOperands t
  , A.inResult = P.testResult t
  , A.inFlags = filt . map toFlag . P.testConditions $ t
  }
  where
    filt | rmve = filter (not . (`elem` infoFlags))
         | otherwise = id
    infoFlags = [ C.clamped, C.lostDigits, C.rounded, C.subnormal ]
    toFlag str
      | s == "clamped" = C.clamped
      | s == "conversion_syntax" = C.conversionSyntax
      | s == "division_by_zero" = C.divisionByZero
      | s == "division_impossible" = C.divisionImpossible
      | s == "division_undefined" = C.divisionUndefined
      | s == "inexact" = C.inexact
      | s == "insufficient_storage" = C.insufficientStorage
      | s == "invalid_context" = C.invalidContext
      | s == "invalid_operation" = C.invalidOperation
      | s == "lost_digits" = C.lostDigits
      | s == "overflow" = C.overflow
      | s == "rounded" = C.rounded
      | s == "subnormal" = C.subnormal
      | s == "underflow" = C.underflow
      | otherwise = error "specToInputs: could not read flag"
      where
        s = map toLower . BS8.unpack $ str

addDirective
  :: P.Keyword
  -> P.Value
  -> Y.Directives Maybe
  -> Y.Directives Maybe
addDirective (P.Keyword kw) (P.Value val) d
  | k == "precision" =
      let prec = fromMaybe
            (error $ "could not set precision: " ++ v) $ do
              n <- readNumber v
              C.precision n
      in d { Y.precision = Just prec }

  | k == "rounding" =
      let r | v == "ceiling" = C.roundCeiling
            | v == "down" = C.roundDown
            | v == "floor" = C.roundFloor
            | v == "half_down" = C.roundHalfDown
            | v == "half_even" = C.roundHalfEven
            | v == "half_up" = C.roundHalfUp
            | v == "up" = C.roundUp
            | v == "05up" = C.round05Up
            | otherwise = error "addDirective: rounding not recognized"
      in d { Y.rounding = Just r }

  | k == "maxexponent" =
      let ex = fromMaybe
            (error $ "could not set max exponent: " ++ v) $ do
              n <- readNumber v
              C.emax n
      in d { Y.emax = Just ex }

  | k == "minexponent" =
      let ex = fromMaybe
            (error $ "could not set min exponent: " ++ v) $ do
              n <- readNumber v
              C.emin n
      in d { Y.emin = Just ex }

  | k == "extended" =
      let b = case safeRead v of
            Nothing -> error $ "could not parse extended: " ++ v
            Just r
              | r == (0 :: Int) -> False
              | r == 1 -> True
              | otherwise -> error $ "could not parse extended: " ++ v
      in d { Y.extended = b }

  | k == "clamp" =
      let b = case safeRead v of
            Nothing -> error $ "could not parse clamp: " ++ v
            Just r
              | r == (0 :: Int) -> False
              | r == 1 -> True
              | otherwise -> error $ "could not parse clamp: " ++ v
      in d { Y.clamp = b }

  | otherwise = d

  where
    k = map toLower . BS8.unpack $ kw
    v = BS8.unpack val

safeRead :: Read a => String -> Maybe a
safeRead a = case reads a of
  (x, ""):[] -> Just x
  _ -> Nothing

readNumber :: Read a => String -> Maybe a
readNumber a = case a of
  [] -> Nothing
  x:xs -> if x == '+' then safeRead xs else safeRead (x:xs)
