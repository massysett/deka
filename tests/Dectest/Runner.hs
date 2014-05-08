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
  => [(BS8.ByteString, Y.ApplyTest a)]
  -> P.File
  -> [Item]
runTests lkp f
  = Item (P.fileName f) SwitchFiles
  : go Y.blankDirectives (P.fileContents f)
  where
    go _ [] = []
    go d (x:xs) = case x of
      Left file -> runTests lkp file ++ go d xs
      Right ixn -> case ixn of
        P.Blank -> go d xs
        P.Directive k v ->
          (Item (P.fileName f) (Directive k v))
          : go (addDirective k v d) xs
        P.Test ts -> (Item (P.fileName f) (Result tr)) : go d xs
          where
            tr = TestResult ts dirs res
            res = A.applyTest lkp dirs (specToInputs ts)
            dirs = Y.setDirectives d

specToInputs :: P.TestSpec -> A.TestInputs
specToInputs t = A.TestInputs
  { A.inName = P.testOperation t
  , A.inOperands = P.testOperands t
  , A.inResult = P.testResult t
  , A.inFlags = map toFlag . P.testConditions $ t
  }
  where
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
              n <- case reads v of
                (x, ""):[] -> return x
                _ -> Nothing
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
              n <- case reads v of
                (x, ""):[] -> return x
                _ -> Nothing
              C.emax n
      in d { Y.emax = Just ex }

  | k == "minexponent" =
      let ex = fromMaybe
            (error $ "could not set min exponent: " ++ v) $ do
              n <- case reads v of
                (x, ""):[] -> return x
                _ -> Nothing
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
