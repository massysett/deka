module Dectest.Apply.Apply where

import Data.Char (toLower)
import qualified Data.ByteString.Char8 as BS8
import qualified Deka.Context as C
import Data.Functor.Identity
import Data.List (find)
import qualified Dectest.Interp.Result as R
import Dectest.Interp.Operand
import Dectest.Interp.Octothorpe (WhichPrecision)
import Data.List (sort)
import Dectest.Apply.Types

-- | Given the name of a test and an Alist of test functions, picks
-- the test to apply.
pickTestFn
  :: BS8.ByteString
  -- ^ Name of property being tested
  -> [(BS8.ByteString, ApplyTest a)]
  -- ^ A-list of property names and test functions
  -> Either Bypass (ApplyTest a)
pickTestFn n ls = case find pdct ls of
  Nothing -> Left Skip
  Just (_, f) -> Right f
  where
    pdct (tn, _) = lower tn == lower n
    lower = map toLower . BS8.unpack

-- | Given a list of operands, parse them.  Bypasses if any of the
-- operands are null.
interpOperands
  :: Operand a
  => [BS8.ByteString]
  -> Either Bypass [WhichPrecision -> C.Ctx a]
interpOperands bss = case mapM operand bss of
  Nothing -> Left Null
  Just rs -> return rs

-- | Given a test function and a list of parsed operands, apply the
-- test function to the operands.  Bypass if there is an operand type
-- mismatch.

applyOperands
  :: ApplyTest a
  -> [WhichPrecision -> C.Ctx a]
  -> Either Bypass (C.Ctx a)
applyOperands f = maybe (Left OperandMismatch) return . f

-- | Given a ByteString for the target test result, return a
-- function that, when applied to the result of applying the test
-- function, indicates success or failure.

interpResult
  :: R.Result a
  => BS8.ByteString
  -> Either Bypass (a -> C.Ctx Bool)
interpResult = maybe (Left Null) return . R.result

-- | Given the result of a computation and how to determine whether
-- it succeeded or failed, make the determination.

applyResult
  :: R.ToByteString a
  => Directives Identity
  -- ^ Initial directives
  -> (a -> C.Ctx Bool)
  -- ^ How to determine whether test succeeded or failed
  -> [C.Flag]
  -- ^ Desired ending flags
  -> C.Ctx a
  -- ^ How to calculate final result
  -> Either Bypass ()
applyResult d getCmp gs getRes =
  let (r, fl, str) = C.runCtx C.initQuad $ do
        applyDirectives d
        res <- getRes
        cmp <- getCmp res
        flgs <- C.getStatus
        let bs = R.toByteString res
        return (cmp, flgs, bs)
  in if sort fl /= sort gs || not r
      then Left (Fail str fl) else return ()

multif :: a -> [(Bool, a)] -> a
multif a [] = a
multif a ((b, r):xs)
  | b = r
  | otherwise = multif a xs

applyDirectives :: Directives Identity -> C.Ctx ()
applyDirectives ds = do
  C.setPrecision . runIdentity . precision $ ds
  C.setRound . runIdentity . rounding $ ds
  C.setEmax . runIdentity . emax $ ds
  C.setEmin . runIdentity . emin $ ds
  C.setExtended . extended $ ds
  C.setClamp . clamp $ ds

data TestInputs a = TestInputs
  { inName :: BS8.ByteString
  , inOperands :: [BS8.ByteString]
  , inResult :: BS8.ByteString
  , inFlags :: [C.Flag]
  } deriving Show

{-

  1. pickTestFn, store output
  2. interpOperands, store output
  3. Using results from 1 and 2, applyOperands, store output
  4. interpResult, store output
  5. applyResult using output from 3, 4

-}

applyTest
  :: (Operand a, R.Result a, R.ToByteString a)
  => [(BS8.ByteString, ApplyTest a)]
  -- ^ A-list of property names and test functions
  -> Directives Identity
  -> TestInputs a
  -> Maybe Bypass
  -- ^ Nothing if test succeeds; Just Bypass if the test did not
  -- succeed for some reason (does not necessarily indicate failure)
applyTest lkp ds inp = either Just (const Nothing) $ do
  testFn <- pickTestFn (inName inp) lkp
  ops <- interpOperands (inOperands inp)
  fnOut <- applyOperands testFn ops
  trpRslt <- interpResult (inResult inp)
  applyResult ds trpRslt (inFlags inp) fnOut


