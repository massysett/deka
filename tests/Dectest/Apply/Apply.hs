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
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class

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
  -> Either Bypass (C.Ctx (BS8.ByteString, BS8.ByteString -> Maybe (C.Ctx Bool)))
applyOperands f = maybe (Left OperandMismatch) return . f

-- | Given the function that determines how to interpret a result
-- operand, and the result token, compute the result if possible.
interpResult
  :: (C.Ctx (BS8.ByteString, BS8.ByteString -> Maybe (C.Ctx Bool)))
  -> BS8.ByteString
  -> EitherT Bypass C.Ctx (BS8.ByteString, Bool)
interpResult f bs = do
  (shw, getMay) <- lift f
  let mayGetBool = getMay bs
  getBool <- case mayGetBool of
    Nothing -> left Null
    Just may -> return may
  bl <- lift getBool
  return (shw, bl)

-- | Given the result of a computation and how to determine whether
-- it succeeded or failed, make the determination.

applyResult
  :: Directives Identity
  -- ^ Initial directives
  -> [C.Flag]
  -- ^ Desired ending flags
  -> EitherT Bypass C.Ctx (BS8.ByteString, Bool)
  -- ^ How to calculate final result, and how to show the result of
  -- the computation in case it failed
  -> Either Bypass ()
applyResult d gs getRes = C.runCtx C.initQuad . runEitherT $ do
  lift $ applyDirectives d
  (bs, res) <- getRes
  flgs <- lift C.getStatus
  if sort flgs /= sort gs || not res
      then left $ Fail bs flgs else return ()

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

data TestInputs = TestInputs
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
  -> TestInputs
  -> Maybe Bypass
  -- ^ Nothing if test succeeds; Just Bypass if the test did not
  -- succeed for some reason (does not necessarily indicate failure)
applyTest lkp ds inp = either Just (const Nothing) $ do
  testFn <- pickTestFn (inName inp) lkp
  ops <- interpOperands (inOperands inp)
  apOp <- applyOperands testFn ops
  let trpRslt = interpResult apOp (inResult inp)
  applyResult ds (inFlags inp) trpRslt

