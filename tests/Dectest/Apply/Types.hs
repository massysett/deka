module Dectest.Apply.Types where

import Data.Char (toLower)
import qualified Data.ByteString.Char8 as BS8
import qualified Deka.Context as C
import Data.Functor.Identity
import Data.List (find)
import qualified Dectest.Interp.Result as R
import Dectest.Interp.Operand
import Dectest.Interp.Octothorpe (WhichPrecision)
import Data.List (sort)

data Bypass
  = Skip
  -- ^ No suitable test for this operation; skip the test
  | Null
  -- ^ One of the operands was null; skip the test
  | Fail BS8.ByteString [C.Flag]
  -- ^ Comes with the string representation of the test result
  | OperandMismatch
  -- ^ Test did not come with right number of operands

data Directives a = Directives
  { precision :: a C.Precision
  , rounding :: a C.Round
  , emax :: a C.Emax
  , emin :: a C.Emin
  , extended :: Bool
  , clamp :: Bool
  }

type ApplyTest a
  = [WhichPrecision -> C.Ctx a]
  -- ^ Operands
  -> Maybe (C.Ctx a)
  -- ^ Return Nothing if there is an operand type mismatch;
  -- otherwise, return the result of the function

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

-- | Given the result of a computation and how to apply it, 

{-
applyTest
  :: (Operand a, R.Result a, R.ToByteString a)
  => [(BS8.ByteString, ApplyTest a)]
  -- ^ Association list of operation names and the function that
  -- applies each operation.
  -> Directives Identity
  -- ^ Directives currently in force
  -> BS8.ByteString
  -- ^ Operation name
  -> [BS8.ByteString]
  -- ^ Operands
  -> (BS8.ByteString, [C.Flag])
  -- ^ Result and expected status flags
  -> Result
applyTest ls dir nm os (rslt, fs) = either id id $ do
  let pdct (k, _) = (map toLower . BS8.unpack $ k)
        == (map toLower . BS8.unpack $ nm)
  fn <- maybe (Left Skip) (return . snd) $ find pdct ls
  kos <- maybe (Left Null) return $ mapM operand os
  getR <- maybe (Left Null) return $ R.result rslt
  compR <- maybe (Left OperandMismatch) return $ fn kos
  let comp = do
        applyDirectives dir
        n <- compR
        compR n
      (r, rFl) = C.runCtxStatus C.initQuad comp
      bs = R.toByteString r
      throwFlags | sort rFl == sort fs = Right ()
                 | otherwise = Left (Fail bs rFl)
      throwFail | r = Right ()
                | otherwise = Left (Fail bs rFl)
  throwFlags
  throwFail
  return Pass
-}

applyDirectives = undefined
