module Dectest.Apply.Types where

import qualified Data.ByteString.Char8 as BS8
import qualified Deka.Context as C
import Data.Functor.Identity
import Data.List (find)
import Dectest.Interp.Result
import Dectest.Interp.Operand

data Result
  = Skip
  -- ^ No suitable test for this operation; skip the test
  | Null
  -- ^ One of the operands was null; skip the test
  | Pass
  | Fail BS8.ByteString
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
  = [C.Ctx (WhichPrecision -> a)]
  -- ^ Operands
  -> Maybe (C.Ctx a)
  -- ^ Return Nothing if there is an operand type mismatch;
  -- otherwise, return the result of the function

applyTest
  :: (Operand a, Result a, ToByteString a)
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
  fn <- maybe (Left Skip) return $ find pdct ls
  kos <- maybe (Left Null) return $ mapM operand os
  getR <- maybe (Left Null) return $ result rslt
  compR <- maybe (Left OperandMismatch) return $ fn kos
  let comp = do
        applyDirectives dir
        n <- compR
        compR n
      (r, rFl) = runCtxStatus initQuad comp
      bs = toByteString r
  maybe 

case find pdct ls of
  Nothing -> Skip
  Just fn -> case mapM operand os of
    Just kos -> case result rslt of
      Nothing -> 
