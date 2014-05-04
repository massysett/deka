module Dectest.Apply.Types where

import qualified Data.ByteString.Char8 as BS8

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

type RunTest
  = BS8.ByteString
  -- ^ Operation name
  [BS8.ByteString]
  -- ^ Operands
  -> BS8.ByteString
  -- ^ Result
  -> 

