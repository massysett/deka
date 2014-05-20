module Types where

import qualified Data.ByteString.Char8 as BS8
import qualified Parse as P
import Data.Sequence

-- | All tests must conform to this interface.

type Test

  = Seq (P.Keyword, P.Value)
  -- ^ All directives in force when the test is run.  Oldest
  -- directives are on the left side of the Seq; newest ones, on
  -- the right side.

  -> [BS8.ByteString]
  -- ^ Operands

  -> BS8.ByteString
  -- ^ Result

  -> [BS8.ByteString]
  -- ^ Conditions.  These are already sorted.

  -> (Maybe Bool, Seq BS8.ByteString)
  -- ^ The test returns a Maybe Bool to indicate the test result.
  -- Passage is Just True, failure is Just False, skip is Nothing.
  -- Use Nothing for inputs that the implementation does not
  -- support, such as null operands.  For all other failures
  -- (including programmer errors or test configuration errors such
  -- as mismatches in the number of operands), use Just False.
  --
  -- Also returned is a list of ByteString.  These are narratives.
  -- Put one pice of data in each line.  For example  you might
  -- include information on how the operands parsed, how the result
  -- parsed, what information the test function returned, etc.
