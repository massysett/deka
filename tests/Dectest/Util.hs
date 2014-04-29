module Dectest.Util where

import Foreign.Safe
import qualified Data.ByteString as BS
import qualified Deka.Internal.Decnumber.DecNumber as D
import Deka.Internal.DecNum.Util

-- | Compare two DecNumber to see if they are equal.  All DecNumber
-- are always canonical.
equalDecNumber :: Ptr D.C'decNumber -> Ptr D.C'decNumber -> IO Bool
equalDecNumber x y = do
  dx <- peek (D.p'decNumber'digits x)
  dy <- peek (D.p'decNumber'digits y)
  if dx /= dy
    then return False
    else do
      let nBytes = mallocAmount dx
      bx <- BS.packCStringLen (castPtr x, nBytes)
      by <- BS.packCStringLen (castPtr y, nBytes)
      return $ bx == by


