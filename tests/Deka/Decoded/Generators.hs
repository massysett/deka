module Deka.Decoded.Generators where

import Deka.Decoded
import Test.QuickCheck

genSign :: Gen Sign
genSign = elements [ minBound..maxBound ]

genBinaryMSD :: Gen Digit
genBinaryMSD = return D1

genBinaryNonMSD :: Gen Digit
genBinaryNonMSD = elements [D0, D1]

binaryDigs :: (Gen Digit, Gen Digit)
binaryDigs = (genBinaryMSD, genBinaryNonMSD)

genDecimalMSD :: Gen Digit
genDecimalMSD = elements [ D1, D2, D3, D4, D5,
                           D6, D7, D8, D9 ]

genDecimalNonMSD :: Gen Digit
genDecimalNonMSD = elements
  [ D0, D1, D2, D3, D4, D5,
    D6, D7, D8, D9 ]

decimalDigs :: (Gen Digit, Gen Digit)
decimalDigs = (genDecimalMSD, genDecimalNonMSD)

-- | Given a length, generate a list of digits.  All lists generated
-- will be exactly the length given.
genDigits
  :: Int
  -- ^ Length
  -> (Gen Digit, Gen Digit)
  -- ^ Generate MSD, remaining digits
  -> Gen [Digit]
genDigits l (gm, gr) = do
  msd <- gm
  rs <- vectorOf (l - 1) gr
  return $ msd : rs

-- | Given a maximum length, generate lists of digits that are no
-- longer than the length given.  The list will be of a random
-- length, but it will be no longer than the larger of the size
-- parameter and the given maximum length.  The list will always be
-- at least one element long regardless of the maximum length passed
-- in.
sizedDigits
  :: Int
  -- ^ Maximum length. (Size parameter determines the maximum
  -- length, but it will not exceed this amount.)
  -> (Gen Digit, Gen Digit)
  -- ^ Generate MSD, remaining digits
  -> Gen [Digit]
sizedDigits m (gm, gr) = sized $ \s -> do
  let sz = max 1 s
      maxLen = min sz m
  len <- choose (1, maxLen)
  genDigits len (gm, gr)

