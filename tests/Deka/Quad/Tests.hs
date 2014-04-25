module Deka.Quad.Tests where

import Deka.Internal.Decnumber.Types
import Deka.Class
import Deka.Quad
import Deka.Decoded.Generators
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Deka.Internal.Quad.Decoding.Generators
import Deka.Internal.Context.Generators
import Prelude hiding (round)

testBoolean
  :: String
  -- ^ Name
  -> Gen Decoded
  -- ^ Generates decodes that should succeed
  -> (Decoded -> Bool)
  -- ^ This predicate returns True on successful decodes
  -> (Quad -> Bool)
  -- ^ Function to test
  -> TestTree
testBoolean n g pd f = testGroup n
  [ testProperty "predicate returns true on generated decodes" $
    forAll g $ \d -> pd d
  
  , testProperty "succeeds when it should" $
    forAll g $ \dcd ->
      let q = fromBCD dcd
      in f q

  , testProperty "fails when it should" $
    forAll (genDecoded `suchThat` (not . pd)) $ \dcd ->
      let q = fromBCD dcd
      in not $ f q

  , testProperty "decNumber and Deka predicate return same result"
    $ forAll genDecoded $ \dcd ->
      let q = fromBCD dcd
          b = f q
      in b == pd dcd
  ]

-- | Tests functions that deal with DecClass.
testDecClass
  :: Class
  -- ^ Class being tested
  -> Gen Decoded
  -- ^ Generates Decoded that are in this class
  -> (Decoded -> Bool)
  -- ^ This function should return True on Decoded that are in the
  -- class
  -> TestTree

testDecClass c ge f = testGroup (show c)
  [ testProperty "predicate returns True on generated decodes" $
    forAll ge f

  , testProperty "decClass returns matching class" $
    forAll ge $ \dcd -> let q = fromBCD dcd in decClass q == c

  , testProperty "decClass does not return matching class otherwise" $
    forAll (genDecoded `suchThat` (not . f)) $ \dcd ->
    let q = fromBCD dcd in decClass q /= c
  ]

classTests :: TestTree
classTests = testGroup "classes"
  [ testDecClass sNaN
    (genNaNDcd genSign (return Signaling) (payloadDigits decimalDigs))
    dIsNaN

  , testDecClass qNaN
    (genNaNDcd genSign (return Quiet) (payloadDigits decimalDigs))
    dIsNaN

  , testDecClass negInf
    (genInfinite (return Neg)) dIsNegInf

  , testDecClass negNormal
    (genNormal (return Neg)
      (sizedDigits coefficientLen decimalDigs)) dIsNegNormal

  , testDecClass negSubnormal
    (genSubnormal (return Neg)
      (sizedDigits (coefficientLen - 1) decimalDigs))
      dIsNegSubnormal

  , testDecClass negZero genNegZero dIsNegZero
  , testDecClass posZero genPosZero dIsPosZero

  , testDecClass posSubnormal
    (genSubnormal (return NonNeg)
      (sizedDigits (coefficientLen - 1) decimalDigs))
      dIsPosSubnormal

  , testDecClass posNormal
    (genNormal (return NonNeg)
      (sizedDigits coefficientLen decimalDigs)) dIsPosNormal

  , testDecClass posInf
    (genInfinite (return NonNeg)) dIsPosInf

  ] -- classes

intConversion
  :: (Show a, Eq a)
  => String
  -- ^ Name
  -> Gen a
  -> (a -> Quad)
  -- ^ Convert from C int
  -> (Round -> Quad -> Ctx a)
  -- ^ Convert to C int
  -> TestTree
intConversion n gen fr to = testGroup (n ++ " conversions")
  [ testProperty "convert from C integer to Quad and back" $
    forAll round $ \r ->
    forAll gen $ \i ->
    let q = fr i
        (i', fl) = runQuadStatus $ to r q
    in null fl && i' == i
  ]

genInt32 :: Gen C'int32_t
genInt32 = choose (minBound, maxBound)

genUInt32 :: Gen C'uint32_t
genUInt32 = choose (minBound, maxBound)

convTests :: TestTree
convTests = testGroup "integer conversions"
  [ intConversion "int32" genInt32 fromInt32 toInt32
  , intConversion "uint32" genUInt32 fromUInt32 toUInt32
  , intConversion "int32 exact" genInt32 fromInt32 toInt32Exact
  , intConversion "uint32 exact" genUInt32 fromUInt32 toUInt32Exact
  ] -- integer conversions

