module Deka.Quad.Tests where

import Deka.Class
import Deka.Quad
import Deka.Decoded.Generators
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Deka.Internal.Quad.Decoding.Generators
import Deka.Internal.Context.Generators
import Prelude hiding (round, isInfinite, exponent, isNaN)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as BS8
import Data.Int
import Data.Word

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

genInt32 :: Gen Int32
genInt32 = choose (minBound, maxBound)

genUInt32 :: Gen Word32
genUInt32 = choose (minBound, maxBound)

convTests :: TestTree
convTests = testGroup "integer conversions"
  [ intConversion "int32" genInt32 fromInt32 toInt32
  , intConversion "uint32" genUInt32 fromUInt32 toUInt32
  , intConversion "int32 exact" genInt32 fromInt32 toInt32Exact
  , intConversion "uint32 exact" genUInt32 fromUInt32 toUInt32Exact
  ] -- integer conversions

strTests :: TestTree
strTests = testGroup "string conversions"
  [ testProperty ("Decoded -> Quad -> ByteString"
      ++ " -> Quad -> Decoded") $
    forAll genDecoded $ \d ->
      let q = fromBCD d
          bs = toByteString q
          q' = runQuad $ fromByteString bs
          d' = toBCD q'
          desc = "toByteString: " ++ BS8.unpack bs
            ++ " toBCD: " ++ show d'
      in counterexample desc $ d' == d

  , testProperty ("fromBCD and (fromByteString . scientific) "
      ++ "give same result") $
    forAll genDecoded $ \d ->
    let qD = fromBCD d
        (qS, fl) = runQuadStatus . fromByteString
                    . BS8.pack . scientific $ d
        compared = compareTotal qD qS == EQ
    in compared && null fl

  , testProperty ("fromBCD and (fromByteString . ordinary) "
      ++ "give results that compare equal") $
    forAll genDecoded $ \d ->
    let qD = fromBCD d
        str = ordinary d
        (qS, fl) = runQuadStatus . fromByteString
                    . BS8.pack $ str
        cmpResult 
          | isNormal qD = compareOrd qD qS == Just EQ
          | otherwise = compareTotal qD qS == EQ
        noFlags f = null f
        desc = "string: " ++ str
          ++ " fromByteString result: " ++ show qS
    in noFlags fl ==> counterexample desc cmpResult

  , testProperty "toByteString -> fromByteString" $
    forAll genDecoded $ \d ->
    let q = fromBCD d
        bs = toByteString q
        (q', fl) = runQuadStatus . fromByteString $ bs
        cmpRes = compareTotal q q' == EQ
    in cmpRes && null fl

  , testProperty "toEngByteString -> fromByteString" $
    forAll genDecoded $ \d ->
    let q = fromBCD d
        bs = toEngByteString q
        (q', fl) = runQuadStatus . fromByteString $ bs
        cmpRes = compareOrd q q' == Just EQ
        cmpResTot = compareTotal q q' == EQ
        res = if isFinite q then cmpRes else cmpResTot
    in null fl ==> res
  ] -- string conversions

boolTests :: TestTree
boolTests = testGroup "booleans"
  [ testBoolean "isFinite" genFinite dIsFinite isFinite

  , testBoolean "isInfinite" (genInfinite genSign)
      dIsInfinite isInfinite

  , testGroup "isInteger"
    [ testBoolean "isInteger" genInteger dIsInteger isInteger

    , let e = fromMaybe (error "isInteger exponent failed")
            . exponent $ 2
          c = fromMaybe (error "isInteger coefficient failed")
            . coefficient $ [D3]
          dcd = Decoded NonNeg (Finite c e)
          d = fromBCD dcd
      in testProperty "returns False on 3 * 10 ^ 2" . once
          . not . isInteger $ d
    ]

  , testBoolean "isLogical" genLogical
      dIsLogical isLogical

  , testBoolean "isNaN"
    (genNaNDcd genSign genNaN (payloadDigits decimalDigs))
      dIsNaN isNaN

  , testBoolean "isNegative" genNegative
    dIsNegative isNegative

  , testBoolean "isNormal"
    (genNormal genSign (sizedDigits coefficientLen decimalDigs))
      dIsNormal isNormal

  , testBoolean "isPositive" genPositive
      dIsPositive isPositive

  , testBoolean "isSignaling" genSignaling
      dIsSignaling isSignaling

  , testBoolean "isSigned" genSigned
      dIsSigned isSigned

  , testBoolean "isSubnormal"
      (genSubnormal genSign (sizedDigits (coefficientLen - 1) decimalDigs))
      dIsSubnormal isSubnormal

  , testBoolean "isZero" genZero dIsZero isZero

  ]

attributeTests :: TestTree
attributeTests = testGroup "attributes"
  [ testGroup "digits"
    [ testProperty "gets same result as length of decoded coeff" $
      forAll genFinite $ \d ->
      let digs = digits . fromBCD $ d
      in case dValue d of
          Finite c _ -> length (unCoefficient c) == digs
          _ -> False
    ]
  ] -- attributes

decodeTests :: TestTree
decodeTests = testGroup "decode and encode"
  [ testProperty "round trip from Decoded" $
      forAll genDecoded $ \d ->
      let r = toBCD . fromBCD $ d
      in counterexample ("result: " ++ show r) (r == d)
  ]

tests :: TestTree
tests = testGroup "Quad tests"
  [ classTests
  , convTests
  , strTests
  , boolTests
  , attributeTests
  , decodeTests
  ]
