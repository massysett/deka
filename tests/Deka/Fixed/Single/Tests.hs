module Deka.Fixed.Single.Tests where

import Deka.Fixed.Single
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Deka.Internal.Single.Decoding.Generators
import Prelude hiding (round, isInfinite, exponent, isNaN)
import qualified Data.ByteString.Char8 as BS8

testBoolean
  :: String
  -- ^ Name
  -> Gen Decoded
  -- ^ Generates decodes that should succeed
  -> (Decoded -> Bool)
  -- ^ This predicate returns True on successful decodes
  -> (Single -> Bool)
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

equal :: Single -> Single -> Bool
equal s1 s2 = toByteString s1 == toByteString s2

strTests :: TestTree
strTests = testGroup "string conversions"
  [ testProperty ("Decoded -> Single -> ByteString"
      ++ " -> Single -> Decoded") $
    forAll genDecoded $ \d ->
      let q = fromBCD d
          bs = toByteString q
          q' = runSingle $ fromByteString bs
          d' = toBCD q'
          desc = "toByteString: " ++ BS8.unpack bs
            ++ " toBCD: " ++ show d'
      in counterexample desc $ d' == d

  , testProperty ("fromBCD and (fromByteString . scientific) "
      ++ "give same result") $
    forAll genDecoded $ \d ->
    let qD = fromBCD d
        (qS, fl) = runSingleStatus . fromByteString
                    . BS8.pack . scientific $ d
        compared = equal qD qS
    in compared && null fl

  , testProperty "toByteString -> fromByteString" $
    forAll genDecoded $ \d ->
    let q = fromBCD d
        bs = toByteString q
        (q', fl) = runSingleStatus . fromByteString $ bs
        cmpRes = equal q q'
    in cmpRes && null fl

  ] -- string conversions

decodeTests :: TestTree
decodeTests = testGroup "decode and encode"
  [ testProperty "round trip from Decoded" $
      forAll genDecoded $ \d ->
      let r = toBCD . fromBCD $ d
      in counterexample ("result: " ++ show r) (r == d)
  ]

tests :: TestTree
tests = testGroup "Single tests"
  [ strTests
  , decodeTests
  ]
