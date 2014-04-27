-- | Dealing with binary encoding of hexadecimal strings.

module Dectest.Binary where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Word
import Data.Char (toLower)
import Data.Bits
import Test.QuickCheck hiding ((.&.))
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty (testGroup, TestTree)
import Deka.Internal.Decnumber.DecNumberLocal

hexToNibble :: Char -> Word8
hexToNibble c = case toLower c of
  { '0' -> 0x0; '1' -> 0x1; '2' -> 0x2; '3' -> 0x3; '4' -> 0x4;
    '5' -> 0x5; '6' -> 0x6; '7' -> 0x7; '8' -> 0x8; '9' -> 0x9;
    'a' -> 0xa; 'b' -> 0xb; 'c' -> 0xc; 'd' -> 0xd; 'e' -> 0xe;
    'f' -> 0xf; _ -> error "hexToNibble: out of range" }

nibbleToHex :: Word8 -> Char
nibbleToHex w = case w of
  { 0x0 -> '0'; 0x1 -> '1'; 0x2 -> '2'; 0x3 -> '3'; 0x4 -> '4';
    0x5 -> '5'; 0x6 -> '6'; 0x7 -> '7'; 0x8 -> '8'; 0x9 -> '9';
    0xa -> 'a'; 0xb -> 'b'; 0xc -> 'c'; 0xd -> 'd'; 0xe -> 'e';
    0xf -> 'f'; _ -> error "nibbleToHex: out of range" }

hexToByte :: (Char, Char) -> Word8
hexToByte (msn, lsn)
  = ((.|.) (hexToNibble lsn))
  . flip shiftL 4
  $ hexToNibble msn

byteToHex :: Word8 -> (Char, Char)
byteToHex w = (msn, lsn)
  where
    lsn = nibbleToHex $ w .&. 0x0f
    msn = nibbleToHex . flip shiftR 4 $ w

--
-- Lists
--

listToPairs :: [a] -> [(a, a)]
listToPairs ls = case ls of
  [] -> []
  _:[] -> error "listToPairs: uneven list"
  x:y:xs -> (x, y) : listToPairs xs

reorder :: [a] -> [a]
reorder
  | littleEndian = reverse
  | otherwise = id

-- | Takes a hex ByteString and packs it to a binary ByteString.
-- Assumes the binary ByteString is packed in machine byte order,
-- and that hex ByteString is big endian.
packHexList :: BS8.ByteString -> BS.ByteString
packHexList = BS.pack . reorder . map hexToByte
  . listToPairs . BS8.unpack

-- | Takes a binary ByteString and unpacks it to a hex ByteString.
-- Assumes the binary ByteString is packed in machine byte order,
-- and that hex ByteString is big endian.
unpackHexList :: BS.ByteString -> BS8.ByteString
unpackHexList
  = BS8.pack
  . concatMap flatten
  . reorder
  . map byteToHex
  . BS.unpack
  where
    flatten (a, b) = [a, b]


--
-- Tests
--

hexChar :: Gen Char
hexChar = elements $ ['0'..'9'] ++ ['a' .. 'f']

hexToByteToHex :: TestTree
hexToByteToHex = testProperty d t
  where
    d = "hex to byte back to hex"
    t = forAll hexChar $ \c1 ->
      forAll hexChar $ \c2 ->
      let r = byteToHex $ hexToByte (c1, c2)
      in r == (c1, c2)

byteToHexToByte :: TestTree
byteToHexToByte = testProperty d t
  where
    d = "byte to hex back to byte"
    t b = r == b
      where
        r = hexToByte $ byteToHex b

hexList :: Gen BS8.ByteString
hexList = sized $ \s -> do
  let n | odd s = s + 1
        | otherwise = s
  hcs <- vectorOf n hexChar
  return $ BS8.pack hcs

binList :: Gen BS.ByteString
binList = fmap BS.pack $ listOf (choose (minBound, maxBound))

hexListToBinListToHexList :: TestTree
hexListToBinListToHexList = testProperty d t
  where
    d = "list: hex to binary to hex"
    t = forAll hexList $ \h ->
      let r = unpackHexList . packHexList $ h
      in r == h

binListToHexListToBinList :: TestTree
binListToHexListToBinList = testProperty d t
  where
    d = "list: bin to hex to bin"
    t = forAll binList $ \b ->
      let r = packHexList . unpackHexList $ b
      in r == b

tests :: TestTree
tests = testGroup "Dectest.Binary"
  [ hexToByteToHex
  , byteToHexToByte
  , hexListToBinListToHexList
  , binListToHexListToBinList
  ]
