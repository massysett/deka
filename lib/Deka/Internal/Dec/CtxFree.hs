{-# LANGUAGE EmptyDataDecls, Safe #-}

module Deka.Internal.Dec.CtxFree where

import Foreign.Safe
import Deka.Internal.Dec.Dec
import Deka.Internal.Decnumber.DecNumber
import Deka.Internal.Decnumber.Context
import qualified Data.ByteString.Char8 as BS8
import Deka.Decoded
import Deka.Context
import Prelude
import Foreign.C.Types
import Deka.Internal.Dec.Util

-- # Conversions

fromInt32 :: Int32 -> IO Dec
fromInt32 i = do
  dn <- newDecSize 10
  withForeignPtr (unDec dn) $ \ptr -> do
    _ <- c'decNumberFromInt32 ptr i
    return dn

fromUInt32 :: Word32 -> IO Dec
fromUInt32 i = do
  dn <- newDecSize 10
  withForeignPtr (unDec dn) $ \ptr -> do
    _ <- c'decNumberFromUInt32 ptr i
    return dn

toEngByteString :: Dec -> IO BS8.ByteString
toEngByteString dn =
  withForeignPtr (unDec dn) $ \pDn ->
  peek (p'decNumber'digits pDn) >>= \digs ->
  let digsTot = fromIntegral digs + 14 in
  allocaBytes digsTot $ \pStr ->
  c'decNumberToEngString pDn pStr >>
  BS8.packCString pStr

sameQuantum :: Dec -> Dec -> IO Dec
sameQuantum (Dec x) (Dec y) =
  withForeignPtr x $ \px ->
  withForeignPtr y $ \py ->
  oneDigitDec >>= \o ->
  withForeignPtr (unDec o) $ \po ->
  c'decNumberSameQuantum po px py >>
  return o

copyAbs
  :: Dec
  -- ^ Source of sign
  -> Dec
  -- ^ Copy sign to this destination
  -> IO Dec
  -- ^ Result
copyAbs src dest =
  copyDec dest >>= \r ->
  withForeignPtr (unDec r) $ \pr ->
  withForeignPtr (unDec src) $ \ps ->
  c'decNumberCopyAbs pr ps >>
  return r

-- CopyNegate, CopySign

negate :: Dec -> IO Dec
negate src =
  copyDec src >>= \r ->
  withForeignPtr (unDec r) $ \pr ->
  withForeignPtr (unDec src) $ \ps ->
  c'decNumberCopyNegate pr ps >>
  return r

copySign
  :: Dec
  -- ^ Source of content (except sign)
  -> Dec
  -- ^ Source of sign
  -> IO Dec
copySign src sgn =
  withForeignPtr (unDec src) $ \pc ->
  peek (p'decNumber'digits pc) >>= \dgts ->
  newDecSize dgts >>= \dn' ->
  withForeignPtr (unDec dn') $ \dp' ->
  withForeignPtr (unDec sgn) $ \pn ->
  c'decNumberCopySign dp' pc pn >>
  return dn'

trim :: Dec -> IO Dec
trim src =
  copyDec src >>= \dest ->
  withForeignPtr (unDec dest) $ \pd ->
  c'decNumberTrim pd >>
  return dest

version :: IO BS8.ByteString
version =
  c'decNumberVersion >>= \pv ->
  BS8.packCString pv

zero :: IO Dec
zero =
  oneDigitDec >>= \od ->
  withForeignPtr (unDec od) $ \pod ->
  c'decNumberZero pod >>
  return od

testBool
  :: (Ptr C'decNumber -> IO CInt)
  -> Dec
  -> IO Bool
testBool f (Dec dn) =
  withForeignPtr dn $ \pn ->
  f pn >>= \bl ->
  return (toBool bl)

isCanonical :: Dec -> IO Bool
isCanonical = testBool c'decNumberIsCanonical

isFinite :: Dec -> IO Bool
isFinite = testBool c'decNumberIsFinite

isInfinite :: Dec -> IO Bool
isInfinite = testBool c'decNumberIsInfinite

isNaN :: Dec -> IO Bool
isNaN = testBool c'decNumberIsNaN

isNegative :: Dec -> IO Bool
isNegative = testBool c'decNumberIsNegative

isQNaN :: Dec -> IO Bool
isQNaN = testBool c'decNumberIsQNaN

isSNaN :: Dec -> IO Bool
isSNaN = testBool c'decNumberIsSNaN

isSpecial :: Dec -> IO Bool
isSpecial = testBool c'decNumberIsSpecial

isZero :: Dec -> IO Bool
isZero = testBool c'decNumberIsZero

--
-- # Native conversions
--

-- | The unadjusted, non-biased exponent of a floating point number.
newtype Exponent = Exponent { unExponent :: Int32 }
  deriving (Eq, Ord, Show)

-- | The adjusted exponent; that is, the exponent that results if
-- only one digit is to the left of the decimal point.
newtype AdjExponent = AdjExponent { unAdjExponent :: Int32 }
  deriving (Eq, Ord, Show)

adjExponent :: Exponent -> Coefficient -> AdjExponent
adjExponent (Exponent ex) (Coefficient ds) =
  AdjExponent $ ex + (fromIntegral . length $ ds) - 1

-- | Is this exponent valid?
checkExp
  :: Maybe Precision
  -> Exponent
  -> Coefficient
  -> Bool
checkExp mnd i coe
  | unAdjExponent adj > 999999999 = False
  | unAdjExponent adj < minAdjExp = False
  | otherwise = True
  where
    adj = adjExponent i coe
    minAdjExp = case mnd of
      Nothing -> -999999999
      Just prc -> -999999999 - (unPrecision prc - 1)

-- | A fully decoded 'Dec'.
data Decoded = Decoded
  { dcdSign :: Sign
  , dcdPayload :: Payload
  } deriving (Eq, Ord, Show)

-- | The bulk of the information from a fully decoded 'Dec'
-- (except the 'Sign').
data Payload
  = Infinity
  | NaN NaN Coefficient
  | NotSpecial Exponent Coefficient
  deriving (Eq, Ord, Show)

-- | The coefficient of a non-special number, or the diagnostic
-- information of an NaN.  Consists of a list of 'Digit'.
newtype Coefficient = Coefficient { unCoefficient :: [Digit] }
  deriving (Eq, Ord, Show)

-- | Creates a 'Coefficient'.  Checks to ensure it is not null and
-- that it is not longer than the maximum coefficient length and
-- that it does not have leading zeroes (if it is 0, a single 'D0'
-- is allowed).
coefficient :: [Digit] -> Maybe Coefficient
coefficient ls
  | null ls = Nothing
  | length ls > c'DEC_MIN_DIGITS && head ls == D0 = Nothing
  | length ls > c'DEC_MAX_DIGITS = Nothing
  | otherwise = Just . Coefficient $ ls

-- | Coefficient of 'D0'
zeroCoefficient :: Coefficient
zeroCoefficient = Coefficient [D0]

-- | Coefficient of 'D1'
oneCoefficient :: Coefficient
oneCoefficient = Coefficient [D1]

-- # Decoding

-- | Take a C 'Dec' and convert it to Haskell types.
decode :: Dec -> IO Decoded
decode dn =
  withForeignPtr (unDec dn) $ \fp ->
  peek (p'decNumber'bits fp) >>= \bits ->
  decodeCoeff dn >>= \coe ->
  decodeExponent dn >>= \ex ->
  decodeSign dn >>= \sgn ->
  let getInf
        | toBool (bits .&. c'DECNAN) = NaN Quiet coe
        | toBool (bits .&. c'DECSNAN) = NaN Signaling coe
        | toBool (bits .&. c'DECINF) = Infinity
        | otherwise = NotSpecial ex coe in
  return (Decoded sgn getInf)

decodeSign :: Dec -> IO Sign
decodeSign (Dec fp) =
  withForeignPtr fp $ \ptr ->
  peek (p'decNumber'bits ptr) >>= \bts ->
  let isSet = toBool $ bts .&. c'DECNEG
      r | isSet = Neg
        | otherwise = NonNeg
  in return r

decodeCoeff :: Dec -> IO Coefficient
decodeCoeff (Dec fp) =
  withForeignPtr fp $ \ptr ->
  peek (p'decNumber'digits ptr) >>= \dgs ->
  allocaBytes (fromIntegral dgs) $ \arr ->
  let _types = arr :: Ptr Word8 in
  c'decNumberGetBCD ptr arr >>
  peekArray (fromIntegral dgs) arr >>= \dgts ->
  return . Coefficient . map intToDigit $ dgts

-- | The space for the Dec must have already been allocated
-- properly.
encodeCoeff :: Coefficient -> Dec -> IO ()
encodeCoeff (Coefficient ds) (Dec fp) =
  withForeignPtr fp $ \dptr ->
  let len = length ds in
  allocaArray len $ \arr ->
  pokeArray arr (map digitToInt ds) >>
  c'decNumberSetBCD dptr arr (fromIntegral len) >>
  return ()


decodeExponent :: Dec -> IO Exponent
decodeExponent (Dec fp) =
  withForeignPtr fp $ \ptr ->
  peek (p'decNumber'exponent ptr) >>= \ex ->
  return (Exponent ex)

-- # Encoding

-- | Encodes positive or negative infinities.
infinity :: Sign -> IO Dec
infinity s =
  oneDigitDec >>= \dn ->
  withForeignPtr (unDec dn) $ \pd ->
  poke (p'decNumber'digits pd) 1 >>
  poke (p'decNumber'exponent pd) 0 >>
  poke (p'decNumber'lsu pd) 0 >>
  let bSgn | s == Neg = c'DECNEG
           | otherwise = 0
      bts = bSgn .|. c'DECINF in
  poke (p'decNumber'bits pd) bts >>
  return dn

-- | Encodes quiet or signaling NaNs.
notANumber :: Sign -> NaN -> Coefficient -> IO Dec
notANumber s nt coe =
  let len = length . unCoefficient $ coe in
  newDecSize (fromIntegral len) >>= \dn ->
  withForeignPtr (unDec dn) $ \dPtr ->
  poke (p'decNumber'digits dPtr) (fromIntegral len) >>
  poke (p'decNumber'exponent dPtr) 0 >>
  let bSgn | s == Neg = c'DECNEG
           | otherwise = 0
      bNaN | nt == Quiet = c'DECNAN
           | otherwise = c'DECSNAN
      bts = bSgn .|. bNaN in
  poke (p'decNumber'bits dPtr) bts >>
  encodeCoeff coe dn >>
  return dn

-- | Encodes non-special numbers (also known as finite numbers.)
-- Does not need the context; however, you will have to supply
-- information about whether subnormal values are allowed.
nonSpecialCtxFree
  :: Maybe Precision
  -- ^ If Just, allow subnormal values.  In that case, the maximum
  -- number of digits is needed in order to compute the lower limit
  -- for the exponent.  If Nothing, do not allow subnormal values.

  -> Sign
  -> Coefficient
  -> Exponent
  -> IO (Maybe Dec)
  -- ^ Fails if the exponent is out of range.
nonSpecialCtxFree mnd sgn coe ex
  | not $ checkExp mnd ex coe = return Nothing
  | otherwise = fmap Just $
    let len = length . unCoefficient $ coe in
    newDecSize (fromIntegral len) >>= \dn ->
    withForeignPtr (unDec dn) $ \dPtr ->
    poke (p'decNumber'digits dPtr) (fromIntegral len) >>
    poke (p'decNumber'exponent dPtr) (unExponent ex) >>
    let bSgn | sgn == Neg = c'DECNEG
             | otherwise = 0 in
    poke (p'decNumber'bits dPtr) bSgn >>
    encodeCoeff coe dn >>
    return dn

