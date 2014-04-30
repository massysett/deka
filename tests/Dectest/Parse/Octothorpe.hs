{-# LANGUAGE OverloadedStrings #-}
-- | Parse octothorpe-containing operands and results.

module Dectest.Parse.Octothorpe
  ( OctoParsers(..)
  , Parsed(..)
  , parseOcto
  ) where

import Control.Monad
import Dectest.Binary
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Deka.DecNum as DN
import Deka.Internal.DecNum.DecNum
import Deka.Internal.Decnumber.DecNumber (c'decNumberPlus)
import Deka.Context
import Deka.Internal.Context
import Deka.Internal.Decnumber.Context
import Deka.Internal.DecNum.Util
import qualified Deka.Internal.Decnumber.DecNumber as D
import qualified Deka.Internal.Decnumber.Decimal32 as D32
import qualified Deka.Internal.Decnumber.Decimal64 as D64
import qualified Deka.Internal.Decnumber.Decimal128 as D128
import Deka.Internal.Decnumber.DecQuad
import Deka.Internal.Decnumber.DecDouble
import Foreign.Safe hiding (void)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS

type ApplyPrecision = Bool

data Parsed
  = NotOcto
  | Null
  | Octo OctoParsers

isNull :: BS8.ByteString -> Bool
isNull = (== "#")

-- | When parsing a result token, parseOcto returns a function that
-- you use to compare the result of the test computation to the
-- token.
type CompareResult a = Ptr a -> Ctx Bool

type ConvertOp a = ApplyPrecision -> Ctx (ForeignPtr a)

data OctoParsers = OctoParsers
  { opOperandDec :: ConvertOp D.C'decNumber
  , opOperand32 :: ConvertOp D32.C'decimal32
  , opOperand64 :: ConvertOp D64.C'decimal64
  , opOperand128 :: ConvertOp D128.C'decimal128
  , opResultDec :: CompareResult D.C'decNumber
  , opResult32 :: CompareResult D32.C'decimal32
  , opResult64 :: CompareResult D64.C'decimal64
  , opResult128 :: CompareResult D128.C'decimal128
  }

parseOcto :: BS8.ByteString -> Parsed
parseOcto s
  | isNull s = Null
  | not ('#' `BS8.elem` s) = NotOcto
  | otherwise = Octo $ OctoParsers
      { opOperandDec = opDec s
      , opOperand32 = op32 s
      , opOperand64 = op64 s
      , opOperand128 = op128 s
      , opResultDec = rsDec s
      , opResult32 = rs32 s
      , opResult64 = rs64 s
      , opResult128 = rs128 s
      }

octoHexPrecision :: OctoHex -> Int32
octoHexPrecision o = case o of
  H32 _ -> 7
  H64 _ -> 16
  H128 _ -> 34

-- | Tries to get an OctoHex as a hex from a string.  If that fails,
-- get it as a number string.
getOctoHex :: BS8.ByteString -> SetFlags -> Ptr C'decContext -> IO OctoHex
getOctoHex bs setF ptr = do
  mayOh <- octoHex bs
  case mayOh of
    Just ioh -> return ioh
    Nothing -> unCtx (octoString bs setF) ptr


opDec :: BS8.ByteString -> ConvertOp D.C'decNumber
opDec bs apPrec = Ctx $ \pCtx -> do
  oh <- getOctoHex bs True pCtx
  dn <- octoHexToDecNum oh
  let pcsn = octoHexPrecision oh
      mayPrec
        | apPrec = Nothing
        | otherwise = Just pcsn
  _ <- unCtx (applyDirectivesToDecNumber mayPrec dn) pCtx
  return dn


op32 :: BS8.ByteString -> ConvertOp D32.C'decimal32
op32 bs _ = Ctx $ \pCtx -> do
  oh <- getOctoHex bs True pCtx
  return $ case oh of
    H32 p -> p
    _ -> error "op32: unsupported conversion"

op64 :: BS8.ByteString -> ConvertOp D64.C'decimal64
op64 bs apPrec = Ctx $ \pCtx -> do
  oh <- getOctoHex bs True pCtx
  unCtx (applyDirectivesToOctoHex apPrec oh) pCtx
  case oh of
    H64 p -> return p
    _ -> error "op64: unsupported conversion"

op128 :: BS8.ByteString -> ConvertOp D128.C'decimal128
op128 bs apPrec = Ctx $ \pCtx -> do
  oh <- getOctoHex bs True pCtx
  unCtx (applyDirectivesToOctoHex apPrec oh) pCtx
  case oh of
    H128 p -> return p
    _ -> error "op128: unsupported conversion"

rsDec :: BS8.ByteString -> CompareResult D.C'decNumber
rsDec bs pTestResult = Ctx $ \pCtx -> do
  oh <- getOctoHex bs False pCtx
  case oh of
    H32 fpTarget32 -> withForeignPtr fpTarget32 $ \pTarget32 ->
      allocaBytes D32.c'decimal32'sizeOf $ \pConvTestResult ->
      D32.c'decimal32FromNumber pConvTestResult pTestResult pCtx >>= \_ ->
      equalDecimal32 pConvTestResult pTarget32

    H64 fpTarget64 -> withForeignPtr fpTarget64 $ \pTarget64 ->
      allocaBytes D64.c'decimal64'sizeOf $ \pConvTestResult ->
      D64.c'decimal64FromNumber pConvTestResult pTestResult pCtx >>= \_ ->
      equalDecimal64 pConvTestResult pTarget64

    H128 fpTarget128 -> withForeignPtr fpTarget128 $ \pTarget128 ->
      allocaBytes D128.c'decimal128'sizeOf $ \pConvTestResult ->
      D128.c'decimal128FromNumber pConvTestResult pTestResult pCtx >>= \_ ->
      equalDecimal128 pConvTestResult pTarget128


rs32 :: BS8.ByteString -> CompareResult D32.C'decimal32
rs32 bs pTestRslt = Ctx $ \pCtx -> do
  oh <- getOctoHex bs False pCtx
  case oh of
    H32 fpTarget32 -> withForeignPtr fpTarget32 $ \pTgt ->
      equalDecimal32 pTgt pTestRslt
    _ -> error "rs32: unsupported conversion"

rs64 :: BS8.ByteString -> CompareResult D64.C'decimal64
rs64 bs pTestRslt = Ctx $ \pCtx -> do
  oh <- getOctoHex bs False pCtx
  case oh of
    H64 fpTarget64 -> withForeignPtr fpTarget64 $ \pTgt ->
      equalDecimal64 pTgt pTestRslt
    _ -> error "rs64: unsupported conversion"

rs128 :: BS8.ByteString -> CompareResult D128.C'decimal128
rs128 bs pTestRslt = Ctx $ \pCtx -> do
  oh <- getOctoHex bs False pCtx
  case oh of
    H128 fpTarget128 -> withForeignPtr fpTarget128 $ \pTgt ->
      equalDecimal128 pTgt pTestRslt
    _ -> error "rs128: unsupported conversion"


applyDirectivesToDecNumber
  :: Maybe Int32
  -- ^ If Just, apply the given precision to the operand.
  -- Otherwise, apply the precision from the context.
  -> ForeignPtr D.C'decNumber
  -> Ctx ()
applyDirectivesToDecNumber mayPrec fp = Ctx $ \pCtx ->
  unCtx getPrecision pCtx >>= \pOld ->
  let pNew = case mayPrec of
        Nothing -> pOld
        Just pc -> Precision pc in
  unCtx (setPrecision pNew) pCtx >>= \_ ->
  withForeignPtr fp $ \pDn ->
  c'decNumberPlus pDn pDn pCtx >>= \_ ->
  unCtx (setPrecision pOld) pCtx >>= \_ ->
  return ()

-- | Applies directives to an OctoHex.  EXCEPTION - does not apply
-- directives to a decimal32, as no function will do this.
applyDirectivesToOctoHex
  :: ApplyPrecision
  -> OctoHex
  -> Ctx ()
applyDirectivesToOctoHex apPrec oh = Ctx $ \pCtx ->
  let mayPrec
        | apPrec = Nothing
        | otherwise = Just (octoHexPrecision oh) in
  unCtx getPrecision pCtx >>= \pOld ->
  let pNew = case mayPrec of
        Nothing -> pOld
        Just pc -> Precision pc in
  unCtx (setPrecision pNew) pCtx >>= \_ ->
  let setOcto = case oh of
        H32 _ -> return ()
        H64 fp -> withForeignPtr fp $ \p64 ->
          c'decDoublePlus (castPtr p64) (castPtr p64) pCtx >>= \_ ->
          return ()
        H128 fp -> withForeignPtr fp $ \p128 ->
          c'decQuadPlus (castPtr p128) (castPtr p128) pCtx >>= \_ ->
          return () in
  setOcto >>= \_ ->
  unCtx (setPrecision pOld) pCtx >>= \_ ->
  return ()

data OctoHex
  = H32 (ForeignPtr D32.C'decimal32)
  | H64 (ForeignPtr D64.C'decimal64)
  | H128 (ForeignPtr D128.C'decimal128)

octoHexToDecNum :: OctoHex -> IO (ForeignPtr D.C'decNumber)
octoHexToDecNum h = case h of
  H32 p -> withForeignPtr p $ \pN ->
    newDecNumSize 7 >>= \(DecNum fpD) ->
    withForeignPtr fpD $ \pD ->
    D32.c'decimal32ToNumber pN pD >>= \_ ->
    return fpD
  H64 p -> withForeignPtr p $ \pN ->
    newDecNumSize 16 >>= \(DecNum fpD) ->
    withForeignPtr fpD $ \pD ->
    D64.c'decimal64ToNumber pN pD >>= \_ ->
    return fpD
  H128 p -> withForeignPtr p $ \pN ->
    newDecNumSize 34 >>= \(DecNum fpD) ->
    withForeignPtr fpD $ \pD ->
    D128.c'decimal128ToNumber pN pD >>= \_ ->
    return fpD

-- | Decode an octothorpe that represents a hex format.  Octothorpe
-- should still be at the front of the list; returns Nothing if no
-- octothorpe at the front of the list.
octoHex :: BS8.ByteString -> IO (Maybe OctoHex)
octoHex bsFull = runMaybeT $ do
  (first, rest) <- maybe mzero return $ BS8.uncons bsFull
  guard $ first == '#'
  let len = BS8.length rest
      r | len == 8 = go H32 32
        | len == 16 = go H64 64
        | len == 32 = go H128 128
        | otherwise = error "octoHex: unknown length"
      go ctor bytes = 
        let hex = packHexList rest in
        mallocForeignPtrBytes bytes >>= \fp ->
        withForeignPtr fp $ \ptr ->
        BS.useAsCString hex $ \cstr ->
        copyBytes (castPtr ptr) cstr bytes >>
        return (ctor fp)
  lift r


type SetFlags = Bool

-- | Decode an octothorpe that represents a string.  The octothorpe
-- and the digits representing the string should still be in the
-- ByteString.

octoString
  :: BS8.ByteString
  -> SetFlags
  -- ^ When parsing an operand, converting the decNumber to the
  -- decimalXX format may set conditions.  If True, set these
  -- conditions.  When parsing results, no conditions are set.  In
  -- that case, pass False.
  -> Ctx OctoHex
octoString bs setFlgs
  | lead == "32" = go H32 f32 32
  | lead == "64" = go H64 f64 64
  | lead == "128" = go H128 f128 128
  | otherwise = error "octoString: unknown length"
  where
    lcl | setFlgs = local
        | otherwise = id
    lead = BS8.takeWhile (/= '#') bs
    rest = BS8.drop 1 $ BS8.dropWhile (/= '#') bs
    f32 = \d n c -> void $ D32.c'decimal32FromNumber (castPtr d) n c
    f64 = \d n c -> void $ D64.c'decimal64FromNumber (castPtr d) n c
    f128 = \d n c -> void $ D128.c'decimal128FromNumber (castPtr d) n c
    go ctor mkNum bytes = lcl $ Ctx $ \pCtx ->
      unCtx (DN.fromByteString rest) pCtx >>= \dn ->
      withForeignPtr (unDecNum dn) $ \pDn ->
      mallocForeignPtrBytes bytes >>= \fpR ->
      withForeignPtr fpR $ \pR ->
      mkNum pR pDn pCtx >>
      return (ctor fpR)

-- | Compare two Decimal32 to see if they are equal.
equalDecimal32 :: Ptr D32.C'decimal32 -> Ptr D32.C'decimal32 -> IO Bool
equalDecimal32 x y = do
  bx <- BS.packCStringLen (castPtr x, D32.c'decimal32'sizeOf)
  by <- BS.packCStringLen (castPtr y, D32.c'decimal32'sizeOf)
  return $ bx == by

-- | Compare two Decimal64 to see if they are equal.
equalDecimal64 :: Ptr D64.C'decimal64 -> Ptr D64.C'decimal64 -> IO Bool
equalDecimal64 x y = do
  bx <- BS.packCStringLen (castPtr x, D64.c'decimal64'sizeOf)
  by <- BS.packCStringLen (castPtr y, D64.c'decimal64'sizeOf)
  return $ bx == by

-- | Compare two Decimal128 to see if they are equal.
equalDecimal128 :: Ptr D128.C'decimal128 -> Ptr D128.C'decimal128 -> IO Bool
equalDecimal128 x y = do
  bx <- BS.packCStringLen (castPtr x, D128.c'decimal128'sizeOf)
  by <- BS.packCStringLen (castPtr y, D128.c'decimal128'sizeOf)
  return $ bx == by

--
--
--

{- Parsing Operand Octothorpes

   Branch 1.  Octothorpe alone is a null reference.

   Branch 2.  Octothorpe followed by 8, 16, or 32 hexadecimal digits
   is a decimal16, decimal32, or decimal64, respectively.

       Step a.  Decode the hex digits without loss or constraint
       into the respective decimalXX format.

       Step b.  If the decimalXX format must be converted to the format
       being used for the test calculation, do so now.  decimalXX to
       decNumber conversions are lossless.  There are no instances
       of decimalXX formats that require conversion to another
       decimalXX format.

       Step c.  Apply the directives to the operand.  Apply precision
       only when the operation is toSci, toEng, or apply; otherwise,
       use sufficient precision so that rounding is avoided.
       (precision will be an issue only when converting to
       decNumber.)  This step may set flags.

       EXCEPTION - do not apply directives to decimal32 operands, as
       no function permits this.

   Branch 3.  2 digits followed by an octothorpe followed by a numeric
   string specifies a number in the specified format.  (These occur
   only in decNumber tests.)

       Step a.  Convert the numeric string to a decNumber.  Then, convert
       the resulting decNumber to the target decimalXX format.  This
       may cause rounding or other conditions; the flags are set as
       usual.

       Step b.  Follow step 2b and 2c above.
-}

{- Parsing Result Octothorpes

   Branch 1.  Octothorpe alone is a null reference.

   Branch 2.  Octothorpe followed by 8, 16, or 32 hexadecimal digits
   is a decimal16, decimal32, or decimal64, respectively.

       Step a.  Decode the hex digits without loss or constraint
       into the respective decimalXX format.

       Step b.  Convert the result of the test calculation to the
       type of the given decoding.  This may modify or clamp the
       result to fit the format, giving a different result than if
       the result had been expressed as a string and possibly
       raising new conditions.

   Branch 3.  2 digits followed by an octothorpe followed by a numeric
   string specifies a number in the specified format.  (These occur
   only in decNumber tests.)

       Step a.  The numeric string is first converted to a decNumber
       and is then encoded in the selected format.  This may cause
       rounding or other conditions, but NO conditions are set by
       this process.

       Step b.  Follow step 2b above.

-}
