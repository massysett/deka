-- | Uses the specification for string conversions given in the
-- General Decimal Arithmetic Specification to convert strings to an
-- abstract syntax tree.  The specification for string conversions
-- is at
--
-- <http://speleotrove.com/decimal/daconvs.html>
--
-- The functions and types in this module fall into two groups.  The
-- first group converts a string to a 'NumericString', which is an
-- abstract representation of the grammar given in the General
-- Decimal Arithmetic Specification.  These functions use Parsec to
-- parse the string.  The second group transforms the
-- 'NumericString' to an 'A.Abstract', a form which more closely
-- aligns with the abstract representation given at
--
-- <http://speleotrove.com/decimal/damodel.html>.
--
-- You can transform an 'A.Abstract' to a numeric string; no
-- functions are provided to transform a 'NumericString' directly
-- back to a string.
module Deka.Native.FromString where

import Data.Char (toLower)
import Control.Applicative
import Text.Parsec.String
import Text.Parsec.Prim (tokenPrim, try, parse)
import Text.Parsec.Pos
import Text.Parsec.Char (char, string)
import Text.Parsec.Combinator (many1, eof)
import qualified Deka.Native.Abstract as A
import Deka.Native.Abstract
  (Decem(..), Novem(..), decemListToInt)
import Deka.Dec (Sign(..))
import qualified Deka.Dec as D
import qualified Data.ByteString.Char8 as BS8

sign :: Parser Sign
sign = tokenPrim show next f
  where
    next pos c _ = updatePosChar pos c
    f c = case c of
      '-' -> Just Sign1
      '+' -> Just Sign0
      _ -> Nothing

optSign :: Parser Sign
optSign = do
  s <- optional sign
  return $ maybe Sign0 id s

digit :: Parser Decem
digit = tokenPrim show next f
  where
    next pos c _ = updatePosChar pos c
    f c = case c of
      { '0' -> Just D0; '1' -> Just $ Nonem D1; '2' -> Just $ Nonem D2;
        '3' -> Just $ Nonem D3; '4' -> Just $ Nonem D4;
        '5' -> Just $ Nonem D5; '6' -> Just $ Nonem D6;
        '7' -> Just $ Nonem D7; '8' -> Just $ Nonem D8;
        '9' -> Just $ Nonem D9; _ -> Nothing }

indicator :: Parser ()
indicator = () <$ char 'e'

digits :: Parser [Decem]
digits = many1 digit

data DecimalPart
  = WholeFrac [Decem] [Decem]
  | WholeOnly [Decem]
  deriving (Eq, Ord, Show)

decimalPart :: Parser DecimalPart
decimalPart = do
  ds1 <- optional digits
  case ds1 of
    Nothing -> do
      _ <- char '.'
      fmap WholeOnly digits
    Just ds -> do
      dot <- optional (char '.')
      case dot of
        Just _ -> do
          ds2 <- many digit
          return $ WholeFrac ds ds2
        Nothing -> return $ WholeOnly ds

data ExponentPart = ExponentPart
  { expSign :: Sign
  , expDigits :: [Decem]
  } deriving (Eq, Ord, Show)

exponentPart :: Parser ExponentPart
exponentPart = do
  indicator
  sgn <- optSign
  ds <- digits
  return $ ExponentPart sgn ds

infinity :: Parser ()
infinity = try $ do
  _ <- string "inf"
  _ <- optional (string "inity")
  return ()

nanId :: Parser A.Noisy
nanId = try (string "nan" >> return A.Quiet)
  <|> try (string "snan" >> return A.Signaling)

data NaN = NaN A.Noisy [Decem]
  deriving (Eq, Ord, Show)

nan :: Parser NaN
nan = liftA2 NaN nanId (many digit)

data NumericValue
  = NVDec DecimalPart (Maybe ExponentPart)
  | Infinity
  deriving (Eq, Ord, Show)

numericValue :: Parser NumericValue
numericValue =
  (Infinity <$ infinity)
  <|> liftA2 NVDec decimalPart (optional exponentPart)

data NumericString = NumericString
  { nsSign :: Sign
  , nsValue :: Either NumericValue NaN
  } deriving (Eq, Ord, Show)

numericString :: Parser NumericString
numericString = liftA2 NumericString optSign ei
  where
    ei = (fmap Left numericValue <|> fmap Right nan)

parseNumericString :: String -> Either String NumericString
parseNumericString s =
  case parse (numericString <* eof) "" (map toLower s) of
    Left e -> Left (show e)
    Right g -> Right g

numericStringToAbstract :: NumericString -> A.Abstract
numericStringToAbstract (NumericString sgn ei) = A.Abstract sgn val
  where
    val = case ei of
      Left nv -> case nv of
        NVDec dp me -> uncurry A.Finite $ finiteToAbstract dp me
        Infinity -> A.Infinite
      Right nn -> A.NotANumber . nanToAbstract $ nn

nanToAbstract
  :: NaN
  -> A.NonNum
nanToAbstract (NaN nsy ds) = A.NonNum nsy . fmap A.Diagnostic
  . A.decemListToDecuple $ ds

finiteToAbstract
  :: DecimalPart
  -> Maybe ExponentPart
  -> (A.Coefficient, A.Exponent)
finiteToAbstract dp mep = (coe, ex)
  where
    ex = abstractExponent . actualExponent dp
      . givenExponent $ mep
    coe = abstractCoeff dp
    

-- | A numeric value for the exponent that was given in the input
-- string.

givenExponent :: Maybe ExponentPart -> Integer
givenExponent me = case me of
  Nothing -> 0
  Just (ExponentPart s ds) -> getSgn $ decemListToInt ds
    where
      getSgn = case s of
        Sign0 -> id
        Sign1 -> negate

-- | The number of digits after the decimal point, subtracted from
-- the numeric value for the exponent given in the string

actualExponent
  :: DecimalPart
  -> Integer
  -- ^ Output from 'givenExponent'
  -> Integer
actualExponent d i = case d of
  WholeFrac _ ds -> i - fromIntegral (length ds)
  _ -> i

-- The value of the abstract exponent.

abstractExponent
  :: Integer
  -- ^ The output from 'actualExponent'
  -> A.Exponent
abstractExponent = A.Exponent . A.intToFirmado

abstractCoeff :: DecimalPart -> A.Coefficient
abstractCoeff d =
  let ds = case d of
        WholeFrac d1 d2 -> d1 ++ d2
        WholeOnly d1 -> d1
  in A.Coefficient $ A.decemListToAut ds

stringToAbstract

  :: String
  -- ^ Input string

  -> Either String A.Abstract
  -- ^ Returns a Right with the abstract representation of the input
  -- string, if the input conformed to the numeric string
  -- specification given in the General Decimal Arithmetic
  -- Specification.  Otherwise, returns a Left with an error
  -- message.

stringToAbstract = fmap numericStringToAbstract . parseNumericString

-- | Transforms a 'Dec' to an 'Abstract'.
decToAbstract :: D.Dec -> A.Abstract
decToAbstract = either (error msg) id . stringToAbstract
  . BS8.unpack . D.toByteString
  where
    msg = "decToAbstract: error: could not parse output from "
      ++ "toByteString"
