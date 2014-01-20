{-# LANGUAGE Safe #-}

module Data.Deka
  ( Deka(..)
  , DekaT(..)
  , crashy
  , integralToDeka
  , strToDeka
  , checked
  ) where

import Control.Monad.Trans.Class
import Data.Deka.Safe
import qualified Data.Deka.Safe as S
import Data.Deka.Pure
import qualified Data.ByteString.Char8 as BS8
import Control.Monad.Trans.Either

checkSignals :: Env (Maybe String)
checkSignals = eitherT (return . Just) (const (return Nothing)) $ do
  st <- lift getStatus
  let f s g = if isSet (unStatus st) g
        then left s
        else return ()
  f "ieeeInvalidOperation" ieeeInvalidOperation
  f "clamped" clamped
  f "divisionByZero" divisionByZero
  f "fpuError" fpuError
  f "inexact" inexact
  f "notImplemented" notImplemented
  f "overflow" overflow
  f "rounded" rounded
  f "subnormal" subnormal
  f "underflow" underflow

eqMpd :: Mpd -> Mpd -> Bool
eqMpd x y = 
  case eval $ cmp x y of
    EQ -> True
    _ -> False

cmpMpd :: Mpd -> Mpd -> Ordering
cmpMpd x y = eval $ cmp x y

showMpd :: Mpd -> String
showMpd
  = BS8.unpack . eval . mpdToSci expLower

newtype Deka = Deka { unDeka :: Mpd }

-- | Eq compares by value.  For instance, @3.5 == 3.500@.
instance Eq Deka where
  Deka x == Deka y = eqMpd x y

-- | Ord compares by value.  For instance, @compare 3.5 3.500 ==
-- EQ@.
instance Ord Deka where
  compare (Deka x) (Deka y) = cmpMpd x y

-- | Show always converts to scientific notation.
instance Show Deka where
  show = showMpd . unDeka

addMpd :: Mpd -> Mpd -> Mpd
addMpd x y = eval $ S.add x y

subtMpd :: Mpd -> Mpd -> Mpd
subtMpd x y = eval $ S.sub x y

multMpd :: Mpd -> Mpd -> Mpd
multMpd x y = eval $ S.mul x y

instance Num Deka where
  Deka x + Deka y = Deka $ addMpd x y
  Deka x - Deka y = Deka $ subtMpd x y
  Deka x * Deka y = Deka $ multMpd x y
  negate = Deka . eval . S.minus . unDeka
  abs = Deka . eval . S.abs . unDeka
  signum (Deka x)
    | f isZero = fromInteger 0
    | f isNegative = fromInteger (-1)
    | otherwise = fromInteger 1
    where
      f g = eval . g $ x
  fromInteger = Deka . eval . setIntegral

checked :: Env a -> Either String a
checked a = evalEnvPure maxContext $ do
  r <- a
  ck <- checkSignals
  return $ case ck of
    Nothing -> Right r
    Just err -> Left err

eval :: Env c -> c
eval = either (error . ("Deka: error: " ++)) id . checked

-- | Multiprecision decimals with a total ordering.
newtype DekaT = DekaT { unDekaT :: Deka }
  deriving Show

-- | Eq compares by a total ordering.
instance Eq DekaT where
  x == y = compare x y == EQ

-- | Ord compares by a total ordering.
instance Ord DekaT where
  compare (DekaT (Deka x)) (DekaT (Deka y)) = eval $ cmpTotal x y

fromStr :: BS8.ByteString -> Either String Mpd
fromStr = checked . setString 

strToDeka :: BS8.ByteString -> Either String Deka
strToDeka = fmap (fmap Deka) fromStr

crashy :: Either String a -> a
crashy = either (error . ("Deka: error: " ++)) id

fromInt :: Integral a => a -> Either String Mpd
fromInt = checked . setIntegral

integralToDeka :: Integral a => a -> Either String Deka
integralToDeka = fmap (fmap Deka) fromInt

format
  :: BS8.ByteString
  -- ^ Formatting string.  This should be ASCII, or expect trouble.
  -> Deka
  -> Either String BS8.ByteString
format fmt = checked . mpdFormat fmt . unDeka
