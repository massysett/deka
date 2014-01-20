{-# LANGUAGE Safe #-}

module Data.Deka
  ( Deka(..)
  , DekaT(..)
  , strToDeka
  , strToDekaT
  , crashy
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

eval :: Env c -> c
eval e = evalEnvPure maxContext $ do
  r <- e
  ck <- checkSignals
  case ck of
    Nothing -> return r
    Just err -> error $ "Deka: error: " ++ err

-- | Multiprecision decimals with a total ordering.
newtype DekaT = DekaT { unDekaT :: Mpd }

-- | Eq compares by a total ordering.
instance Eq DekaT where
  x == y = compare x y == EQ

-- | Ord compares by a total ordering.
instance Ord DekaT where
  compare (DekaT x) (DekaT y) = eval $ cmpTotal x y

instance Show DekaT where
  show = showMpd . unDekaT

instance Num DekaT where
  DekaT x + DekaT y = DekaT $ addMpd x y
  DekaT x - DekaT y = DekaT $ subtMpd x y
  DekaT x * DekaT y = DekaT $ multMpd x y
  negate = DekaT . eval . S.minus . unDekaT
  abs = DekaT . eval . S.abs . unDekaT
  signum (DekaT x)
    | f isZero = fromInteger 0
    | f isNegative = fromInteger (-1)
    | otherwise = fromInteger 1
    where
      f g = evalEnvPure maxContext . g $ x
  fromInteger = DekaT . eval . setIntegral


fromStr :: String -> Either String Mpd
fromStr s = evalEnvPure maxContext $ do
  r <- setString (BS8.pack s)
  c <- checkSignals
  case c of
    Nothing -> return . Right $ r
    Just g -> return . Left $ g

strToDeka :: String -> Either String Deka
strToDeka = fmap (fmap Deka) fromStr

strToDekaT :: String -> Either String DekaT
strToDekaT = fmap (fmap DekaT) fromStr

crashy :: Either String a -> a
crashy = either (error . ("Deka: error: " ++)) id
