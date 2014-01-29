{-# LANGUAGE Safe #-}

module Data.Deka
  ( Deka(..)
  , DekaT(..)
  , crashy
  , integralToDeka
  , strToDeka
  , checked
  ) where

import Control.Arrow hiding (left)
import Control.Monad.Trans.Class
import Data.Deka.Pure
import Data.List (intersperse)
import qualified Data.Deka.Pure as P
import qualified Data.ByteString.Char8 as BS8
import Control.Monad.Trans.Either

flagsErrorMessage :: Flags -> Either String ()
flagsErrorMessage fl = case flagList fl of
  [] -> Right ()
  xs -> Left ("flags set: " ++
                (concat . intersperse ", " $ xs))

checked :: Env a -> Either String a
checked a =
  let (r, fl) = runEnv a
  in flagsErrorMessage fl >> return r

eval :: Env c -> c
eval = either (error . ("Deka: error: " ++)) id . checked

evalEither :: EitherT String Env a -> a
evalEither
  = either checkOuterE id
  . checked
  . fmap (either checkE id)
  . runEitherT
  where
    checkE = error . ("Deka: Deka error: " ++)
    checkOuterE = error . ("Deka: decQuad error: " ++)

eqDec :: Quad -> Quad -> EitherT String Env Bool
eqDec x y = fmap (== EQ) $ cmpDec x y

cmpDec :: Quad -> Quad -> EitherT String Env Ordering
cmpDec x y = do
  r <- lift $ P.compare x y
  decToOrd r

cmpDecTotal :: Quad -> Quad -> EitherT String Env Ordering
cmpDecTotal x y = do
  r <- lift $ P.compareTotal x y
  decToOrd r

eqDecTotal :: Quad -> Quad -> EitherT String Env Bool
eqDecTotal x y = fmap (== EQ) $ cmpDecTotal x y

-- | Runs monadic actions.  When the first action returns True,
-- return the corresponding result.  If no action returns True,
-- return the default result.
successfulPair :: Monad m => m r -> [(m Bool, r)] -> m r
successfulPair d ls = case ls of
  [] -> d
  x:xs -> do
    r <- fst x
    if r then return (snd x) else successfulPair d xs

-- | Runs an action. If it is true, run the next set of actions,
-- otherwise return the given result.
runIf :: Monad m => m r -> m Bool -> m r -> m r
runIf dflt a rs = do
  r <- a
  if r then rs else dflt

decToOrd :: Quad -> EitherT String Env Ordering
decToOrd d
  = runIf (left "decToOrd: non-finite operand") (lift (isFinite d))
  .  successfulPair (left "decToOrd: nonsense result")
  . map (first lift)
  $ [ (isPositive d, GT)
    , (isZero d, EQ)
    , (isNegative d, LT)
    ]

showDec :: Quad -> String
showDec = BS8.unpack . eval . toString

newtype Deka = Deka { unDeka :: Quad }

-- | Eq compares by value.  For instance, @3.5 == 3.500@.
instance Eq Deka where
  Deka x == Deka y = evalEither $ eqDec x y

-- | Ord compares by value.  For instance, @compare 3.5 3.500 ==
-- EQ@.
instance Ord Deka where
  compare (Deka x) (Deka y) = evalEither $ cmpDec x y

-- | Show always converts to scientific notation.
instance Show Deka where
  show = showDec . unDeka

instance Num Deka where
  Deka x + Deka y = Deka . eval $ P.add x y
  Deka x - Deka y = Deka . eval $ P.subtract x y
  Deka x * Deka y = Deka . eval $ P.multiply x y
  negate = Deka . eval . P.minus . unDeka
  abs = Deka . eval . P.abs . unDeka
  signum (Deka x)
    | f isZero = fromInteger 0
    | f isNegative = fromInteger (-1)
    | otherwise = fromInteger 1
    where
      f g = eval . g $ x
  fromInteger = either (error . ("Deka: fromInteger: error: " ++))
    id . integralToDeka

-- | Multiprecision decimals with a total ordering.
newtype DekaT = DekaT { unDekaT :: Deka }
  deriving Show

-- | Eq compares by a total ordering.
instance Eq DekaT where
  DekaT (Deka x) == DekaT (Deka y) = evalEither $ eqDecTotal x y

-- | Ord compares by a total ordering.
instance Ord DekaT where
  compare (DekaT (Deka x)) (DekaT (Deka y)) = evalEither $ cmpDecTotal x y

crashy :: Either String a -> a
crashy = either (error . ("Deka: error: " ++)) id


integralToDeka :: Integral a => a -> Either String Deka
integralToDeka i = do
  coe <- coefficient . fromIntegral $ i
  en <- P.exponent 0
  let d = Decoded sgn (Finite coe en)
      sgn = if i < 0 then Negative else Positive
  fmap Deka . checked $ encode d

strToDeka :: String -> Either String Deka
strToDeka s =
  fmap Deka . fst . runEnv $ do
    d <- fromString (BS8.pack s)
    fl <- getStatus
    case flagsErrorMessage fl of
      Left e -> return $ Left e
      Right _ -> do
        fin <- isFinite d
        return $ if not fin then (Left "result not finite")
          else Right d
