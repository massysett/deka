{-# LANGUAGE MultiParamTypeClasses #-}
module Deka.Interface where

import qualified Deka.Classes as C
import Deka.Internal.Context
import Deka.Internal.Decnumber.Context
import Data.Int
import Foreign.Safe
import Prelude hiding (abs)

unary
  :: C.Unary a
  -> (w -> ForeignPtr a)
  -> (Int32 -> IO w)
  -> w
  -> Ctx w
unary f up fty w = Ctx $ \pCtx -> do
  pcsn <- peek (p'decContext'digits pCtx)
  nw <- fty pcsn
  let fp = up nw
  withForeignPtr fp $ \pNew ->
    withForeignPtr (up w) $ \pOp -> do
      _ <- f pNew pOp pCtx
      return nw

abs :: C.Arith w a => w -> Ctx w
abs = unary C.abs C.unwrap C.create
