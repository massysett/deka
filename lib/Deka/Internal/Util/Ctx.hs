{-# LANGUAGE Safe #-}
module Deka.Internal.Util.Ctx where

import Deka.Internal.Mpdec
import Deka.Internal.Context
import Foreign.Safe

type Unary
  = Mpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

unary :: Unary -> Dec -> Ctx Dec
unary f d = Ctx $ \p ->
  newDec $ \nw ->
  withDec d $ \old ->
  f nw old p

type Binary
  = Mpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

binary :: Binary -> Dec -> Dec -> Ctx Dec
binary f a b = Ctx $ \p ->
  newDec $ \nw ->
  withDec a $ \pa ->
  withDec b $ \pb ->
  f nw pa pb p

type Ternary
  = Mpd
  -> CMpd
  -> CMpd
  -> CMpd
  -> Ptr C'mpd_context_t
  -> IO ()

ternary :: Ternary -> Dec -> Dec -> Dec -> Ctx Dec
ternary f a b c = Ctx $ \p ->
  newDec $ \n ->
  withDec a $ \pa ->
  withDec b $ \pb ->
  withDec c $ \pc ->
  f n pa pb pc p
