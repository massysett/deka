{-# LANGUAGE ForeignFunctionInterface, Safe #-}

#include <stdint.h>

module Deka.Decnumber.Types where

import Foreign.Safe

c'NULL :: Num a => a
c'NULL = #const NULL

type C'int32_t = #type int32_t
type C'uint8_t = #type uint8_t
type C'uint16_t = #type uint16_t
type C'uint32_t = #type uint32_t
type C'uint64_t = #type uint64_t

