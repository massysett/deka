module Deka.Internal.Unsafe where

import System.IO.Unsafe (unsafePerformIO)

unsafe0 :: IO a -> a
unsafe0 = unsafePerformIO

unsafe1 :: (a -> IO b) -> a -> b
unsafe1 f a = unsafePerformIO (f a)

unsafe2 :: (a -> b -> IO c) -> a -> b -> c
unsafe2 f a b = unsafePerformIO (f a b)

unsafe3 :: (a -> b -> c -> IO d) -> a -> b -> c -> d
unsafe3 f a b c = unsafePerformIO (f a b c)

unsafe4 :: (a -> b -> c -> d -> IO e) -> a -> b -> c -> d -> e
unsafe4 f a b c d = unsafePerformIO (f a b c d)

