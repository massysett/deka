module Deka.Internal.Unsafe where

import System.IO.Unsafe (unsafePerformIO)

unsafe0 :: IO a -> a
unsafe0 = unsafePerformIO

unsafe1 :: (a -> IO b) -> a -> b
unsafe1 = fmap unsafePerformIO

unsafe2 :: (a -> b -> IO c) -> a -> b -> c
unsafe2 = fmap (fmap unsafePerformIO)

unsafe3 :: (a -> b -> c -> IO d) -> a -> b -> c -> d
unsafe3 = fmap (fmap (fmap unsafePerformIO))

unsafe4 :: (a -> b -> c -> d -> IO e) -> a -> b -> c -> d -> e
unsafe4 = fmap (fmap (fmap (fmap unsafePerformIO)))

