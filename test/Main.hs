{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (when)
import Data.Primitive
import GHC.Exts (RealWorld)

import HighToLowGt (incrementHighToLowGt)
import LowToHigh (incrementLowToHigh)

attempt :: (MutablePrimArray RealWorld Int -> IO ()) -> IO Bool
attempt filler = do
  marr <- newPrimArray 100
  setPrimArray marr 0 100 (0 :: Int)
  filler marr
  arr <- unsafeFreezePrimArray marr
  pure (arr == fromList (replicate 100 1))

main :: IO ()
main = do
  b1 <- attempt incrementLowToHigh
  when (not b1) $ do
    fail "LowToHigh"
  b2 <- attempt incrementHighToLowGt
  when (not b2) $ do
    fail "HighToLowGt"
  putStrLn "Success!"
