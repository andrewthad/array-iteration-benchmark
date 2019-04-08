{-# language BangPatterns #-}

{-# OPTIONS_GHC -O2 #-}

module LowToHigh
  ( incrementLowToHigh
  ) where

import Data.Primitive
import Control.Monad.Primitive

incrementLowToHigh :: MutablePrimArray RealWorld Int -> IO ()
incrementLowToHigh m = modify' (+1) m

-- | Strictly modify the elements of a mutable array in-place.
modify' :: (Prim a, PrimMonad m)
  => (a -> a)
  -> MutablePrimArray (PrimState m) a
  -> m ()
modify' f marr = do
  !sz <- getSizeofMutablePrimArray marr
  let go !ix = if ix < sz
        then do
          x <- readPrimArray marr ix
          let !y = f x
          writePrimArray marr ix y
          go (ix + 1)
        else return ()
  go 0
{-# inline modify' #-}

