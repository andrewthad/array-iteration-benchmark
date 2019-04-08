{-# language BangPatterns #-}

module HighToLowNeq
  ( incrementHighToLowNeq
  ) where

import Data.Primitive
import Control.Monad.Primitive

incrementHighToLowNeq :: MutablePrimArray RealWorld Int -> IO ()
incrementHighToLowNeq m = modify' (+1) m

-- | Strictly modify the elements of a mutable array in-place.
modify' :: (Prim a, PrimMonad m)
  => (a -> a)
  -> MutablePrimArray (PrimState m) a
  -> m ()
modify' f marr = do
  let go !ix = if ix /= (-1)
        then do
          x <- readPrimArray marr ix
          let !y = f x
          writePrimArray marr ix y
          go (ix - 1)
        else return ()
  !sz <- getSizeofMutablePrimArray marr
  go (sz - 1)
{-# inline modify' #-}


