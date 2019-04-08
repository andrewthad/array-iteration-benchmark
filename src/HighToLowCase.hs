{-# language BangPatterns #-}

module HighToLowCase
  ( incrementHighToLowCase
  ) where

import Data.Primitive
import Control.Monad.Primitive

incrementHighToLowCase :: MutablePrimArray RealWorld Int -> IO ()
incrementHighToLowCase m = modify' (+1) m

-- | Strictly modify the elements of a mutable array in-place.
modify' :: (Prim a, PrimMonad m)
  => (a -> a)
  -> MutablePrimArray (PrimState m) a
  -> m ()
modify' f marr = do
  let go !ix = case ix of
        (-1) -> return ()
        _ -> do
          x <- readPrimArray marr ix
          let !y = f x
          writePrimArray marr ix y
          go (ix - 1)
  !sz <- getSizeofMutablePrimArray marr
  go (sz - 1)
{-# inline modify' #-}


