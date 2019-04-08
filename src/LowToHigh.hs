{-# language BangPatterns #-}

module LowToHigh
  ( incrementLowToHigh
  ) where

import Data.Primitive
import Control.Monad.Primitive

-- Why not inline? See https://gitlab.haskell.org/ghc/ghc/issues/16556
{-# NOINLINE incrementLowToHigh #-}
incrementLowToHigh :: MutablePrimArray RealWorld Int -> IO ()
incrementLowToHigh m = modify' (+1) m

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

