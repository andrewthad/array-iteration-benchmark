{-# language BangPatterns #-}

module HighToLowCase
  ( incrementHighToLowCase
  ) where

import Data.Primitive
import Control.Monad.Primitive

-- Why not inline? See https://gitlab.haskell.org/ghc/ghc/issues/16556
{-# NOINLINE incrementHighToLowCase #-}
incrementHighToLowCase :: MutablePrimArray RealWorld Int -> IO ()
incrementHighToLowCase m = modify' (+1) m

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


