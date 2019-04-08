{-# language BangPatterns #-}

import Criterion (bench,whnfIO)
import Criterion.Main (defaultMain)

import Data.Primitive (newPrimArray,setPrimArray)

import LowToHigh (incrementLowToHigh)
import HighToLowNeq (incrementHighToLowNeq)
import HighToLowCase (incrementHighToLowCase)
import HighToLowGt (incrementHighToLowGt)

main :: IO ()
main = do
  let sz = 100000
  !bigArr <- newPrimArray sz
  setPrimArray bigArr 0 sz (0 :: Int)
  defaultMain
    [ bench "HighToLowCaseA" (whnfIO (incrementHighToLowCase bigArr))
    , bench "HighToLowCaseB" (whnfIO (incrementHighToLowCase bigArr))
    , bench "HighToLowGt" (whnfIO (incrementHighToLowGt bigArr))
    , bench "LowToHigh" (whnfIO (incrementLowToHigh bigArr))
    , bench "HighToLowNeq" (whnfIO (incrementHighToLowNeq bigArr))
    ]
