module Pretty (pretty) where

import Control.Monad (forM_, liftM2)
import Data.Array.MArray (thaw)
import Data.Array.ST (runSTUArray, writeArray, readArray)
import Data.Array.Unboxed (UArray, bounds)
import Data.Text.Lazy (Text, pack)
import Data.List (sortBy)
import Data.Map (Map, toList)
import Data.Ord (comparing)

import Types

pretty :: Bool -> Header -> UArray (Int, Int) Double -> Map Text Int -> (([Double], [Double]), ([Text], UArray (Int, Int) Double))
pretty noTraces header vals bs =
  let sticks = uncurry (ticks 20) (hSampleRange header)
      vticks = uncurry (ticks 20) (hValueRange header)
      labels = pack "(trace elements)" : (map fst . sortBy (comparing snd) . toList $ bs)
      coords = accumulate noTraces vals
  in  ((sticks, vticks), (labels, coords))

accumulate :: Bool -> UArray (Int, Int) Double -> UArray (Int, Int) Double
accumulate noTraces a0 = runSTUArray $ do
  let ((b0,s0),(b1,s1)) = bounds a0
  a <- thaw a0
  forM_ [s0 .. s1] $ \s ->
    forM_ [b0 + 1 .. b1] $ \b -> do
      x <- readArray a (b - 1, s)
      y <- readArray a (b, s)
      if b == 0 && noTraces
        then writeArray a (b, s) 0
        else writeArray a (b, s) (x + y)
  return a

ticks :: Int -> Double -> Double -> [Double]
ticks n mi ma =
  let k = nearestNice $ (ma - mi) / fromIntegral n
      m0 = fromIntegral (ceiling (mi / k) :: Integer) * k
      m1 = fromIntegral (floor   (ma / k) :: Integer) * k
  in  [m0, m0 + k .. m1 ]

nearestNice :: Double -> Double
nearestNice k0 = head . dropWhile (< k0) $ nices

nices :: [Double]
nices = [ f * k | f <- map (10**) [-6 ..], k <- [1,2,5] ]
