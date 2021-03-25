module Prune
  ( prune
  , Compare
  , cmpName
  , cmpSize
  , cmpStdDev
  ) where

import Data.Text.Lazy (Text, isInfixOf, pack)
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import Data.Map.Strict (Map, toList, fromList, filterWithKey)

type Compare a = a -> a -> Ordering

cmpName, cmpSize, cmpStdDev
  :: (Compare (Text, (Double, Double)), Compare (Text, (Double, Double)))
cmpName = (cmpNameAscending, cmpNameDescending)
cmpSize = (cmpSizeDescending, cmpSizeAscending)
cmpStdDev = (cmpStdDevDescending, cmpStdDevAscending)

cmpNameAscending, cmpNameDescending,
  cmpStdDevAscending, cmpStdDevDescending,
  cmpSizeAscending, cmpSizeDescending :: Compare (Text, (Double, Double))
cmpNameAscending = comparing fst
cmpNameDescending = flip cmpNameAscending
cmpStdDevAscending = comparing (snd . snd)
cmpStdDevDescending = flip cmpStdDevAscending
cmpSizeAscending = comparing (fst . snd)
cmpSizeDescending = flip cmpSizeAscending

prune :: [String] -> Compare (Text, (Double, Double)) -> Double -> Int -> Map Text (Double, Double) -> Map Text Int
prune filters cmp tracePercent nBands ts =
  let ts' = filterWithKey (\k _ -> applyFilter (map pack filters) k) ts
      ccTotals = sortBy cmpSizeDescending (toList ts')
      sizes = map (fst . snd) ccTotals
      total = sum' sizes
      limit = (1 - tracePercent / 100) * total
      bigs = takeWhile (< limit) . scanl (+) 0 $ sizes
      bands = zipWith const ccTotals $ take nBands bigs
      ccs = map fst (sortBy cmp bands)
  in  fromList (reverse ccs `zip` [1 ..])

sum' :: [Double] -> Double
sum' = foldl' (+) 0

applyFilter :: [Text] -> Text -> Bool
applyFilter [] _ = True
applyFilter fs t = any (\f -> f `isInfixOf` t) fs
