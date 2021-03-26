{-# LANGUAGE BangPatterns #-}
module Total (total) where

import Control.Monad.State.Strict (State(), execState, get, put)
import Data.List (foldl')
import Data.Map (Map, empty, lookup, insert, alter)
import Prelude hiding (init, lookup, lines, words, drop, length)
import Data.Text.Lazy (Text, init, pack, unpack, lines, words, isPrefixOf, drop, length, toStrict)
import Data.Attoparsec.Text (parseOnly, double)
import Data.Maybe (fromMaybe)

import Types

data Parse =
  Parse
  { symbols   :: !(Map Text Text) -- intern symbols to save RAM
  , totals    :: !(Map Text (Double, Double)) -- compute running totass and total of squares
  , sampleMin :: !Double
  , sampleMax :: !Double
  , valueMin  :: !Double
  , valueMax  :: !Double
  , count     :: !Int                         -- number of frames
  }

parse0 :: Parse
parse0 = Parse{ symbols = empty, totals = empty, sampleMin = 0, sampleMax = 0, valueMin = 0, valueMax = 0, count = 0 }

total :: Maybe Double -> Maybe Double -> Text -> (Header, Map Text (Double, Double))
total minX maxX s =
  let ls = lines s
      (hs, ss) = splitAt 4 ls
      [job, date, smpU, valU] = zipWith header [sJOB, sDATE, sSAMPLE_UNIT, sVALUE_UNIT] hs
      parse1 = flip execState parse0 . mapM_ (parseFrame minX maxX) . chunkSamples $ ss
  in  ( Header
        { hJob        = job
        , hDate       = date
        , hSampleUnit = smpU
        , hValueUnit  = valU
        , hSampleRange= (fromMaybe (sampleMin parse1) minX, fromMaybe (sampleMax parse1) maxX)
        , hValueRange = (valueMin parse1, valueMax parse1)
        , hCount      = count parse1
        }
      , fmap (stddev $ fromIntegral (count parse1)) (totals parse1)
      )

stddev :: Double -> (Double, Double) -> (Double, Double)
stddev s0 (s1, s2) = (s1, sqrt (s0 * s2 - s1 * s1) / s0)

header :: Text -> Text -> Text
header name h =
  if name `isPrefixOf` h
  then init . drop (length name + 2) $ h -- drop the name and the quotes
  else error $ "Parse.header: expected " ++ unpack name

chunkSamples :: [Text] -> [[Text]]
chunkSamples [] = []
chunkSamples (x:xs)
  | sBEGIN_SAMPLE `isPrefixOf` x =
      let (ys, zs) = break (sEND_SAMPLE `isPrefixOf`) xs
      in  case zs of
            [] -> [] -- discard incomplete sample
            (_:ws) -> (x:ys) : chunkSamples ws
  | otherwise = [] -- expected BEGIN_SAMPLE or EOF...

parseFrame :: Maybe Double -> Maybe Double -> [Text] -> State Parse ()
parseFrame _ _ [] = error "Parse.parseFrame: empty"
parseFrame minX maxX (l:ls) = do
  let !time = sampleTime sBEGIN_SAMPLE l
  samples <- mapM inserter ls
  p <- get
  let v = foldl' (+) 0 samples
      sMin = if count p == 0 then time else time `min` sampleMin p
      sMax = if count p == 0 then time else time `max` sampleMax p
      vMin = if checkMin minX time && checkMax maxX time then v `min` valueMin p else valueMin p
      vMax = if checkMin minX time && checkMax maxX time then v `max` valueMax p else valueMax p

  put $! p{ count = count p + 1, sampleMin = sMin, sampleMax = sMax, valueMin = vMin, valueMax = vMax }

checkMin, checkMax :: Maybe Double -> Double -> Bool
checkMin Nothing _ = True
checkMin (Just m) v = v >= m
checkMax Nothing _ = True
checkMax (Just m) v = v <= m

inserter :: Text -> State Parse Double
inserter s = do
  let [k,vs] = words s
      !v = readDouble vs
  p <- get
  k' <- case lookup k (symbols p) of
    Nothing -> do
      put $! p{ symbols = insert k k (symbols p) }
      return k
    Just kk -> return kk
  p' <- get
  put $! p'{ totals = alter (accum  v) k' (totals p') }
  return $! v

accum :: Double -> Maybe (Double, Double) -> Maybe (Double, Double)
accum x Nothing  = Just $! (((,) $! x) $! (x * x))
accum x (Just (y, yy)) = Just $! (((,) $! (x + y)) $! (x * x + yy))

sampleTime :: Text -> Text -> Double
sampleTime name h =
  if name `isPrefixOf` h
  then readDouble .  drop (length name + 1) $ h
  else error $ "Parse.sampleTime: expected " ++ unpack name ++ " but got " ++ unpack h

readDouble :: Text -> Double
readDouble s = case parseOnly double (toStrict s) of
  Right x -> x
  _ -> error $ "Parse.readDouble: no parse " ++ unpack s

sJOB, sDATE, sSAMPLE_UNIT, sVALUE_UNIT, sBEGIN_SAMPLE, sEND_SAMPLE :: Text
sJOB = pack "JOB"
sDATE = pack "DATE"
sSAMPLE_UNIT = pack "SAMPLE_UNIT"
sVALUE_UNIT = pack "VALUE_UNIT"
sBEGIN_SAMPLE = pack "BEGIN_SAMPLE"
sEND_SAMPLE = pack "END_SAMPLE"
