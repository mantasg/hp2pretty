module Print (print, printKey) where

import Prelude hiding (print)
import Data.Array.Unboxed (UArray, bounds, (!))
import Data.Text.Lazy (Text, pack, unpack)
import Numeric (showFFloat)
import System.FilePath (replaceExtension)

import Types
import Graphics
import SVG (fillStyleName)

first :: (a -> c) -> (a, b) -> (c, b)
first f (a, b) = (f a, b)

filled0 :: Graphics -> Either PatternID RGB -> [Text] -> [Text]
filled0 gfx c = visual gfx (Just c) Nothing Nothing Nothing

print :: Graphics -> Bool -> Bool -> Bool -> Header -> [Double] -> [Double] -> [Text] -> UArray Int Double -> UArray (Int, Int) Double -> [Text]
print gfx notitle sepkey patterned header sticks vticks labels times coords =
  let bands = toPoints (bounds coords) times coords
      labels' = reverse labels
      filled = filled0 gfx
      (colours, defs)
        | patterned = first (map Left) . unzip $ map (pattern gfx) labels'
        | otherwise = (map (Right . colour) labels', [])
      polygons = concat . zipWith (\c ps -> filled c (polygon gfx ps)) colours . map (map p) $ bands
      key = concat . zipWith3 (keyBox (gW + border * 2.5) (border * 1.5) (gH / 16)) [(0::Int) ..] colours $ labels'
      keyBox x y0 dy i c l =
        let y = y0 + fromIntegral i * dy
        in  (filled c $ rect gfx (x, y + 0.1 * dy) (dy * 0.8, dy * 0.8)) ++
            text gfx Nothing Start 15 (x + dy, y + dy * 0.6) [l]
      w = 3280
      h = 720
      gW = (if sepkey then 1280 else 960) - 2 * border
      gH = if notitle then 720 - 2 * border else 720 - 3 * border
      border = 60
      textOffset = 10
      (xMin, xMax) = hSampleRange header
      (yMin, yMax) = hValueRange header
      gRange@((gx0,gy0),(gx1,gy1)) = ((border*1.5, gH + if notitle then border*0.5 else border*1.5), (gW + border*1.5, if notitle then border*0.5 else border*1.5))
      p = rescalePoint ((xMin, yMin), (xMax, yMax)) gRange
      title = text gfx Nothing Middle 25 (w / 2, border * 0.75) [hJob header, pack " (", hDate header, pack ")"]
      background = filled (Right white) $ rect gfx (0,0) (w,h)
      box = filled (Right white) $ rect gfx (gx0,gy1) (gW,gH)
      leftLabel = text gfx (Just (-90)) Middle 20 (border/2, (gy0 + gy1)/2) [hValueUnit header]
      leftTicks = concatMap (\(y,l) -> let { (x1, y1) = p (xMin, y) ; (x2, y2) = p (xMax, y) } in
          line gfx (x1 - border/2, y1) (x2, y2) ++
          if l then [] else text gfx Nothing End 15 (x1 - textOffset, y1 - textOffset) (showSI y)
        ) (zip vticks (replicate (length vticks - 1) False ++ [True]))
      bottomLabel = text gfx Nothing Middle 20 ((gx0 + gx1)/2, gy0 + border) [hSampleUnit header]
      bottomTicks = concatMap (\(x,l) -> let { (x1, y1) = p (x, yMin) ; (x2, y2) = p (x, yMax) } in
          line gfx (x1, y1 + border/2) (x2, y2) ++
          if l then [] else text gfx Nothing Start 15 (x1 + textOffset, y1+2*textOffset) (showSI x)
        ) (zip sticks (replicate (length sticks - 1) False ++ [True]))
  in  document gfx (w,h) (concat defs) . concat $
        [ background
        , visual gfx (Just (Right black)) Nothing (Just black) (Just 1) $ concat $
            (if notitle then [] else [title]) ++
            [ leftLabel
            , bottomLabel
            , leftTicks
            , bottomTicks
            , visual gfx Nothing (Just 0.7) Nothing Nothing $ concat $
                [ box
                , polygons
                ] ++ if sepkey then [] else [key]
            ]
        ]

printKey :: Graphics -> Bool {- ^ patterned -} -> Text -> (FilePath, [Text])
printKey gfx True label =
  let (pname, pdef) = pattern gfx label
  in  printKey' gfx pdef (Left pname) label
printKey gfx False label =
  let cname = colour label
  in  printKey' gfx [] (Right cname) label

printKey' :: Graphics -> [Text] -> Either PatternID RGB -> Text -> (FilePath, [Text])
printKey' gfx defs c label =
  ( replaceExtension (unpack (fillStyleName c)) "svg"
  , document gfx (boxSize, boxSize) defs (filled0 gfx c $ rect gfx (0, 0) (boxSize, boxSize))
  )

boxSize :: Double
boxSize = 27

toPoints :: ((Int,Int),(Int,Int)) -> UArray Int Double -> UArray (Int,Int) Double -> [[(Double,Double)]]
toPoints ((b0,s0),(b1,s1)) times coords =
  [ fwd ++ rwd
  | b <- [b1, b1 - 1 .. b0 + 1]
  , let fwd = [ (times ! s, coords ! (b - 1, s)) | s <- up b ]
  , let rwd = [ (times ! s, coords ! (b, s)) | s <- down b ]
  ]
  where
    -- ugly hack to force recomputation instead of sharing a large list
    up   b = [s0 + b - b .. s1]
    down b = [s1 + b - b, s1 - 1 .. s0]

showSI :: Double -> [Text]
showSI x | x < 1e3   = [ showF  x       ""  ]
         | x < 1e6   = [ showF (x/1e3 ) "k" ]
         | x < 1e9   = [ showF (x/1e6 ) "M" ]
         | x < 1e12  = [ showF (x/1e9 ) "G" ]
         | x < 1e15  = [ showF (x/1e12) "T" ]
         | otherwise = [ showF (x/1e15) "P" ]
  where
    showF y si = let (xs, ys) = break ('.' ==) $ showFFloat (Just 3) y ""
                     zs = reverse . dropWhile ('0' ==) . reverse . drop 1 $ ys
                in  case zs of
                      "" -> pack $ xs ++ si
                      ws -> pack $ xs ++ "." ++ ws ++ si
