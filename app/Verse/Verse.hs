module Verse.Verse where

import Zero.Zero

import Verse.Types
import Verse.Conf

import Terminal.Game (
   Draw,
   color, rgbColor, paletteColor,
   sRGB24, sRGBBounded, sRGB, sRGB24read,
   xterm6LevelRGB, xterm24LevelGray, xtermSystem )
import Data.Word ( Word8 )
import Control.Arrow
import Data.Tuple ( swap )

{- Each node connects to it's adjacent 6


       + + + +              z z z x x x y y y
      z z z x +            - - - - - - - - -
     z z z x x +          x x x y y y z z z
    z z z x x x +   ->   x x x y y y z z z
     x x y y y +        x x x y y y z z z
      x y y y +        - - - - - - - - -
       y y y +        y y y z z z x x x


       + + + +            + + + +               6 7 8 0 1 2 3 4 5      0 0 0 0 0 0 0 0 0
      6 7 8 0 +          2 2 2 2 +             - - - - - - - - -      - - - - - - - - -
     6 7 8 0 1 +        1 1 1 1 1 +           0 1 2 3 4 5 6 7 8      2 2 2 2 2 2 2 2 2
    6 7 8 0 1 2 +      0 0 0 0 0 0 +         0 1 2 3 4 5 6 7 8      1 1 1 1 1 1 1 1 1
     1 2 3 4 5 +        2 2 2 2 2 +         0 1 2 3 4 5 6 7 8      0 0 0 0 0 0 0 0 0
      2 3 4 5 +          1 1 1 1 +         - - - - - - - - -      - - - - - - - - -
       3 4 5 +            0 0 0 +         3 4 5 6 7 8 0 1 2      2 2 2 2 2 2 2 2 2

         x                  y                     x                      y

-}

pixel :: Some -> Char
pixel = (" ·~+=≠co" !!) . fromEnum

stone :: Some -> Draw
stone S0 = paletteColor $ xterm6LevelRGB 0 1 0  -- xterm24LevelGray 2
stone S1 = paletteColor $ xterm6LevelRGB 0 1 1  -- xterm24LevelGray 5
stone S2 = paletteColor $ xterm6LevelRGB 1 1 2  -- xterm24LevelGray 8
stone S3 = paletteColor $ xterm6LevelRGB 2 1 3  -- xterm24LevelGray 11
stone S4 = paletteColor $ xterm6LevelRGB 3 1 4  -- xterm24LevelGray 14
stone S5 = paletteColor $ xterm6LevelRGB 4 2 5  -- xterm24LevelGray 17
stone S6 = paletteColor $ xterm6LevelRGB 5 3 5  -- xterm24LevelGray 23
stone S7 = paletteColor $ xterm6LevelRGB 5 4 5  -- xterm24LevelGray 23

verse :: Verse
verse = fromList $ take (width * height) $ (id &&& atom) <$> [0..]

atom :: Int -> Node
atom i = (Atom S1,adjacents)
   where
   adjacents = ($ i) . move 1 <$> total
-- (x,y) = indexToCoord i
-- (q,r) = quotRem i width

sup :: (Some -> Some) -> Int -> Verse -> Verse
sup f i v = adjust (const (Atom (f s),ns)) i v
   where
   (Atom s,ns) = v ! i

sal :: Node -> Int
sal (Atom s,_) = fromEnum s

coordToIndex :: (Int,Int) -> Int
coordToIndex (x,y) = y * width + x

indexToCoord :: Int -> (Int,Int)
indexToCoord = swap . flip divMod width

distance :: Int -> Int -> Float
distance a b = undefined

move :: Int -> Dir -> Int -> Int
move n d i
   | U <- d = coordToIndex $ f (mod (pred x) width , y + n)
   | I <- d = coordToIndex $ f (x , y + n)
   | H <- d = coordToIndex $ f (mod (x - n) width , y)
   | L <- d = coordToIndex $ f (mod (x + n) width , y)
   | N <- d = coordToIndex $ f (x , y - n)
   | M <- d = coordToIndex $ f (mod (succ x) width , y - n)
   where
   (x,y) = indexToCoord i
   f (x',y')
      | y' >= height || y' < 0 = (mod (x' + t * 2 * pred radius) width , mod y' height)
      | otherwise = (x' , y')
      where
      t = div y' height  -- outbound multiplier

