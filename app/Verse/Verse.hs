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

   o o o o o o o o   0 1 2 3 4 5 6 7
    o o o o o o o o   8 9 ...
   o o o o o o o o
    o o o o o o o o
   o o o o o o o o
    o o o o o o o o
   o o o o o o o o

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
   adjacents
      | even y = ($ i) . move 1 <$> total
      |  odd y = ($ i) . move 1 <$> total
   (q,r) = quotRem i width
   (x,y) = indexToCoord i

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
   | U <- d = coordToIndex (mod (pred x') width , mod (y - n) height)
   | I <- d = coordToIndex (x' , mod (y - n) height)
   | H <- d = coordToIndex (mod (x - n) width , y)
   | L <- d = coordToIndex (mod (x + n) width , y)
   | N <- d = coordToIndex (mod (pred x') width , mod (y + n) height)
   | M <- d = coordToIndex (x' , mod (y + n) height)
   where
   (x,y) = indexToCoord i
   x' = if odd y then mod (succ x) width else x

