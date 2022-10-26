module Verse.Verse where

import Zero.Zero

import Verse.Types
import Verse.Conf

import Terminal.Game (
   Draw,
   color, rgbColor, paletteColor,
   sRGB24, sRGBBounded, sRGB, sRGB24read,
   xterm6LevelRGB, xterm24LevelGray, xtermSystem )
import Control.Arrow
import Data.Tuple ( swap )

-- art

pixel :: Layer -> Atom -> Char
pixel l a
   | Plasma <- υ a = " ·~+=≠co" !! fromEnum (α a)
   | _ <- υ a = 'x'
-- | _ <- υ a = '#'
-- | _ <- υ a = ':'
-- | _ <- υ a = '>'
-- | _ <- υ a = '<'
-- | _ <- υ a = ".',\":;*^" !! fromEnum (α a)
-- | _ <- υ a = "░▒▓█░▒▓█" !! fromEnum (α a)

stone :: Layer -> Atom -> Draw
stone l a
   | S0 <- α a = paletteColor $ xterm6LevelRGB 0 1 0  -- xterm24LevelGray 2
   | S1 <- α a = paletteColor $ xterm6LevelRGB 0 1 1  -- xterm24LevelGray 5
   | S2 <- α a = paletteColor $ xterm6LevelRGB 1 1 2  -- xterm24LevelGray 8
   | S3 <- α a = paletteColor $ xterm6LevelRGB 2 1 3  -- xterm24LevelGray 11
   | S4 <- α a = paletteColor $ xterm6LevelRGB 3 1 4  -- xterm24LevelGray 14
   | S5 <- α a = paletteColor $ xterm6LevelRGB 4 2 5  -- xterm24LevelGray 17
   | S6 <- α a = paletteColor $ xterm6LevelRGB 5 3 5  -- xterm24LevelGray 23
   | S7 <- α a = paletteColor $ xterm6LevelRGB 5 4 5  -- xterm24LevelGray 23

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

-- initial state

state :: State
state = State {
   ν = verse ,
   λ = Test ,
   σ = Terra ,
   ρ = [] ,
   ι = ' ' ,
   φ = 0 ,
   τ = mempty ,
   κ = 0 ,
   μ = Pause }

-- initial universe

verse :: Verse
verse = fromList $ take (width * height) $ (id &&& n) <$> [0..]
   where
   n :: Int -> Node
   n i = (atom , ($ i) . move 1 <$> total)

-- get node sval
sal :: Node -> Int
sal (a,_) = fromEnum (α a)

-- update node sval
sup :: (Some -> Some) -> Node -> Node
sup f (a,ns) = (a { α = f (α a) } , ns)

-- update specific atom sval in verse
val :: Verse -> Int -> Int
val v i = sal (v ! i)

-- update specific atom sval in verse
vup :: (Some -> Some) -> Int -> Verse -> Verse
vup f = adjust (sup f)

distance :: Int -> Int -> Float
distance a b = undefined

coordToIndex :: (Int,Int) -> Int
coordToIndex (x,y) = y * width + x

indexToCoord :: Int -> (Int,Int)
indexToCoord = swap . flip divMod width

move :: Int -> Dir -> Int -> Int
move n d i
   | L <- d = coordToIndex $ f (mod (x + n) width , y)
   | I <- d = coordToIndex $ f (x , y + n)
   | U <- d = move n I . move n H $ i
   | H <- d = move (negate n) L i
   | N <- d = move (negate n) I i
   | M <- d = move (negate n) U i
   where
   (x,y) = indexToCoord i
   f (x',y')
      | y' >= height || y' < 0 = (mod (x' + t * 2 * radius) width , mod y' height)
      | otherwise = (x' , y')
      where
      t = div y' height  -- outbound multiplier

