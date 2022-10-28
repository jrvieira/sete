module Verse.Art where

import Zero.Zero hiding ( (#) )

import Verse.Conf
import Verse.Verse

import Terminal.Game
-- color :: Color -> ColorIntensity -> Draw
-- ^
-- ^

-- paletteColor :: Word8 -> Draw
-- ^
-- xterm24LevelGray :: Int -> Word8
-- xterm6LevelRGB :: Int -> Int -> Int -> Word8
-- xtermSystem :: ColorIntensity -> Color -> Word8

-- rgbColor :: Colour Float -> Draw
-- ^
-- sRGB24 :: (Ord a, Floating a) => Word8 -> Word8 -> Word8 -> Colour a
-- sRGBBounded :: (Ord b, Floating b, Integral a, Bounded a) => a -> a -> a -> Colour b
-- sRGB :: (Ord a, Floating a) => a -> a -> a -> Colour a
-- sRGB24read :: (Ord a, Floating a) => String -> Colour a

import Data.Char ( intToDigit )
import Data.Bifunctor ( bimap )
import Control.Monad ( join )

-- base color for each element

elementColor :: Element -> Draw
elementColor e
   | Α <- e    = color White Dull
   | Ω <- e    = color Blue Dull
   | Φ <- e    = color Red Dull
   | Ε <- e    = color Green Dull
   | Ψ <- e    = color Magenta Dull
-- |   <- e    = color Cyan Dull  -- reserved for ui (focus)
-- |   <- e    = color Yellow Dull  -- reserved for ui (info)
   | otherwise = color Black Vivid

-- draw atom

pixel :: State -> (Int,Int) -> Draw
pixel st (x,y) = c %.< cell chr # clr
   where

   s :: Some
      | Superficial <- λ st = α a
      | Elemental   <- λ st = gel a e
      | otherwise           = maxBound  -- make errors obvious !
   l = λ st
   e = ε st
   u = υ a

   fi = φ st
   (f,fis) = get (ν st) fi
   (a,ns) = get (ν st) n

   -- get index of node taking scroll into account
   n = move mx L . move my I $ coordToIndex (mod (x - div y height * radius) width , mod y height)
      where
      (mx,my) = indexToCoord (κ st)

   -- stretch, tilt, margin, translate to library coordinate system (1-based (y,x))
   c = join bimap succ (y + radius + marginY , 2 * (x + radius + marginX) + y)

   selected :: Bool = n == fi
   adjacent :: Bool = n ∈ fis
   targeted :: Bool = n ∈ τ st

   chr
      |                    Void   <- u = ' '
      | Atomic      <- l               = intToDigit $ fromEnum s
      | Elemental   <- l               = intToDigit $ fromEnum s
      | Superficial <- l , Plasma <- u = "·-~+=≠cs" !! fromEnum s
      | Superficial <- l , Flame  <- u = "#'\"\"^^xx" !! fromEnum s
   -- |                         _ <- u = 'x'
   -- |                         _ <- u = '+'
   -- |                         _ <- u = ':'
   -- |                         _ <- u = '>'
   -- |                         _ <- u = '<'
   -- |                         _ <- u = ".',\":;*^" !! fromEnum s
   -- |                         _ <- u = "░▒▓█░▒▓█" !! fromEnum s
      | otherwise                      = '?'

   clr :: Draw
      | Menu <- μ st                   = color Black Vivid
      | selected      , targeted       = color Red Dull
      | adjacent      , targeted       = color Red Dull
      |                 targeted       = color Red Dull
      | selected      , Elemental <- l = elementColor e
      | selected                       = color Cyan Dull
      | adjacent      , Elemental <- l = elementColor e
      | adjacent                       = color Cyan Vivid
      | Pause <- μ st                  = greyed
      | Atom {} <- a                   = stone
      | otherwise                      = color White Dull

   greyed :: Draw = paletteColor (xterm24LevelGray $ max 3 $ fromEnum s + 2 * fromEnum s)

   stone :: Draw
      |                    Error  <- u           = paletteColor $ xterm6LevelRGB 1 0 0
      |                    Void   <- u           = paletteColor $ xterm6LevelRGB 0 0 0

      -- Units

      | Superficial <- l , Plasma <- u , S0 <- s = paletteColor $ xterm6LevelRGB 0 1 0
      | Superficial <- l , Plasma <- u , S1 <- s = paletteColor $ xterm6LevelRGB 0 1 1
      | Superficial <- l , Plasma <- u , S2 <- s = paletteColor $ xterm6LevelRGB 1 1 2
      | Superficial <- l , Plasma <- u , S3 <- s = paletteColor $ xterm6LevelRGB 2 1 3
      | Superficial <- l , Plasma <- u , S4 <- s = paletteColor $ xterm6LevelRGB 3 1 4
      | Superficial <- l , Plasma <- u , S5 <- s = paletteColor $ xterm6LevelRGB 4 2 5
      | Superficial <- l , Plasma <- u , S6 <- s = paletteColor $ xterm6LevelRGB 5 3 5
      | Superficial <- l , Plasma <- u , S7 <- s = paletteColor $ xterm6LevelRGB 5 4 5

      | Superficial <- l , Flame  <- u , S0 <- s = paletteColor $ xterm6LevelRGB 0 0 1  -- orange
      | Superficial <- l , Flame  <- u , S1 <- s = paletteColor $ xterm6LevelRGB 1 0 1  -- red
      | Superficial <- l , Flame  <- u , S2 <- s = paletteColor $ xterm6LevelRGB 2 1 1  -- ...
      | Superficial <- l , Flame  <- u , S3 <- s = paletteColor $ xterm6LevelRGB 3 2 1
      | Superficial <- l , Flame  <- u , S4 <- s = paletteColor $ xterm6LevelRGB 4 3 1
      | Superficial <- l , Flame  <- u , S5 <- s = paletteColor $ xterm6LevelRGB 5 4 2
      | Superficial <- l , Flame  <- u , S6 <- s = paletteColor $ xterm6LevelRGB 5 5 3
      | Superficial <- l , Flame  <- u , S7 <- s = paletteColor $ xterm6LevelRGB 5 5 4

      -- Atomic

      | Atomic      <- l               , S0 <- s = paletteColor $ xterm6LevelRGB 0 1 0
      | Atomic      <- l               , S1 <- s = paletteColor $ xterm6LevelRGB 0 1 1
      | Atomic      <- l               , S2 <- s = paletteColor $ xterm6LevelRGB 1 1 2
      | Atomic      <- l               , S3 <- s = paletteColor $ xterm6LevelRGB 2 1 3
      | Atomic      <- l               , S4 <- s = paletteColor $ xterm6LevelRGB 3 1 4
      | Atomic      <- l               , S5 <- s = paletteColor $ xterm6LevelRGB 4 2 5
      | Atomic      <- l               , S6 <- s = paletteColor $ xterm6LevelRGB 5 3 5
      | Atomic      <- l               , S7 <- s = paletteColor $ xterm6LevelRGB 5 4 5

      | otherwise                                = greyed

