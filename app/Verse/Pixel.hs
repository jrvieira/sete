module Verse.Pixel where

import Zero hiding ( (#) )

import Verse.Setup
import Verse.State
import Verse.Verse
import Verse.Atoms

import Terminal.Game

import Data.IntMap qualified as IntMap ( (!) )
import Data.IntSet qualified as IntSet ( member )
import Data.Colour ( blend )
import Data.Bifunctor ( bimap )
import Control.Monad ( join )

plane :: State -> Plane
plane st = hcat [hex,ui]
   & (1,1) % makeTransparent ' ' (word $ show (focus st))

   where

   canvas :: Plane
   canvas = blankPlane (2 * succ (2 * radius) + 2 * 2 * marginX) (succ (2 * radius) + 2 * marginY)

   hexagon :: [(Int,Int)]
   hexagon = [ (x,y) | x <- [-radius..radius] , y <- [-radius..radius] , abs (x + y) <= radius ]

   fi = focus st
   (f,fis) = nodes st IntMap.! fi

   -- | Map rendering

   hex :: Plane
   hex = foldl (&) canvas $ map pixel hexagon

   pixel :: (Int,Int) -> Draw
   pixel (x,y) = c %.< cell chr # clr
      where

      (a,_) = nodes st IntMap.! n

      -- get index of node taking scroll into account
      n = move mx L . move my I $ coordToIndex (mod (x - div y height * radius) width , mod y height)
         where
         (mx,my) = indexToCoord (center st)

      -- stretch, tilt, margin, translate to Terminal.Game coordinate system (1-based (y,x))
      c = join bimap succ (y + radius + marginY , 2 * (x + radius + marginX) + y)

      selected :: Bool = n == focus st
      adjacent :: Bool = n ∈ fis
      targeted :: Bool = IntSet.member n (targets st)

      chr :: Char
         | Atom { } <- f = 'a'
         | Void     <- f = '∙'
         | otherwise = '?'

      clr :: Draw
         | targeted      = color Red Dull
         | selected      = color Cyan Dull
         | adjacent      = color Cyan Vivid

         | not $ play st = rgbColor grey

         | otherwise     = color White Dull

   -- | UI rendering

   ui :: Plane
   ui = word "<fixed width ui>"

-- | Assumed terminal background color

termBG :: Colour Float
termBG = sRGB24 0x22 0x22 0x22

-- | Useful grey tone

grey :: Colour Float
grey = blend 0.1 (sRGB24 0xff 0xff 0xff) termBG
