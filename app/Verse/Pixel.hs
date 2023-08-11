module Verse.Pixel where

import Zero hiding ( (#) )

import Verse.Setup qualified as Setup
import Verse.State
import Verse.Verse

import Terminal.Game

import Data.IntMap qualified as IntMap ( (!), (!?) )
import Data.IntSet qualified as IntSet ( member )
import Data.Colour ( blend )
import Data.Bifunctor ( bimap )
import Control.Monad ( join )

plane :: State -> Plane
plane st = hcat [hex,ents,ui]
   & (1,1) % makeTransparent ' ' info

   where

   canvas :: Plane
   canvas = blankPlane (2 * succ (2 * Setup.radius) + 2 * 2 * Setup.marginX) (succ (2 * Setup.radius) + 2 * Setup.marginY)

   hexagon :: [(Int,Int)]
   hexagon = [ (x,y) | x <- [-Setup.radius..Setup.radius] , y <- [-Setup.radius..Setup.radius] , abs (x + y) <= Setup.radius ]

   -- focused
   fi :: Int
   fi = focus st
   f :: View
   f = maybe base id $ view st IntMap.!? fi
   fis :: Edge Int
   (_,fis) = ν st IntMap.! fi

   -- | Map rendering

   hex :: Plane
   hex = foldl (&) canvas $ map pixel hexagon

   pixel :: (Int,Int) -> Draw
   pixel (x,y) = c %.< cell chr # clr
      where

      v :: View
      v = maybe base id $ view st IntMap.!? n

      -- get index of node taking scroll into account
      n = move mx L . move my I $ coordToIndex (mod (x - div y Setup.height * Setup.radius) Setup.width , mod y Setup.height)
         where
         (mx,my) = indexToCoord (center st)

      -- stretch, tilt, Setup.margin, translate to Terminal.Game coordinate system (1-based (y,x))
      c = join bimap succ (y + Setup.radius + Setup.marginY , 2 * (x + Setup.radius + Setup.marginX) + y)

      selected :: Bool = zlevel st == z v && n == focus st
      adjacent :: Bool = zlevel st == z v && n ∈ fis
      targeted :: Bool = zlevel st == z v && IntSet.member n (targets st)

      chr :: Char
         | Nothing    <- structure (atom v) , z v       == 0 = '.'
         | Nothing    <- structure (atom v)                  = ' '
         | Just Track <- structure (atom v)                  = '#'
         | otherwise                                         = '?'

      clr :: Draw
         | targeted                                         = color Red Dull
         | selected                                         = color Cyan Dull
         | adjacent                                         = color Cyan Vivid

         | not (play st)                                    = rgbColor $ fog grey
         | zlevel st /= z v                                 = rgbColor $ fog grey

         | Nothing    <- material (atom v)                  = rgbColor $ fog grey
         | Just Dirt  <- material (atom v)                  = rgbColor $ fog $ sRGB24 0x30 0x10 0x00
         | Just Wood  <- material (atom v)                  = rgbColor $ fog $ sRGB24 0x60 0x30 0x00
         | Just Stone <- material (atom v)                  = rgbColor $ fog $ sRGB24 0x16 0x16 0x16
         | Just Metal <- material (atom v)                  = rgbColor $ fog $ sRGB24 0x30 0x30 0x60

         | otherwise                                        = color White Dull

      fog :: Colour Float -> Colour Float
      fog = fade (zlevel st - z v)

   -- | Entities

   ents :: Plane
   ents = word []

   -- | UI rendering

   info :: Plane
   info = word $ unwords [show (zlevel st),show (focus st),show (q_structure st)]

   ui :: Plane
   ui = word $ unlines [maybe "" id $ string <$> building (atom f)]

-- | Assumed terminal background color

termBG :: Colour Float
termBG = sRGB24 0x22 0x22 0x22

-- | Useful grey tone

grey :: Colour Float
grey = blend 0.1 (sRGB24 0xff 0xff 0xff) termBG

-- | Color blending

fade :: Word -> Colour Float -> Colour Float
fade l k = mix l termBG k

mix :: Word -> Colour Float -> Colour Float -> Colour Float
mix l ka kb = (grade <$> steps) !! fromEnum l
   where
   grade x = blend x ka kb
   steps = [0,0.25..1] <> repeat 1
