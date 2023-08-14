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

      (tz,ta) = top v

      -- get index of node taking scroll into account
      n = move mx L . move my I $ coordToIndex (mod (x - div y Setup.height * Setup.radius) Setup.width , mod y Setup.height)
         where
         (mx,my) = indexToCoord (center st)

      -- stretch, tilt, Setup.margin, translate to Terminal.Game coordinate system (1-based (y,x))
      c = join bimap succ (y + Setup.radius + Setup.marginY , 2 * (x + Setup.radius + Setup.marginX) + y)

      selected :: Bool = zlevel st == tz && n == focus st
      adjacent :: Bool = zlevel st == tz && n ∈ fis
      targeted :: Bool = IntSet.member n (targets st)

      chr :: Char
      chr = atom_chr ta

      clr :: Draw

         | targeted        = rgbColor $ sRGB24 0xff 0xff 0xff
         | selected        = rgbColor $ sRGB24 0xff 0xff 0xff
         | adjacent        = rgbColor $ mix 1 (sRGB24 0xff 0xff 0xff) $ atom_clr $ atom v

         | not (play st)   = rgbColor $ fog $ grey
         | zlevel st /= tz = rgbColor $ fog $ grey

         | otherwise       = rgbColor $ fog $ atom_clr $ atom v

      fog :: Colour Float -> Colour Float
      fog = fade (zlevel st - tz)

   -- | Entities

   ents :: Plane
   ents = word []

   -- | UI rendering

   info :: Plane
   info = word $ unwords [
   -- show (zlevel st,tz) ,
   -- show tz ,
   -- show (focus st) ,
   -- string $ Building (q_structure st) (q_material st) ,
      show $ zlevel st ,
      show $ structure $ atom $ maybe base id $ view st IntMap.!? (focus st) ,
      "|" ,
      maybe "" string $ building $ atom f ]

   ui :: Plane
   ui = word $ "<ui>"

-- | Assumed terminal background color
termBG :: Colour Float
termBG = sRGB24 0x22 0x22 0x22 -- | Useful grey tone

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

-- | Atoms

atom_chr :: Atom -> Char
atom_chr a

   | Nothing    <- structure a = ' '
   | Just Path  <- structure a = '.'
   | Just Track <- structure a = '#'
   | otherwise                 = '?'

atom_clr :: Atom -> Colour Float
atom_clr a

   | Nothing    <- material a = grey
   | Just Dirt  <- material a = sRGB24 0x30 0x10 0x00
   | Just Wood  <- material a = sRGB24 0x60 0x30 0x00
   | Just Stone <- material a = sRGB24 0x16 0x16 0x16
   | Just Metal <- material a = sRGB24 0x30 0x30 0x60
   | otherwise                = sRGB24 0x00 0x30 0x30

