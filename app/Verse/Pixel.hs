module Verse.Pixel where

import Zero hiding ( (#) )

import Verse.Setup qualified as Setup
import Verse.State
import Verse.Verse

import Terminal.Game

import Data.Char ( toLower )
import Data.IntMap qualified as IntMap ( (!), (!?) )
import Data.IntSet qualified as IntSet ( member )
import Data.Colour ( blend )
import Data.Bifunctor ( bimap )
import Control.Monad ( join )

plane :: State -> Plane
plane st = hcat [hex,ui]
   & (1,1) % info

   where

   canvas :: Plane
   canvas = blankPlane (2 * succ (2 * Setup.radius) + 2 * 2 * Setup.marginX) (succ (2 * Setup.radius) + 2 * Setup.marginY)

   hexagon :: [(Int,Int)]
   hexagon = [ (x,y) | x <- [-Setup.radius..Setup.radius] , y <- [-Setup.radius..Setup.radius] , abs (x + y) <= Setup.radius ]

   -- focused
   fi :: Int
   fi = focus st
   fv :: View
   fv = maybe base id $ view st IntMap.!? fi
   fa :: Atom
   fa = maybe void id $ atoms fv IntMap.!? fromIntegral (zlevel st)
   fis :: Edge Int
   (_,fis) = ν st IntMap.! fi

   -- | Map rendering

   hex :: Plane
   hex = foldl (&) canvas $ map pixel hexagon

   pixel :: (Int,Int) -> Draw
   pixel (x,y) = c %.< cell chr # clr
      where

      v :: View
      v = view st IntMap.! n

      -- top visible atom until zlevel
      ta :: Atom
      tz :: Word
      (ta,tz) = go (zlevel st)
         where
         go :: Word -> (Atom,Word)
         go z
            | visible t || z == 0 = (t,z)
            | otherwise = go $ pred z
            where
            t = maybe void id $ atoms v IntMap.!? fromIntegral z


      -- get index of node taking scroll into account
      n :: Int
      n = move mx L . move my I $ coordToIndex (mod (x - div y Setup.height * Setup.radius) Setup.width , mod y Setup.height)
         where
         (mx,my) = indexToCoord (center st)

      -- stretch, tilt, Setup.margin, translate to Terminal.Game coordinate system (1-based (y,x))
      c :: (Int, Int)
      c = join bimap succ (y + Setup.radius + Setup.marginY , 2 * (x + Setup.radius + Setup.marginX) + y)

      a :: Atom
      a = maybe void id $ atoms v IntMap.!? fromIntegral (zlevel st)

      viewbase :: Bool = ta == void
      selected :: Bool = zlevel st == tz && n == fi
      adjacent :: Bool = zlevel st == tz && n ∈ fis
      targeted :: Bool = IntSet.member n (targets st)

      overvoid :: Bool = n == fi && not (visible a)

      chr :: Char
      chr
         | edit st , overvoid = 'x'
         | edit st , targeted , not (visible a) = 'x'
         | edit st , targeted = atom_chr a
         | visible ta = atom_chr ta
         | viewbase  = '∙'
         | otherwise = ' '

      clr :: Draw
         | edit st , targeted       = rgbColor white
         | edit st , overvoid       = rgbColor white
         |           viewbase       = rgbColor $ fog $ blend 0.05 white termBG
         | edit st , selected       = rgbColor white
         | edit st , adjacent       = rgbColor $ mix 1 white $ atom_clr ta
         | edit st , zlevel st > tz = rgbColor $ fog $ grey
      -- | zlevel st < tz           = error $ unwords ["view top z should never be > zlevel",show $ zlevel st,show tz]
         | not (play st)            = rgbColor $ fog $ blend 0.7 grey white
         | otherwise                = rgbColor $ fog $ atom_clr ta

      fog :: Colour Float -> Colour Float
      fog = fade (zlevel st - tz)

   -- | Entities  ( todo )

   ents :: Plane
   ents = word "<ents>"

   -- | UI rendering

   info :: Plane
      | edit st = word $ unwords
         [ "z" <> show (zlevel st)  -- current z level
      -- , show $ fi
         , string $ Building (q_structure st) (q_material st)  -- selected building
         ]
      | play st = word "|>"
      | otherwise = word "||"

   ui :: Plane
      | edit st = canvas
      & (1,1) % vcat
         [ word (toLower <$> unwords [ pure $ atom_chr fa {--, maybe "" show $ material $ fa , maybe "" show $ structure $ fa --}]) # rgbColor (atom_clr fa)
         , word $ unwords [ maybe "" string $ building fa ]
      -- , word ""
      -- , textBoxLiquid Setup.width (show fv)
         , word ""
         , textBoxLiquid Setup.width $ maybe "" detail $ building fa
         ]
      | otherwise = canvas

-- | Assumed terminal background color
termBG :: Colour Float
termBG = sRGB24 0x22 0x22 0x22 -- | Useful grey tone

white :: Colour Float
white = sRGB24 0xff 0xff 0xff

grey :: Colour Float
grey = blend 0.1 white termBG

-- | Color blending

fade :: Word -> Colour Float -> Colour Float
fade l k = mix l termBG k

mix :: Word -> Colour Float -> Colour Float -> Colour Float
mix l ka kb = (grade <$> steps) !! fromEnum l
   where
   grade x = blend x ka kb
-- steps = [0,0.25,0.50,0.65,0.80,0.90,1] <> repeat 1
   steps = [0,0.50,0.80,0.95,1] <> repeat 1

-- | Atoms

atom_chr :: Atom -> Char
atom_chr a

   | Nothing    <- structure a = ' '
   | Just Path  <- structure a = '∙'
   | Just Track <- structure a = '#'
   | otherwise                 = '?'

atom_clr :: Atom -> Colour Float
atom_clr a

   | Nothing    <- material a = grey
   | Just Dirt  <- material a = sRGB24 0xcc 0x99 0x00
   | Just Wood  <- material a = sRGB24 0x60 0x30 0x00
   | Just Stone <- material a = sRGB24 0x16 0x16 0x16
   | Just Metal <- material a = sRGB24 0x30 0x30 0x60
   | otherwise                = sRGB24 0x00 0x30 0x30

