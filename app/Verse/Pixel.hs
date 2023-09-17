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
   pixel (x,y) = c %.< cell chr # rgbColor clr
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
      selected :: Bool = n == fi && zlevel st == tz
      overvoid :: Bool = n == fi && not (visible a)
      adjacent :: Bool = zlevel st == tz && n ∈ fis
      targeted :: Bool = IntSet.member n (targets st)


      chr :: Char
      chr
         | edit st , overvoid                   = 'o'
         | edit st , targeted , not (visible a) = 'o'
         | edit st , targeted                   = atom_chr a
         | viewbase                             = '∙'
         | otherwise                            = atom_chr ta

      clr :: Colour Float
         | edit st , targeted , selected = blink (white k) $ blend 1   k_pointer go
         | edit st            , selected =                   blend 1   k_pointer go
         | edit st , targeted , adjacent = blink (white k) $ blend 0.5 k_pointer go
         | edit st            , adjacent =                   blend 0.5 k_pointer go
         | edit st , targeted            = blink (white k)                       go
         | otherwise                     =                                       go
         where
         go
            | edit st , overvoid       = k_pointer
            |           viewbase       = fog $ blend 0.05 (white k) termBG
            | edit st , zlevel st > tz = fog $ blend 0.1 (white k) termBG
            | not (play st)            = fog $ blend 0.3 (white k) termBG
            | otherwise                = fog $ atom_clr ta

      fog :: Colour Float -> Colour Float
      fog = fade (zlevel st - tz)

   -- | Entities  ( todo )

   ents :: Plane
   ents = word "<ents>"

   -- | UI rendering

   info :: Plane
      | edit st = word $ unwords
         [ pure $ atom_chr (void { unit = Just (Unit (Building (q_structure st) (q_material st)) L0 mempty) })
         , string $ Building (q_structure st) (q_material st)  -- selected building
      -- , show (zlevel st)  -- current z level
      -- , show $ fi
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

   -- | Animation

   blink :: Colour Float -> Colour Float -> Colour Float
   blink = blend (abs $ fromInteger (steps - x) / fromInteger steps)
      where
      steps = 9
      x = mod (fromInteger $ δ st) (2 * steps)

-- | Assumed terminal background color
termBG :: Colour Float
termBG = sRGB24 0x22 0x22 0x22 -- | Useful grey tone

-- grey :: Colour Float
-- grey = blend 0.1 (white k) termBG

-- | Color blending

fade :: Word -> Colour Float -> Colour Float
fade l c = (grade <$> steps) !! fromEnum l
   where
   grade x = blend x termBG c
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

   | Nothing    <- material a = grey k
   | Just Dirt  <- material a = yellow k
   | Just Wood  <- material a = orange k
   | Just Stone <- material a = grey k
   | Just Metal <- material a = purple k
   | otherwise                = red k

-- | Colors

k :: Palette
k = pastel

k2 :: Palette
k2 = kd

k_pointer :: Colour Float
k_pointer = sRGB24 0x00 0xcc 0xff

rose :: Colour Float = sRGB24 0xff 0x00 0x7f

data Palette = K8
   { white :: Colour Float
   , grey :: Colour Float
   , red :: Colour Float
   , orange :: Colour Float
   , yellow :: Colour Float
   , green :: Colour Float
   , cyan :: Colour Float
   , blue :: Colour Float
   , purple :: Colour Float
   } | KD
   { kd_r :: Bool -> Colour Float
   , kd_y :: Bool -> Colour Float
   , kd_g :: Bool -> Colour Float
   , kd_b :: Bool -> Colour Float
   }

pastel :: Palette
pastel = K8
   { white = sRGB24 0xff 0xff 0xff
   , grey = sRGB24 0x30 0x30 0x30
   , red = sRGB24 0xce 0x65 0x64
   , orange = sRGB24 0xe0 0x93 0x5a
   , yellow = sRGB24 0xf1 0xc7 0x6e
   , green = sRGB24 0xb5 0xbe 0x63
   , cyan = sRGB24 0x88 0xbe 0xb7
   , blue = sRGB24 0x80 0xa2 0xbf
   , purple = sRGB24 0xb3 0x93 0xbc
   }

kd :: Palette
kd = KD
   { kd_r = \d -> if d then sRGB24 0xde 0x32 0x14 else sRGB24 0xe3 0x50 0x07
   , kd_y = \d -> if d then sRGB24 0xf1 0x9e 0x00 else sRGB24 0xf4 0xba 0x00
   , kd_g = \d -> if d then sRGB24 0x00 0xae 0x79 else sRGB24 0x59 0xbb 0x81
   , kd_b = \d -> if d then sRGB24 0x00 0x92 0xc1 else sRGB24 0x00 0xa1 0xdf
   }
