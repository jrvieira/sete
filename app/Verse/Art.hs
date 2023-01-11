module Verse.Art where

import Zero.Zero hiding ( (#) )
import Verse.Conf
import Verse.Verse
import Terminal.Game
import Data.Colour ( blend )
import Data.Char ( intToDigit )
import Data.List ( intersperse )
import Data.Map.Strict qualified as Map ( (!), elems )
import Data.IntMap qualified as IntMap ( (!), elems )
import Data.IntSet qualified as IntSet ( member )
import Data.Bifunctor ( bimap )
import Control.Monad ( join )

-- | Base color for each element

elementColor :: Element -> Colour Float
elementColor e
   | Ar    <- e = sRGB24 0xd3 0xd3 0xd3
   | Agua  <- e = sRGB24 0x66 0x99 0xcc
   | Fogo  <- e = sRGB24 0xf2 0x77 0x7a
   | Terra <- e = sRGB24 0x99 0xcc 0x99
   | Eter  <- e = sRGB24 0xcc 0x99 0xcc
-- |       <- e = sRGB24 0x66 0xcc 0xcc  -- reserved for ui (focus)
-- |       <- e = sRGB24 0xff 0xcc 0x66  -- reserved for ui (info)

-- | Graphic

termBG :: Colour Float
termBG = sRGB24 0x22 0x22 0x22

fade :: Level -> Colour Float -> Colour Float
fade l k = mix l k termBG

mix :: Level -> Colour Float -> Colour Float -> Colour Float
mix l ka kb = (grade <$> steps) !! fromEnum l
   where
   grade x = blend x ka kb
   steps = [0,0.1..]

grey :: Colour Float
grey = sRGB24 0x99 0x99 0x99

art :: State -> [Draw]
art st = map pixel hexagon <> ui
   where

   hexagon :: [(Int,Int)]
   hexagon = [ (x,y) | x <- [-radius..radius] , y <- [-radius..radius] , abs (x + y) <= radius ]

   fi = φ st
   (f,fis) = ν st IntMap.! fi

   -- | Map rendering

   pixel :: (Int,Int) -> Draw
   pixel (x,y) = c %.< cell chr # clr
      where

      (a,_) = ν st IntMap.! n
      l e = ες a Map.! e

      -- get index of node taking scroll into account
      n = move mx L . move my I $ coordToIndex (mod (x - div y height * radius) width , mod y height)
         where
         (mx,my) = indexToCoord (κ st)

      -- stretch, tilt, margin, translate to library coordinate system (1-based (y,x))
      c = join bimap succ (y + radius + marginY , 2 * (x + radius + marginX) + y)

      selected :: Bool = n == φ st
      adjacent :: Bool = n ∈ fis
      targeted :: Bool = IntSet.member n (τ st)

      chr :: Char
         | Elemental <- λ st , selected = intToDigit $ fromEnum (l (ε st))

         | Void    <- υ a = '.'
         | Plasma  <- υ a = "·-~+=≠cs" !! fromEnum (ες a Map.! Eter)
         | Flame   <- υ a = "#'\"\"^^xx" !! fromEnum (ες a Map.! Fogo)
         | Wire    <- υ a = '*'
         | Battery <- υ a = 'E'
      -- | _       <- υ a = 'x'
      -- | _       <- υ a = '+'
      -- | _       <- υ a = ':'
      -- | _       <- υ a = '>'
      -- | _       <- υ a = '<'
      -- | _       <- υ a = ".',\":;*^" !! fromEnum (l (ε st))
      -- | _       <- υ a = "░▒▓█░▒▓█" !! fromEnum (l (ε st))
         | otherwise      = '?'

      clr :: Draw

         | Menu <- μ st                      = rgbColor grey

         | selected , targeted               = color Red Dull
         | adjacent , targeted               = color Red Dull
         |            targeted               = color Red Dull

         | selected , Elemental <- λ st      = rgbColor (elementColor (ε st))
         | selected                          = color Cyan Dull
         | adjacent , Elemental <- λ st      = rgbColor (elementColor (ε st))
         | adjacent                          = color Cyan Vivid

         | Pause <- μ st , Elemental <- λ st = greyed (ε st)
         | Pause <- μ st                     = rgbColor grey
         | Atom {} <- a                      = stone

      -- | otherwise                         = color White Dull

      greyed :: Element -> Draw
      greyed e = rgbColor $ mix (l e) grey termBG

      stone :: Draw
         |                       Void    <- υ a                = rgbColor $ sRGB24 0 0 0

         -- Elemental

         | Elemental   <- λ st                                 = rgbColor $ fade (l (ε st)) $ elementColor (ε st)

         -- Units

         | Superficial <- λ st , Plasma <- υ a = rgbColor $ mix (l Eter) (sRGB24 255 204 255) (sRGB24 0 51 0)

         | Superficial <- λ st , Flame  <- υ a  , L0 <- l Fogo = rgbColor $ sRGB24 0 0 0  -- orange
         | Superficial <- λ st , Flame  <- υ a  , L1 <- l Fogo = rgbColor $ sRGB24 51 51 51  -- red
         | Superficial <- λ st , Flame  <- υ a  , L2 <- l Fogo = rgbColor $ sRGB24 102 51 51  -- ...
         | Superficial <- λ st , Flame  <- υ a  , L3 <- l Fogo = rgbColor $ sRGB24 153 102 51
         | Superficial <- λ st , Flame  <- υ a  , L4 <- l Fogo = rgbColor $ sRGB24 204 153 51
         | Superficial <- λ st , Flame  <- υ a  , L5 <- l Fogo = rgbColor $ sRGB24 255 204 102
         | Superficial <- λ st , Flame  <- υ a  , L6 <- l Fogo = rgbColor $ sRGB24 255 255 153
         | Superficial <- λ st , Flame  <- υ a  , L7 <- l Fogo = rgbColor $ sRGB24 255 255 204

         | otherwise                                           = color White Vivid

   -- | User interface

   ui :: [Draw]
   ui = [
      (1,1) % hcat (intersperse (cell ' ') [focus,layer,stat,invi]) ,
      (2,1) % mode ,
      (succ marginY,1) %^> unit ,
      (succ marginY,1) %.> elements ]
      where

      focus = word (show (υ f) <> " " <> show (indexToCoord $ φ st)) # k (color Cyan Dull)
      layer = word (show (λ st)) # k (color White Dull)
      stat = word (show $ fromEnum $ sum $ map (sum . Map.elems . ες . fst) $ IntMap.elems $ ν st) # k (color Yellow Dull)
      invi = word (show $ ι st) # k (color Magenta Dull)

      -- inactive color
      k :: Draw -> Draw
      k c
         | Menu <- μ st = rgbColor grey
         | otherwise    = c

      mode :: Plane
      mode
         | Play  <- μ st = word "Play" # k (rgbColor $ sRGB24 153 51 204)
         | Menu  <- μ st = word "Pause for Menu" # color Cyan Dull
         | Pause <- μ st = word "Pause" # rgbColor grey

      unit :: Plane
      unit
         | Superficial <- λ st = word $ show (ο st)
         | otherwise           = word ""

      elements :: Plane
      elements
         | λ st ∈ [Superficial,Elemental] = vcat $ bar <$> total
         | otherwise                      = word ""
         where

         bar :: Element -> Plane
         bar e = hcat $ intersperse (cell ' ') [
            level # k id ,
            point # k id ]
            where

            point :: Plane
            point
               | Superficial <- λ st            = cell symbol # rgbColor (fade L5 grey)
               | Elemental   <- λ st , selected = cell symbol # rgbColor (elementColor e)
               | Elemental   <- λ st            = cell symbol # rgbColor (fade L5 grey)
               | otherwise                      = cell ' '

            level :: Plane
            level = hcat $ zipWith (rgbColor . flip fade x) total $ map cell $ take (fromEnum (maxBound :: Level)) $ replicate (fromEnum $ ες f Map.! e) '|' <> repeat '·'
               where
               x
                  | Superficial <- λ st            = elementColor e
                  | Elemental   <- λ st , selected = elementColor e
                  | otherwise = grey

            symbol = head $ show e
            selected = e == ε st

