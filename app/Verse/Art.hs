module Verse.Art where

import Zero.Zero hiding ( (#) )
import Verse.Conf
import Verse.Verse
import Terminal.Game  -- remove
import Data.Char ( intToDigit )
import Data.List ( intersperse )
import Data.Map.Strict qualified as Map ( (!), elems )
import Data.IntMap qualified as IntMap ( (!), elems )
import Data.IntSet qualified as IntSet ( member )
import Data.Bifunctor ( bimap )
import Control.Monad ( join )

-- | Base color for each element

elementColor :: Element -> Draw
elementColor e
   | Ar    <- e = color White Dull
   | Agua  <- e = color Blue Dull
   | Fogo  <- e = color Red Dull
   | Terra <- e = color Green Dull
   | Eter  <- e = color Magenta Dull
-- |       <- e = color Cyan Dull  -- reserved for ui (focus)
-- |       <- e = color Yellow Dull  -- reserved for ui (info)

-- | Graphic

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

         | Menu <- μ st                      = color Black Vivid

         | selected , targeted               = color Red Dull
         | adjacent , targeted               = color Red Dull
         |            targeted               = color Red Dull

         | selected , Elemental <- λ st      = elementColor (ε st)
         | selected                          = color Cyan Dull
         | adjacent , Elemental <- λ st      = elementColor (ε st)
         | adjacent                          = color Cyan Vivid

         | Pause <- μ st , Elemental <- λ st = greyed (ε st)
         | Pause <- μ st                     = color Black Vivid
         | Atom {} <- a                      = stone

      -- | otherwise                         = color White Dull

      greyed :: Element -> Draw
      greyed e = paletteColor (xterm24LevelGray $ max 3 $ fromEnum (l e) + 2 * fromEnum (l e))

      stone :: Draw
         |                       Void    <- υ a                = paletteColor $ xterm6LevelRGB 0 0 0

         -- Elemental

         | Elemental   <- λ st                                 = greyed (ε st)

         -- Units

         | Superficial <- λ st , Plasma <- υ a  , L0 <- l Eter = paletteColor $ xterm6LevelRGB 0 1 0
         | Superficial <- λ st , Plasma <- υ a  , L1 <- l Eter = paletteColor $ xterm6LevelRGB 0 1 1
         | Superficial <- λ st , Plasma <- υ a  , L2 <- l Eter = paletteColor $ xterm6LevelRGB 1 1 2
         | Superficial <- λ st , Plasma <- υ a  , L3 <- l Eter = paletteColor $ xterm6LevelRGB 2 1 3
         | Superficial <- λ st , Plasma <- υ a  , L4 <- l Eter = paletteColor $ xterm6LevelRGB 3 1 4
         | Superficial <- λ st , Plasma <- υ a  , L5 <- l Eter = paletteColor $ xterm6LevelRGB 4 2 5
         | Superficial <- λ st , Plasma <- υ a  , L6 <- l Eter = paletteColor $ xterm6LevelRGB 5 3 5
         | Superficial <- λ st , Plasma <- υ a  , L7 <- l Eter = paletteColor $ xterm6LevelRGB 5 4 5

         | Superficial <- λ st , Flame  <- υ a  , L0 <- l Fogo = paletteColor $ xterm6LevelRGB 0 0 1  -- orange
         | Superficial <- λ st , Flame  <- υ a  , L1 <- l Fogo = paletteColor $ xterm6LevelRGB 1 0 1  -- red
         | Superficial <- λ st , Flame  <- υ a  , L2 <- l Fogo = paletteColor $ xterm6LevelRGB 2 1 1  -- ...
         | Superficial <- λ st , Flame  <- υ a  , L3 <- l Fogo = paletteColor $ xterm6LevelRGB 3 2 1
         | Superficial <- λ st , Flame  <- υ a  , L4 <- l Fogo = paletteColor $ xterm6LevelRGB 4 3 1
         | Superficial <- λ st , Flame  <- υ a  , L5 <- l Fogo = paletteColor $ xterm6LevelRGB 5 4 2
         | Superficial <- λ st , Flame  <- υ a  , L6 <- l Fogo = paletteColor $ xterm6LevelRGB 5 5 3
         | Superficial <- λ st , Flame  <- υ a  , L7 <- l Fogo = paletteColor $ xterm6LevelRGB 5 5 4

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
         | Menu <- μ st = color Black Vivid
         | otherwise    = c

      mode :: Plane
      mode
         | Play  <- μ st = word "Play" # k (paletteColor $ xterm6LevelRGB 3 1 4)
         | Menu  <- μ st = word "Pause for Menu" # color Cyan Dull
         | Pause <- μ st = word "Pause" # color Black Vivid

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
            level # k c ,
            point # k id ]
            where

            point :: Plane
            point
               | Superficial <- λ st            = cell symbol # color Black Vivid
               | Elemental   <- λ st , selected = cell symbol # elementColor e
               | Elemental   <- λ st            = cell symbol # color Black Vivid
               | otherwise                      = cell ' '

            level :: Plane
            level = word $ take (fromEnum (maxBound :: Level)) $ replicate (fromEnum $ ες f Map.! e) '|' <> repeat '·'

            c :: Draw
               | Superficial <- λ st            = elementColor e
               | Elemental   <- λ st , selected = elementColor e
               | otherwise                      = color Black Vivid

            symbol = head $ show e
            selected = e == ε st

