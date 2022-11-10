module Verse.Art where

import Zero.Zero hiding ( (#) )
import Verse.Verse
import Verse.Conf
import Terminal.Game  -- remove
import Data.List ( intersperse )

art :: State -> [Draw]
art st = map pixel hexagon
   <> ui

   where

   (f,_) = get (ν st) (φ st)

   s :: Level
      | Superficial <- λ st = α a
      | Elemental   <- λ st = gel a e
      | otherwise           = maxBound  -- make errors obvious !
   l = λ st
   e = ε st
   u = υ a

   fi = φ st
   (a,ns) = get (ν st) n


   -- | User interface

   ui :: [Draw]
   ui = [
      (1,1) % hcat (intersperse (cell ' ') [focus,layer,stat,invi]) ,
      (2,1) % mode ,
      (marginY,1) %.> elements ]

      where

      focus = word (show (υ f) <> " " <> show (indexToCoord $ φ st)) # k (color Cyan Dull)
      layer = word (show (λ st)) # k (color White Dull)
      stat = word (show $ sum $ val (ν st) <$> take (width * height) [0..]) # k (color Yellow Dull)
      invi = word (show $ ι st) # k (color Magenta Dull)

      -- inactive color
      k :: Draw -> Draw
      k c
         | Menu <- μ st = color Black Vivid
         | otherwise    = c

      mode :: Plane
      mode
         | Play  <- μ st = word (unwords [show $ σ st    ]) # k (paletteColor $ xterm6LevelRGB 3 1 4)
         | Menu  <- μ st = word (unwords [show $ σ st    ]) # color Cyan Dull
         | Pause <- μ st = word (unwords [show $ σ st,"p"]) # color Black Vivid
      -- | otherwise     = word (unwords [show $ σ st,"?"]) # k (color Black Vivid)

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

            point
               | Superficial <- λ st            = cell symbol # color Black Vivid
               | Elemental   <- λ st , selected = cell symbol # elementColor e
               | Elemental   <- λ st            = cell symbol # color Black Vivid
               | otherwise                      = cell ' '

            level = word $ take (pred $ length (total :: [Level])) $ replicate (fromEnum s) '~' <> repeat ' '

            c
               | Superficial <- λ st            = elementColor e
               | Elemental   <- λ st , selected = elementColor e
               | otherwise                      = color Black Vivid

            symbol = head $ show e
            s = gel f e
            selected = e == ε st

   -- | Map rendering

   pixel :: (Int,Int) -> Draw
   pixel (x,y) = c %.< cell chr # clr
      where

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
         |                    Void   <- u           = paletteColor $ xterm6LevelRGB 0 0 0

         -- Units

         | Superficial <- l , Plasma <- u , L0 <- s = paletteColor $ xterm6LevelRGB 0 1 0
         | Superficial <- l , Plasma <- u , L1 <- s = paletteColor $ xterm6LevelRGB 0 1 1
         | Superficial <- l , Plasma <- u , L2 <- s = paletteColor $ xterm6LevelRGB 1 1 2
         | Superficial <- l , Plasma <- u , L3 <- s = paletteColor $ xterm6LevelRGB 2 1 3
         | Superficial <- l , Plasma <- u , L4 <- s = paletteColor $ xterm6LevelRGB 3 1 4
         | Superficial <- l , Plasma <- u , L5 <- s = paletteColor $ xterm6LevelRGB 4 2 5
         | Superficial <- l , Plasma <- u , L6 <- s = paletteColor $ xterm6LevelRGB 5 3 5
         | Superficial <- l , Plasma <- u , L7 <- s = paletteColor $ xterm6LevelRGB 5 4 5

         | Superficial <- l , Flame  <- u , L0 <- s = paletteColor $ xterm6LevelRGB 0 0 1  -- orange
         | Superficial <- l , Flame  <- u , L1 <- s = paletteColor $ xterm6LevelRGB 1 0 1  -- red
         | Superficial <- l , Flame  <- u , L2 <- s = paletteColor $ xterm6LevelRGB 2 1 1  -- ...
         | Superficial <- l , Flame  <- u , L3 <- s = paletteColor $ xterm6LevelRGB 3 2 1
         | Superficial <- l , Flame  <- u , L4 <- s = paletteColor $ xterm6LevelRGB 4 3 1
         | Superficial <- l , Flame  <- u , L5 <- s = paletteColor $ xterm6LevelRGB 5 4 2
         | Superficial <- l , Flame  <- u , L6 <- s = paletteColor $ xterm6LevelRGB 5 5 3
         | Superficial <- l , Flame  <- u , L7 <- s = paletteColor $ xterm6LevelRGB 5 5 4

         -- Atomic

         | Atomic      <- l               , L0 <- s = paletteColor $ xterm6LevelRGB 0 1 0
         | Atomic      <- l               , L1 <- s = paletteColor $ xterm6LevelRGB 0 1 1
         | Atomic      <- l               , L2 <- s = paletteColor $ xterm6LevelRGB 1 1 2
         | Atomic      <- l               , L3 <- s = paletteColor $ xterm6LevelRGB 2 1 3
         | Atomic      <- l               , L4 <- s = paletteColor $ xterm6LevelRGB 3 1 4
         | Atomic      <- l               , L5 <- s = paletteColor $ xterm6LevelRGB 4 2 5
         | Atomic      <- l               , L6 <- s = paletteColor $ xterm6LevelRGB 5 3 5
         | Atomic      <- l               , L7 <- s = paletteColor $ xterm6LevelRGB 5 4 5

         | otherwise                                = greyed

