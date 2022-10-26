
module Verse.Verse where

import Verse.Conf

import Zero.Zero

import System.Random ( Random(..) )
import Terminal.Game (
   Draw,
   -- * color reexports
   -- | make color from value
   color, rgbColor, paletteColor,
   sRGB24, sRGBBounded, sRGB, sRGB24read,
   xterm6LevelRGB, xterm24LevelGray, xtermSystem )

import Data.IntMap qualified as IntMap ( fromList, toList )
import Data.Map.Strict qualified as Map ( fromList, toList )
import Data.IntMap ( IntMap, (!?), adjust, elems, insert )
import Data.Map.Strict ( Map )
import Data.Set ( Set, insert, delete, singleton )
import Data.Tuple ( swap )
import Control.Arrow

-- STATE

data State = State {
   ν :: Verse ,
   λ :: Layer ,
   σ :: Sim ,
   ρ :: [Some] ,  -- randoms
   ι :: Char ,  -- last input
   φ :: Int ,  -- focused atom
   τ :: Set Int ,  -- targeted atoms
   κ :: Int ,  -- center
   μ :: Mode }

state :: State
state = State {
   ν = verse (repeat (atom Plasma S1)) ,
   λ = Test ,
   σ = Terra ,
   ρ = [] ,
   ι = ' ' ,
   φ = 0 ,
   τ = mempty ,
   κ = 0 ,
   μ = Pause }

-- DATA

-- base val (~ Word3)
data Some = S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7
   deriving ( Eq, Ord, Enum, Bounded )

instance Random Some where
   random g = let (r, g') = randomR (0,7) g in (toEnum r , g')
   randomR (a,b) g = let (r,g') = randomR (fromEnum a , fromEnum b) g in (toEnum r , g')

instance Semigroup Some where
   a <> b = toEnum $ min 7 (fromEnum a + fromEnum b)

instance Monoid Some where
   mempty = S0

-- game modes
data Mode = Play | Pause | Menu

-- simulations
data Sim = Smoke | Terra | Bees | Fish | Fish2 | Glide | Glide2 | Ripple | Rippl2 | Id | Nil
   deriving ( Show, Eq, Enum, Bounded )

-- GRAPH

-- units
data Unit = Error | Void | Plasma

-- elements
data Element = Δ | Ω
   deriving ( Show, Eq, Ord )

-- ui layers
data Layer = Superficial | Schematic | Atomic | Elemental | Test
   deriving ( Show, Eq, Enum, Bounded )

-- atoms
data Atom = Atom {
   υ :: Unit ,
   α :: Some ,  -- alpha value
   ε :: Map Element Some }

atom :: Unit -> Some -> Atom
atom u s = Atom { α = s , υ = u , ε = mempty }

instance Enum Atom where
   toEnum n = atom Void (toEnum n)
   fromEnum = fromEnum . α

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

type Verse = IntMap Node
type Node = (Atom,Map Dir Int)

verse :: [Atom] -> Verse
verse as = IntMap.fromList $ take (width * height) $ n <$> zip [0..] (as <> repeat (atom Void S0))
   where
   n :: (Int,Atom) -> (Int,Node)
   n (i,a) = (i, (a , Map.fromList $ (id &&& ($ i) . move 1) <$> total))

-- | Utility functions for getting and setting nested data
-- and abstract over type conversions (like Some -> Int)

-- get node from index
get :: Verse -> Int -> Node
get v i
   | Just n  <- v !? i = n
   | Nothing <- v !? i = (atom Error S0 , mempty)  -- svalue = error code

-- get node sval
sal :: Node -> Int
sal (a,_) = fromEnum (α a)

-- update node sval
sup :: (Some -> Some) -> Node -> Node
sup f (a,ns) = (a { α = f (α a) } , ns)

-- get specific atom sval in verse
val :: Verse -> Int -> Int
val v i = sal (get v i)

-- update specific atom sval in verse
vup :: (Some -> Some) -> Int -> Verse -> Verse
vup f = adjust (sup f)

-- single element set
single :: a -> Set a
single = singleton

-- distance between two nodes
distance :: Int -> Int -> Float
distance a b = undefined

-- convert coords to index
coordToIndex :: (Int,Int) -> Int
coordToIndex (x,y) = y * width + x

-- convert index to coords
indexToCoord :: Int -> (Int,Int)
indexToCoord = swap . flip divMod width

-- PLANE

data Dir = U | I | H | L | N | M
   deriving ( Eq, Ord, Enum, Bounded )

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

-- ART

pixel :: Layer -> Atom -> Char
pixel l a
   | Void   <- υ a = ' '
   | Plasma <- υ a = " ·~+=≠co" !! fromEnum (α a)
   | _ <- υ a = '?'
-- | _ <- υ a = 'x'
-- | _ <- υ a = '+'
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

