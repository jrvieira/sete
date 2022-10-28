
module Verse.Verse where


import Prelude hiding ( lookup )

import Verse.Conf
import Zero.Zero

import System.Random ( Random(..) )

import Data.IntMap qualified as IntMap ( fromList, insert, adjust )
import Data.Map.Strict qualified as Map ( fromList, adjust )
import Data.IntMap ( IntMap, (!?) )
import Data.Map.Strict ( Map, lookup )
import Data.Set ( Set )
import Data.Tuple ( swap )
import Control.Arrow

-- STATE

data State = State {
   ν :: Verse ,
   λ :: Layer ,
   ε :: Element ,  -- element
   ο :: Unit ,  -- object
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
   λ = Superficial ,
   ε = Ψ ,  -- element
   ο = Plasma ,
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

-- instance Num Some where
--    a + b = toEnum $ min (fromEnum $ (maxBound :: Some)) (fromEnum a + fromEnum b)
--    a - b = toEnum $ max (fromEnum $ (minBound :: Some)) (fromEnum a - fromEnum b)
--    a * b = toEnum $ min (fromEnum $ (maxBound :: Some)) (fromEnum a * fromEnum b)
--    abs = id
--    signum = const 1
--    fromInteger = toEnum . fromIntegral

instance Random Some where
   random g = let (r, g') = randomR (0,7) g in (toEnum r , g')
   randomR (a,b) g = let (r,g') = randomR (fromEnum a , fromEnum b) g in (toEnum r , g')

instance Semigroup Some where
   a <> b = toEnum $ min (fromEnum $ (maxBound :: Some)) (fromEnum a + fromEnum b)

instance Monoid Some where
   mempty = minBound

-- game modes
data Mode = Play | Pause | Menu

-- simulations
data Sim = Smoke | Terra | Id | Noise | Bees | Fish | Fish2 | Glide | Glide2 | Ripple | Rippl2 | Nil
   deriving ( Show, Eq, Enum, Bounded )

-- GRAPH

-- elements
data Element = Α | Ω | Φ | Ε | Ψ  -- air, water, fire, earth, aether
   deriving ( Show, Eq, Ord, Enum, Bounded )

-- units
data Unit = Error | Void | Light | Plant | Cat | Flame | Plasma
   deriving ( Show, Eq, Ord, Enum, Bounded )

-- ui layers
data Layer = Superficial | Atomic | Elemental | Schematic
   deriving ( Show, Eq, Enum, Bounded )

-- atoms
data Atom = Atom {
   υ :: Unit ,
   α :: Some ,  -- alpha value
   ς :: Map Element Some }

atom :: Unit -> Some -> Atom
atom u s = Atom { α = s , υ = u , ς = Map.fromList $ zip total (repeat minBound) }

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
verse as = IntMap.fromList $ take (width * height) $ n <$> zip [0..] (as <> repeat (atom Void minBound))
   where
   n :: (Int,Atom) -> (Int,Node)
   n (i,a) = (i, (a , Map.fromList $ (id &&& ($ i) . move 1) <$> total))

-- utility functions for getting and setting nested data
-- and abstract over type conversions (like Some -> Int)

-- get node from index
get :: Verse -> Int -> Node
get v i = maybe (atom Error maxBound , mempty) id (v !? i)

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
vup f = IntMap.adjust (sup f)

-- get atom's element value
gel :: Atom -> Element -> Some
gel a e = maybe minBound id $ lookup e (ς a)

-- update atom's element value
gup :: Element -> (Some -> Some) -> Atom -> Atom
gup e f a = a { ς = Map.adjust f e (ς a) }

-- update specific node's element value in verse
eup :: Element -> (Some -> Some) -> Int -> Verse -> Verse
eup e f i v = IntMap.insert i (gup e f a , ns) v
   where
   (a,ns) = get v i

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
