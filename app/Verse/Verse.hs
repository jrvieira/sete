module Verse.Verse where

import Zero.Zero

import Verse.Conf

import System.Random ( Random(..) )
import Data.Tuple ( swap )
import Data.IntSet ( IntSet )
import Data.IntMap ( IntMap )
import Data.IntMap qualified as IntMap ( fromList, adjust )
import Data.Map.Strict ( Map )
import Data.Map.Strict qualified as Map ( fromList, adjust )
import Control.Arrow

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

-- level

data Level = L0 | L1 | L2 | L3 | L4 | L5 | L6 | L7
   deriving ( Eq, Enum, Ord, Bounded )

instance Num Level where
   a + b = toEnum $ min (fromEnum (maxBound :: Level)) (fromEnum a + fromEnum b)
   a - b = toEnum $ max (fromEnum (minBound :: Level)) (fromEnum a - fromEnum b)
   a * b = toEnum $ min (fromEnum (maxBound :: Level)) (fromEnum a * fromEnum b)
   abs = id
   signum = const 1
   fromInteger = toEnum . fromInteger

instance Random Level where
   random g = let (r,g') = randomR (0,7) g in (toEnum r , g')
   randomR (a,b) g = let (r,g') = randomR (fromEnum a , fromEnum b) g in (toEnum r , g')

-- elements

data Element = Air | Water | Fire | Earth | Aether  -- air, water, fire, earth, aether
   deriving ( Eq, Enum, Bounded, Show, Ord )

-- units

data Unit = Void | Light | Plant | Cat | Flame | Plasma | Computer | Pipe | Wire
   deriving ( Eq, Enum, Bounded, Show, Ord )

-- ui layers

data Layer = Superficial | Elemental | Schematic
   deriving ( Eq, Enum, Bounded, Show )

-- game modes

data Mode = Play | Pause | Menu

-- state

data State = State {
   ν :: Verse ,
   λ :: Layer ,
   ε :: Element ,
   ο :: Unit ,
   ρ :: [Level] ,  -- randoms
   ι :: Char ,  -- last input
   φ :: Int ,  -- focused atom
   τ :: IntSet ,  -- targeted atoms
   κ :: Int ,  -- center
   μ :: Mode }

state :: State
state = State {
   ν = verse $ repeat $ atom Plasma ,
   λ = Superficial ,
   ε = minBound ,
   ο = minBound ,
   ρ = [] ,
   ι = ' ' ,
   φ = 0 ,
   τ = mempty ,
   κ = 0 ,
   μ = Pause }

-- atoms

data Atom = Atom { υ :: Unit , ες :: Map Element Level }

atom :: Unit -> Atom
atom u = Atom { υ = u , ες = Map.fromList $ zip total $ repeat minBound }

-- verse

type Verse = IntMap Node
type Node = (Atom,Map Dir Int)

verse :: [Atom] -> Verse
verse as = IntMap.fromList $ take (width * height) $ n <$> zip [0..] (as <> repeat (atom Void))
   where
   n :: (Int,Atom) -> (Int,Node)
   n (i,a) = (i, (a , Map.fromList $ (id &&& ($ i) . move 1) <$> total))

-- update

up :: Element -> (Level -> Level) -> Node -> Node
up e f (a,ns) = (a { ες = Map.adjust f e (ες a) } , ns)

nup :: Element -> (Level -> Level) -> Int -> Verse -> Verse
nup e f i = IntMap.adjust (up e f) i

-- coord

distance :: Int -> Int -> Float
distance a b = undefined

coordToIndex :: (Int,Int) -> Int
coordToIndex (x,y) = y * width + x

indexToCoord :: Int -> (Int,Int)
indexToCoord = swap . flip divMod width

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

{- HLINT ignore "Use fromMaybe" -}
