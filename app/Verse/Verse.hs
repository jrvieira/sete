module Verse.Verse where

import Zero.Zero

import Verse.Conf
import Verse.Util

import Data.Set hiding ( take, fromList )
import Data.IntMap hiding ( take )
import Data.Tuple ( swap )
import Data.Bifunctor
import Control.Monad
import Control.Arrow

{- Each node connects to it's adjacent 6

   o o o o o o o o   0 1 2 3 4 5 6 7
    o o o o o o o o   8 9 ...
   o o o o o o o o
    o o o o o o o o
   o o o o o o o o
    o o o o o o o o
   o o o o o o o o

-}

data Some = S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7
   deriving ( Eq, Ord, Enum, Bounded )

pixel :: Some -> Char
pixel = (" ⋅∶⋮░▒▓█" !!) . fromEnum

instance Semigroup Some where
   a <> b = toEnum $ min 7 (fromEnum a + fromEnum b)

instance Monoid Some where
   mempty = S0

data Atom = Atom { α ::Some }

type Node = (Atom,[Int])

type Verse = IntMap Node

data Layer = Superficial | Schematic | Atomic | Test
   deriving ( Show, Eq, Enum, Bounded )

data Mode = Normal | Pause | Menu
   deriving ( Show )

verse :: Verse
verse = fromList $ take (width * height) $ (id &&& atom) <$> [0..]
   where
   atom :: Int -> Node
   atom i = (Atom S1,adjacents)
      where
      adjacents
         | even y = ($ i) . move 1 <$> total
         |  odd y = ($ i) . move 1 <$> total
      (q,r) = quotRem i width
      (x,y) = indexToCoord i

-- util

data State = State {
   ν :: Verse ,
   λ :: Layer ,
   φ :: Int ,  -- focused atom
   τ :: Set Int , -- targeted atoms
   μ :: Mode }

sup :: (Some -> Some) -> Int -> Verse -> Verse
sup f i v = adjust (const (Atom (f s),ns)) i v
   where
   (Atom s,ns) = v ! i

sal :: Node -> Int
sal (Atom s,_) = fromEnum s

