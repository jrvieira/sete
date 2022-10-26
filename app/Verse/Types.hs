module Verse.Types ( module Verse.Types,
   (!), adjust, toList, fromList, elems,
   insert, delete )
   where

import Data.IntMap ( IntMap, (!), adjust, toList, fromList, elems )
import Data.Set ( Set, insert, delete )
import System.Random ( Random(..) )

data Some = S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7
   deriving ( Eq, Ord, Enum, Bounded )

instance Random Some where
   random g = let (r, g') = randomR (0,7) g in (toEnum r , g')
   randomR (a,b) g = let (r,g') = randomR (fromEnum a, fromEnum b) g in (toEnum r , g')

instance Semigroup Some where
   a <> b = toEnum $ min 7 (fromEnum a + fromEnum b)

instance Monoid Some where
   mempty = S0

data Dir = U | I | H | L | N | M
   deriving ( Enum, Bounded )

data Element = Δ | Ω

data Unit = Void | Plasma

data Layer = Superficial | Schematic | Atomic | Elemental | Test
   deriving ( Show, Eq, Enum, Bounded )

data Atom = Atom {
   α :: Some ,  -- alpha value
   υ :: Unit ,
   ε :: [Element] }

atom :: Atom
atom = Atom { α = S1 , υ = Plasma , ε = [] }

instance Enum Atom where
   toEnum n = atom { α = toEnum n }
   fromEnum = fromEnum . α

type Node = (Atom,[Int])

type Verse = IntMap Node

data Mode = Play | Pause | Menu

data Sim = Smoke | Terra | Bees | Fish | Fish2 | Glide | Glide2 | Ripple | Rippl2 | Id | Nil
   deriving ( Show, Eq, Enum, Bounded )

data State = State {
   ν :: Verse ,
   λ :: Layer ,
   σ :: Sim ,
   ρ :: [Some] ,  -- random numbers
   ι :: Char ,  -- last input
   φ :: Int ,  -- focused atom
   τ :: Set Int ,  -- targeted atoms
   κ :: Int ,  -- center
   μ :: Mode }

