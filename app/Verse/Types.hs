module Verse.Types (
   module Verse.Types ,
   (I.!) , I.adjust, I.fromList )
   where

import Data.IntMap as I
import Data.Set as S

data Some = S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7
   deriving ( Eq, Ord, Enum, Bounded )

instance Semigroup Some where
   a <> b = toEnum $ min 7 (fromEnum a + fromEnum b)

instance Monoid Some where
   mempty = S0

data Dir = U | I | H | L | N | M
   deriving ( Enum, Bounded )

newtype Atom = Atom { α ::Some }

instance Enum Atom where
   toEnum = Atom . toEnum
   fromEnum = fromEnum . α

type Node = (Atom,[Int])

type Verse = IntMap Node

data Layer = Superficial | Schematic | Atomic | Test
   deriving ( Show, Eq, Enum, Bounded )

data Mode = Play | Pause | Menu

data Sim = Dois | Terra | Bees | Fish | Fish2 | Glider | Id
   deriving ( Show, Eq, Enum, Bounded )

data State = State {
   ν :: Verse ,
   λ :: Layer ,
   σ :: Sim ,
   ρ :: [Atom] ,  -- random list of atoms
   ι :: Char ,  -- last input
   φ :: Int ,  -- focused atom
   τ :: Set Int ,  -- targeted atoms
   μ :: Mode }

