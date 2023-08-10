module Verse.Verse where

import Zero ( total )

import Verse.Setup qualified as Setup ( radius, width, height )

import Data.Tuple ( swap )
import Data.IntMap ( IntMap )
import Data.IntMap qualified as IntMap ( fromList )

type Node a = (a,Edge Int)

verse :: [[Atom]] -> IntMap (Node [Atom])
verse as = IntMap.fromList $ take (Setup.width * Setup.height) $ map n $ zip [0..] (as <> repeat [])
   where
   n :: (Int,[Atom]) -> (Int,Node [Atom])
   n (ix,a) = (ix, (a , Edge u i h l n m))
      where
      [u,i,h,l,n,m] = ($ ix) . move 1 <$> total

-- coord

distance :: Int -> Int -> Float
distance _ _ = undefined

coordToIndex :: (Int,Int) -> Int
coordToIndex (x,y) = y * Setup.width + x

indexToCoord :: Int -> (Int,Int)
indexToCoord = swap . flip divMod Setup.width

data Edge a = Edge a a a a a a
   deriving ( Foldable, Functor )

edge :: Dir -> Edge a -> a
edge d (Edge u i h l n m)
   | L <- d = l
   | I <- d = i
   | U <- d = u
   | H <- d = h
   | N <- d = n
   | M <- d = m

data Dir = U | I | H | L | N | M
   deriving ( Eq, Ord, Enum, Bounded )

move :: Int -> Dir -> Int -> Int
move n d i
   | L <- d = coordToIndex $ f (mod (x + n) Setup.width , y)
   | I <- d = coordToIndex $ f (x , y + n)
   | U <- d = move n I . move n H $ i
   | H <- d = move (negate n) L i
   | N <- d = move (negate n) I i
   | M <- d = move (negate n) U i
   where
   (x,y) = indexToCoord i
   f (x',y')
      | y' >= Setup.height || y' < 0 = (mod (x' + t * 2 * Setup.radius) Setup.width , mod y' Setup.height)
      | otherwise = (x' , y')
      where
      t = div y' Setup.height  -- outbound multiplier

data Layer = Surface | Inspect | Numeric | Density
   deriving ( Eq, Enum, Bounded, Show )

-- View is interface accessible information
-- INDEPENDENT from Pixel rendering
data View = View {
   atom :: Atom ,
   z :: Word }
   deriving ( Show )

base :: View
base = View {
   atom = Base ,
   z = 0 }

-- Level

data Level = L1 | L2 | L3 | L4 | L5 | L6 | L7
   deriving ( Enum, Bounded )

-- Atoms

-- Entities live in a IntMap [Entity] that is calculated after
data Entity = Cat | Bird
   deriving Show

data Item = Pebble | Statue
   deriving Show

data Unit = Floor Material | Track Material
   deriving Show

data Form = Gas | Liquid | Solid | Plasma

-- Units have material
data Material = Dirt | Wood | Stone | Metal
   deriving Show

data Atom = Base | Void | Atom {
   unit :: Unit ,
   items :: [Item] }
   deriving Show

class Object a where
   prop :: Property -> a -> Bool

data Property
   = Block  -- blocks things from passing
   | Fixed  -- does not fall when unsupported
   | Float  -- floats above liquids
   | Holds  -- supports top stuff
   | Toxic  -- harms life
   | Fatal  -- kills life

instance Object Unit where
   prop Block (Track _) = False
   prop Block _ = False
   prop Fixed (Track _) = False
   prop Fixed _ = False
   prop Float (Track _) = False
   prop Float _ = False
   prop Holds (Track _) = True
   prop Holds _ = True
   prop Toxic (Track _) = False
   prop Toxic _ = False
   prop Fatal (Track _) = False
   prop Fatal _ = False

instance Object Atom where
   prop p a = prop p (unit a)


