module Verse.Verse where

import Zero ( total )

import Verse.Setup qualified as Setup ( radius, width, height )

import Data.Tuple ( swap )
import Data.Bifunctor ( first )
import Data.Set ( Set )
import Data.Set qualified as Set ( fromList )
import Data.Map.Strict ( Map )
import Data.Map.Strict qualified as Map ( fromList, filter, keys )
import Data.IntMap ( IntMap )
import Data.IntMap qualified as IntMap ( fromList, adjust )

type Node a = (a,Edge Int)

type Verse = IntMap (Node [Atom])

verse :: [[Atom]] -> Verse
verse as = IntMap.fromList $ take (Setup.width * Setup.height) $ map n $ zip [0..] (as <> repeat [])
   where
   n :: (Int,[Atom]) -> (Int,Node [Atom])
   n (ix,a) = (ix,(a,Edge u i h l n m))
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

-- | Manipulation

-- TODO:
-- each unit should have a material cost
-- n of that material should be present in the Atom (do a items check)
-- build if possible, don't if not
add :: Unit -> (Int,Word) -> Verse -> Verse
add u (k,z)
   | False = id  -- check raw materials
   | otherwise = IntMap.adjust (first (go z)) k
   where
   go 0 (a:as) = a { unit = u {- , items = subtract recipe -} } : as
   go i [] = replicate (fromIntegral i) Void <> [Atom { unit = u , items = mempty }]
   go i as = go (pred i) (tail as)

del :: (Int,Word) -> Verse -> Verse
del (k,z) = IntMap.adjust (first (go z)) k
   where
   go 0 (_:as) = as
   go _ [] = []
   go i as = go (pred i) (tail as)

upd :: (Atom -> Atom) -> (Int,Word) -> Verse -> Verse
upd f (k,z) = IntMap.adjust (first (go z)) k
   where
   go 0 (a:as) = f a : as
   go _ [] = []
   go i as = go (pred i) (tail as)

-- Atom

data Atom = Base | Void | Atom {
   unit :: Unit ,
   items :: Map Item Word ,
   elements :: Map Element Level }
   deriving Show

-- atomic lenses

building :: Atom -> Building
building = (\(Unit b _ _) -> b) . unit

structure :: Atom -> Structure
structure = (\(Building s _) -> s) . (\(Unit b _ _) -> b) . unit

material :: Atom -> Material
material = (\(Building _ m) -> m) . (\(Unit b _ _) -> b) . unit

level :: Atom -> Level
level = (\(Unit _ l _) -> l) . unit

properties :: Atom -> Set Property
properties = (\(Unit _ _ ps) -> ps) . unit

-- View is interface accessible information INDEPENDENT from Pixel rendering
data View = View {
   atom :: Atom ,
   z :: Word }
   deriving ( Show )

base :: View
base = View {
   atom = Base ,
   z = 0 }

-- | Model

-- buildings_list :: Map String Unit
-- buildings_list = Map.fromList [
--    ("book",Building)
--    ]
--
-- data Building = Building {
--    building_unit :: Unit ,
--    building_material :: Material ,
--    building_cost :: Word }

data Element = Fire | Water | Light | Radio
   deriving Show

class Product a where
   recipe :: a -> Map Item Word

-- Atoms have units
-- Units have buildings
-- Buildings have structures
--
-- getter through the Atom, always

data Material = Dirt | Wood | Stone | Metal
   deriving ( Eq, Ord, Show )

data Structure = Wall | Track | Sculpture
   deriving ( Eq, Enum, Bounded, Show )

data Building = Building Structure Material
   deriving Show

-- L0 is completely broken
-- properties dictate atom behaviour (holds, block, ...)
data Unit = Unit Building Level (Set Property)
   deriving Show

instance Product Building where

   recipe (Building Wall Dirt)       = Map.fromList []
   recipe (Building Wall Wood)       = Map.fromList []
   recipe (Building Wall Stone)      = Map.fromList []
   recipe (Building Wall Metal)      = Map.fromList []

   recipe (Building Track Dirt)      = Map.fromList []
   recipe (Building Track Wood)      = Map.fromList []
   recipe (Building Track Stone)     = Map.fromList []
   recipe (Building Track Metal)     = Map.fromList []

   recipe (Building Sculpture Dirt)  = Map.fromList []
   recipe (Building Sculpture Wood)  = Map.fromList []
   recipe (Building Sculpture Stone) = Map.fromList []
   recipe (Building Sculpture Metal) = Map.fromList []

   recipe _                          = Map.fromList []

-- Items can be transfered around
data Item = Raw Material | Box | Book | Paper
   deriving ( Eq, Ord, Show )

instance Product Item where

   recipe (Raw Dirt)       = Map.fromList []
   recipe (Raw Wood)       = Map.fromList []
   recipe (Raw Stone)      = Map.fromList []
   recipe (Raw Metal)      = Map.fromList []

   recipe Paper            = Map.fromList [(Raw Wood,1)]
   recipe Box              = Map.fromList []
   recipe Book             = Map.fromList [(Raw Wood,1),(Paper,1)]

   recipe _                = Map.fromList []

-- Entities live in a IntMap [Entity] that is calculated after
data Entity = Cat | Bird
   deriving Show

data Form = Gas | Liquid | Solid | Plasma

data Property = Perms | Burns | Float | Fixed | Holds | Block | Toxic | Fatal
   deriving Show

-- class Object a where
--
--    perms :: a -> Bool  -- lets water through
--    burns :: a -> Bool  -- flammable
--    float :: a -> Bool  -- floats above water
--
--    fixed :: a -> Bool  -- does not fall when unsupported
--    holds :: a -> Bool  -- supports top stuff (if not gets destroyed under weight)
--    block :: a -> Bool  -- blocks things from passing
--    toxic :: a -> Bool  -- harms life
--    fatal :: a -> Bool  -- kills life

-- Meta

data Layer = Surface | Inspect | Numeric | Density
   deriving ( Eq, Enum, Bounded, Show )

data Level = L0 | L1 | L2 | L3 | L4 | L5 | L6 | L7
   deriving ( Eq, Enum, Ord, Bounded, Show )

instance Num Level where
   a + b = toEnum $ min (fromEnum (maxBound :: Level)) (fromEnum a + fromEnum b)
   a - b = toEnum $ max (fromEnum (minBound :: Level)) (fromEnum a - fromEnum b)
   a * b = toEnum $ min (fromEnum (maxBound :: Level)) (fromEnum a * fromEnum b)
   abs = id
   signum = const 1
   fromInteger = toEnum . fromInteger

-- instance Random Level where
--    random g = let (r,g') = randomR (0,7) g in (toEnum r , g')
--    randomR (a,b) g = let (r,g') = randomR (fromEnum a , fromEnum b) g in (toEnum r , g')
