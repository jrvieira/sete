module Verse.Verse where

import Zero ( total )

import Verse.Setup qualified as Setup ( radius, width, height )

import Data.Char ( toLower )
import Data.Tuple ( swap )
import Data.Bifunctor ( first )
import Data.Set ( Set )
import Data.Map.Strict ( Map )
import Data.Map.Strict qualified as Map ( fromList )
import Data.IntMap ( IntMap )
import Data.IntMap qualified as IntMap ( fromList, adjust )

type Node a = (a,Edge Int)

verse :: [[Atom]] -> IntMap (Node [Atom])
verse as = IntMap.fromList $ take (Setup.width * Setup.height) $ zipWith n [0..] (as <> repeat [])
   where
   n :: Int -> [Atom] -> (Int,Node [Atom])
   n ix a = (ix,(a,Edge u i h l n m))
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
   deriving ( Eq, Ord, Enum, Bounded, Show )

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
-- build if possible otherwise dont
add :: Unit -> Word -> Int -> IntMap (Node [Atom]) -> IntMap (Node [Atom])
add u z k
   | False = id  -- check raw materials
   | otherwise = IntMap.adjust (first $ go z) k
   where
   go 0 (a:as) = a { unit = Just u {- , items = subtract recipe -} } : as
   go i [] = replicate (fromIntegral i) void <> [Atom { unit = Just u , actor = Nothing , items = mempty , elements = mempty }]
   go i (a:as) = a : go (pred i) as

del :: Word -> Int -> IntMap (Node [Atom]) -> IntMap (Node [Atom])
del z = IntMap.adjust (first $ go z)
   where
   go :: Word -> [Atom] -> [Atom]
   go _ [] = []
   go 0 (_:as) = void : as
   go i (a:as) = a : go (pred i) as

upd :: (Atom -> Atom) -> Word -> Int -> IntMap (Node [Atom]) -> IntMap (Node [Atom])
upd f z k = undefined

-- Atom

data Atom = Atom
   { unit :: Maybe Unit
   , actor :: Maybe Actor
   , items :: Map Item Word
   , elements :: Map Element Level
   } deriving ( Show, Eq )

base :: View
base = View {
   atoms = mempty }

void :: Atom
void = Atom
   { unit = Nothing
   , actor = Nothing
   , items = mempty
   , elements = mempty
   }

-- atomic lenses

building :: Atom -> Maybe Building
building = fmap f . unit
   where
   f = \(Unit b _ _) -> b

structure :: Atom -> Maybe Structure
structure = fmap f . unit
   where
   f = (\(Building s _) -> s) . (\(Unit b _ _) -> b)

material :: Atom -> Maybe Material
material = fmap f . unit
   where
   f = (\(Building _ m) -> m) . (\(Unit b _ _) -> b)

condition :: Atom -> Maybe Level
condition = fmap f . unit
   where
   f = \(Unit _ l _) -> l

properties :: Atom -> Maybe (Set Property)
properties = fmap f . unit
   where
   f = \(Unit _ _ p) -> p

-- View is user interface accessible information INDEPENDENT from Pixel rendering
data View = View {
   atoms :: IntMap Atom }
   deriving ( Show, Eq )

visible :: Atom -> Bool
visible a
   | Nothing <- unit a = False
   | otherwise = True

-- | Model

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

data Element = Fire | Water | Light | Radio
   deriving ( Eq, Ord, Show )

data Form = Gas | Liquid | Solid | Plasma

data Property = Perms | Burns | Float | Fixed | Holds | Block | Toxic | Fatal
   deriving ( Eq, Ord, Show )
-- perms :: a -> Bool  -- lets water through
-- burns :: a -> Bool  -- flammable
-- float :: a -> Bool  -- floats above water
--
-- fixed :: a -> Bool  -- does not fall when unsupported
-- holds :: a -> Bool  -- supports top stuff (if not gets destroyed under weight)
-- block :: a -> Bool  -- blocks things from passing
-- toxic :: a -> Bool  -- harms life
-- fatal :: a -> Bool  -- kills life

data Entity = Train | Cat | Bird
   deriving ( Eq, Show )

data Actor = Actor
   { entity :: Entity
   , dir :: Maybe Dir
   , carry :: IntMap Item
   } deriving ( Eq, Show )

-- Atoms have units
-- Units have buildings
-- Buildings have structures
--
-- getter through the Atom, always (see: atomic lenses)

data Material = Dirt | Wood | Stone | Metal
   deriving ( Eq, Enum, Bounded, Ord, Show )

data Structure = Path | Pipe | Wall | Track | Bridge | Table | Art | House
   deriving ( Eq, Enum, Bounded, Show )

data Building = Building Structure Material
   deriving ( Show, Eq )

-- L0 means completely broken
-- properties dictate atom behaviour (holds, block, ...)
data Unit = Unit Building Level (Set Property)
   deriving ( Show, Eq )

-- Items can be transfered around
data Item = Raw Material | Box | Book | Paper
   deriving ( Eq, Ord, Show )

-- Products

class Product a where

   string :: a -> String
   detail :: a -> String
   recipe :: a -> Map Item Word

instance Product Building where

   string (Building Art Dirt)        = "sand castle"
   string (Building Art Metal)       = "copper statue"
   string (Building Art Stone)       = "stone statue"
   string (Building Art Wood)        = "decorative table"
   string (Building Track Metal)     = "railroad track"
   string (Building Track Wood)      = "wooden floor"
   string (Building b m)             = toLower <$> unwords [show m,show b]

   detail (Building Track Metal)     = "state of the art reinforced steel track"
   detail (Building Track Stone)     = "exquisite marble tiled floor"
   detail (Building Track Wood)      = "here you step on wood planks"
   detail (Building b m)             = toLower <$> unwords ["just a",show m,show b]

   recipe (Building Art Dirt)        = Map.fromList []
   recipe (Building Art Metal)       = Map.fromList []
   recipe (Building Art Stone)       = Map.fromList []
   recipe (Building Art Wood)        = Map.fromList []
   recipe (Building Bridge Dirt)     = Map.fromList []
   recipe (Building Bridge Metal)    = Map.fromList []
   recipe (Building Bridge Stone)    = Map.fromList []
   recipe (Building Bridge Wood)     = Map.fromList []
   recipe (Building Table Dirt)      = Map.fromList []
   recipe (Building Table Metal)     = Map.fromList []
   recipe (Building Table Stone)     = Map.fromList []
   recipe (Building Table Wood)      = Map.fromList []
   recipe (Building Track m)         = Map.fromList [(Raw m,10)]
   recipe (Building Wall m)          = Map.fromList [(Raw m,70)]
   recipe _                          = Map.fromList []

instance Product Item where

   string (Raw m)                    = toLower <$> show m
   string Paper                      = "stack of paper"
   string i                          = toLower <$> show i

   detail i                          = unwords ["just one",string i]

   recipe (Raw Dirt)                 = Map.fromList []
   recipe (Raw Metal)                = Map.fromList []
   recipe (Raw Stone)                = Map.fromList []
   recipe (Raw Wood)                 = Map.fromList []
   recipe Book                       = Map.fromList [(Raw Wood,1),(Paper,1)]
   recipe Box                        = Map.fromList []
   recipe Paper                      = Map.fromList [(Raw Wood,1)]
   recipe _                          = Map.fromList []

instance Product Entity where

   string Train                      = "Train"
   string Cat                        = "Cat"
   string Bird                       = "Bird"
   string _                          = "unspecified entity"

   detail i                          = unwords ["regular",string i]

   recipe _                          = Map.fromList []

