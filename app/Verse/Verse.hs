module Verse.Verse where

import Zero ( total )

import Verse.Setup
import Verse.Atoms

import Data.Tuple ( swap )
import Data.Map.Strict ( Map )
import Data.Map.Strict qualified as Map ( fromList )
import Data.IntMap ( IntMap )
import Data.IntMap qualified as IntMap ( fromList )
import Control.Arrow

type Node = (Atom,Map Dir Int)

verse :: [Atom] -> IntMap Node
verse as = IntMap.fromList $ take (width * height) $ map n $ zip [0..] (as <> repeat Void)
   where
   n :: (Int,Atom) -> (Int,Node)
   n (i,a) = (i, (a , Map.fromList $ (id &&& ($ i) . move 1) <$> total))

-- coord

distance :: Int -> Int -> Float
distance _ _ = undefined

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

