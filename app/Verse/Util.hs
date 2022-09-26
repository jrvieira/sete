module Verse.Util where

import Verse.Conf
import Data.Tuple ( swap )

import Data.Bifunctor
import Control.Monad

coordToIndex :: (Int,Int) -> Int
coordToIndex (x,y) = y * width + x

indexToCoord :: Int -> (Int,Int)
indexToCoord = swap . flip divMod width

distance :: Int -> Int -> Float
distance a b = undefined

data Dir = U | I | H | L | N | M
   deriving ( Enum, Bounded )

move :: Int -> Dir -> Int -> Int
move n d i
   | U <- d = coordToIndex (mod (pred x') width , mod (y - n) height)
   | I <- d = coordToIndex (x' , mod (y - n) height)
   | H <- d = coordToIndex (mod (x - n) width , y)
   | L <- d = coordToIndex (mod (x + n) width , y)
   | N <- d = coordToIndex (mod (pred x') width , mod (y + n) height)
   | M <- d = coordToIndex (x' , mod (y + n) height)
   where
   (x,y) = indexToCoord i
   x' = if odd y then mod (succ x) width else x

