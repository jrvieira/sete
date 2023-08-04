module Verse.State where

import Verse.Plane ( Node, plane )

import Data.IntMap ( IntMap )
import Data.IntSet ( IntSet )

data State = Σ {
   nodes :: IntMap Node ,
   play :: Bool ,
   input :: Char ,
   center :: Int ,
   focus :: Int ,
   targets :: IntSet }

state :: State
state = Σ {
   nodes = plane [] ,
   play = False ,
   input = 'p' ,
   center = 0 ,
   focus = 0 ,
   targets = mempty }
