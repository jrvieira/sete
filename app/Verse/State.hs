module Verse.State where

import Verse.Verse ( Node, verse )

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
   nodes = verse [] ,
   play = False ,
   input = 'p' ,
   center = 0 ,
   focus = 0 ,
   targets = mempty }
