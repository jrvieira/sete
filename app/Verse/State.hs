module Verse.State where

import Verse.Setup qualified as Setup
import Verse.Verse

import Data.IntMap ( IntMap )
import Data.IntSet ( IntSet )

data State = Σ {
   ν :: IntMap (Node [Atom]) ,
   play :: Bool ,
   input :: Char ,
   center :: Int ,
   focus :: Int ,
   targets :: IntSet ,
   layer :: Layer ,
   zlevel :: Word ,
   view :: IntMap View }

state :: State
state = Σ {
   ν = seed ,
   play = False ,
   input = 'p' ,
   center = 0 ,
   focus = 0 ,
   targets = mempty ,
   layer = Surface ,
   zlevel = Setup.zlevel ,
   view = mempty }
   where
   seed = verse []
