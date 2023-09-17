module Verse.State where

import Verse.Setup qualified as Setup
import Verse.Verse

import Data.IntMap ( IntMap )
import Data.IntSet ( IntSet )

data State = Σ {
   δ :: Integer ,
   ν :: Verse ,
   play :: Bool ,
   edit :: Bool ,
   input :: Char ,
   center :: Int ,
   focus :: Int ,
   targets :: IntSet ,
   q_structure :: Structure ,
   q_material :: Material ,
   layer :: Layer ,
   zlevel :: Word ,
   view :: IntMap View }

state :: State
state = Σ {
   δ = 0 ,
   ν = seed ,
   play = False ,
   edit = True ,
   input = 'p' ,
   center = 0 ,
   focus = 0 ,
   targets = mempty ,
   q_structure = toEnum 0 ,
   q_material = toEnum 0 ,
   layer = Surface ,
   zlevel = Setup.zlevel ,
   view = mempty }
   where
   seed = verse []

