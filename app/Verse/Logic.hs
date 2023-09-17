module Verse.Logic where

import Verse.State
import Verse.Verse

import Data.Bifunctor ( bimap )
import Data.IntMap ( IntMap, foldrWithKey, insert )
import Data.IntMap qualified as IntMap

-- | One tick

step :: State -> State
step st = st { ν = ν' , view = view' }
   where

   -- type Node a = (a,Edge Int)
   -- type Verse = IntMap (Node [Atom])
   -- ν st :: Verse

   -- run the simulation through each Node accumulating all Views and Nodes
   (view',ν') :: (IntMap View,IntMap (Node [Atom]))
      = foldrWithKey sim (mempty,mempty) (ν st)

   sim :: Int -> Node [Atom] -> (IntMap View,Verse) -> (IntMap View,Verse)
   sim k (as,e) = bimap (insert k v) (insert k (as',e))
      where

      (v,as') :: (View,[Atom])
         = go base 0 as es

      -- atoms of edges
      es :: Edge [Atom]
         = fst . (ν st IntMap.!) <$> e

   -- each Node stores a vertical list of atoms and Edge (set of pointers to adjacent nodes)
   -- each list of Atoms is traversed from bottom to top, the traversal is synced between all 7 nodes
   -- each Atom updates by looking (and only looking) at its neighbours and the next
   go :: View -> Word -> [Atom] -> Edge [Atom] -> (View,[Atom])
   go v z [] _
      | z <= zlevel st = (v,[])
      | otherwise = (v,[])
   go v z (a:as) es = (v'',a':as')
      where

      -- final view and updated rest of atoms
      (v'',as') :: (View,[Atom])
         = go v' (succ z) as (drop 1 <$> es)

      -- add atom to View (which for now is just an IntMap of atoms)
      v' :: View = v { atoms = insert (fromIntegral z) a (atoms v) }

      -- This needs some work
      -- updated atom
      a' :: Atom
         | play st = a  -- update things
         | otherwise = a

      adj :: Edge Atom
         = f <$> es
         where
         f [] = void
         f (x:_) = x

   -- next atom
   -- aa :: Atom
   --    | x:_ <- as = x
   --    | otherwise = void

      -- TODO:

      -- transfer function for items (both to/from neighbours and vertically)
      --                                   ^^^^^^^ think about this

      -- bridge atoms have to be either supported themselves by bottom atom or have a supported neighbour

      -- implement gravity physics (fall and climb)

      -- TODO:
      -- water physics

      -- view layer for Entities

      -- while Units are manipulated directly by the user
      -- Items are manipulated ONLY by automated Units and (automated or user controlled) Entities

      -- fire spreads like water (when an atom reaches L7 it "spews" to flammable neighbours)
      -- radio waves spread on L1

      -- water spills to neighbours when it reaches 7:
      -- 1 remains in center, all neighbours get 1
      -- neighbours know this by "transfer" mechanism
      -- water flows upwards when it cant flow down or sides


