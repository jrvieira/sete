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

   (view',ν') = foldrWithKey sim (mempty,mempty) (ν st)

   sim :: Int -> Node [Atom] -> (IntMap View,Verse) -> (IntMap View,Verse)
   sim k (as,es) = bimap (insert k v) (insert k (as',es))
      where

      (v,as') = go as e 0 base

      e = (<> repeat void) . fst . (ν st IntMap.!) <$> es

      -- each Node stores a vertical list of atoms and Edge, a set of pointers to adjacent nodes
      -- each list of Atoms is traversed from bottom to top, the traversal is synced between all 7 nodes
      -- each Atom updates by looking (only looking) at its neighbours and the next
      go :: [Atom] -> Edge [Atom] -> Word -> View -> (View,[Atom])
      go [] _ _ v' = (v',[])
      go (a:as) e' z v' = (v''',a':as')
         where

         aa :: Atom
         aa
            | x:_ <- as = x
            | otherwise = void

         Edge (u:us) (i:is) (h:hs) (l:ls) (n:ns) (m:ms) = e'
         e = Edge u i h l n m

         (v''',as') = go as (Edge us is hs ls ns ms) (succ z) v''

         v'' :: View
         v''
            | z > zlevel st = v'
            | z == zlevel st = v' { atom = a , top = top' }
            | Nothing <- building a = v'
            | otherwise = v' { top = top' }
            where
            top'
               | visible a = (z,a)
               | otherwise = top v'

         -- TODO:

         -- transfer function for items (both to/from neighbours and vertically)
         --                                   ^^^^^^^ think about this

         -- bridge atoms have to be either supported themselves by bottom atom or have a supported neighbour

         -- implement gravity physics (fall and climb)

         -- This needs some work
         a' :: Atom
         a'
            | play st = a  -- update things
            | otherwise = a

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


