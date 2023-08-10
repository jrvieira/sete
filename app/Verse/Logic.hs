module Verse.Logic where

import Verse.State
import Verse.Verse

import Data.IntMap ( IntMap, foldrWithKey, insert )
import Data.IntMap qualified as IntMap

-- | One tick

step :: State -> State
step st
   | play st = st { ν = ν' , view = view' }
   | otherwise = st
   where

   (view',ν') = foldrWithKey sim (mempty,mempty) (ν st)

   sim :: Int -> Node [Atom] -> (IntMap View,IntMap (Node [Atom])) -> (IntMap View,IntMap (Node [Atom]))
   sim k (as,es) (vi,ve) = (insert k v vi,insert k (as',es) ve)
      where

      (v,as') = go as e 0 base

      e = (<> repeat Void) . fst . (ν st IntMap.!) <$> es

      -- each Node stores a vertical list of atoms and Edge, a set of pointers to adjacent nodes
      -- each list of Atoms is traversed from bottom to top, the traversal is synced between all 7 nodes
      -- each Atom updates by looking (only looking) at its neighbours and the next
      go :: [Atom] -> Edge [Atom] -> Word -> View -> (View,[Atom])
      go [] _ _ v' = (v',[])
      go (a:as) e' z v' = (v''',a':as')
         where

         Edge (u:us) (i:is) (h:hs) (l:ls) (n:ns) (m:ms) = e'
         e = Edge u i h l n m

         (v''',as') = go as (Edge us is hs ls ns ms) (succ z) v''

         v'' :: View
         v''
            | zlevel st > z = v'
            | Void <- a = v'
            | otherwise = v' {
               atom = a ,
               z = z }

         aa :: Atom
         aa
            | (x:_) <- as = x
            | otherwise = Void

         -- TODO:
         -- implement gravity physics fall

         -- This needs some work
         a' :: Atom
         a'
            | Void <- a = a
            | Atom { } <- a = a
            | otherwise = a

         -- TODO:
         -- water physics
