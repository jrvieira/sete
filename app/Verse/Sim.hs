module Verse.Sim where

import Zero.Zero
import Verse.Verse
import Data.IntMap qualified as IntMap ( (!) )
import Data.Map.Strict qualified as Map ( (!), mapWithKey, elems )
import Data.Foldable ( toList )

-- | A step in simulation

sim :: State -> Node -> Node
sim st (a,ns) = ob (a { ες = Map.mapWithKey el (ες a) } , ns)
   where

   el :: Element -> Level -> Level
   el e
      | Air   <- e = smoke
      | Water <- e = rippl'
      | Fire  <- e = bees
      | Earth <- e = terra
      | otherwise  = noise
      where

      -- | Get atomic level by index

      lev :: Int -> Level
      lev i = es Map.! e
         where
         (Atom _ es,_) = ν st IntMap.! i

      -- | Automata

      gene :: [Int] -> [Int] -> Int -> Level -> Level
      gene survive born n l
         | length (filter (== maxBound) nvs) ∈ survive , fromEnum l > n-2 = maxBound
         | length (filter (== maxBound) nvs) ∈ born , l < succ minBound = maxBound
         | l <= toEnum (n - 2) = prev l
         | otherwise = toEnum (n-2)
         where
         nvs = toList $ lev <$> ns

      fish   = gene [    2        ] [    2        ] 3
      fish'  = gene [    2,  4    ] [    2,  4    ] 3
      glide  = gene [    2,  4,5,6] [    2,3,4,5,6] 4
      glide' = gene [  1,  3,4    ] [    2,3,4,5  ] 5
      ripple = gene [  1          ] [  1,2,3,4,5,6] 7
      rippl' = gene [  1,2,  4,5,6] [    2,3,4    ] 7

      terra :: Level -> Level
      terra l
         | leq > 2 = l
         | lgt < llt = prev l
         | otherwise = next l
         where
         leq = length $ filter (== l) nvs
         lgt = length $ filter (>  l) nvs
         llt = length $ filter (<  l) nvs
         nvs = toList $ lev <$> ns

      smoke :: Level -> Level
      smoke l
         | length (filter (== minimum nvs) nvs) <= (fromEnum l - fromEnum (minimum nvs)) = prev l
         | sum (fromEnum <$> filter (> l) nvs) > 6 = next l
         | otherwise = l
         where
         nvs = toList $ lev <$> ns

      bees :: Level -> Level
      bees l
         | length (filter (> l) nvs) ∈ [2] = next l
         | length (filter (> l) nvs) ∈ [2] = l
         | otherwise = prev l
         where
         nvs = toList $ lev <$> ns

      -- get the (sum ns) index of ρ for a pseudorandom level
      noise :: Level -> Level
      noise _ = toEnum $ mod (sum $ fromEnum . lev <$> ns) (fromEnum (maxBound :: Level))

   -- | Update surface object, depending on current state and Atom properties

   ob :: Node -> Node
   ob = id

