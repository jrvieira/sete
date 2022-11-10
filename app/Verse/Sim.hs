module Verse.Sim where

import Zero.Zero
import Verse.Verse
import Data.Foldable ( toList )

sim :: State -> Node -> Node
sim st n@(a,ns)
   | True        = n
   | Terra  <- s = terra
   | Noise  <- s = noise
   | Smoke  <- s = smoke
   | Bees   <- s = bees
   | Fish   <- s = gene [    2        ] [    2        ] 3
   | Fish2  <- s = gene [    2,  4    ] [    2,  4    ] 3
   | Glide  <- s = gene [    2,  4,5,6] [    2,3,4,5,6] 4
   | Glide2 <- s = gene [  1,  3,4    ] [    2,3,4,5  ] 5
   | Ripple <- s = gene [  1          ] [  1,2,3,4,5,6] 7
   | Rippl2 <- s = gene [  1,2,  4,5,6] [    2,3,4    ] 7
   | otherwise   = n

   where

   s = σ st

-- -- get value from this node
-- s :: Level

-- -- set this node's value
-- up :: (Level -> Level) -> Node

-- -- get value from other node in verse
-- si :: Int -> Level


   gene :: [Int] -> [Int] -> Int -> Node
   gene survive born i
      | length (filter (== maxBound) nvs) ∈ survive , s > toEnum (i-2) = up (const maxBound)
      | length (filter (== maxBound) nvs) ∈ born , s < succ minBound = up (const maxBound)
      | s <= toEnum (i - 2) = up prev
      | otherwise = up (const $ toEnum (i-2))
      where
      nvs = toList $ si <$> ns

   terra :: Node
      | leq > 2 = n
      | lgt < llt = up prev
      | otherwise = up next
      where
      leq = length $ filter (== s) nvs
      lgt = length $ filter (>  s) nvs
      llt = length $ filter (<  s) nvs
      nvs = toList $ si <$> ns

   smoke :: Node
      | length (filter (== minimum nvs) nvs) <= (fromEnum s - fromEnum (minimum nvs)) = up prev
      | sum (fromEnum <$> filter (> s) nvs) > 6 = up next
      | otherwise = n
      where
      nvs = toList $ si <$> ns

   bees :: Node
      | length (filter (> s) nvs) ∈ [2] = up next
      | length (filter (> s) nvs) ∈ [2] = n
      | otherwise = sup prev n
      where
      nvs = toList $ si <$> ns

   noise :: Node = up (const $ ρ st !! sal n)

