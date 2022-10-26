module Verse.Sim where

import Zero.Zero

import Verse.Types
import Verse.Verse

sim :: State -> Node -> Node
sim st n@(a,ns)
   | Terra  <- σ st = terra
   | Smoke  <- σ st = smoke
   | Bees   <- σ st = bees
   | Fish   <- σ st = gene [2] [2] 3
   | Fish2  <- σ st = gene [2,4] [2,4] 3
   | Glide  <- σ st = gene [2,4,5,6] [2,3,4,5,6] 4
   | Glide2 <- σ st = gene [1,3,4] [2,3,4,5] 5
   | Ripple <- σ st = gene [1] [1,2,3,4,5,6] 14
   | Rippl2 <- σ st = gene [1,2,4,5,6] [2,3,4] 8
   | Id     <- σ st = n
   | otherwise      = sup (const S1) n

   where

   terra :: Node
      | leq > 2 = n
      | lgt < llt = sup prev n
      | otherwise = sup next n
      where
      leq = length $ filter (== sal n) nvs
      lgt = length $ filter (>  sal n) nvs
      llt = length $ filter (<  sal n) nvs
      nvs = sal . (ν st !) <$> ns
   -- avg = div (sum (sal . (ν st !) <$> ns)) (genericLength ns)

   gene :: [Int] -> [Int] -> Int -> Node
   gene survive born i
      | length (filter (== 7) nvs) ∈ survive , sal n > (i-2) = sup (const S7) n
      | length (filter (== 7) nvs) ∈ born , sal n < 1 = sup (const S7) n
      | sal n <= (i-2) = sup prev n
      | otherwise = sup (const $ toEnum (i-2)) n
      where
      nvs = sal . (ν st !) <$> ns

   smoke :: Node
      | length (filter (== minimum nvs) nvs) <= (sal n - minimum nvs) = sup prev n
      | sum (filter (> sal n) nvs) > 6 = sup next n
      | otherwise = n
      where
      nvs = sal . (ν st !) <$> ns

   bees :: Node
      | length (filter (> sal n) nvs) ∈ [2] = sup next n
      | length (filter (> sal n) nvs) ∈ [2] = n
      | otherwise = sup prev n
      where
      nvs = sal . (ν st !) <$> ns

