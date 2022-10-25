module Verse.Sim where

import Zero.Zero

import Verse.Types
import Verse.Verse

sim :: State -> Node -> Node
sim st a@(Atom s,ns)
   | Terra  <- σ st = terra
   | Dois   <- σ st = dois
   | Bees   <- σ st = bees
   | Fish   <- σ st = gene [2] [2] 3
   | Fish2  <- σ st = gene [2,4] [2,4] 3
   | Glide  <- σ st = gene [2,4,5,6] [2,3,4,5,6] 4
   | Glide2 <- σ st = gene [1,3,4] [2,3,4,5] 5
   | Ripple <- σ st = gene [1] [1,2,3,4,5,6] 14
   | Rippl2 <- σ st = gene [1,2,4,5,6] [2,3,4] 8
   | Id     <- σ st = a
   | otherwise      = sup (const S1) a

   where

   terra :: Node
      | leq > 2 = a
      | lgt < llt = (Atom $ prev s,ns)
      | otherwise = (Atom $ next s,ns)
      where
      leq = length $ filter (== sal a) nvs
      lgt = length $ filter (>  sal a) nvs
      llt = length $ filter (<  sal a) nvs
      nvs = sal . (ν st !) <$> ns
   -- avg = div (sum (sal . (ν st !) <$> ns)) (genericLength ns)

   gene :: [Int] -> [Int] -> Int -> Node
   gene survive born n
      | length (filter (== 7) nvs) ∈ survive , sal a > (n-2) = (Atom S7,ns)
      | length (filter (== 7) nvs) ∈ born , sal a < 1 = (Atom S7,ns)
      | sal a <= (n-2) = (Atom $ prev s,ns)
      | otherwise = (Atom $ toEnum (n-2),ns)
      where
      nvs = sal . (ν st !) <$> ns

   dois :: Node
      | length (filter (== minimum nvs) nvs) <= (sal a - minimum nvs) = (Atom $ prev s,ns)
      | sum (filter (> sal a) nvs) > 6 = (Atom $ next s,ns)
      | otherwise = a
      where
      nvs = sal . (ν st !) <$> ns

   bees :: Node
      | length (filter (> sal a) nvs) ∈ [2] = (Atom $ next s,ns)
      | length (filter (> sal a) nvs) ∈ [2] = a
      | otherwise = (Atom $ prev s,ns)
      where
      nvs = sal . (ν st !) <$> ns

