module Verse.Sim where

import Zero.Zero
import Verse.Verse
import Data.Foldable ( toList )

sim :: State -> Node -> Node
sim st n@(a,ns)
   | Terra  <- σ st = terra
   | Id     <- σ st = id
   | Noise  <- σ st = noise
   | Smoke  <- σ st = smoke
   | Bees   <- σ st = bees
   | Fish   <- σ st = gene [    2        ] [    2        ] 3
   | Fish2  <- σ st = gene [    2,  4    ] [    2,  4    ] 3
   | Glide  <- σ st = gene [    2,  4,5,6] [    2,3,4,5,6] 4
   | Glide2 <- σ st = gene [  1,  3,4    ] [    2,3,4,5  ] 5
   | Ripple <- σ st = gene [  1          ] [  1,2,3,4,5,6] 7
   | Rippl2 <- σ st = gene [  1,2,  4,5,6] [    2,3,4    ] 7
   | otherwise      = sup (const S1) n

   where

   -- layer aware getters + setters

   -- get value from this node
   s :: Some
      | Superficial <- λ st = α a
      | Elemental   <- λ st = gel a (ε st)
      | otherwise           = maxBound  -- make errors obvious !

   -- set this node's value
   up :: (Some -> Some) -> Node
   up f
      | Superficial <- λ st = sup f n
      | Elemental   <- λ st = (gup (ε st) f a , ns)
      | otherwise           = n

   -- get value from other node in verse
   si :: Int -> Some
   si i
      | Superficial <- λ st = α ai
      | Elemental   <- λ st = gel ai (ε st)
      | otherwise           = maxBound  -- make errors obvious !
      where
      (ai,_) = get (ν st) i


   gene :: [Int] -> [Int] -> Int -> Node
   gene survive born i
      | length (filter (== maxBound) nvs) ∈ survive , s > (toEnum $ i-2) = up (const maxBound)
      | length (filter (== maxBound) nvs) ∈ born , s < succ minBound = up (const maxBound)
      | s <= (toEnum $ i - 2) = up prev
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

   id :: Node = up (const $ α a)

   noise :: Node = up (const $ ρ st !! sal n)
