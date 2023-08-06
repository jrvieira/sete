{-# LANGUAGE Trustworthy, BangPatterns #-}

module Zero.Zero where

import Zero.Color
-- import Zero.Draw
import Debug.Trace qualified as Debug ( trace )
import Data.List
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Arrow

-- trace
infix 1 #
(#) :: a -> String -> a
(#) a s = Debug.trace (clr Bold $ clr Blue $ "# " ++ s) a

trace :: Show b => String -> (a -> b) -> a -> a
trace s f x = x  # unwords [s,show $ f x]

size :: (Foldable t, Enum i, Num i) => t a -> i
size = foldl' (flip $ const succ) 0

-- full range of bounded set
total :: (Bounded a,Enum a) => [a]
total = [minBound..]

-- bounded enum
next :: (Eq a, Enum a, Bounded a) => a -> a
next a = if a == maxBound then a else succ a

prev :: (Eq a, Enum a, Bounded a) => a -> a
prev a = if a == minBound then a else pred a

-- cycle through enums
forw :: (Eq a, Enum a, Bounded a) => a -> a
forw a = if a == maxBound then minBound else succ a

back :: (Eq a, Enum a, Bounded a) => a -> a
back a = if a == minBound then maxBound else pred a

-- parse numbers from a string
parseNums :: (Read a,Num a) => String -> [a]
parseNums xs
      | null n = []
      | otherwise = read n : parseNums r
      where
      (n,r) = span numeric $ dropWhile (not . numeric) xs

      numeric :: Char -> Bool
      numeric c = or [isDigit c,c ∈ "-"]

-- remove duplicates
unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

-- count each element's occurrence
index :: forall f a n. (Foldable f, Ord a, Num n) => f a -> Map a n
index = foldl' go Map.empty
   where
   go :: Map a n -> a -> Map a n
   go m x = Map.insertWith (+) x 1 m

-- last n elements
takeLast :: Int -> [a] -> [a]
takeLast n l = go (drop n l) l
   where
   go [] rs = rs
   go xs ys = go (tail xs) (tail ys)

packs :: [Int] -> [a] -> ([[a]],[a])
packs nn ls = first (go nn) $ splitAt (sum nn) ls
   where
   go :: [Int] -> [a] -> [[a]]
   go [] xs = pure xs
   go (n:ns) xs = let (x,xs') = splitAt n xs in x : go ns xs'

-- get chunks between prefix and suffix, delimited
insides :: (a -> Bool) -> (a -> Bool) -> a -> [a] -> [a]
insides ps pe d = go False
   where
   go _ [] = []
   go False (x:xs) = go (ps x) xs
   go True (x:xs)
      | pe x = d : go False xs
      | otherwise = x : go True xs

-- fixed point of iteration
fixp :: Eq a => (a -> a) -> a -> a
fixp f !a
   | a == a' = a
   | otherwise = fixp f a'
   where
   a' = f a

-- remove element from assoc by key
remove :: Eq k => k -> [(k,v)] -> [(k,v)]
remove _ [] = []
remove k (x:xs)
   | k == fst x = remove k xs
   | otherwise = x : remove k xs

-- remove element from assoc by value
erase :: Eq v => v -> [(k,v)] -> [(k,v)]
erase _ [] = []
erase v (x:xs)
   | v == snd x = erase v xs
   | otherwise = x : erase v xs

-- tortoise and hare cycle finding
floyd :: Eq a => [a] -> Maybe (Integer,a)
floyd [] = Nothing
floyd (a:as) = go 1 1 a as
   where
   go _ _ _ [] = Nothing
   go pow lam x (y:ys)
      | x == y     = Just (lam,x)
      | pow == lam = go (2*pow) 1 y ys
      | otherwise  = go pow (1+lam) x ys

-- simple plot from list of ints
-- plot :: String -> [Int] -> IO ()
-- plot f = draw f . go 0
--    where
--    go _ [] = []
--    go n (x:xs) = [((n,y),True) | y <- [0..x]] ++ go (succ n) xs

-- test
tept :: Show a => String -> (a -> Bool) -> a -> IO ()
tept t p a = putStrLn $ unwords ['\n' : t , clr c m , '\n' : clr c (show a) , "\n"]
   where
   (c,m)
      | p a = (Green,"v")
      | otherwise = (Red,"x")

teqt :: (Eq a,Show a) => String -> a -> a -> IO ()
teqt t e a = putStrLn $ unwords ['\n' : t , clr c m , r , '\n' : clr c (show a) , "\n"]
   where
   (c,m,r)
      | a == e = (Green,"v","")
      | otherwise = (Red,"x",'\n' : show e)

-- set
(∈) :: (Foldable t, Eq a) => a -> t a -> Bool
(∈) = elem

(∉) :: (Foldable t, Eq a) => a -> t a -> Bool
(∉) = notElem

(∋) :: (Foldable t, Eq a) => t a -> a -> Bool
(∋) = flip elem

class Comparable s where
   (∪) :: s -> s -> s
   (∩) :: s -> s -> s

instance Eq a => Comparable [a] where
   (∪) = union
   (∩) = intersect

instance Ord a => Comparable (Set a) where
   (∪) = Set.union
   (∩) = Set.intersection
