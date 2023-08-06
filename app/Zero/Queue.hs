{-# LANGUAGE Safe #-}

module Zero.Queue where

import Data.Foldable

-- queue
data Queue a = Queue [a] [a]

instance Eq a => Eq (Queue a) where
   qa == qb = toList qa == toList qb

instance Semigroup (Queue a) where
   Queue ia oa <> Queue ib ob = Queue ib (oa <> reverse ia <> ob)

instance Monoid (Queue a) where
   mempty = Queue [] []

instance Functor Queue where
   fmap f (Queue i o) = Queue (fmap f i) (fmap f o)

instance Foldable Queue where
   foldr f z (Queue i o) = foldr f (foldr f z $ reverse i) o

instance Show a => Show (Queue a) where
   show q = unwords ["queue",show $ toList q]

queue :: [a] -> Queue a
queue = Queue []

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue i o) = Queue (x:i) o

infixr 7 >|
(>|) :: a -> Queue a -> Queue a
(>|) = enqueue

dequeue :: Queue a -> Maybe (a,Queue a)
dequeue q@(Queue i o)
   | null q = Nothing
   | null o = dequeue $ Queue [] (reverse i)
   | otherwise = Just (head o,Queue i (tail o))

