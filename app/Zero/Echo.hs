module Zero.Echo ( echo ) where

import Data.List ( intersperse )
import Data.List.Split ( chunksOf )
import Data.Map.Strict
import Data.Bifunctor ( bimap )
import Control.Arrow ( (&&&) )
import Control.Monad ( join )

echo :: [((Int,Int),Char)] -> IO ()
echo l = putStrLn $ unlines $ chunksOf (2 * (succ $ xω - xα)) $ intersperse ' ' $ cell <$> [ (x,y) | y <- [yα..yω] , x <- [xα..xω] ]
   where
   cell = maybe ' ' id . (m !?)
   ((xα,xω),(yα,yω)) = join bimap (minimum &&& maximum) $ unzip $ keys m
   m = fromList l

