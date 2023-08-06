{-# LANGUAGE Safe #-}

module Zero.Draw (draw) where

import Codec.Picture
import Data.Bifunctor
import Control.Monad
import Control.Arrow
import Data.Maybe

type Map a = [((Int,Int),a)]

draw :: String -> Map Bool -> IO ()
draw f m = savePngImage (f ++ ".png") $ ImageY8 (generateImage pxl w h)
   where
   pxl x y = fromMaybe 63 $ lookup (x,y) m'
   (w,h) = join bimap (succ . maximum) . unzip $ map fst m'
   m' = translate m

translate :: Map Bool -> Map Pixel8
translate m = map (bimap (+ δx) (+ δy) . fst &&& toPxl . snd) m
   where
   (δx,δy) = join bimap (negate . minimum) . unzip $ map fst m
   toPxl :: Bool -> Pixel8
   toPxl True = 255
   toPxl False = 127
{-
range :: (Ord a,Num a) => [a] -> a
range [] = 0
range (x:xs) = go x x xs
   where
   go mx mn [] = mx - mn
   go mx mn (x':xs') = go (max mx x') (min mn x') xs'
-}
