module Main where

import Verse.Verse
import Terminal.Game
import Data.Colour

main :: IO ()
main = do
   playGame $ Game {
      gTPS = 1 ,
      gInitState = state ,
      gLogicFunction = logic ,
      gDrawFunction = draw ,
      gQuitFunction = const False }

logic :: GEnv -> State -> Event -> State
logic _ st _ = st

draw :: GEnv -> State -> Plane
draw _ _ = vcat [
   word "blending test:" ,
   gradient (sRGB24 51 255 51) (sRGB24 255 10 11) ,
   gradient (sRGB24 255 255 255) (sRGB24 0 0 0) ,
   word "done" ]
   where
   gradient a b = hcat $ tone . fade a b <$> steps
   tone k = cell '█' # rgbColor k
   fade a b x = blend x a b
   steps = [0,0.3,0.4,0.5,0.6,0.7,1]

