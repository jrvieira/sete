module Verse.Logic where

import Verse.State

-- | One tick

step :: State -> State
step st
   | play st = id st
   | otherwise = st

