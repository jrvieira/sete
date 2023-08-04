module Main where

import Verse.State
import Verse.Logic ( step )
import Verse.Plane
import Verse.Print

import Terminal.Game

import Data.IntSet qualified as IntSet ( member, insert, delete )

main :: IO ()
main = do
   playGame $ Game {
      gTPS = 16 ,
      gInitState = state ,
      gLogicFunction = logic ,
      gDrawFunction = draw }

-- | Render the universe

draw :: GEnv -> State -> Plane
draw ge = centerFull ge . picture

-- | Catch user input

logic :: GEnv -> State -> Event -> Either () State
logic _ st Tick = Right $ step st
logic _ st (KeyPress k) = Right $ st' { input = k }
   where

   st'

      -- play / pause

      | 'p' <- k             = st { play = not $ play st }
      | 'P' <- k             = (step st { play = True }) { play = False }

      -- movement

      | 'h' <- k             = st { focus = move 1 H (focus st) }
      | 'j' <- k             = st { focus = move 1 N (focus st) }
      | 'k' <- k             = st { focus = move 1 I (focus st) }
      | 'l' <- k             = st { focus = move 1 L (focus st) }
      | 'u' <- k             = st { focus = move 1 U (focus st) }
      | 'i' <- k             = st { focus = move 1 I (focus st) }
      | 'n' <- k             = st { focus = move 1 N (focus st) }
      | 'm' <- k             = st { focus = move 1 M (focus st) }
      | 'c' <- k             = st { focus = center st }  -- focus on center

      -- scroll

      | 'C' <- k             = st { center = focus st }  -- center on focus
      | 'H' <- k             = st { center = move 1 H (center st) , focus = move 1 H (focus st) }
      | 'J' <- k             = st { center = move 1 N (center st) , focus = move 1 N (focus st) }
      | 'K' <- k             = st { center = move 1 I (center st) , focus = move 1 I (focus st) }
      | 'L' <- k             = st { center = move 1 L (center st) , focus = move 1 L (focus st) }
      | 'U' <- k             = st { center = move 1 U (center st) , focus = move 1 U (focus st) }
      | 'I' <- k             = st { center = move 1 I (center st) , focus = move 1 I (focus st) }
      | 'N' <- k             = st { center = move 1 N (center st) , focus = move 1 N (focus st) }
      | 'M' <- k             = st { center = move 1 M (center st) , focus = move 1 M (focus st) }

      -- target

      | 't' <- k             = st { targets = if IntSet.member (focus st) (targets st) then IntSet.delete (focus st) (targets st) else IntSet.insert (focus st) (targets st) }
      | 'T' <- k             = st { targets = mempty }

      -- none

      | otherwise            = st

