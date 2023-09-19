module Main where

import Zero ( next, prev, forw, back )

import Verse.State
import Verse.Verse
import Verse.Logic ( step )
import Verse.Pixel

import Terminal.Game

import Data.Char ( toLower )
import Data.IntMap qualified as IntMap ( (!) )
import Data.IntSet qualified as IntSet ( member, insert, delete, map, elems )

main :: IO ()
main = do
   playGame $ Game {
      gTPS = 16 ,
      gInitState = state ,
      gLogicFunction = logic ,
      gDrawFunction = draw }

-- | Render the universe

draw :: GEnv -> State -> Plane
draw g = centerFull g . plane

-- | Catch user input

logic :: GEnv -> State -> Event -> Either () State
logic _ st Tick = Right $ step st
logic _ st (KeyPress k) = Right $ st' { input = k }
   where

   st'

      -- play / pause

      | 'p' <- k                                                   = st { play = not $ play st }
      | 'P' <- k                                                   = (step st { play = True }) { play = False }

      -- z level

      | '.' <- k                                                   = st { zlevel = next (zlevel st) }
      | ',' <- k                                                   = st { zlevel = prev (zlevel st) }
      | '<' <- k                                                   = st { zlevel = 0 }
      | '>' <- k                                                   = st { zlevel = prev $ fromIntegral $ length $ atoms $ view st IntMap.! focus st }

      -- toggle edit

      | 'e' <- k                                                   = st { edit = True }
      | 'E' <- k                                                   = st { edit = False }

      -- movement

      | 'h' <- k , edit st                                         = st { focus = move 1 H (focus st) }
      | 'j' <- k , edit st                                         = st { focus = move 1 N (focus st) }
      | 'k' <- k , edit st                                         = st { focus = move 1 I (focus st) }
      | 'l' <- k , edit st                                         = st { focus = move 1 L (focus st) }
      | 'u' <- k , edit st                                         = st { focus = move 1 U (focus st) }
      | 'i' <- k , edit st                                         = st { focus = move 1 I (focus st) }
      | 'n' <- k , edit st                                         = st { focus = move 1 N (focus st) }
      | 'm' <- k , edit st                                         = st { focus = move 1 M (focus st) }
      | 'c' <- k , edit st                                         = st { focus = center st }  -- focus on center
      | 'C' <- k , edit st                                         = st { center = focus st }  -- focus on center

      -- scroll

      | 'h' <- toLower k                                           = st { center = move 1 H (center st) , focus = move 1 H (focus st) , targets = IntSet.map (move 1 H) (targets st) }
      | 'j' <- toLower k                                           = st { center = move 1 N (center st) , focus = move 1 N (focus st) , targets = IntSet.map (move 1 N) (targets st) }
      | 'k' <- toLower k                                           = st { center = move 1 I (center st) , focus = move 1 I (focus st) , targets = IntSet.map (move 1 I) (targets st) }
      | 'l' <- toLower k                                           = st { center = move 1 L (center st) , focus = move 1 L (focus st) , targets = IntSet.map (move 1 L) (targets st) }
      | 'u' <- toLower k                                           = st { center = move 1 U (center st) , focus = move 1 U (focus st) , targets = IntSet.map (move 1 U) (targets st) }
      | 'i' <- toLower k                                           = st { center = move 1 I (center st) , focus = move 1 I (focus st) , targets = IntSet.map (move 1 I) (targets st) }
      | 'n' <- toLower k                                           = st { center = move 1 N (center st) , focus = move 1 N (focus st) , targets = IntSet.map (move 1 N) (targets st) }
      | 'm' <- toLower k                                           = st { center = move 1 M (center st) , focus = move 1 M (focus st) , targets = IntSet.map (move 1 M) (targets st) }

      -- target

      | 't' <- k , edit st , IntSet.member (focus st) (targets st) = st { targets = IntSet.delete (focus st) (targets st) }
      | 't' <- k , edit st                                         = st { targets = IntSet.insert (focus st) (targets st) }
      | 'T' <- k , edit st                                         = st { targets = mempty }

      -- manipulation

      | ']' <- k , edit st                                         = st { q_structure = forw (q_structure st) }
      | '[' <- k , edit st                                         = st { q_structure = back (q_structure st) }
      | '}' <- k , edit st                                         = st { q_material  = forw (q_material st) }
      | '{' <- k , edit st                                         = st { q_material  = back (q_material st) }

      | 'q' <- k , edit st                                         = st { ν = add q (zlevel st) (focus st) (ν st) }
      | 'x' <- k , edit st                                         = st { ν = del   (zlevel st) (focus st) (ν st) }
      | 'Q' <- k , edit st                                         = st { ν = foldr (.) id (add q (zlevel st) <$> IntSet.elems (targets st)) (ν st) }
      | 'X' <- k , edit st                                         = st { ν = foldr (.) id (del   (zlevel st) <$> IntSet.elems (targets st)) (ν st) }

      -- none

      | otherwise                                                  = st

      where

      q = Unit (Building (q_structure st) (q_material st)) L0 mempty

