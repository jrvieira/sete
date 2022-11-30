module Main where

import Verse.Conf
import Verse.Verse
import Verse.Sim
import Verse.Art

import Zero.Zero

import Terminal.Game
import System.Random ( randomRs, initStdGen )
import Data.IntSet qualified as IntSet ( empty, member, insert, delete )
import Data.IntMap qualified as IntMap ( keys )

main :: IO ()
main = do
   r <- randomRs (minBound,maxBound) <$> initStdGen
   playGame $ Game {
      gTPS = 19 ,
      gInitState = state { ρ = r } ,
      gLogicFunction = logic ,
      gDrawFunction = draw ,
      gQuitFunction = quit }

-- | Quit condition

quit :: State -> Bool
quit = const False

-- | Render the universe

draw :: GEnv -> State -> Plane
draw _ = foldl (&) canvas . art
   where
   canvas :: Plane
   canvas = blankPlane (2 * succ (2 * radius) + 2 * 2 * marginX) (succ (2 * radius) + 2 * marginY)

-- | One tick

step :: State -> State
step st
   | Play <- μ st = st { ν = sim st <$> ν st }
   | otherwise = st

-- | Catch user input

logic :: GEnv -> State -> Event -> State
logic _ st Tick = step st
logic _ st (KeyPress k) = st' { ι = k }
   where

   st'
      -- randomise

      | 'r' <- k                       = let (r,r') = splitAt (size (ν st)) (ρ st) in st { ν = nap (ε st) r (ν st) , ρ = r' }
      | 'R' <- k                       = let (rs,r') = packs (replicate (length (total :: [Level])) (size (ν st))) (ρ st) in st { ν = foldr ($) (ν st) (zipWith nap total rs) , ρ = r' }

      -- step

      | 'P' <- k                       = (step st { μ = Play }) { μ = Pause }

--    -- simulations menu

--    | 'h' <- k , Menu  <- μ st       = st { σ = back (σ st) }
--    | 'j' <- k , Menu  <- μ st       = st { σ = back (σ st) }
--    | 'k' <- k , Menu  <- μ st       = st { σ = forw (σ st) }
--    | 'l' <- k , Menu  <- μ st       = st { σ = forw (σ st) }
--    | 'p' <- k , Menu  <- μ st       = st { μ = Play }
--    | '\n'<- k , Menu  <- μ st       = st { μ = Pause }  -- close menu
--    | 'S' <- k , Menu  <- μ st       = st { μ = Pause }  -- close menu
--    |  _  <- k , Menu  <- μ st       = st  -- block other input while in menu

--    -- open menu

--    | 's' <- k                       = st { μ = Menu }

      -- pause

      | 'p' <- k , Pause <- μ st       = st { μ = Play }
      | 'p' <- k , Play  <- μ st       = st { μ = Pause }

      -- movement

      | 'h' <- k                       = st { φ = move 1 H (φ st) }
      | 'j' <- k                       = st { φ = move 1 N (φ st) }
      | 'k' <- k                       = st { φ = move 1 I (φ st) }
      | 'l' <- k                       = st { φ = move 1 L (φ st) }
      | 'u' <- k                       = st { φ = move 1 U (φ st) }
      | 'i' <- k                       = st { φ = move 1 I (φ st) }
      | 'n' <- k                       = st { φ = move 1 N (φ st) }
      | 'm' <- k                       = st { φ = move 1 M (φ st) }
      | 'c' <- k                       = st { φ = κ st }  -- focus on center

      -- scroll

      | 'C' <- k                       = st { κ = φ st }  -- center on focus
      | 'H' <- k                       = st { κ = move 1 H (κ st) , φ = move 1 H (φ st) }
      | 'J' <- k                       = st { κ = move 1 N (κ st) , φ = move 1 N (φ st) }
      | 'K' <- k                       = st { κ = move 1 I (κ st) , φ = move 1 I (φ st) }
      | 'L' <- k                       = st { κ = move 1 L (κ st) , φ = move 1 L (φ st) }
      | 'U' <- k                       = st { κ = move 1 U (κ st) , φ = move 1 U (φ st) }
      | 'I' <- k                       = st { κ = move 1 I (κ st) , φ = move 1 I (φ st) }
      | 'N' <- k                       = st { κ = move 1 N (κ st) , φ = move 1 N (φ st) }
      | 'M' <- k                       = st { κ = move 1 M (κ st) , φ = move 1 M (φ st) }

      -- target

      | 'T' <- k                       = st { τ = IntSet.empty }
      | 't' <- k                       = st { τ = if IntSet.member (φ st) (τ st) then IntSet.delete (φ st) (τ st) else IntSet.insert (φ st) (τ st) }

      -- manipulation

      | ')' <- k                       = st { ν = up (ε st) (const 0) <$> ν st }
      | '!' <- k                       = st { ν = up (ε st) (const 1) <$> ν st }
      | '@' <- k                       = st { ν = up (ε st) (const 2) <$> ν st }
      | '#' <- k                       = st { ν = up (ε st) (const 3) <$> ν st }
      | '$' <- k                       = st { ν = up (ε st) (const 4) <$> ν st }
      | '%' <- k                       = st { ν = up (ε st) (const 5) <$> ν st }
      | '^' <- k                       = st { ν = up (ε st) (const 6) <$> ν st }
      | '&' <- k                       = st { ν = up (ε st) (const 7) <$> ν st }

      | '0' <- k                       = st { ν = nup (ε st) (const 0) (φ st) (ν st) }
      | '1' <- k                       = st { ν = nup (ε st) (const 1) (φ st) (ν st) }
      | '2' <- k                       = st { ν = nup (ε st) (const 2) (φ st) (ν st) }
      | '3' <- k                       = st { ν = nup (ε st) (const 3) (φ st) (ν st) }
      | '4' <- k                       = st { ν = nup (ε st) (const 4) (φ st) (ν st) }
      | '5' <- k                       = st { ν = nup (ε st) (const 5) (φ st) (ν st) }
      | '6' <- k                       = st { ν = nup (ε st) (const 6) (φ st) (ν st) }
      | '7' <- k                       = st { ν = nup (ε st) (const 7) (φ st) (ν st) }
      | '=' <- k                       = st { ν = nup (ε st) next (φ st) (ν st) }
      | '-' <- k                       = st { ν = nup (ε st) prev (φ st) (ν st) }
      | '+' <- k                       = st { ν = foldr ($) (ν st) $ nup (ε st) next <$> IntMap.keys (ν st) }
      | '_' <- k                       = st { ν = foldr ($) (ν st) $ nup (ε st) prev <$> IntMap.keys (ν st) }

      -- change layer

      | 'v' <- k                       = st { λ = forw (λ st) }
      | 'V' <- k                       = st { λ = back (λ st) }
      | '>' <- k                       = st { λ = forw (λ st) }
      | '<' <- k                       = st { λ = back (λ st) }
      | ' ' <- k                       = st { λ = Superficial }
      | 'e' <- k                       = st { λ = Elemental }
      | 'x' <- k                       = st { λ = Schematic }

      -- change sublayer

      | ',' <- k , Superficial <- λ st = st { ο = back (ο st) }
      | '.' <- k , Superficial <- λ st = st { ο = forw (ο st) }

      | ',' <- k , Elemental   <- λ st = st { ε = back (ε st) }
      | '.' <- k , Elemental   <- λ st = st { ε = forw (ε st) }

      -- place unit

      | 'q' <- k , Superficial <- λ st = st { ν = qup (ο st) (φ st) (ν st) }
      | 'Q' <- k , Superficial <- λ st = st { ν = foldr ($) (ν st) $ qup (ο st) <$> IntMap.keys (ν st) }

      -- nothing

      | otherwise                      = st

