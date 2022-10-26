module Main where

import Zero.Zero hiding ( (#) )
import Verse.Conf
import Verse.Types
import Verse.Verse
import Verse.Sim

import Terminal.Game
import System.Random ( randomRs, initStdGen )
import Data.Bifunctor ( bimap )
import Control.Monad ( join )

main :: IO ()
main = do
   r <- randomRs (S0,S7) <$> initStdGen
   playGame $ Game {
      gTPS = 16 ,
      gInitState = state { ρ = r } ,
      gLogicFunction = catch ,
      gDrawFunction = render ,
      gQuitFunction = const False }

render :: GEnv -> State -> Plane
render _ st = foldl (&) (blankPlane (2 * succ (2 * radius) + 2 * 2 * marginX) (succ (2 * radius) + 2 * marginY)) $ concat [
   cells ,
   ui ]
   where

   fi = φ st
   (f,fis) = ν st ! fi

   -- get cell from any coord relative to origin
   cells :: [Draw]
   cells = map go hexagon
      where
      hexagon = [ (x,y) | x <- [-radius..radius] , y <- [-radius..radius] , abs (x + y) <= radius ]
      go (x,y)
         | Menu <- μ st        = c %.< cell p # paletteColor (xterm24LevelGray $ 2 + 2 * fromEnum a)
         | selected , targeted = c %.< cell s # color Red   Dull
         | adjacent , targeted = c %.< cell p # color Red   Dull
         |            targeted = c %.< cell p # color Red   Dull
         | selected            = c %.< cell s # color Cyan  Dull
         | adjacent            = c %.< cell p # color Cyan  Dull
         | Pause <- μ st       = c %.< cell p # paletteColor (xterm24LevelGray $ 2 + 2 * fromEnum a)
         | Atom {} <- a        = c %.< cell p # stone a
      -- | otherwise           = c %.< cell '?' # color White Dull
         where
         -- stretch, tilt, margin, translate to library coordinate system (1-based (y,x))
         c = join bimap succ (y + radius + marginY , 2 * (x + radius + marginX) + y)
         -- Some value
         s = head . show $ sal (a,ns)
         p = pixel a
         (a,ns) = ν st ! n
         -- get index of node taking scroll into account
         n = move mx L . move my I $ coordToIndex (mod (x - div y height * radius) width , mod y height)
            where
            (mx,my) = indexToCoord (κ st)
         selected :: Bool = n == fi
         adjacent :: Bool = n ∈ fis
         targeted :: Bool = n ∈ τ st

   ui :: [Draw]
   ui = [
      (1,1) % focus ,
      (1,fw + 2) % layer ,
      (1,fw + lw + 3) % stat ,
      (1,fw + lw + sw + 4) % invi ,
      (2,1) % mode ]
      where

      focus = word (show $ indexToCoord $ φ st) # k (color Cyan Dull)
      layer = word (show (λ st)) # k (color White Dull)
      stat = word (show $ sum $ sal <$> elems (ν st)) # k (color Yellow Dull)
      invi = word (show $ ι st) # k (color Magenta Dull)
      (fw,_) = planeSize focus
      (lw,_) = planeSize layer
      (sw,_) = planeSize stat

      -- inactive color
      k :: Draw -> Draw
      k c
         | Menu <- μ st = color Black Vivid
         | otherwise    = c

      mode :: Plane
      mode
         | Play  <- μ st = word (show $ σ st) # k (paletteColor $ xterm6LevelRGB 3 1 4)
         | Menu  <- μ st = word (show $ σ st) # color  Cyan  Dull
         | Pause <- μ st = word (unwords [show $ σ st,"p"]) # color  Black Vivid
      -- | otherwise     = word (unwords [show $ σ st,"?"]) # k (color Black Vivid)

catch :: GEnv -> State -> Event -> State
catch _ st Tick = step st
catch _ st (KeyPress k) = st' { ι = k }
   where

   fi = φ st
   (f,fis) = ν st ! fi

   st'
      -- zero out
      | 'z' <- k                  = st { ν = sup (const S0) <$> ν st }
      | 'f' <- k                  = st { ν = sup (const S7) <$> ν st }
      -- randomise
      | 'r' <- k                  = let (r,r') = splitAt (size $ ν st) (ρ st) in st { ν = fromList $ zipWith (\(i,(a,ns)) s -> (i,(a { α = s },ns))) (toList $ ν st) r , ρ = r' }
      -- step
      | '.' <- k                  = (step st { μ = Play }) { μ = Pause }
      -- simulations menu
      | 'h' <- k , Menu   <- μ st = st { σ = back (σ st) }
      | 'j' <- k , Menu   <- μ st = st { σ = back (σ st) }
      | 'k' <- k , Menu   <- μ st = st { σ = forw (σ st) }
      | 'l' <- k , Menu   <- μ st = st { σ = forw (σ st) }
      | 'p' <- k , Menu   <- μ st = st { μ = Play }
      | '\n'<- k , Menu   <- μ st = st { μ = Pause }  -- close menu
      | 's' <- k , Menu   <- μ st = st { μ = Pause }  -- close menu
      |  _  <- k , Menu   <- μ st = st  -- block other input while in menu
      -- open menu
      | 's' <- k                  = st { μ = Menu }
      -- pause
      | 'p' <- k , Pause  <- μ st = st { μ = Play }
      | 'p' <- k , Play   <- μ st = st { μ = Pause }
      -- movement
      | 'h' <- k                  = st { φ = move 1 H fi }
      | 'j' <- k                  = st { φ = move 1 N fi }
      | 'k' <- k                  = st { φ = move 1 I fi }
      | 'l' <- k                  = st { φ = move 1 L fi }
      | 'u' <- k                  = st { φ = move 1 U fi }
      | 'i' <- k                  = st { φ = move 1 I fi }
      | 'n' <- k                  = st { φ = move 1 N fi }
      | 'm' <- k                  = st { φ = move 1 M fi }
      | 'c' <- k                  = st { φ = κ st }  -- focus on center
      -- scroll
      | 'C' <- k                  = st { κ = φ st }  -- center on focus
      | 'H' <- k                  = st { κ = move (negate 1) H (κ st) }
      | 'J' <- k                  = st { κ = move (negate 1) N (κ st) }
      | 'K' <- k                  = st { κ = move (negate 1) I (κ st) }
      | 'L' <- k                  = st { κ = move (negate 1) L (κ st) }
      | 'U' <- k                  = st { κ = move (negate 1) U (κ st) }
      | 'I' <- k                  = st { κ = move (negate 1) I (κ st) }
      | 'N' <- k                  = st { κ = move (negate 1) N (κ st) }
      | 'M' <- k                  = st { κ = move (negate 1) M (κ st) }
      -- target
      | 'T' <- k , Atom {} <- f   = st { τ = insert fi mempty }
      | 't' <- k , Atom {} <- f   = st { τ = if fi ∈ τ st then delete fi (τ st) else insert fi (τ st) }
      -- manipulation
      | '0' <- k                  = st { ν = vup (const S0) fi (ν st) }
      | '1' <- k                  = st { ν = vup (const S1) fi (ν st) }
      | '2' <- k                  = st { ν = vup (const S2) fi (ν st) }
      | '3' <- k                  = st { ν = vup (const S3) fi (ν st) }
      | '4' <- k                  = st { ν = vup (const S4) fi (ν st) }
      | '5' <- k                  = st { ν = vup (const S5) fi (ν st) }
      | '6' <- k                  = st { ν = vup (const S6) fi (ν st) }
      | '7' <- k                  = st { ν = vup (const S7) fi (ν st) }
      | '=' <- k                  = st { ν = vup next fi (ν st) }
      | '+' <- k                  = st { ν = vup next fi (ν st) }
      | '-' <- k                  = st { ν = vup prev fi (ν st) }
      -- layer
      | 'v' <- k                  = st { λ = forw (λ st) }
      | 'V' <- k                  = st { λ = back (λ st) }
      -- nothing
      | otherwise                 = st

step :: State -> State
step st
   | Play <- μ st = st { ν = sim st <$> ν st }
   | otherwise = st

