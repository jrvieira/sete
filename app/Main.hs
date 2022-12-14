module Main where

import Zero.Zero hiding ( (#) )
import Verse.Conf
import Verse.Types
import Verse.Verse
import Verse.Sim

import Terminal.Game
import System.Random ( randomRs, initStdGen )
import Data.IntMap ( toList, elems )
import Data.Set ( insert, delete )
import Data.Tuple ( swap )
import Data.Bifunctor

main :: IO ()
main = do
   r <- randomA
   playGame $ Game {
      gTPS = 15 ,
      gInitState = state { ρ = r } ,
      gLogicFunction = catch ,
      gDrawFunction = render ,
      gQuitFunction = const False }
   where

-- randomise :: Verse -> IO Verse
-- randomise v = do
--    gen <- initStdGen
--    pure $ fromList $ zipWith (\(i,(_,ns)) a -> (i,(a,ns))) (toList v) (Atom . toEnum <$> randomRs (0,7) gen)

   randomA :: IO [Atom]
   randomA = do
      gen <- initStdGen
      pure $ Atom . toEnum <$> randomRs (0,7) gen

state :: State
state = State {
   ν = verse ,
   λ = Test ,
   σ = Terra ,
   ρ = [] ,
   ι = ' ' ,
   φ = coordToIndex (div width 2,div height 2) ,
   τ = mempty ,
   μ = Pause }

render :: GEnv -> State -> Plane
render _ st = foldl (&) (blankPlane (2 * width + marginY) (height + (2 * marginX))) $ concat [
   each tile ,
   ui ]
   where

   fi = φ st
   (f,fis) = ν st ! fi
   k :: Draw -> Draw
   k c
      | Menu <- μ st = color Black Vivid
      | otherwise    = c

   each :: ((Int,Node) -> Draw) -> [Draw]
   each r = r <$> toList (ν st)

   tile :: (Int,Node) -> Draw
   tile (n,(a,ns))
      | Menu <- μ st        = c % cell x # paletteColor (xterm24LevelGray $ 2 + 2 * fromEnum a)
      | selected , targeted = c % cell s # color Red   Dull
      | adjacent , targeted = c % cell x # color Red   Dull
      |            targeted = c % cell x # color Red   Dull
      | selected            = c % cell s # color Cyan  Dull
      | adjacent            = c % cell x # color Cyan  Dull
      | Pause <- μ st       = c % cell x # paletteColor (xterm24LevelGray $ 2 + 2 * fromEnum a)
      | Atom s <- a         = c % cell x # stone s
      | otherwise           = c % cell x # color White Dull
      where

      selected :: Bool = n == φ st
      adjacent :: Bool = n ∈ fis
      targeted :: Bool = n ∈ τ st
      -- translate square to exagonal
      c = let (y,x) = c' in if even y then (y,succ x) else (y,x)
      -- translate to library coordinate system (one based (y,x))
      c' = bimap ((+ marginY) . succ) ((+ (2 * marginX)) . succ . (* 2)) . swap $ indexToCoord n
      -- cell
      x = pixel (α a)
      -- Some value
      s = head $ show $ fromEnum $ α a

   ui :: [Draw]
   ui = [
      (1,1) % focus ,
      (1,fw + 2) % layer ,
      (1,fw + lw + 3) % stat ,
      (1,fw + lw + sw + 4) % invi ,
      (2,1) % mode
      ]
      where

      focus = word (show $ indexToCoord $ φ st) # k (color Cyan Dull)
      layer = word (show (λ st)) # k (color White Dull)
      stat = word (show $ sum $ sal <$> elems (ν st)) # k (color Yellow Dull)
      invi = word (show $ ι st) # k (color Magenta Dull)
      (fw,_) = planeSize focus
      (lw,_) = planeSize layer
      (sw,_) = planeSize stat

      mode :: Plane
      mode
         | Play  <- μ st = word (show $ σ st) # k (stone S4)
         | Menu  <- μ st = word (show $ σ st) # color  Cyan  Dull
         | Pause <- μ st = word (unwords [show $ σ st,"p"]) # color  Black Vivid
         | otherwise     = word (unwords [show $ σ st,"?"]) # k (color Black Vivid)

catch :: GEnv -> State -> Event -> State
catch _ st Tick = step st
catch _ st (KeyPress k) = st' { ι = k }
   where

   fi = φ st
   (f,fis) = ν st ! fi

   st'
      -- zero out
      | 'z' <- k                  = st { ν = (\(Atom _,ns) -> (Atom S0,ns)) <$> ν st }
      -- randomise
      | 'r' <- k                  = let (r,r') = splitAt (size $ ν st) (ρ st) in st { ν = fromList $ zipWith (\(i,(_,ns)) a -> (i,(a,ns))) (toList $ ν st) r , ρ = r' }
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
      -- target
      | 'T' <- k , Atom {} <- f   = st { τ = insert fi mempty }
      | 't' <- k , Atom {} <- f   = st { τ = if fi ∈ τ st then delete fi (τ st) else insert fi (τ st) }
      -- manipulation
      | '0' <- k                  = st { ν = sup (const S0) fi (ν st) }
      | '1' <- k                  = st { ν = sup (const S1) fi (ν st) }
      | '2' <- k                  = st { ν = sup (const S2) fi (ν st) }
      | '3' <- k                  = st { ν = sup (const S3) fi (ν st) }
      | '4' <- k                  = st { ν = sup (const S4) fi (ν st) }
      | '5' <- k                  = st { ν = sup (const S5) fi (ν st) }
      | '6' <- k                  = st { ν = sup (const S6) fi (ν st) }
      | '7' <- k                  = st { ν = sup (const S7) fi (ν st) }
      | '=' <- k                  = st { ν = sup next fi (ν st) }
      | '+' <- k                  = st { ν = sup next fi (ν st) }
      | '-' <- k                  = st { ν = sup prev fi (ν st) }
      -- layer
      | 'v' <- k                  = st { λ = forw (λ st) }
      | 'V' <- k                  = st { λ = back (λ st) }
      -- nothing
      | otherwise                 = st

step :: State -> State
step st
   | Play <- μ st = st { ν = sim st <$> ν st }
   | otherwise = st

