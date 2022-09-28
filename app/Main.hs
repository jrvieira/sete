module Main where

import Zero.Zero hiding ( (#) )
import Verse.Conf
import Verse.Util
import Verse.Verse

import Terminal.Game
import System.Random hiding ( next )
import Data.List ( genericLength, partition )
import Data.IntMap ( fromList, toList, (!), adjust, elems )
import Data.Set ( Set, insert, delete )
import Data.Maybe ( fromJust )
import Data.Tuple
import Data.Bifunctor
import Control.Monad

main :: IO ()
main = do
   r <- randomA
   playGame $ Game {
      gTPS = 12 ,
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
   color' c i
      | Menu <- μ st = color Black Vivid
      | otherwise    = color c i

   each :: ((Int,Node) -> Draw) -> [Draw]
   each r = r <$> toList (ν st)

   tile :: (Int,Node) -> Draw
   tile (n,(a,ns))
      | selected                = c % cell (head $ show $ fromEnum $ α a) # color' Cyan Dull # bold
      | adjacent                = c % cell x # color' Cyan Vivid # bold
      | otherwise               = c % cell x # color' White Dull
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

   ui :: [Draw]
   ui = [
      (1,1) % focus ,
      (1,fw + 2) % layer ,
      (1,fw + lw + 3) % stat ,
      (1,fw + lw + sw + 4) % invi ,
      (2,1) % mode
      ]
      where

      focus = word (show $ indexToCoord $ φ st) # color' Cyan Dull
      (fw,_) = planeSize focus
      layer = word (show (λ st)) # color' White Dull
      (lw,_) = planeSize layer
      stat = word (show $ sum $ sal <$> elems (ν st)) # color' Yellow Dull
      (sw,_) = planeSize stat
      invi = word (show $ ι st) # color' Magenta Dull
      mode
         | Play  <- μ st = word (show $ σ st) # color' White Dull
         | Menu  <- μ st = word (show $ σ st) # color  Cyan  Dull
         | Pause <- μ st = word (unwords [show $ σ st,"p"]) # color  Black Vivid
         | otherwise     = word (unwords [show $ σ st,"?"]) # color' Black Vivid

catch :: GEnv -> State -> Event -> State
catch _ st Tick = step st
catch _ st (KeyPress k) = st' { ι = k }
   where

   fi = φ st
   (f,fis) = ν st ! fi
   t = τ st :: Set Int

   st'
      -- step
      | '.' <- k                  = (step st { μ = Play }) { μ = Pause }
      -- simulations menu
      | 'h' <- k , Menu   <- μ st = st { σ = back (σ st) }
      | 'j' <- k , Menu   <- μ st = st { σ = back (σ st) }
      | 'k' <- k , Menu   <- μ st = st { σ = forw (σ st) }
      | 'l' <- k , Menu   <- μ st = st { σ = forw (σ st) }
      | 'p' <- k , Menu   <- μ st = st { μ = Play }  -- exit menu and play
      | '\n'<- k , Menu   <- μ st = st { μ = Pause }  -- exit menu
      | 's' <- k , Menu   <- μ st = st { μ = Pause }  -- exit menu
      |  _  <- k , Menu   <- μ st = st  -- block other input while in menu
      -- open menu
      | 's' <- k                  = st { μ = Menu }
      -- randomise
      | 'r' <- k                  = let (r,r') = splitAt (size $ ν st) (ρ st) in st { ν = fromList $ zipWith (\(i,(_,ns)) a -> (i,(a,ns))) (toList $ ν st) r , ρ = r' }
      -- zero out
      | 'z' <- k                  = st { ν = (\(Atom _,ns) -> (Atom S0,ns)) <$> ν st }
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
      | 't' <- k , Atom {} <- f   = st { τ = insert fi mempty }
      | 'T' <- k , Atom {} <- f   = st { τ = if fi ∈ t then delete fi t else insert fi t }
      -- manipulation
      | '0' <- k                  = st { ν = sup (const S0) fi (ν st) }
      | '1' <- k                  = st { ν = sup (const S1) fi (ν st) }
      | '2' <- k                  = st { ν = sup (const S2) fi (ν st) }
      | '3' <- k                  = st { ν = sup (const S3) fi (ν st) }
      | '4' <- k                  = st { ν = sup (const S4) fi (ν st) }
      | '5' <- k                  = st { ν = sup (const S5) fi (ν st) }
      | '6' <- k                  = st { ν = sup (const S6) fi (ν st) }
      | '7' <- k                  = st { ν = sup (const S7) fi (ν st) }
      | '+' <- k                  = st { ν = sup next fi (ν st) }
      | '-' <- k                  = st { ν = sup prev fi (ν st) }
      -- layer
      | 'v' <- k                  = st { λ = forw (λ st) }
      | 'V' <- k                  = st { λ = back (λ st) }
      -- nothing
      | otherwise                 = st

step :: State -> State
step st
   | Play <- μ st = st { ν = run (σ st) <$> ν st }
   | otherwise = st
   where

   run :: Simulation -> Node -> Node

   run Terra a@(Atom s,ns)
      | leq > 2 = a
      | lgt < llt = (Atom $ prev s,ns)
      | otherwise = (Atom $ next s,ns)
      where
      leq = length $ filter (== sal a) nvs
      lgt = length $ filter (>  sal a) nvs
      llt = length $ filter (<  sal a) nvs
      nvs = sal . (ν st !) <$> ns
   -- avg = div (sum (sal . (ν st !) <$> ns)) (genericLength ns)

   run Dois a@(Atom s,ns)
      | length (filter (== minimum nvs) nvs) <= (sal a - minimum nvs) = (Atom $ prev s,ns)
      | sum (filter (> sal a) nvs) > 6 = (Atom $ next s,ns)
      | otherwise = a
      where
      nvs = sal . (ν st !) <$> ns

   run Bees a@(Atom s,ns)
      | length (filter (> sal a) nvs) ∈ [2] = (Atom $ next s,ns)
      | length (filter (> sal a) nvs) ∈ [2] = a
      | otherwise = (Atom $ prev s,ns)
      where
      nvs = sal . (ν st !) <$> ns

   run Fish a@(Atom s,ns)
      | length (filter (== 7) nvs) ∈ survive , sal a > (n-2) = (Atom S7,ns)
      | length (filter (== 7) nvs) ∈ born , sal a < 1 = (Atom S7,ns)
      | sal a <= (n-2) = (Atom $ prev s,ns)
      | otherwise = (Atom $ toEnum (n-2),ns)
      where
      survive = [2]
      born = [2]
      n = 3  -- number of states
      nvs = sal . (ν st !) <$> ns

   run Fish2 a@(Atom s,ns)
      | length (filter (== 7) nvs) ∈ survive , sal a > (n-2) = (Atom S7,ns)
      | length (filter (== 7) nvs) ∈ born , sal a < 1 = (Atom S7,ns)
      | sal a <= (n-2) = (Atom $ prev s,ns)
      | otherwise = (Atom $ toEnum (n-2),ns)
      where
      survive = [2,4]
      born = [2,4]
      n = 3  -- number of states
      nvs = sal . (ν st !) <$> ns

   run Glider a@(Atom s,ns)
      | length (filter (== 7) nvs) ∈ survive , sal a > (n-2) = (Atom S7,ns)
      | length (filter (== 7) nvs) ∈ born , sal a < 1 = (Atom S7,ns)
      | sal a <= (n-2) = (Atom $ prev s,ns)
      | otherwise = (Atom $ toEnum (n-2),ns)
      where
      survive = [1,3,4]
      born = [2,3,4,5]
      n = 5  -- number of states
      nvs = sal . (ν st !) <$> ns

