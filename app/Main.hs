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

randomise :: Verse -> IO Verse
randomise v = do
   gen <- initStdGen
   pure $ fromList $ zipWith (\(i,(_,ns)) a -> (i,(a,ns))) (toList v) (Atom . toEnum <$> randomRs (0,7) gen)

-- adjustWithKey :: (Key -> a -> a) -> Key -> IntMap a -> IntMap a

main :: IO ()
main = do
   v <- randomise verse
   playGame $ Game {
      gTPS = 30 ,
      gInitState = state { ν = v } ,
      gLogicFunction = catch ,
      gDrawFunction = render ,
      gQuitFunction = const False }

state :: State
state = State {
   ν = verse ,
   λ = Test ,
   σ = Terra ,
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
      | n == φ st , Menu <- μ st = c % cell x # color' White Dull
      | n == φ st                = c % cell (head $ show $ fromEnum $ α a) # color' Cyan Dull # bold
      | n ∈ fis                  = c % cell x # color' Cyan Vivid # bold
      | otherwise                = c % cell x # color' White Dull
      where
      selected :: Bool = φ st == n
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
      (1,fx + 2) % layer ,
      (1,fx + lx + 3) % stat ,
      (2,1) % mode
      ]
      where
      focus = word (show $ indexToCoord $ φ st) # color' Cyan Dull
      (fx,_) = planeSize focus
      layer = word (show (λ st)) # color' White Dull
      (lx,_) = planeSize layer
      stat = word (show $ sum $ sal <$> elems (ν st)) # color' Yellow Dull
      mode
         | Play  <- μ st = word (show $ σ st) # color' White Dull
         | Menu  <- μ st = word (show $ σ st) # color  Cyan  Dull
         | Pause <- μ st = word (unwords [show $ σ st,"p"]) # color  Black Vivid
         | otherwise     = word (unwords [show $ σ st,"?"]) # color' Black Vivid

catch :: GEnv -> State -> Event -> State
catch _ st Tick = step st
catch _ st (KeyPress k)
   -- simulations menu
   | 'h' <- k , Menu   <- μ st = st { σ = forw (σ st) }
   | 'j' <- k , Menu   <- μ st = st { σ = back (σ st) }
   | 'k' <- k , Menu   <- μ st = st { σ = forw (σ st) }
   | 'l' <- k , Menu   <- μ st = st { σ = back (σ st) }
   | 'p' <- k , Menu   <- μ st = st { μ = Play }  -- exit menu and play
   | 's' <- k , Menu   <- μ st = st { μ = Pause }  -- exit menu
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
   | otherwise = st
   where
   fi = φ st
   (f,fis) = ν st ! fi
   t = τ st :: Set Int

step :: State -> State
step st
   | Play <- μ st , Terra <- σ st = st { ν = terra <$> ν st }
   | Play <- μ st , Dois  <- σ st = st { ν = dois  <$> ν st }
   | otherwise = st
   where

   terra :: Node -> Node
   terra a@(Atom s,ns)
      | leq > 2 = a
      | lgt < llt = (Atom $ prev s,ns)
      | otherwise = (Atom $ next s,ns)
      where
      leq = length $ filter (== sal a) nvs
      lgt = length $ filter (>  sal a) nvs
      llt = length $ filter (<  sal a) nvs
      nvs = sal . (ν st !) <$> ns
   -- avg = div (sum (sal . (ν st !) <$> ns)) (genericLength ns)

   dois :: Node -> Node
   dois a@(Atom s,ns)
      | length (filter (== minimum nvs) nvs) <= (sal a - minimum nvs) = (Atom $ prev s,ns)
      | sum (filter (> sal a) nvs) > 6 = (Atom $ next s,ns)
      | otherwise = a
      where
      nvs = sal . (ν st !) <$> ns

