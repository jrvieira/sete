module Main where

import Zero.Zero hiding ( (#) )
import Verse.Conf
import Verse.Verse
import Verse.Sim
import Verse.Art

import Terminal.Game
import System.Random ( randomRs, initStdGen )
import Data.List ( intersperse )
import Data.Set qualified as Set ( insert, delete )

main :: IO ()
main = do
   r <- randomRs (minBound,maxBound) <$> initStdGen
   playGame $ Game {
      gTPS = 16 ,
      gInitState = state { ρ = r } ,
      gLogicFunction = catch ,
      gDrawFunction = render ,
      gQuitFunction = const False }

render :: GEnv -> State -> Plane
render _ st = foldl (&) (blankPlane (2 * succ (2 * radius) + 2 * 2 * marginX) (succ (2 * radius) + 2 * marginY)) $ concat [
   map (pixel st) hexagon ,
   ui ]
   where

   hexagon = [ (x,y) | x <- [-radius..radius] , y <- [-radius..radius] , abs (x + y) <= radius ]

   fi = φ st
   (f,fis) = get (ν st) fi

   ui :: [Draw]
   ui = [
      (1,1) % (hcat $ intersperse (cell ' ') [focus,layer,stat,invi]) ,
      (2,1) % mode ,
      (marginY,1) %.> elements ]
      where

      focus = word ((show $ υ f) <> " " <> (show $ indexToCoord fi)) # k (color Cyan Dull)
      layer = word (show (λ st)) # k (color White Dull)
      stat = word (show $ sum $ val (ν st) <$> take (width * height) [0..]) # k (color Yellow Dull)
      invi = word (show $ ι st) # k (color Magenta Dull)

      -- inactive color
      k :: Draw -> Draw
      k c
         | Menu <- μ st = color Black Vivid
         | otherwise    = c

      mode :: Plane
      mode
         | Play  <- μ st = word (unwords [show $ σ st    ]) # k (paletteColor $ xterm6LevelRGB 3 1 4)
         | Menu  <- μ st = word (unwords [show $ σ st    ]) # color Cyan Dull
         | Pause <- μ st = word (unwords [show $ σ st,"p"]) # color Black Vivid
      -- | otherwise     = word (unwords [show $ σ st,"?"]) # k (color Black Vivid)

      elements :: Plane
      elements
         | λ st ∈ [Superficial,Elemental] = vcat $ bar <$> total
         | otherwise                      = word ""
         where

         bar :: Element -> Plane
         bar e = hcat $ intersperse (cell ' ') [
            level # k c ,
            point # k id ]
            where

            point
               | Superficial <- λ st            = cell symbol # color Black Vivid
               | Elemental   <- λ st , selected = cell symbol # elementColor e
               | Elemental   <- λ st            = cell symbol # color Black Vivid
               | otherwise                      = cell ' '

            level = word $ take (pred $ length (total :: [Some])) $ replicate (fromEnum s) '~' <> repeat ' '

            c
               | Superficial <- λ st            = elementColor e
               | Elemental   <- λ st , selected = elementColor e
               | otherwise                      = color Black Vivid

            symbol = head $ show e
            s = gel f e
            selected = e == ε st


catch :: GEnv -> State -> Event -> State
catch _ st Tick = step st
catch _ st (KeyPress k) = st' { ι = k }
   where

   fi = φ st
   (f,fis) = get (ν st) fi

   m = μ st
   l = λ st
   t = τ st
   e = ε st
   o = ο st
   v = ν st

   -- layer aware node update
   lup :: (Some -> Some) -> Node -> Node
   lup f n@(a,ns)
      | Superficial <- λ st = sup f n
      | Elemental   <- λ st = (gup e f a , ns)
      | otherwise           = n

   -- layer aware specific node update in verse
   lvp :: (Some -> Some) -> Int -> Verse -> Verse
   lvp
      | Superficial <- λ st = vup
      | Elemental   <- λ st = eup e
      | otherwise           = \_ _ -> id

   -- update state

   st'
      -- randomise
      | 'r' <- k                    = let (r,r') = splitAt (size $ ν st) (ρ st) in st { ν = foldr ($) v $ zipWith (lvp . const) r [0..] , ρ = r' }
   -- | 'r' <- k                    = let (r,r') = splitAt (size $ ν st) (ρ st) in st { ν = verse (atom (ο st) <$> r) , ρ = r' }
      -- step
      | 'P' <- k                    = (step st { μ = Play }) { μ = Pause }
      -- simulations menu
      | 'h' <- k , Menu  <- m       = st { σ = back (σ st) }
      | 'j' <- k , Menu  <- m       = st { σ = back (σ st) }
      | 'k' <- k , Menu  <- m       = st { σ = forw (σ st) }
      | 'l' <- k , Menu  <- m       = st { σ = forw (σ st) }
      | 'p' <- k , Menu  <- m       = st { μ = Play }
      | '\n'<- k , Menu  <- m       = st { μ = Pause }  -- close menu
      | 'S' <- k , Menu  <- m       = st { μ = Pause }  -- close menu
      |  _  <- k , Menu  <- m       = st  -- block other input while in menu
      -- open menu
      | 's' <- k                    = st { μ = Menu }
      -- pause
      | 'p' <- k , Pause <- m       = st { μ = Play }
      | 'p' <- k , Play  <- m       = st { μ = Pause }
      -- movement
      | 'h' <- k                    = st { φ = move 1 H fi }
      | 'j' <- k                    = st { φ = move 1 N fi }
      | 'k' <- k                    = st { φ = move 1 I fi }
      | 'l' <- k                    = st { φ = move 1 L fi }
      | 'u' <- k                    = st { φ = move 1 U fi }
      | 'i' <- k                    = st { φ = move 1 I fi }
      | 'n' <- k                    = st { φ = move 1 N fi }
      | 'm' <- k                    = st { φ = move 1 M fi }
      | 'c' <- k                    = st { φ = κ st }  -- focus on center
      -- scroll
      | 'C' <- k                    = st { κ = φ st }  -- center on focus
      | 'H' <- k                    = st { κ = move 1 H (κ st) , φ = move 1 H fi }
      | 'J' <- k                    = st { κ = move 1 N (κ st) , φ = move 1 N fi }
      | 'K' <- k                    = st { κ = move 1 I (κ st) , φ = move 1 I fi }
      | 'L' <- k                    = st { κ = move 1 L (κ st) , φ = move 1 L fi }
      | 'U' <- k                    = st { κ = move 1 U (κ st) , φ = move 1 U fi }
      | 'I' <- k                    = st { κ = move 1 I (κ st) , φ = move 1 I fi }
      | 'N' <- k                    = st { κ = move 1 N (κ st) , φ = move 1 N fi }
      | 'M' <- k                    = st { κ = move 1 M (κ st) , φ = move 1 M fi }
      -- target
      | 'T' <- k , Atom {} <- f     = st { τ = mempty }
      | 't' <- k , Atom {} <- f     = st { τ = if fi ∈ t then Set.delete fi t else Set.insert fi t }

      -- manipulation

      | ')' <- k                    = st { ν = lup (const S0) <$> v }
      | '!' <- k                    = st { ν = lup (const S1) <$> v }
      | '@' <- k                    = st { ν = lup (const S2) <$> v }
      | '#' <- k                    = st { ν = lup (const S3) <$> v }
      | '$' <- k                    = st { ν = lup (const S4) <$> v }
      | '%' <- k                    = st { ν = lup (const S5) <$> v }
      | '^' <- k                    = st { ν = lup (const S6) <$> v }
      | '&' <- k                    = st { ν = lup (const S7) <$> v }

      | '0' <- k                    = st { ν = lvp (const S0) fi v }
      | '1' <- k                    = st { ν = lvp (const S1) fi v }
      | '2' <- k                    = st { ν = lvp (const S2) fi v }
      | '3' <- k                    = st { ν = lvp (const S3) fi v }
      | '4' <- k                    = st { ν = lvp (const S4) fi v }
      | '5' <- k                    = st { ν = lvp (const S5) fi v }
      | '6' <- k                    = st { ν = lvp (const S6) fi v }
      | '7' <- k                    = st { ν = lvp (const S7) fi v }
      | '=' <- k                    = st { ν = lvp next fi v }
      | '+' <- k                    = st { ν = lvp next fi v }
      | '-' <- k                    = st { ν = lvp prev fi v }

      -- change layer
      | 'v' <- k                    = st { λ = forw l }
      | 'V' <- k                    = st { λ = back l }
      -- change layer
      | '<' <- k                    = st { λ = forw l }
      | '>' <- k                    = st { λ = back l }
      | ' ' <- k                    = st { λ = Superficial }
      | 'a' <- k                    = st { λ = Atomic }
      | 'e' <- k                    = st { λ = Elemental }
      | 'x' <- k                    = st { λ = Schematic }
      -- change sublayer
      | '.' <- k , Superficial <- l = st { ο = back o }
      | ',' <- k , Superficial <- l = st { ο = forw o }
      | '.' <- k , Elemental <- l   = st { ε = back e }
      | ',' <- k , Elemental <- l   = st { ε = forw e }
      -- nothing
      | otherwise                   = st

step :: State -> State
step st
   | Play <- μ st = st { ν = sim st <$> ν st }
   | otherwise = st

