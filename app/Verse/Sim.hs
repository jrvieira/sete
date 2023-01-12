module Verse.Sim where

-- import Zero.Zero
import Verse.Verse
import Verse.Unit
import Data.IntMap qualified as IntMap ( (!) )
import Data.Map.Strict qualified as Map ( (!), mapWithKey )

-- | A step in simulation

sim :: State -> Node -> Node
sim st (a,ns) = ob (a { ες = Map.mapWithKey el (ες a) } , ns)
   where
   el :: Element -> Level -> Level
   el e = go . op
      where

      -- | Factor object properties

      op :: Level -> Level
      op = conductivity e (υ a)

      -- | Elemental behaviour

      go
         | Volt  <- e = noise
         | O2    <- e = noise
         | H2O   <- e = noise
         | Heat  <- e = noise
         | Food  <- e = noise
         | _     <- e = noise

      -- | Get atomic level by index

      lev :: Int -> Level
      lev i = es Map.! e
         where
         (Atom _ es,_) = ν st IntMap.! i

      -- | get the (sum ns) index of ρ for a pseudorandom level

      noise :: Level -> Level
      noise _ = toEnum $ mod (sum $ fromEnum . lev <$> ns) (fromEnum (maxBound :: Level))

   -- | Update surface object, depending on current state and Atom properties

   ob :: Node -> Node
   ob = id

