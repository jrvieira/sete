module Verse.Unit where

import Verse.Verse
-- import Data.Map

-- | Elemental traits of units

{- Note: elements are *something like* the property

   (Air :: Element) is atmospheric pressure
   (Water :: Element) is wetness
   (Fire :: Element) is heat
   (Earth :: Element) is density
   (Aether :: Element) is electricity

-- 1) each atom updates its state from neighbours and the unit's properties
-- 2) each unit updates itself (not its status, units are stateless)
--    from its elemental atoms

-}

type Property = Element -> Unit -> Level -> Level

-- -- unit creates element up to L : productive, amplifier, +
-- inductive :: Property
-- inductive Fogo u
--    | Flame <- u = maxBound
-- inductive _ _ = minBound

-- unit allows element up to L : permeable, non blocking, =

conductivity :: Property
conductivity Eter u
   | Battery <- u = const maxBound
   | Wire    <- u = id
   | Lamp    <- u = (- 2)
conductivity Ar u
   | Void    <- u = id
   | Flame   <- u = id
conductivity Terra u
   | Void    <- u = const minBound
conductivity _ _  = const minBound

-- -- unit consumes element up to L : absorvent, resistive, -
-- abductive :: Property
-- abductive Fogo
--    | Water <- u = maxBound
-- abductive Agua
--    | Flame <- u = maxBound
-- abductive _ _ = minBound
--
-- type Category = Unit -> Bool
--
-- organic :: Category
-- organic Cat = True
-- organic _ = False
--
-- mechanic :: Category
-- mechanic Computer = True
-- mechanic _ = False
--
-- -- a unit's condition is uniquely derived from its elemental layers
--
-- data Trait = Trait Condition Bool
--
-- data Condition = Functional | A
--
-- trait :: Atom -> [Trait]
-- trait (u,es)
--    | mechanic u , es ! Eter = Functional True
--    | organic u = es ! Ar == 0 = Alive (es ! Ar /= 0)


{- Unit index -}
