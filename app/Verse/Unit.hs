module Verse.Unit where

-- import Verse.Verse
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

-- type Property = Element -> Unit -> Level
--
-- -- unit creates element up to L
-- -- : productive, +
-- inductivity :: Property
-- inductivity Fogo u
--    | Flame <- u = maxBound
-- inductivity _ _ = minBound
--
-- -- unit consumes element up to L
-- -- : absorvent, -
-- abductivity :: Property
-- abductivity Fogo
--    | Water <- u = maxBound
-- abductivity Agua
--    | Flame <- u = maxBound
-- abductivity _ _ = minBound
--
-- -- unit allows element up to L
-- -- : permeable, non blocking, =
-- conductivity :: Property
-- conductivity Ar u
--    | Void  <- u = maxBound
--    | Wall  <- u = minBound
--    | Wall  <- u = minBound
-- conductivity _ _ = minBound
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
