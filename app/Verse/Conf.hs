module Verse.Conf where

import Data.Bifunctor ( bimap )
import Control.Monad ( join )

-- testing

test = True :: Bool

-- verse

(width,height) = (16,2 * 9) :: (Int,Int)

(marginX,marginY) = (1,2) :: (Int,Int)

