{- | Fully-strict version of "Data.Vector"

Unlike "Data.Vector" t'Data.Vector.Vector' the instances of our {- haddock
#1251 -} t'Data.Strict.Vector.Vector' are all strict as well.

You should be able to switch from the former simply by changing your module
imports, and your package dependency from @vector@ to @strict-containers@.
If this doesn't work, please file a bug.

The documentation in the auto-generated modules have not been updated in a
particularly sophisticated way, so may sound weird or contradictory. In those
cases, please re-interpret such weird wording in the context of the above.
-}
module Data.Strict.Vector
  ( module Data.Strict.Vector.Autogen
  ) where

import Data.Strict.Vector.Internal
import Data.Strict.Vector.Autogen
