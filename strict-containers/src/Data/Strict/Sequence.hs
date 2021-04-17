{- | Fully-strict version of "Data.Sequence"

Unlike "Data.Sequence" t'Data.Sequence.Seq' the instances of our {- haddock
#1251 -} t'Data.Strict.Sequence.Seq' are all strict as well.

You should be able to switch from the former simply by changing your module
imports, and your package dependency from @containers@ to @strict-containers@.
If this doesn't work, please file a bug.

The documentation in the auto-generated modules have not been updated in a
particularly sophisticated way, so may sound weird or contradictory. In those
cases, please re-interpret such weird wording in the context of the above.
-}
module Data.Strict.Sequence
  ( module Data.Strict.Sequence.Autogen
  ) where

import Data.Strict.Sequence.Internal
import Data.Strict.Sequence.Autogen
