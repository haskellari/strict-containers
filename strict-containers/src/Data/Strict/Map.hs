{- | Fully-strict version of "Data.Map.Strict"

Unlike @Data.Map.Strict@ t'Data.Map.Strict.Map' which is an alias to
@Data.Map.Lazy@ t'Data.Map.Lazy.Map', the instances of our {- haddock #1251 {-
-} -} t'Data.Strict.Map.Map' are all strict as well.

You should be able to switch from the former simply by changing your module
imports, and your package dependency from @containers@ to @strict-containers@.
If this doesn't work, please file a bug.

The documentation in the auto-generated modules have not been updated in a
particularly sophisticated way, so may sound weird or contradictory. In those
cases, please re-interpret such weird wording in the context of the above.
-}
module Data.Strict.Map
  ( module Data.Strict.Map.Autogen.Strict
  ) where

import Data.Strict.Map.Internal
import Data.Strict.Map.Autogen.Strict
