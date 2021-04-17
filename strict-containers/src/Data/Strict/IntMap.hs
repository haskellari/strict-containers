{- | Fully-strict version of "Data.IntMap.Strict"

Unlike @Data.IntMap.Strict@ t'Data.IntMap.Strict.IntMap' which is an alias to
@Data.IntMap.Lazy@ t'Data.IntMap.Lazy.IntMap', the instances of our {- haddock
 #1251 -} t'Data.Strict.IntMap.IntMap' are all strict as well.

You should be able to switch from the former simply by changing your module
imports, and your package dependency from @containers@ to @strict-containers@.
If this doesn't work, please file a bug.

The documentation in the auto-generated modules have not been updated in a
particularly sophisticated way, so may sound weird or contradictory. In those
cases, please re-interpret such weird wording in the context of the above.
-}
module Data.Strict.IntMap
  ( module Data.Strict.IntMap.Autogen.Strict
  ) where

import Data.Strict.IntMap.Internal
import Data.Strict.IntMap.Autogen.Strict
