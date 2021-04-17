{- | Fully-strict version of "Data.HashMap.Strict".

Unlike @Data.HashMap.Strict@ t'Data.HashMap.Strict.HashMap' which is an alias
to @Data.HashMap.Lazy@ t'Data.HashMap.Lazy.HashMap', the instances of our {-
haddock #1251 -} t'Data.Strict.HashMap.HashMap' are all strict as well.

You should be able to switch from the former simply by changing your module
imports, and your package dependency from @containers@ to @strict-containers@.
If this doesn't work, please file a bug.

The documentation in the auto-generated modules have not been updated in a
particularly sophisticated way, so may sound weird or contradictory. In those
cases, please re-interpret such weird wording in the context of the above.
-}
module Data.Strict.HashMap
  ( module Data.Strict.HashMap.Autogen.Strict
  ) where

import Data.HashMap.Strict ()
import Data.Strict.HashMap.Internal
import Data.Strict.HashMap.Autogen.Strict
