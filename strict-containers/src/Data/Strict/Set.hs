{- | Module alias of "Data.Set"

This module re-exports "Data.Set" for convenience, so that you can avoid an
additional direct dependency on @containers@ if you want to.
-}
module Data.Strict.Set
  ( module Data.Set
  ) where

import Data.Set
