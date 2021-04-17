{- | Module alias of "Data.IntSet"

This module re-exports "Data.IntSet" for convenience, so that you can avoid an
additional direct dependency on @containers@ if you want to.
-}
module Data.Strict.IntSet
  ( module Data.IntSet
  ) where

import Data.IntSet
