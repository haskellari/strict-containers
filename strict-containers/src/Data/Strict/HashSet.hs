{- | Module alias of "Data.HashSet"

This module re-exports "Data.HashSet" for convenience, so that you can avoid an
additional direct dependency on @containers@ if you want to.
-}
module Data.Strict.HashSet
  ( module Data.HashSet
  ) where

import Data.HashSet
