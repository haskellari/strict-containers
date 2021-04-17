{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Strict.IntMap.Internal where

import Data.IntMap.Lazy                  as L
import Data.Strict.IntMap.Autogen.Strict as S

import Control.Monad
import Data.Binary
import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Traversable.WithIndex
import Data.Semigroup (Semigroup (..)) -- helps with compatibility
import Data.Strict.Classes

instance Strict (L.IntMap e) (S.IntMap e) where
  toStrict = S.fromList . L.toList
  toLazy = L.fromList . S.toList
  {-# INLINE toStrict #-}
  {-# INLINE toLazy #-}

-- code copied from indexed-traversable

instance FunctorWithIndex Int S.IntMap where
  imap = S.mapWithKey
  {-# INLINE imap #-}

instance FoldableWithIndex Int S.IntMap where
  ifoldMap = S.foldMapWithKey
  {-# INLINE ifoldMap #-}
  ifoldr   = S.foldrWithKey
  {-# INLINE ifoldr #-}
  ifoldl'  = S.foldlWithKey' . flip
  {-# INLINE ifoldl' #-}

instance TraversableWithIndex Int S.IntMap where
  itraverse = S.traverseWithKey
  {-# INLINE itraverse #-}

-- code copied from binary

instance (Binary e) => Binary (S.IntMap e) where
    put m = put (S.size m) <> mapM_ put (S.toAscList m)
    get   = liftM S.fromDistinctAscList get
