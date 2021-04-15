{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Strict.Map.Internal where

import Data.Map.Lazy                  as L
import Data.Strict.Map.Autogen.Strict as S

import Control.Monad
import Data.Binary
import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Traversable.WithIndex
import Data.Semigroup (Semigroup (..)) -- helps with compatibility
import Data.Strict.Classes

instance (Eq k, Ord k) => Strict (L.Map k v) (S.Map k v) where
  toStrict = S.fromList . L.toList
  toLazy = L.fromList . S.toList
  {-# INLINE toStrict #-}
  {-# INLINE toLazy #-}

-- code copied from indexed-traversable

instance FunctorWithIndex k (S.Map k) where
  imap = S.mapWithKey
  {-# INLINE imap #-}

instance FoldableWithIndex k (S.Map k) where
  ifoldMap = S.foldMapWithKey
  {-# INLINE ifoldMap #-}
  ifoldr   = S.foldrWithKey
  {-# INLINE ifoldr #-}
  ifoldl'  = S.foldlWithKey' . flip
  {-# INLINE ifoldl' #-}

instance TraversableWithIndex k (S.Map k) where
  itraverse = S.traverseWithKey
  {-# INLINE itraverse #-}

-- code copied from binary

instance (Binary k, Binary e) => Binary (S.Map k e) where
    put m = put (S.size m) <> mapM_ put (S.toAscList m)
    get   = liftM S.fromDistinctAscList get
