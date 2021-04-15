{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Strict.HashMap.Internal where

import Data.HashMap.Lazy                  as L
import Data.Strict.HashMap.Autogen.Strict as S

import Data.Binary
import Data.Hashable
import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Traversable.WithIndex
import Data.Strict.Classes

instance (Eq k, Hashable k) => Strict (L.HashMap k v) (S.HashMap k v) where
  toStrict = S.fromList . L.toList
  toLazy = L.fromList . S.toList
  {-# INLINE toStrict #-}
  {-# INLINE toLazy #-}

-- code copied from indexed-traversable-instances

instance FunctorWithIndex k (S.HashMap k) where
  imap = S.mapWithKey
  {-# INLINE imap #-}

instance FoldableWithIndex k (S.HashMap k) where
  ifoldr  = S.foldrWithKey
  {-# INLINE ifoldr #-}
  ifoldl' = S.foldlWithKey' . flip
  {-# INLINE ifoldl' #-}

instance TraversableWithIndex k (S.HashMap k) where
  itraverse = S.traverseWithKey
  {-# INLINE itraverse #-}

-- code copied from binary-instances

instance (Hashable k, Eq k, Binary k, Binary v) => Binary (S.HashMap k v) where
    get = fmap S.fromList get
    put = put . S.toList
