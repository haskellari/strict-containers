{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Strict.Vector.Internal where

import Data.Vector                as L
import Data.Strict.Vector.Autogen as S

import Data.Binary
import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Traversable.WithIndex
import Data.Semigroup (Semigroup (..)) -- helps with compatibility
import Data.Strict.Classes
import Data.Vector.Binary

instance Strict (L.Vector k) (S.Vector k) where
  toStrict = S.fromList . L.toList
  toLazy = L.fromList . S.toList
  {-# INLINE toStrict #-}
  {-# INLINE toLazy #-}

-- code copied from indexed-traversable-instances

instance FunctorWithIndex Int S.Vector where
  imap = S.imap
  {-# INLINE imap #-}

instance FoldableWithIndex Int S.Vector where
  ifoldr = S.ifoldr
  {-# INLINE ifoldr #-}
  ifoldl = S.ifoldl . flip
  {-# INLINE ifoldl #-}
  ifoldr' = S.ifoldr'
  {-# INLINE ifoldr' #-}
  ifoldl' = S.ifoldl' . flip
  {-# INLINE ifoldl' #-}

instance TraversableWithIndex Int S.Vector where
  itraverse f v =
    let !n = S.length v in S.fromListN n <$> itraverse f (S.toList v)
  {-# INLINE itraverse #-}

-- code copied from vector-binary-instances

instance Binary a => Binary (S.Vector a) where
    put = genericPutVector
    get = genericGetVector
    {-# INLINE get #-}
