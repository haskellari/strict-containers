{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Strict.Sequence.Internal where

import Data.Sequence                as L
import Data.Strict.Sequence.Autogen as S

import Data.Binary
import Data.Foldable
import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Traversable.WithIndex
import Data.Semigroup (Semigroup (..)) -- helps with compatibility
import Data.Strict.Classes

instance Strict (L.Seq k) (S.Seq k) where
  toStrict = S.fromList . toList
  toLazy = L.fromList . toList
  {-# INLINE toStrict #-}
  {-# INLINE toLazy #-}

-- code copied from indexed-traversable

-- | The position in the 'Seq' is available as the index.
instance FunctorWithIndex Int S.Seq where
  imap = S.mapWithIndex
  {-# INLINE imap #-}

instance FoldableWithIndex Int S.Seq where
  ifoldMap = S.foldMapWithIndex
  {-# INLINE ifoldMap #-}
  ifoldr = S.foldrWithIndex
  {-# INLINE ifoldr #-}
  ifoldl f = S.foldlWithIndex (flip f)
  {-# INLINE ifoldl #-}

instance TraversableWithIndex Int S.Seq where
  itraverse = S.traverseWithIndex
  {-# INLINE itraverse #-}

-- code copied from binary

instance (Binary e) => Binary (S.Seq e) where
    put s = put (S.length s) <> mapM_ put s
    get = do n <- get :: Get Int
             rep S.empty n get
      where rep xs 0 _ = return $! xs
            rep xs n g = xs `seq` n `seq` do
                           x <- g
                           rep (xs S.|> x) (n-1) g
