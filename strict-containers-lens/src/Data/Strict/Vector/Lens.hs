{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Strict.Vector.Lens
  ( toVectorOf
  , vector
  , forced
  , sliced
  , ordinals
  ) where

import           Control.Lens
import           Control.Lens.Internal.List (ordinalNub)
import           Data.Vector.Generic.Lens (vectorTraverse)
import           Data.Monoid (Endo)

import qualified Data.Strict.Vector as V

import           Data.Strict.Vector (Vector)


#if !MIN_VERSION_lens(5,0,0)
instance FunctorWithIndex Int Vector where
  imap = V.imap
  {-# INLINE imap #-}

instance FoldableWithIndex Int Vector where
  ifoldr = V.ifoldr
  {-# INLINE ifoldr #-}
  ifoldl = V.ifoldl . flip
  {-# INLINE ifoldl #-}
  ifoldr' = V.ifoldr'
  {-# INLINE ifoldr' #-}
  ifoldl' = V.ifoldl' . flip
  {-# INLINE ifoldl' #-}

instance TraversableWithIndex Int Vector where
  itraverse f v =
    let !n = V.length v in V.fromListN n <$> itraverse f (V.toList v)
  {-# INLINE itraverse #-}
#endif

type instance Index (Vector a) = Int
type instance IxValue (Vector a) = a
instance Ixed (Vector a) where
  ix i f v
    | 0 <= i && i < V.length v = f (v V.! i) <&> \a -> v V.// [(i, a)]
    | otherwise                     = pure v
  {-# INLINE ix #-}

instance AsEmpty (Vector a) where
  _Empty = nearly V.empty V.null
  {-# INLINE _Empty #-}

instance Each (Vector a) (Vector b) a b where
  each = vectorTraverse
  {-# INLINE each #-}

instance (t ~ Vector a') => Rewrapped (Vector a) t
instance Wrapped (Vector a) where
  type Unwrapped (Vector a) = [a]
  _Wrapped' = iso V.toList V.fromList
  {-# INLINE _Wrapped' #-}

instance Cons (Vector a) (Vector b) a b where
  _Cons = prism (uncurry V.cons) $ \v ->
    if V.null v
    then Left V.empty
    else Right (V.unsafeHead v, V.unsafeTail v)
  {-# INLINE _Cons #-}

instance Snoc (Vector a) (Vector b) a b where
  _Snoc = prism (uncurry V.snoc) $ \v -> if V.null v
    then Left V.empty
    else Right (V.unsafeInit v, V.unsafeLast v)
  {-# INLINE _Snoc #-}

instance Reversing (Vector a) where
  reversing = V.reverse

-- | Analogous to 'Data.Vector.Lens.sliced'.
sliced :: Int -- ^ @i@ starting index
       -> Int -- ^ @n@ length
       -> Lens' (Vector a) (Vector a)
sliced i n f v = f (V.slice i n v) <&> \ v0 -> v V.// zip [i..i+n-1] (V.toList v0)
{-# INLINE sliced #-}

-- | Analogous to 'Data.Vector.Lens.toVectorOf'.
toVectorOf :: Getting (Endo [a]) s a -> s -> Vector a
toVectorOf l s = V.fromList (toListOf l s)
{-# INLINE toVectorOf #-}

-- | Analogous to 'Data.Vector.Lens.vector'.
vector :: Iso [a] [b] (Vector a) (Vector b)
vector = iso V.fromList V.toList
{-# INLINE vector #-}

-- | Analogous to 'Data.Vector.Lens.forced'.
forced :: Iso (Vector a) (Vector b) (Vector a) (Vector b)
forced = iso V.force V.force
{-# INLINE forced #-}

-- | Analogous to 'Data.Vector.Lens.ordinals'.
ordinals :: [Int] -> IndexedTraversal' Int (Vector a) a
ordinals is f v = fmap (v V.//) $ traverse (\i -> (,) i <$> indexed f i (v V.! i)) $ ordinalNub (length v) is
{-# INLINE ordinals #-}
