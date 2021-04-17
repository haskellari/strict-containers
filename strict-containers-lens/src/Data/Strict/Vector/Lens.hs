{-# LANGUAGE CPP                #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}
#if !MIN_VERSION_lens(5,0,0)
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#endif

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
