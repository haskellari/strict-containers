{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Strict.IntMap.Lens
  (
  ) where

import           Control.Lens

import qualified Data.Strict.IntMap as IM

import           Data.Strict.IntMap (IntMap)


#if !MIN_VERSION_lens(5,0,0)
instance FunctorWithIndex Int IntMap where
  imap = IM.mapWithKey
  {-# INLINE imap #-}

instance FoldableWithIndex Int IntMap where
  ifoldMap = IM.foldMapWithKey
  {-# INLINE ifoldMap #-}
  ifoldr   = IM.foldrWithKey
  {-# INLINE ifoldr #-}
  ifoldl'  = IM.foldlWithKey' . flip
  {-# INLINE ifoldl' #-}

instance TraversableWithIndex Int IntMap where
  itraverse = IM.traverseWithKey
  {-# INLINE itraverse #-}
#endif

type instance Index (IntMap a) = Int
type instance IxValue (IntMap a) = a
instance Ixed (IntMap a) where
  ix k f m = case IM.lookup k m of
     Just v -> f v <&> \v' -> IM.insert k v' m
     Nothing -> pure m
  {-# INLINE ix #-}

instance At (IntMap a) where
  at k f = IM.alterF f k
  {-# INLINE at #-}

instance AsEmpty (IntMap a) where
  _Empty = nearly IM.empty IM.null
  {-# INLINE _Empty #-}

instance Each (IntMap a) (IntMap b) a b where
  each = traversed
  {-# INLINE each #-}

instance (t ~ IntMap a') => Rewrapped (IntMap a) t
instance Wrapped (IntMap a) where
  type Unwrapped (IntMap a) = [(Int, a)]
  _Wrapped' = iso IM.toAscList IM.fromList
  {-# INLINE _Wrapped' #-}

instance TraverseMin Int IntMap where
  traverseMin f m = case IM.minViewWithKey m of
    Just ((k,a), _) -> indexed f k a <&> \v -> IM.updateMin (const (Just v)) m
    Nothing     -> pure m
  {-# INLINE traverseMin #-}

instance TraverseMax Int IntMap where
  traverseMax f m = case IM.maxViewWithKey m of
    Just ((k,a), _) -> indexed f k a <&> \v -> IM.updateMax (const (Just v)) m
    Nothing     -> pure m
  {-# INLINE traverseMax #-}
