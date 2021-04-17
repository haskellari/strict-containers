{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Strict.Map.Lens
  ( toMapOf
  ) where

import           Control.Lens

import qualified Data.Strict.Map as M

import           Data.Strict.Map (Map)


#if !MIN_VERSION_lens(5,0,0)
instance FunctorWithIndex k (Map k) where
  imap = M.mapWithKey
  {-# INLINE imap #-}

instance FoldableWithIndex k (Map k) where
  ifoldMap = M.foldMapWithKey
  {-# INLINE ifoldMap #-}
  ifoldr   = M.foldrWithKey
  {-# INLINE ifoldr #-}
  ifoldl'  = M.foldlWithKey' . flip
  {-# INLINE ifoldl' #-}

instance TraversableWithIndex k (Map k) where
  itraverse = M.traverseWithKey
  {-# INLINE itraverse #-}
#endif

type instance Index (Map k v) = k
type instance IxValue (Map k v) = v

instance Ord k => Ixed (Map k a) where
  ix k f m = case M.lookup k m of
     Just v  -> f v <&> \v' -> M.insert k v' m
     Nothing -> pure m
  {-# INLINE ix #-}

instance Ord k => At (Map k v) where
  at k f = M.alterF f k
  {-# INLINE at #-}

instance AsEmpty (Map k a) where
  _Empty = nearly M.empty M.null
  {-# INLINE _Empty #-}

instance (c ~ d) => Each (Map c a) (Map d b) a b where
  each = traversed
  {-# INLINE each #-}

instance (t ~ Map k' a', Ord k) => Rewrapped (Map k a) t
instance Ord k => Wrapped (Map k a) where
  type Unwrapped (Map k a) = [(k, a)]
  _Wrapped' = iso M.toAscList M.fromList
  {-# INLINE _Wrapped' #-}

instance Ord k => TraverseMin k (Map k) where
  traverseMin f m = case M.minViewWithKey m of
    Just ((k, a), _) -> indexed f k a <&> \v -> M.updateMin (const (Just v)) m
    Nothing          -> pure m
  {-# INLINE traverseMin #-}

instance Ord k => TraverseMax k (Map k) where
  traverseMax f m = case M.maxViewWithKey m of
    Just ((k, a), _) -> indexed f k a <&> \v -> M.updateMax (const (Just v)) m
    Nothing          -> pure m
  {-# INLINE traverseMax #-}

-- | Analogous to 'Data.Map.Lens.toMapOf'.
toMapOf :: IndexedGetting i (Map i a) s a -> s -> Map i a
toMapOf l = iviews l M.singleton
