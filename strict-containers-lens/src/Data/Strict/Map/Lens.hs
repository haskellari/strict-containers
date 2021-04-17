{-# LANGUAGE CPP                #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}
#if !MIN_VERSION_lens(5,0,0)
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#endif

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

-- | Analogous to 'Data.Map.Lens.toMapOf'.
toMapOf :: IndexedGetting i (Map i a) s a -> s -> Map i a
toMapOf l = iviews l M.singleton
