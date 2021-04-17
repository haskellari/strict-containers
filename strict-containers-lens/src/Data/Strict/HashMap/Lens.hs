{-# LANGUAGE CPP                #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}
#if !MIN_VERSION_lens(5,0,0)
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Strict.HashMap.Lens
  (
  ) where

import           Control.Lens
import           Data.Hashable (Hashable)

import qualified Data.Strict.HashMap as HM

import           Data.Strict.HashMap (HashMap)


#if !MIN_VERSION_lens(5,0,0)
instance FunctorWithIndex k (HashMap k) where
  imap = HM.mapWithKey
  {-# INLINE imap #-}

instance FoldableWithIndex k (HashMap k) where
  ifoldr  = HM.foldrWithKey
  {-# INLINE ifoldr #-}
  ifoldl' = HM.foldlWithKey' . flip
  {-# INLINE ifoldl' #-}

instance TraversableWithIndex k (HashMap k) where
  itraverse = HM.traverseWithKey
  {-# INLINE itraverse #-}
#endif

type instance Index (HashMap k v) = k
type instance IxValue (HashMap k v) = v

instance (Eq k, Hashable k) => Ixed (HashMap k a) where
  ix k f m = case HM.lookup k m of
     Just v  -> f v <&> \v' -> HM.insert k v' m
     Nothing -> pure m
  {-# INLINE ix #-}

instance (Eq k, Hashable k) => At (HashMap k v) where
  at k f = HM.alterF f k
  {-# INLINE at #-}
