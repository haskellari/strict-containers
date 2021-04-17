{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

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

instance AsEmpty (HashMap k a) where
  _Empty = nearly HM.empty HM.null
  {-# INLINE _Empty #-}

instance (c ~ d) => Each (HashMap c a) (HashMap d b) a b where
  each = traversed
  {-# INLINE each #-}

instance (t ~ HashMap k' a', Hashable k, Eq k) => Rewrapped (HashMap k a) t
instance (Hashable k, Eq k) => Wrapped (HashMap k a) where
  type Unwrapped (HashMap k a) = [(k, a)]
  _Wrapped' = iso HM.toList HM.fromList
  {-# INLINE _Wrapped' #-}
