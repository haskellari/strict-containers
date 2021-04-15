{-# LANGUAGE CPP                #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}
#if !MIN_VERSION_lens(5,0,0)
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Strict.Containers.Lens
  ( viewL
  , viewR
  , slicedTo
  , slicedFrom
  , sliced
  , seqOf
  , toMapOf
  ) where

import           Control.Lens
import           Data.Hashable (Hashable)

import qualified Data.Strict.HashMap as HM
import qualified Data.Strict.Map as M
import qualified Data.Strict.Sequence as Seq

import           Data.Strict.Map (Map)
import           Data.Strict.HashMap (HashMap)
import           Data.Strict.Sequence (Seq, ViewL(EmptyL), ViewR(EmptyR), (><), viewl, viewr)


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

instance FunctorWithIndex Int Seq where
  imap = Seq.mapWithIndex
  {-# INLINE imap #-}

instance FoldableWithIndex Int Seq where
  ifoldMap = Seq.foldMapWithIndex
  {-# INLINE ifoldMap #-}
  ifoldr = Seq.foldrWithIndex
  {-# INLINE ifoldr #-}
  ifoldl f = Seq.foldlWithIndex (flip f)
  {-# INLINE ifoldl #-}

instance TraversableWithIndex Int Seq where
  itraverse = Seq.traverseWithIndex
  {-# INLINE itraverse #-}
#endif

type instance Index (Map k v) = k
type instance IxValue (Map k v) = v

instance Ord k => Ixed (Map k v) where
  -- default ix

instance Ord k => At (Map k v) where
  at k f = M.alterF f k
  {-# INLINE at #-}

type instance Index (HashMap k v) = k
type instance IxValue (HashMap k v) = v

instance (Eq k, Hashable k) => Ixed (HashMap k v) where
  -- default ix

instance (Eq k, Hashable k) => At (HashMap k v) where
  at k f = HM.alterF f k
  {-# INLINE at #-}

-- | Analogous to 'Data.Sequence.Lens.viewL'.
viewL :: Iso (Seq a) (Seq b) (ViewL a) (ViewL b)
viewL = iso viewl $ \ xs -> case xs of
  EmptyL ->  mempty
  a Seq.:< as -> a Seq.<| as
{-# INLINE viewL #-}

-- | Analogous to 'Data.Sequence.Lens.viewR'.
viewR :: Iso (Seq a) (Seq b) (ViewR a) (ViewR b)
viewR = iso viewr $ \xs -> case xs of
  EmptyR  -> mempty
  as Seq.:> a -> as Seq.|> a
{-# INLINE viewR #-}

-- | Analogous to 'Data.Sequence.Lens.slicedTo'.
slicedTo :: Int -> IndexedTraversal' Int (Seq a) a
slicedTo n f m = case Seq.splitAt n m of
  (l,r) -> (>< r) <$> itraverse (indexed f) l
{-# INLINE slicedTo #-}

-- | Analogous to 'Data.Sequence.Lens.slicedFrom'.
slicedFrom :: Int -> IndexedTraversal' Int (Seq a) a
slicedFrom n f m = case Seq.splitAt n m of
  (l,r) -> (l ><) <$> itraverse (indexed f . (+n)) r
{-# INLINE slicedFrom #-}

-- | Analogous to 'Data.Sequence.Lens.sliced'.
sliced :: Int -> Int -> IndexedTraversal' Int (Seq a) a
sliced i j f s = case Seq.splitAt i s of
  (l,mr) -> case Seq.splitAt (j-i) mr of
     (m, r) -> itraverse (indexed f . (+i)) m <&> \n -> l >< n >< r
{-# INLINE sliced #-}

-- | Analogous to 'Data.Sequence.Lens.seqOf'.
seqOf :: Getting (Seq a) s a -> s -> Seq a
seqOf l = views l Seq.singleton
{-# INLINE seqOf #-}

-- | Analogous to 'Data.Map.Lens.toMapOf'.
toMapOf :: IndexedGetting i (Map i a) s a -> s -> Map i a
toMapOf l = iviews l M.singleton
