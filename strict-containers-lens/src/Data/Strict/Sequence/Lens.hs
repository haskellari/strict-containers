{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Strict.Sequence.Lens
  ( viewL
  , viewR
  , slicedTo
  , slicedFrom
  , sliced
  , seqOf
  ) where

import           Control.Lens

import qualified Data.Foldable as Foldable
import qualified Data.Strict.Sequence as Seq

import           Data.Strict.Sequence (Seq, ViewL(EmptyL), ViewR(EmptyR), (><), viewl, viewr)


#if !MIN_VERSION_lens(5,0,0)
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

type instance Index (Seq a) = Int
type instance IxValue (Seq a) = a
instance Ixed (Seq a) where
  ix i f m
    | 0 <= i && i < Seq.length m = f (Seq.index m i) <&> \a -> Seq.update i a m
    | otherwise                  = pure m
  {-# INLINE ix #-}

instance AsEmpty (Seq a) where
  _Empty = nearly Seq.empty Seq.null
  {-# INLINE _Empty #-}

instance Each (Seq a) (Seq b) a b where
  each = traversed
  {-# INLINE each #-}

instance (t ~ Seq a') => Rewrapped (Seq a) t
instance Wrapped (Seq a) where
  type Unwrapped (Seq a) = [a]
  _Wrapped' = iso Foldable.toList Seq.fromList
  {-# INLINE _Wrapped' #-}

instance Cons (Seq a) (Seq b) a b where
  _Cons = prism (uncurry (Seq.<|)) $ \aas -> case viewl aas of
    a Seq.:< as -> Right (a, as)
    EmptyL  -> Left mempty
  {-# INLINE _Cons #-}

instance Snoc (Seq a) (Seq b) a b where
  _Snoc = prism (uncurry (Seq.|>)) $ \aas -> case viewr aas of
    as Seq.:> a -> Right (as, a)
    EmptyR  -> Left mempty
  {-# INLINE _Snoc #-}

instance Reversing (Seq a) where
  reversing = Seq.reverse

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
