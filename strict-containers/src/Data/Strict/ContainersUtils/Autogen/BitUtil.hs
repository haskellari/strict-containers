{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE MagicHash #-}
#endif
#if !defined(TESTING) && defined(__GLASGOW_HASKELL__)
{-# LANGUAGE Safe #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.ContainersUtils.Autogen.BitUtil
-- Copyright   :  (c) Clark Gaebel 2012
--                (c) Johan Tibel 2012
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
-----------------------------------------------------------------------------
--
-- = WARNING
--
-- This module is considered __internal__.
--
-- The Package Versioning Policy __does not apply__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
--
-- Authors importing this module are expected to track development
-- closely.

module Data.Strict.ContainersUtils.Autogen.BitUtil
    ( bitcount
    , highestBitMask
    , shiftLL
    , shiftRL
    , wordSize
    ) where

import Data.Bits (popCount, unsafeShiftL, unsafeShiftR
    , countLeadingZeros, finiteBitSize
    )


{----------------------------------------------------------------------
  [bitcount] as posted by David F. Place to haskell-cafe on April 11, 2006,
  based on the code on
  http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetKernighan,
  where the following source is given:
    Published in 1988, the C Programming Language 2nd Ed. (by Brian W.
    Kernighan and Dennis M. Ritchie) mentions this in exercise 2-9. On April
    19, 2006 Don Knuth pointed out to me that this method "was first published
    by Peter Wegner in CACM 3 (1960), 322. (Also discovered independently by
    Derrick Lehmer and published in 1964 in a book edited by Beckenbach.)"
----------------------------------------------------------------------}

bitcount :: Int -> Word -> Int
bitcount a x = a + popCount x
{-# INLINE bitcount #-}

-- The highestBitMask implementation is based on
-- http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
-- which has been put in the public domain.

-- | Return a word where only the highest bit is set.
highestBitMask :: Word -> Word
highestBitMask w = shiftLL 1 (wordSize - 1 - countLeadingZeros w)
{-# INLINE highestBitMask #-}

-- Right and left logical shifts.
shiftRL, shiftLL :: Word -> Int -> Word
shiftRL = unsafeShiftR
shiftLL = unsafeShiftL

{-# INLINE wordSize #-}
wordSize :: Int
wordSize = finiteBitSize (0 :: Word)
