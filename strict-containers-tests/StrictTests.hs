{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit

#if __GLASGOW_HASKELL__ >= 806
import GHC.Exts.Heap
#else
import GHC.HeapView
#endif

import qualified Data.Strict.HashMap as SHM
import qualified Data.Strict.IntMap as SIM
import qualified Data.Strict.Map as SM
import qualified Data.Strict.Sequence as SS
import qualified Data.Strict.Vector as SV

import qualified Data.HashMap.Lazy as LHM
import qualified Data.IntMap as LIM
import qualified Data.Map as LM
import qualified Data.Sequence as LS
import qualified Data.Vector as LV

import qualified Data.Binary as B
import qualified Codec.Serialise as S

import Control.Lens
import Control.Exception
import Data.Binary.Instances
import Data.Semigroup (Semigroup (..)) -- helps with compatibility
import Data.Strict.Classes (Strict(..))
import Data.Strict.Containers.Lens ()
import Data.Strict.Containers.Serialise ()

import Debug.Trace (traceShowId)


-- https://stackoverflow.com/a/28701687
evaluated' :: Bool -> a -> IO Bool
evaluated' b = go . asBox
    where
        go box = do
            c <- getBoxedClosureData box
            case (if b then traceShowId else id) c of
                ThunkClosure     {} -> return False
                SelectorClosure  {} -> return False
                APClosure        {} -> return False
                APStackClosure   {} -> return False
                IndClosure       {indirectee = b'} -> go b'
                BlackholeClosure {indirectee = b'} -> go b'
                _ -> return True

evaluated :: a -> IO Bool
evaluated = evaluated' False

assertThrows :: a -> Assertion
assertThrows x = do
  errored <- catch (evaluate x >> pure False) handler
  if errored then
      assertFailure "Did not catch expected error"
  else
      pure ()
  where
     handler :: ErrorCall -> IO Bool
     handler _ = pure True

evaluatedIx
  :: Ixed c
  => Index c ~ k
  => IxValue c ~ a
  => c -> k -> IO Bool
evaluatedIx d k = case d ^? ix k of
  Nothing -> error "impossible"
  Just b -> evaluated b

mkB :: Int -> Bool
mkB i = i >= 0
{-# NOINLINE mkB #-}

mkL :: Int -> LS.Seq Int
mkL i = LS.replicate i 0
{-# NOINLINE mkL #-}

singletonTest
  :: Strict (lc v) (sc v)
  => Int
  -> (Int -> v)
  -> (v -> lc v)
  -> (v -> sc v)
  -> Assertion
singletonTest i' mk lsingleton ssingleton = do
  let i = 10000 * i'
  -- GHC does some special caching for ints so all the numbers we use below
  -- need to be unique at runtime for the tests to pass as expected
  let b = mk i
  evalB_ <- evaluated b
  assertBool "eval before" $ not evalB_
  let lc = lsingleton b
  evalB <- lc `seq` evaluated b
  assertBool "eval after lazy ins" $ not evalB

  let slc = toStrict lc
  evalB <- slc `seq` evaluated b
  assertBool "eval after toStrict" $ evalB

  let b' = mk (succ i)
  let sc = ssingleton b'
  evalB' <- sc `seq` evaluated b'
  assertBool "eval after strict ins" $ evalB'

insertTest
  :: Strict (lc v) (sc v)
  => Monoid (lc v)
  => Monoid (sc v)
  => Int
  -> (Int -> v)
  -> (v -> lc v -> lc v)
  -> (v -> sc v -> sc v)
  -> Assertion
insertTest i' mk linsert sinsert =
  singletonTest i' mk (\b -> linsert b mempty) (\b -> sinsert b mempty)

ixTest
  :: forall lc sc v
   . Ixed (lc v)
  => Index (lc v) ~ Int
  => IxValue (lc v) ~ v
  => Ixed (sc v)
  => Index (sc v) ~ Int
  => IxValue (sc v) ~ v
  => Int
  -> (Int -> v)
  -> (v -> lc v)
  -> (v -> sc v)
  -> Assertion
ixTest i' mk lsingleton ssingleton = do
  let i = 10000 * i' + 1000
  -- GHC does some special caching for ints so all the numbers we use below
  -- need to be unique at runtime for the tests to pass as expected

#if MIN_VERSION_containers(0,6,0)
  -- old containers versions weren't discipline about thunks in Seq.lookup
  -- just ignore it since it's not our concern
  let lc = lsingleton $ mk i
  evalB <- evaluatedIx lc k0
  assertBool "eval after lazy ix" $ not evalB
#endif

  let sc = ssingleton $ mk i
  evalB <- evaluatedIx sc k0
  assertBool "eval after strict ix" $ evalB

binaryTest
  :: forall lc sc v
   . Strict (lc v) (sc v)
  => Ixed (lc v)
  => Index (lc v) ~ Int
  => IxValue (lc v) ~ v
  => Ixed (sc v)
  => Index (sc v) ~ Int
  => IxValue (sc v) ~ v
  => B.Binary (lc v)
  => B.Binary (sc v)
  => Int
  -> (Int -> v)
  -> (v -> lc v)
  -> (v -> sc v)
  -> Assertion
binaryTest i' mk lsingleton ssingleton = do
  let i = 10000 * i' + 2000
  -- GHC does some special caching for ints so all the numbers we use below
  -- need to be unique at runtime for the tests to pass as expected
  let s = B.encode $ lsingleton $ mk i

  -- lazy deserialisation instances do often actually strictly construct the
  -- structures; making this test pass is possible but a pointless exercise,
  -- since we are really interested in the strict behaviour.
  --let ld = B.decode s :: lc v
  --evalB <- evaluatedIx ld k0
  --assertBool "eval after lazy binary-encode" $ not evalB

  let sd = B.decode s :: sc v
  evalB <- evaluatedIx sd k0
  assertBool "eval after strict binary-decode" $ evalB

serialiseTest
  :: forall lc sc v
   . Strict (lc v) (sc v)
  => Ixed (lc v)
  => Index (lc v) ~ Int
  => IxValue (lc v) ~ v
  => Ixed (sc v)
  => Index (sc v) ~ Int
  => IxValue (sc v) ~ v
  => S.Serialise (lc v)
  => S.Serialise (sc v)
  => Int
  -> (Int -> v)
  -> (v -> lc v)
  -> (v -> sc v)
  -> Assertion
serialiseTest i' mk lsingleton ssingleton = do
  let i = 10000 * i' + 3000
  -- GHC does some special caching for ints so all the numbers we use below
  -- need to be unique at runtime for the tests to pass as expected
  let s = S.serialise $ lsingleton $ mk i

  -- lazy deserialisation instances do often actually strictly construct the
  -- structures; making this test pass is possible but a pointless exercise,
  -- since we are really interested in the strict behaviour.
  --let ld = S.deserialise s :: lc v
  --evalB <- evaluatedIx ld k0
  --assertBool "eval after lazy deserialise" $ not evalB

  let sd = S.deserialise s :: sc v
  evalB <- evaluatedIx sd k0
  assertBool "eval after strict deserialise" $ evalB

k0 :: Int
k0 = 0

testContainer
  :: forall lc sc v
   . Strict (lc v) (sc v)
  => Ixed (lc v)
  => Index (lc v) ~ Int
  => IxValue (lc v) ~ v
  => Ixed (sc v)
  => Index (sc v) ~ Int
  => IxValue (sc v) ~ v
  => B.Binary (lc v)
  => B.Binary (sc v)
  => S.Serialise (lc v)
  => S.Serialise (sc v)
  => Monoid (lc v)
  => Monoid (sc v)
  => String
  -> Int
  -> (Int -> v)
  -> (v -> lc v)
  -> (v -> sc v)
  -> (v -> lc v -> lc v)
  -> (v -> sc v -> sc v)
  -> (v -> lc v -> lc v)
  -> (v -> sc v -> sc v)
  -> [TestTree]
testContainer typeName i mk lsingleton ssingleton linsert sinsert linsert2 sinsert2  =
  [ testCase (typeName <> " singleton")  $ singletonTest (0  + i) mk lsingleton ssingleton
  , testCase (typeName <> " insert")     $ insertTest    (10 + i) mk linsert    sinsert
  , testCase (typeName <> " insert 2")   $ insertTest    (20 + i) mk linsert2   sinsert2
  , testCase (typeName <> " ix")         $ ixTest        (50 + i) mk lsingleton ssingleton
  , testCase (typeName <> " binary")     $ binaryTest    (60 + i) mk lsingleton ssingleton
  , testCase (typeName <> " serialise")  $ serialiseTest (70 + i) mk lsingleton ssingleton
  ]

main :: IO ()
main = defaultMain $ testGroup "toplevel"
   $ testContainer "HashMap"  1 mkB (LHM.singleton k0) (SHM.singleton k0)
                                    (LHM.insert k0)    (SHM.insert k0)
                                    (LHM.insert k0)    (SHM.insert k0)
  <> testContainer "IntMap"   2 mkB (LIM.singleton k0) (SIM.singleton k0)
                                    (LIM.insert k0)    (SIM.insert k0)
                                    (LIM.insert k0)    (SIM.insert k0)
  <> testContainer "Map"      3 mkB (LM.singleton k0)  (SM.singleton k0)
                                    (LM.insert k0)     (SM.insert k0)
                                    (LM.insert k0)     (SM.insert k0)
  <> testContainer "Sequence" 4 mkB (LS.singleton)     (SS.singleton)
                                    (LS.<|)            (SS.<|)
                                    (flip (LS.|>))     (flip (SS.|>))
  <> testContainer "Vector"   5 mkB (LV.singleton)     (SV.singleton)
                                    (LV.cons)          (SV.cons)
                                    (flip LV.snoc)     (flip SV.snoc)
