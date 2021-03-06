{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Test.ChasingBottoms.IsBottom
import Test.Framework (Test, TestName, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(arbitrary))
import Test.QuickCheck.Function (Fun(..), apply)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Data.Strict.Map.Autogen.Strict (Map)
import qualified Data.Strict.Map.Autogen.Strict as M
import qualified Data.Map.Lazy as L

import Utils.IsUnit

instance (Arbitrary k, Arbitrary v, Ord k) =>
         Arbitrary (Map k v) where
    arbitrary = M.fromList `fmap` arbitrary

apply2 :: Fun (a, b) c -> a -> b -> c
apply2 f a b = apply f (a, b)

apply3 :: Fun (a, b, c) d -> a -> b -> c -> d
apply3 f a b c = apply f (a, b, c)

------------------------------------------------------------------------
-- * Properties

------------------------------------------------------------------------
-- ** Strict module

pSingletonKeyStrict :: Int -> Bool
pSingletonKeyStrict v = isBottom $ M.singleton (bottom :: Int) v

pSingletonValueStrict :: Int -> Bool
pSingletonValueStrict k = isBottom $ (M.singleton k (bottom :: Int))

pFindWithDefaultKeyStrict :: Int -> Map Int Int -> Bool
pFindWithDefaultKeyStrict def m = isBottom $ M.findWithDefault def bottom m

pFindWithDefaultValueStrict :: Int -> Map Int Int -> Bool
pFindWithDefaultValueStrict k m =
    M.member k m || (isBottom $ M.findWithDefault bottom k m)

pAdjustKeyStrict :: Fun Int Int -> Map Int Int -> Bool
pAdjustKeyStrict f m = isBottom $ M.adjust (apply f) bottom m

pAdjustValueStrict :: Int -> Map Int Int -> Bool
pAdjustValueStrict k m
    | k `M.member` m = isBottom $ M.adjust (const bottom) k m
    | otherwise       = case M.keys m of
        []     -> True
        (k':_) -> isBottom $ M.adjust (const bottom) k' m

pInsertKeyStrict :: Int -> Map Int Int -> Bool
pInsertKeyStrict v m = isBottom $ M.insert bottom v m

pInsertValueStrict :: Int -> Map Int Int -> Bool
pInsertValueStrict k m = isBottom $ M.insert k bottom m

pInsertWithKeyStrict :: Fun (Int, Int) Int -> Int -> Map Int Int -> Bool
pInsertWithKeyStrict f v m = isBottom $ M.insertWith (apply2 f) bottom v m

pInsertWithValueStrict :: Fun (Int, Int) Int -> Int -> Int -> Map Int Int
                       -> Bool
pInsertWithValueStrict f k v m
    | M.member k m = (isBottom $ M.insertWith (const2 bottom) k v m) &&
                     not (isBottom $ M.insertWith (const2 1) k bottom m)
    | otherwise    = isBottom $ M.insertWith (apply2 f) k bottom m

pInsertLookupWithKeyKeyStrict :: Fun (Int, Int, Int) Int -> Int
                              -> Map Int Int -> Bool
pInsertLookupWithKeyKeyStrict f v m = isBottom $ M.insertLookupWithKey (apply3 f) bottom v m

pInsertLookupWithKeyValueStrict :: Fun (Int, Int, Int) Int -> Int -> Int
                                -> Map Int Int -> Bool
pInsertLookupWithKeyValueStrict f k v m
    | M.member k m = (isBottom $ M.insertLookupWithKey (const3 bottom) k v m) &&
                     not (isBottom $ M.insertLookupWithKey (const3 1) k bottom m)
    | otherwise    = isBottom $ M.insertLookupWithKey (apply3 f) k bottom m

------------------------------------------------------------------------
-- check for extra thunks
--
-- These tests distinguish between `()`, a fully evaluated value, and
-- things like `id ()` which are extra thunks that should be avoided
-- in most cases. An exception is `L.fromListWith const`, which cannot
-- evaluate the `const` calls.

tExtraThunksM :: Test
tExtraThunksM = testGroup "Map.Strict - extra thunks" $
    if not isUnitSupported then [] else
    -- for strict maps, all the values should be evaluated to ()
    [ check "singleton"           $ m0
    , check "insert"              $ M.insert 42 () m0
    , check "insertWith"          $ M.insertWith const 42 () m0
    , check "fromList"            $ M.fromList [(42,()),(42,())]
    , check "fromListWith"        $ M.fromListWith const [(42,()),(42,())]
    , check "fromAscList"         $ M.fromAscList [(42,()),(42,())]
    , check "fromAscListWith"     $ M.fromAscListWith const [(42,()),(42,())]
    , check "fromDistinctAscList" $ M.fromAscList [(42,())]
    ]
  where
    m0 = M.singleton 42 ()
    check :: TestName -> M.Map Int () -> Test
    check n m = testCase n $ case M.lookup 42 m of
        Just v -> assertBool msg (isUnit v)
        _      -> assertString "key not found"
      where
        msg = "too lazy -- expected fully evaluated ()"

tExtraThunksL :: Test
tExtraThunksL = testGroup "Map.Lazy - extra thunks" $
    if not isUnitSupported then [] else
    -- for lazy maps, the *With functions should leave `const () ()` thunks,
    -- but the other functions should produce fully evaluated ().
    [ check "singleton"       True  $ m0
    , check "insert"          True  $ L.insert 42 () m0
    , check "insertWith"      False $ L.insertWith const 42 () m0
    , check "fromList"        True  $ L.fromList [(42,()),(42,())]
    , check "fromListWith"    False $ L.fromListWith const [(42,()),(42,())]
    , check "fromAscList"     True  $ L.fromAscList [(42,()),(42,())]
    , check "fromAscListWith" False $ L.fromAscListWith const [(42,()),(42,())]
    , check "fromDistinctAscList" True $ L.fromAscList [(42,())]
    ]
  where
    m0 = L.singleton 42 ()
    check :: TestName -> Bool -> L.Map Int () -> Test
    check n e m = testCase n $ case L.lookup 42 m of
        Just v -> assertBool msg (e == isUnit v)
        _      -> assertString "key not found"
      where
        msg | e         = "too lazy -- expected fully evaluated ()"
            | otherwise = "too strict -- expected a thunk"

------------------------------------------------------------------------
-- * Test list

tests :: [Test]
tests =
    [
    -- Basic interface
      testGroup "Map.Strict"
      [ testProperty "singleton is key-strict" pSingletonKeyStrict
      , testProperty "singleton is value-strict" pSingletonValueStrict
      , testProperty "member is key-strict" $ keyStrict M.member
      , testProperty "lookup is key-strict" $ keyStrict M.lookup
      , testProperty "findWithDefault is key-strict" pFindWithDefaultKeyStrict
      , testProperty "findWithDefault is value-strict" pFindWithDefaultValueStrict
      , testProperty "! is key-strict" $ keyStrict (flip (M.!))
      , testProperty "delete is key-strict" $ keyStrict M.delete
      , testProperty "adjust is key-strict" pAdjustKeyStrict
      , testProperty "adjust is value-strict" pAdjustValueStrict
      , testProperty "insert is key-strict" pInsertKeyStrict
      , testProperty "insert is value-strict" pInsertValueStrict
      , testProperty "insertWith is key-strict" pInsertWithKeyStrict
      , testProperty "insertWith is value-strict" pInsertWithValueStrict
      , testProperty "insertLookupWithKey is key-strict"
        pInsertLookupWithKeyKeyStrict
      , testProperty "insertLookupWithKey is value-strict"
        pInsertLookupWithKeyValueStrict
      ]
      , tExtraThunksM
      , tExtraThunksL
    ]

------------------------------------------------------------------------
-- * Test harness

main :: IO ()
main = defaultMain tests

------------------------------------------------------------------------
-- * Utilities

keyStrict :: (Int -> Map Int Int -> a) -> Map Int Int -> Bool
keyStrict f m = isBottom $ f bottom m

const2 :: a -> b -> c -> a
const2 x _ _ = x

const3 :: a -> b -> c -> d -> a
const3 x _ _ _ = x
