{-# LANGUAGE ConstraintKinds #-}
module Tests.Vector.Boxed (tests) where

import Test.Tasty
import qualified Data.Strict.Vector
import Tests.Vector.Property

import GHC.Exts (inline)


testGeneralBoxedVector
  :: forall a. (CommonContext a Data.Strict.Vector.Vector, Ord a, Data a)
  => Data.Strict.Vector.Vector a -> [TestTree]
testGeneralBoxedVector dummy = concatMap ($ dummy)
  [
    testSanity
  , inline testPolymorphicFunctions
  , testOrdFunctions
  , testTuplyFunctions
  , testNestedVectorFunctions
  , testMonoidFunctions
  , testFunctorFunctions
  , testMonadFunctions
  , testApplicativeFunctions
  , testAlternativeFunctions
  , testSequenceFunctions
  , testDataFunctions
  ]

testBoolBoxedVector dummy = concatMap ($ dummy)
  [
    testGeneralBoxedVector
  , testBoolFunctions
  ]

testNumericBoxedVector
  :: forall a. (CommonContext a Data.Strict.Vector.Vector, Ord a, Num a, Enum a, Random a, Data a)
  => Data.Strict.Vector.Vector a -> [TestTree]
testNumericBoxedVector dummy = concatMap ($ dummy)
  [
    testGeneralBoxedVector
  , testNumFunctions
  , testEnumFunctions
  ]

tests =
  [ testGroup "Bool" $
    testBoolBoxedVector (undefined :: Data.Strict.Vector.Vector Bool)
  , testGroup "Int" $
    testNumericBoxedVector (undefined :: Data.Strict.Vector.Vector Int)
  , testGroup "unstream" $ testUnstream (undefined :: Data.Strict.Vector.Vector Int)
  ]
