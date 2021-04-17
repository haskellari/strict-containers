{-# LANGUAGE ConstraintKinds #-}
module Tests.Vector (tests) where

import Test.Tasty (testGroup)
import qualified Tests.Vector.Boxed

tests =
  [ testGroup "Tests.Vector.Boxed" Tests.Vector.Boxed.tests
  ]
