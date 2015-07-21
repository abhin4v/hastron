{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hastron.TestUtils (properties) where

import qualified Test.QuickCheck.Arbitrary as Arbit
import qualified Test.Tasty                as Test
import qualified Test.Tasty.QuickCheck     as QC

import qualified Hastron.Utils             as Utils

newtype Enum' a = Enum' a
  deriving (Eq, Bounded, Enum, Show)

instance (Enum a, Bounded a) => Arbit.Arbitrary (Enum' a) where
  arbitrary = Arbit.arbitraryBoundedEnum

properties :: Test.TestTree
properties = Test.testGroup "Utils Properties"
  [ QC.testProperty "Additive inverse for turning" $
      \n e -> prop_additive_turning_inverse (n :: Int) (e :: Enum' Char)
  ]

prop_additive_turning_inverse :: (Eq a, Show a, Enum a, Bounded a) => Int -> Enum' a -> Bool
prop_additive_turning_inverse n (Enum' e) = e == ((Utils.turnEnum n) . (Utils.turnEnum (-n))) e
