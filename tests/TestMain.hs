module Main where

import qualified Test.Tasty              as Tasty

import qualified Hastron.Game.TestPlayer as Player
import qualified Hastron.TestUtils       as Utils

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "Tests" [properties, unitTests]

unitTests :: Tasty.TestTree
unitTests = Tasty.testGroup "Unit Tests" [hUnitTests]

properties :: Tasty.TestTree
properties = Tasty.testGroup "Quickcheck properties" [ Utils.properties
                                                     , Player.properties
                                                     ]

hUnitTests :: Tasty.TestTree
hUnitTests = Tasty.testGroup "HUnit unit tests" []
