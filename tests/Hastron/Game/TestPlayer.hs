module Hastron.Game.TestPlayer where

import qualified Test.QuickCheck.Arbitrary as Arbit
import qualified Test.Tasty                as Test
import qualified Test.Tasty.QuickCheck     as QC

import qualified Hastron.Game.Player       as Player
import qualified Hastron.Game.Types        as Types
import qualified Hastron.Utils             as Utils

properties :: Test.TestTree
properties = Test.testGroup "Player Properties"
  [ QC.testProperty "Turn player right" prop_turn_player_right
  , QC.testProperty "Turn player left" prop_turn_player_left
  ]

-- | Arbitrary instance declarations
instance Arbit.Arbitrary Types.Direction where
  arbitrary = Arbit.arbitraryBoundedEnum

instance Arbit.Arbitrary Types.Velocity where
  arbitrary = do
    vel <- Arbit.arbitrary
    dir <- Arbit.arbitrary
    return $ Types.Velocity vel dir

instance Arbit.Arbitrary Types.PlayerState where
  arbitrary = Arbit.arbitraryBoundedEnum

instance Arbit.Arbitrary Types.PlayerBoost where
  arbitrary = do
    active <- Arbit.arbitrary
    fuel <- Arbit.arbitrary
    return Types.PlayerBoost { Types.boostActive = active, Types.boostFuel = fuel }

instance Arbit.Arbitrary Types.Player where
  arbitrary = do
    id <- Arbit.arbitrary
    state <- Arbit.arbitrary
    position <- Arbit.arbitrary
    velocity <- Arbit.arbitrary
    trail <- Arbit.arbitrary
    boost <- Arbit.arbitrary
    score <- Arbit.arbitrary
    return Types.Player { Types.playerId = id
                        , Types.playerState = state
                        , Types.playerPosition = position
                        , Types.playerVelocity = velocity
                        , Types.playerTrail = trail
                        , Types.playerBoost = boost
                        , Types.playerScore = score
                        }


instance Arbit.Arbitrary Types.PlayerEndState where
  arbitrary = Arbit.arbitraryBoundedEnum

-- | Properties 
prop_turn_player_right :: Types.Player -> Bool
prop_turn_player_right player = (Player.direction . Player.turnRight) player == (Utils.nextEnum . Player.direction) player

prop_turn_player_left :: Types.Player -> Bool
prop_turn_player_left player = (Player.direction . Player.turnLeft) player == (Utils.prevEnum . Player.direction) player
