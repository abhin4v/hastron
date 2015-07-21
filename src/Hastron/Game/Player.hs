module Hastron.Game.Player where

import           Hastron.Game.Types
import qualified Hastron.Utils      as Utils

turn :: Direction -> Player -> Player
turn dir player = player { playerVelocity = changeDirection (playerVelocity player) dir }
  where changeDirection (Velocity v _) d = Velocity v d

turnRight :: Player -> Player
turnRight player = turn (Utils.nextEnum (direction player)) player

turnLeft :: Player -> Player
turnLeft player = turn (Utils.prevEnum (direction player)) player

direction :: Player -> Direction
direction Player {playerVelocity=(Velocity _ dir)} = dir

toggleBoost :: PlayerBoost -> PlayerBoost
toggleBoost boost@(PlayerBoost { boostActive = curr }) = boost { boostActive = not curr }

updateBoostValue :: (Double -> Double) -> PlayerBoost -> PlayerBoost
updateBoostValue func boost@(PlayerBoost {boostFuel = fuel}) = boost { boostFuel = func fuel }

changeState :: PlayerState -> Player -> Player
changeState new player = player { playerState = new }
