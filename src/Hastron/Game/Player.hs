module Hastron.Game.Player where

import           Hastron.Game.Types
import qualified Hastron.Utils      as Utils


turn :: Direction -> Player -> Player
turn dir player = player { playerVelocity = changeDirection (playerVelocity player) dir }

turnRight :: Player -> Player
turnRight player = turn (Utils.nextEnum (direction player)) player

direction :: Player -> Direction
direction Player {playerVelocity=(Velocity _ dir)} = dir

changeDirection :: Velocity -> Direction -> Velocity
changeDirection (Velocity val _) dir = Velocity val dir
