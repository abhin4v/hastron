{-# LANGUAGE RecordWildCards #-}
module Hastron.Game.Engine where

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import           Data.List           (foldl')
import           Hastron.Game.Types
import           Prelude             hiding (Left, Right)

leftTurn :: Direction -> Direction
leftTurn Left  = Down
leftTurn Right = Up
leftTurn Up    = Left
leftTurn Down  = Right

rightTurn :: Direction -> Direction
rightTurn Left  = Up
rightTurn Right = Down
rightTurn Up    = Right
rightTurn Down  = Left

noTurn :: Direction -> Direction
noTurn = id

move :: Point -> Velocity -> PlayerTrail
move (x, y) (Velocity speed Left)  = tail [(x', y) | x' <- [x, x - 1 .. x - speed]]
move (x, y) (Velocity speed Right) = tail [(x', y) | x' <- [x .. x + speed]]
move (x, y) (Velocity speed Up)    = tail [(x, y') | y' <- [y, y - 1 .. y - speed]]
move (x, y) (Velocity speed Down)  = tail [(x, y') | y' <- [y .. y + speed]]

moveAfterTurn :: (Direction -> Direction) -> Point -> Velocity -> (PlayerTrail, Velocity)
moveAfterTurn turn point (Velocity speed dir) =
  let vel' = Velocity speed $ turn dir
  in (move point vel', vel')

checkTrail :: GameMap -> PlayerTrail -> (PlayerTrail, PlayerState)
checkTrail GameMap{..} trail =
  let trail' = takeWhile (not . flip Set.member gameMapBlockedPoints) trail
  in if trail' == trail
     then (trail, PlayerAlive)
     else (trail', PlayerDead)

stepGame :: Game -> InEvent -> (Game, [OutEvent])
stepGame game@Game{gameMap = gameMap@GameMap{..}, ..} = stepGame'
  where
    stepGame' (InPlayerTurnLeft playerId)  = stepTurnEvent playerId $ moveAfterTurn leftTurn
    stepGame' (InPlayerTurnRight playerId) = stepTurnEvent playerId $ moveAfterTurn rightTurn
    stepGame' (InPlayerIdle playerId)      = stepTurnEvent playerId $ moveAfterTurn noTurn

    stepTurnEvent pId moveFn =
      flip (maybe (game, [])) (Map.lookup pId gamePlayers) $ \player@Player{..} ->
        if playerState /= PlayerAlive
        then (game, [])
        else let
            (revTrail, vel'@(Velocity _ dir')) = moveFn playerPosition playerVelocity
            (checkedTrail, playerState')       = checkTrail gameMap revTrail
            trail                              = reverse checkedTrail
            pos'                               = if null trail then playerPosition else head trail
            player'   = player { playerState    = playerState'
                               , playerPosition = pos'
                               , playerVelocity = vel'
                               , playerTrail    = trail ++ playerTrail
                               , playerScore    = playerScore + score playerPosition pos'
                               }
            gameMap'  = gameMap { gameMapBlockedPoints =
                                   foldl' (flip Set.insert) gameMapBlockedPoints trail }
            game'     = game { gamePlayers = Map.insert playerId player' gamePlayers
                             , gameMap     = gameMap' }
            outEvents = OutPlayerPosition playerId pos' dir' :
                          [OutPlayerStateChange playerId playerState' | playerState /= playerState']
          in (game', outEvents)

    score (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

runGame :: Game -> [InEvent] -> (Game, [OutEvent])
runGame initialGame =
  foldl (\(game, outEvents) inEvent -> let (game', outEvents') = stepGame game inEvent
                                       in (game', outEvents ++ outEvents'))
        (initialGame, [])
