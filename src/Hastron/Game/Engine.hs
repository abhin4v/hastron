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

move :: Int -> Point -> Velocity -> PlayerTrail
move timeElapsed (x, y) (Velocity speed dir) = move' dir
  where
    dist = timeElapsed * speed

    move' Left  = tail [(x', y) | x' <- [x, x - 1 .. x - dist]]
    move' Right = tail [(x', y) | x' <- [x .. x + dist]]
    move' Up    = tail [(x, y') | y' <- [y, y - 1 .. y - dist]]
    move' Down  = tail [(x, y') | y' <- [y .. y + dist]]

moveAfterTurn :: (Direction -> Direction) ->  Int -> Point -> Velocity -> (PlayerTrail, Velocity)
moveAfterTurn turn timeElapsed point (Velocity speed dir) =
  let vel' = Velocity speed $ turn dir
  in (move timeElapsed point vel', vel')

checkTrail :: GameMap -> PlayerTrail -> (PlayerTrail, PlayerState)
checkTrail GameMap{..} trail =
  let trail' = takeWhile (not . flip Set.member gameMapBlockedPoints) trail
  in if trail' == trail
     then (trail, PlayerAlive)
     else (trail', PlayerDead)

stepGame :: Game -> Int -> InEvent -> (Game, [OutEvent])
stepGame game@Game{gameMap = gameMap@GameMap{..}, ..} timeElapsed = stepGame'
  where
    stepGame' (InPlayerTurnLeft playerId)  =
      stepTurnEvent playerId $ moveAfterTurn leftTurn timeElapsed
    stepGame' (InPlayerTurnRight playerId) =
      stepTurnEvent playerId $ moveAfterTurn rightTurn timeElapsed
    stepGame' (InPlayerIdle playerId)      =
      stepTurnEvent playerId $ moveAfterTurn noTurn timeElapsed

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

runGame :: Game -> [(Int, InEvent)] -> (Game, [OutEvent])
runGame initialGame inEvents =
  foldl (\(game, outEvents) (timeElapsed, inEvent) ->
            fmap (outEvents ++) $ stepGame game timeElapsed inEvent)
        (initialGame, []) inEvents
