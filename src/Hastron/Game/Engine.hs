{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Hastron.Game.Engine where

import           Control.Applicative    (Applicative)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.Reader   (MonadReader, ask)
import           Control.Monad.RWS      (MonadRWS, RWST, execRWST)
import           Control.Monad.State    (MonadState, get, put, modify)
import           Control.Monad.Writer   (MonadWriter, tell)
import           Data.Bifunctor         (second)
import           Data.DList             (DList)
import qualified Data.DList             as DList
import qualified Data.HashMap.Strict    as Map
import qualified Data.HashSet           as Set
import           Data.List              (foldl')
import           Hastron.Game.Types
import           Prelude                hiding (Left, Right)

newtype GameEngine a =
  GameEngine { _runGameEngine :: RWST GameSettings (DList OutEvent) (GameMap, Player) Identity a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader GameSettings
           , MonadWriter (DList OutEvent)
           , MonadState (GameMap, Player)
           , MonadRWS GameSettings (DList OutEvent) (GameMap, Player))

stepGameEngine :: GameSettings -> GameMap -> Player -> GameEngine () -> (GameMap, Player, [OutEvent])
stepGameEngine gameSettings gameMap player =
  (\((gameMap', player'), outEvents) -> (gameMap', player', DList.toList outEvents))
  . runIdentity
  . (\rwst -> execRWST rwst gameSettings (gameMap, player))
  . _runGameEngine

move :: TimeInterval -> GameEngine ()
move timeElapsed = do
  state    <- get
  settings <- ask
  go settings state
  where
    go GameSettings{ .. } (gameMap@GameMap{ .. }, player@Player{ .. })
      | playerState /= PlayerAlive    || timeElapsed < 0 = return ()
      | not boostActive || boostFuel >= timeElapsed      = move'
      | otherwise                                        =
          move boostFuel >> move (timeElapsed - boostFuel)
      where
        (x, y)               = playerPosition
        (Velocity speed dir) = playerVelocity
        PlayerBoost{ .. }    = playerBoost
        dist                 =
          timeElapsed * speed * (if boostActive && boostFuel > 0 then gameBoostFactor else 1)

        makeTrail Left  = tail [(x', y) | x' <- [x, x - 1 .. x - dist]]
        makeTrail Right = tail [(x', y) | x' <- [x .. x + dist]]
        makeTrail Up    = tail [(x, y') | y' <- [y, y - 1 .. y - dist]]
        makeTrail Down  = tail [(x, y') | y' <- [y .. y + dist]]

        checkTrail trail =
          let trail' = takeWhile (not . flip Set.member gameMapBlockedPoints) trail
          in if trail' == trail
             then (trail, PlayerAlive)
             else (trail', PlayerDead)

        move' = do
          let
            revTrail                     = makeTrail dir
            (checkedTrail, playerState') = checkTrail revTrail
            trail                        = reverse checkedTrail
            pos'                         = if null trail then playerPosition else head trail
            boostFuel'                   = if boostActive then boostFuel - timeElapsed else boostFuel

            player'   = player { playerState    = playerState'
                               , playerPosition = pos'
                               , playerTrail    = trail ++ playerTrail
                               , playerBoost    = playerBoost { boostFuel = boostFuel' }
                               }
            gameMap'  = gameMap { gameMapBlockedPoints =
                                   foldl' (flip Set.insert) gameMapBlockedPoints trail }
            outEvents = [OutPlayerStateChange playerId playerState' | playerState /= playerState']

          put (gameMap', player')
          tell $ DList.fromList outEvents

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

turn :: (Direction -> Direction) -> GameEngine ()
turn turnFn = modify . second $ \player@Player{ playerVelocity = Velocity speed dir } ->
  player { playerVelocity = Velocity speed $ turnFn dir }

changeBoost :: Bool -> GameEngine ()
changeBoost boostActive = modify . second $ \player@Player{ .. } ->
  player { playerBoost = playerBoost { boostActive = boostActive } }

refillBoost :: Timestamp -> GameEngine ()
refillBoost timeElapsed = do
  (gameMap, player@Player{ .. }) <- get
  GameSettings{ .. }             <- ask
  let boostFuel'   = floor (fromIntegral timeElapsed * gameBoostRefillFactor)
      playerBoost' = playerBoost { boostFuel = boostFuel playerBoost + boostFuel' }
  put (gameMap, player { playerBoost = playerBoost' })
  tell $ DList.fromList [OutPlayerBoostChange playerId playerBoost' | playerBoost /= playerBoost']

stepGame :: Game -> Timestamp -> InEvent -> (Game, [OutEvent])
stepGame game@Game{ gameMap = gameMap@GameMap{ .. }, .. } time = stepGame'
  where
    stepGame' (InPlayerTurnLeft playerId)                = stepEvent playerId $ turn leftTurn
    stepGame' (InPlayerTurnRight playerId)               = stepEvent playerId $ turn rightTurn
    stepGame' (InPlayerIdle playerId)                    = stepEvent playerId $ return ()
    stepGame' (InPlayerBoostChange playerId boostActive) = stepEvent playerId $ changeBoost boostActive

    stepEvent pId step =
      flip (maybe (game, [])) (Map.lookup pId gamePlayers) $ \player@Player{ .. } ->
        if playerState /= PlayerAlive
        then (game, [])
        else let
            timeElapsed = time - playerLastEventTime
            fullStep    = move timeElapsed >> step >> refillBoost timeElapsed

            (gameMap', player'@Player{ playerPosition = pos'
                                     , playerVelocity = Velocity _ dir' }, outEvents) =
              stepGameEngine gameSettings gameMap player fullStep

            player''   = player' { playerScore         = playerScore + score playerPosition pos'
                                 , playerLastEventTime = time }
            game'      = game { gamePlayers = Map.insert playerId player'' gamePlayers
                              , gameMap     = gameMap' }
            outEvents' = outEvents ++ [OutPlayerPosition playerId pos' dir']
          in (game', outEvents')

    score (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

runGame :: Game -> [(Timestamp, InEvent)] -> (Game, [OutEvent])
runGame initialGame =
  foldl (\(game, outEvents) (time, inEvent) ->
            fmap (outEvents ++) $ stepGame game time inEvent)
        (initialGame, [])
