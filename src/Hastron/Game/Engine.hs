{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Hastron.Game.Engine where

import           Control.Applicative    (Applicative)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.Reader   (MonadReader, ask)
import           Control.Monad.RWS      (MonadRWS, RWST, execRWST)
import           Control.Monad.State    (MonadState, get, put)
import           Control.Monad.Writer   (MonadWriter, tell)
import           Data.DList             (DList)
import qualified Data.DList             as DList
import qualified Data.HashMap.Strict    as Map
import qualified Data.HashSet           as Set
import           Data.List              (foldl')
import           Hastron.Game.Types
import           Prelude                hiding (Left, Right)

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

move :: Int -> GameEngine ()
move timeElapsed = do
  state    <- get
  settings <- ask
  go settings state
  where
    go GameSettings{..}
       (gameMap@GameMap{..}, player@Player{playerBoost = playerBoost@PlayerBoost{..}, ..})
      | playerState /= PlayerAlive    || timeElapsed < 0 = return ()
      | not boostActive || boostFuel >= timeElapsed      = movePlayer
      | otherwise                                        =
          move boostFuel >> move (timeElapsed - boostFuel)
      where
        (x, y)               = playerPosition
        (Velocity speed dir) = playerVelocity
        dist                 =
          timeElapsed * speed * (if boostActive && boostFuel > 0 then gameBoostFactor else 1)

        move' Left  = tail [(x', y) | x' <- [x, x - 1 .. x - dist]]
        move' Right = tail [(x', y) | x' <- [x .. x + dist]]
        move' Up    = tail [(x, y') | y' <- [y, y - 1 .. y - dist]]
        move' Down  = tail [(x, y') | y' <- [y .. y + dist]]

        checkTrail trail =
          let trail' = takeWhile (not . flip Set.member gameMapBlockedPoints) trail
          in if trail' == trail
             then (trail, PlayerAlive)
             else (trail', PlayerDead)

        movePlayer = do
          let
            revTrail                     = move' dir
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

turn :: (Direction -> Direction) -> GameEngine ()
turn turnFn = get >>= go
  where
    go (gameMap, player@Player{playerVelocity = Velocity speed dir}) =
      put ( gameMap, player { playerVelocity = Velocity speed $ turnFn dir })

changeBoost :: Bool -> GameEngine ()
changeBoost boostActive = get >>= go
  where
    go (gameMap, player@Player{..}) =
      put (gameMap, player { playerBoost = playerBoost { boostActive = boostActive } })

refillBoost :: Int -> GameEngine ()
refillBoost timeElapsed = do
  state    <- get
  settings <- ask
  go settings state
  where
    go GameSettings{..} (gameMap, player@Player{..}) = do
      let boostFuel'   = floor (fromIntegral timeElapsed * gameBoostRefillFactor)
          playerBoost' = playerBoost { boostFuel = boostFuel' }
      put (gameMap, player { playerBoost = playerBoost' })
      tell $ DList.fromList [OutPlayerBoostChange playerId playerBoost' | playerBoost /= playerBoost']

stepGame :: Game -> Int -> InEvent -> (Game, [OutEvent])
stepGame game@Game{gameMap = gameMap@GameMap{..}, ..} time = stepGame'
  where
    stepGame' (InPlayerTurnLeft playerId)                = stepEvent playerId $ turn leftTurn
    stepGame' (InPlayerTurnRight playerId)               = stepEvent playerId $ turn rightTurn
    stepGame' (InPlayerIdle playerId)                    = stepEvent playerId $ turn noTurn
    stepGame' (InPlayerBoostChange playerId boostActive) = stepEvent playerId $ changeBoost boostActive

    stepEvent pId step =
      flip (maybe (game, [])) (Map.lookup pId gamePlayers) $ \player@Player{..} ->
        if playerState /= PlayerAlive
        then (game, [])
        else let
            timeElapsed = (time - playerLastEventTime)
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

runGame :: Game -> [(Int, InEvent)] -> (Game, [OutEvent])
runGame initialGame =
  foldl (\(game, outEvents) (time, inEvent) ->
            fmap (outEvents ++) $ stepGame game time inEvent)
        (initialGame, [])
