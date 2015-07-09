{-# LANGUAGE RecordWildCards #-}
module Hastron.Game.Types where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as Set
import           Data.Tuple          (swap)

type Point = (Int, Int)

data Direction = Left | Right | Up | Down deriving (Show, Eq, Ord, Enum)

data Velocity = Velocity Int Direction deriving (Show, Eq, Ord)

type PlayerId = Int

data PlayerState = PlayerAlive
                 | PlayerDead
                 | PlayerDisconnected
                 | PlayerLeft
                 deriving (Show, Eq, Ord, Enum)

type PlayerTrail = [Point]

data PlayerBoost = PlayerBoost { boostActive :: Bool
                               , boostFuel   :: Double
                               } deriving (Show, Eq)

type PlayerScore = Int

data Player = Player { playerId       :: PlayerId
                     , playerState    :: PlayerState
                     , playerPosition :: Point
                     , playerVelocity :: Velocity
                     , playerTrail    :: PlayerTrail
                     , playerBoost    :: PlayerBoost
                     , playerScore    :: PlayerScore
                     } deriving (Show, Eq)

data PlayerEndState = PlayerWinner | PlayerLoser | PlayerDropped
                      deriving (Show, Eq, Ord, Enum)

data GameState = GameStarted | GameInit | GameFinished
                 deriving (Show, Eq, Ord, Enum)

data GameMap = GameMap { size                 :: Int
                       , gameMapBlockedPoints :: HashSet Point }
               deriving (Show, Eq)

data Game = Game { gamePlayers :: HashMap PlayerId Player
                 , gameState   :: GameState
                 , gameMap     :: GameMap
                 } deriving (Show, Eq)

type GameResult = HashMap PlayerId (PlayerScore, PlayerEndState)

data InEvent = InPlayerTurnLeft PlayerId
             | InPlayerTurnRight PlayerId
             | InPlayerBoostActivate PlayerId
             | InPlayerBoostDeactivate PlayerId
             | InPlayerStateChange PlayerId PlayerState
             | InPlayerIdle PlayerId
             deriving (Show, Eq, Ord)

data OutEvent = OutPlayerPosition PlayerId Point Direction
              | OutPlayerStateChange PlayerId PlayerState
              | OutGameStateChange GameState
              | OutGameOver GameResult
              deriving (Show, Eq)

emptyGameMap :: Int -> GameMap
emptyGameMap size = GameMap size $ Set.fromList borderPoints
  where
    borderPoints = let xs = [(x, y) | x <- [-1, size], y <- [0 .. size - 1]]
                   in xs ++ map swap xs

emptyGame :: Int -> Game
emptyGame size = Game Map.empty GameInit $ emptyGameMap size

addPlayer :: Game -> Player -> Game
addPlayer game@Game{gameMap = gameMap@GameMap{..}, ..} player@Player{..} =
  game { gamePlayers = Map.insert playerId player gamePlayers
       , gameMap = gameMap { gameMapBlockedPoints =
                               Set.insert playerPosition gameMapBlockedPoints }}
