module Hastron.Game.Types where

import           Data.HashMap.Strict (HashMap)

type Point = (Int, Int)

data Direction = Left
               | Up
               | Right
               | Down
               deriving (Show, Eq, Ord, Enum, Bounded)

data Velocity = Velocity Double Direction deriving (Show, Eq, Ord)

type PlayerId = Int

data PlayerState = PlayerAlive
                 | PlayerDead
                 | PlayerDisconnected
                 | PlayerLeft
                 deriving (Show, Eq, Ord, Enum, Bounded)

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
                      deriving (Show, Eq, Ord, Enum, Bounded)

data GameState = GameStarted | GameInit | GameFinished
                 deriving (Show, Eq, Ord, Enum)

data Game = Game { gamePlayers :: [Player]
                 , gameState   :: GameState
                 } deriving (Show, Eq)

data InEvent = InPlayerTurnLeft PlayerId
             | InPlayerTurnRight PlayerId
             | InPlayerBoostActivate PlayerId
             | InPlayerBoostDeactivate PlayerId
             | InPlayerStateChange PlayerId PlayerState
             deriving (Show, Eq, Ord)

data OutEvent = OutPlayerPosition PlayerId Point Direction
              | OutPlayerStateChange PlayerId PlayerState Point
              | OutGameStateChange GameState
              | OutGameOver (HashMap PlayerId (PlayerScore, PlayerEndState))
              deriving (Show, Eq)
