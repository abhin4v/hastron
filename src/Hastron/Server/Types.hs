{-# LANGUAGE DeriveGeneric #-}
module Hastron.Server.Types where

import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
-- import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as T
import           GHC.Generics


type Point = (Int, Int)

type Velocity = (Double, Double)

data User = User { userName :: T.Text
                 } deriving (Show, Eq, Ord, Generic)
instance Hashable User

data PlayerState = PlayerAlive | PlayerDead | PlayerDisconnected
                   deriving (Show, Eq, Ord, Enum)

type PlayerTrail = [Point]

data PlayerBoost = PlayerBoost { boostActive :: Bool
                               , boostFuel   :: Double
                               } deriving (Show, Eq)

type PlayerScore = Int

data Player = Player { playerUser     :: User
                     , playerState    :: PlayerState
                     , playerPosition :: Point
                     , playerVelocity :: Velocity
                     , playerTrail    :: PlayerTrail
                     , playerBoost    :: PlayerBoost
                     , playerScore    :: PlayerScore
                     } deriving (Show, Eq)

data GameState = GameStarted | GamePaused | GameStarting | GameFinished
                 deriving (Show, Eq, Ord, Enum)

data Game = Game { gamePlayers :: [Player]
                 , gameState   :: GameState
                 } deriving (Show, Eq)

data RoomUserState = RoomUserJoined | RoomUserReady | RoomUserPlaying
                     deriving (Show, Eq)

data Room = Room { roomGames :: [Game]
                 , roomUsers :: HashMap User RoomUserState
                 } deriving (Show, Eq)

data ServerState = ServerState { serverName  :: T.Text
                               , serverUsers :: [User]
                               , serverRooms :: [Room]
                               } deriving (Show, Eq)
