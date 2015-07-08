{-# LANGUAGE DeriveGeneric #-}
module Hastron.Server.Types where

import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
-- import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as T
import           GHC.Generics

import Hastron.Game.Types

data User = User { userName :: T.Text
                 } deriving (Show, Eq, Ord, Generic)
instance Hashable User

data RoomUserState = RoomUserJoined | RoomUserReady | RoomUserPlaying
                     deriving (Show, Eq)

data Room = Room { roomGames :: [Game]
                 , roomUsers :: HashMap User RoomUserState
                 } deriving (Show, Eq)

data ServerState = ServerState { serverName  :: T.Text
                               , serverUsers :: [User]
                               , serverRooms :: [Room]
                               } deriving (Show, Eq)
