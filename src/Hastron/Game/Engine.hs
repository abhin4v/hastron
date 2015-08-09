{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Hastron.Game.Engine where

import           Control.Applicative    (Applicative)
import           Control.Monad          (guard)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.Reader   (MonadReader, ask)
import           Control.Monad.RWS      (MonadRWS, RWST, execRWST)
import           Control.Monad.State    (MonadState, get, modify, put)
import           Control.Monad.Writer   (MonadWriter, tell)
import           Data.Bifunctor         (second)
import           Data.DList             (DList)
import qualified Data.DList             as DList
import qualified Data.HashMap.Strict    as Map
import qualified Data.HashSet           as Set
import           Data.List              (foldl', sortBy)
import           Data.Maybe             (catMaybes, fromJust, isJust, listToMaybe)
import           Data.Ord               (comparing)
import           Hastron.Game.Types
import           Prelude                hiding (Left, Right)
import Data.Foldable (msum)

type Projection = ((Point, Timestamp), (Point, Timestamp))

newtype GameEngine a =
  GameEngine { _runGameEngine :: RWST (GameSettings, GameMap) (DList Projection) Player Identity a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (GameSettings, GameMap)
           , MonadWriter (DList Projection)
           , MonadState Player
           , MonadRWS (GameSettings, GameMap) (DList (Projection)) Player)

stepGameEngine :: GameSettings -> GameMap -> Player -> GameEngine () -> (Player, [Projection])
stepGameEngine gameSettings gameMap player =
  (\(player', outEvents) -> (player', DList.toList outEvents))
  . runIdentity
  . (\rwst -> execRWST rwst (gameSettings, gameMap) player)
  . _runGameEngine

movePlayer :: Timestamp -> Timestamp -> GameEngine ()
movePlayer startTime endTime = do
  state    <- get
  settings <- ask
  go settings state
  where
    go (GameSettings{ .. }, GameMap{ .. }) player@Player{ .. }
      | playerState /= PlayerAlive    || timeElapsed < 0 = return ()
      | not boostActive || boostFuel >= timeElapsed      = movePlayer'
      | otherwise                                        =
          movePlayer startTime (startTime + boostFuel) >>
            movePlayer (startTime + boostFuel) endTime
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
             then (trail, PlayerAlive, endTime)
             -- NOTE: we lose some precision in endTime since we're not using Doubles
             else (trail', PlayerStopped, startTime + length trail' `div` speed)

        movePlayer' = do
          let
            revTrail                     = makeTrail dir
            (checkedTrail, playerState', endTime') = checkTrail revTrail
            trail                        = reverse checkedTrail
            pos'                         = if null trail then playerPosition else head trail
            boostFuel'                   = if boostActive then boostFuel - timeElapsed else boostFuel

            player'   = player { playerState    = playerState'
                               , playerPosition = pos'
                               , playerTrail    = trail ++ playerTrail
                               , playerBoost    = playerBoost { boostFuel = boostFuel' }
                               }
            -- gameMap'  = gameMap { gameMapBlockedPoints =
            --                        foldl' (flip Set.insert) gameMapBlockedPoints trail }
            -- outEvents = [OutPlayerStateChange playerId playerState' | playerState /= playerState']

          put player'
          -- tell $ DList.fromList outEvents
          tell $ DList.fromList [((playerPosition, startTime), (pos', endTime'))]

    timeElapsed = endTime - startTime

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
turn turnFn = modify $ \player@Player{ playerVelocity = Velocity speed dir } ->
  player { playerVelocity = Velocity speed $ turnFn dir }

changeBoost :: Bool -> GameEngine ()
changeBoost boostActive = modify $ \player@Player{ .. } ->
  player { playerBoost = playerBoost { boostActive = boostActive } }

refillBoost :: TimeInterval -> GameEngine ()
refillBoost timeElapsed = do
  player@Player{ .. } <- get
  (GameSettings{ .. }, _)             <- ask
  let boostFuel'   = floor (fromIntegral timeElapsed * gameBoostRefillFactor)
      playerBoost' = playerBoost { boostFuel = boostFuel playerBoost + boostFuel' }
  put player { playerBoost = playerBoost' }
  -- tell $ DList.fromList [OutPlayerBoostChange playerId playerBoost' | playerBoost /= playerBoost']

stepPlayer :: (Timestamp, InEvent) -> GameEngine ()
stepPlayer (time, inEvent) = do
  player@Player { .. } <- get
  let timeElapsed =time - playerLastEventTime
  movePlayer time timeElapsed >> eventStep inEvent >> refillBoost timeElapsed
  put $ player { playerLastEventTime = time }
  where
    eventStep :: InEvent -> GameEngine ()
    eventStep (InPlayerTurnLeft _)                = turn leftTurn
    eventStep (InPlayerTurnRight _)               = turn rightTurn
    eventStep (InPlayerIdle _)                    = return ()
    eventStep (InPlayerBoostChange _ boostActive) = changeBoost boostActive

getPlayerId :: InEvent -> Maybe PlayerId
getPlayerId (InPlayerTurnLeft playerId)      = Just playerId
getPlayerId (InPlayerTurnRight playerId)     = Just playerId
getPlayerId (InPlayerIdle playerId)          = Just playerId
getPlayerId (InPlayerBoostChange playerId _) = Just playerId
getPlayerId _                                = Nothing

-- stepGame :: Game -> (Timestamp, InEvent) -> (Game, [OutEvent])
-- stepGame game@Game{ gameMap = gameMap@GameMap{ .. }, .. } (time, inEvent) =

  -- where
  --   stepGame' (InPlayerTurnLeft playerId)                = stepEvent playerId $ turn leftTurn
  --   stepGame' (InPlayerTurnRight playerId)               = stepEvent playerId $ turn rightTurn
  --   stepGame' (InPlayerIdle playerId)                    = stepEvent playerId $ return ()
  --   stepGame' (InPlayerBoostChange playerId boostActive) = stepEvent playerId $ changeBoost boostActive

  --   stepEvent pId step =
  --     flip (maybe (game, [])) (Map.lookup pId gamePlayers) $ \player@Player{ .. } ->
  --       if playerState /= PlayerAlive
  --       then (game, [])
  --       else let
  --           timeElapsed = time - playerLastEventTime
  --           fullStep    = movePlayer timeElapsed >> step >> refillBoost timeElapsed

  --           (gameMap', player'@Player{ playerPosition = pos'
  --                                    , playerVelocity = Velocity _ dir' }, outEvents) =
  --             stepGameEngine gameSettings gameMap player fullStep

  --           player''   = player' { playerScore         = playerScore + score playerPosition pos'
  --                                , playerLastEventTime = time }
  --           game'      = game { gamePlayers = Map.insert playerId player'' gamePlayers
  --                             , gameMap     = gameMap' }
  --           outEvents' = outEvents ++ [OutPlayerPosition playerId pos' dir']
  --         in (game', outEvents')

  --   score (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

data Slope = Inf | Zero deriving (Eq)

intersectProjections :: Projection -> Projection -> Maybe (Point, Double)
intersectProjections (((xs1, ys1), ts1), ((xe1, ye1), te1)) (((xs2, ys2), ts2), ((xe2, ye2), te2))
  -- Head on collision on x-axis
  | m1 == m2 && m1 == Zero && ys1 == ys2 && signum (xs1 - xs2) /= signum (xe1 - xe2) =
      let (x, t) = headOnCollisionPointAndTime xs1 xe1 xs2 xe2 in Just ((x, ys1), t)
  -- Tail on collition on x-axis
  | m1 == m2 && m1 == Zero && ys1 == ys2 && signum (xs1 - xs2) /= signum (xe1 - xs2) =
      Just ((xs2, ys1), tailOnCollisionTime xs1 xe1 xs2)
  -- Head on collision on y-axis
  | m1 == m2 && m1 == Inf && xs1 == xs2 && signum (ys1 - ys2) /= signum (ye1 - ye2)  =
      let (y, t) = headOnCollisionPointAndTime ys1 ye1 ys2 ye2 in Just ((xs1, y), t)
  -- Tail on collition on y-axis
  | m1 == m2 && m1 == Inf && xs1 == xs2 && signum (ys1 - ys2) /= signum (ye1 - ys2)  =
      Just ((xs1, ys2), tailOnCollisionTime ys1 ye1 ys2)
  -- Parallel projections, no collision
  | m1 == m2   = Nothing
  -- Orthogonal collision x and y axis
  | m1 == Zero = let myTime    = timeAtCollision ts1 te1 xs1 xs2
                     otherTime = timeAtCollision ts2 te2 ys1 ys2
                 in guard (myTime >= otherTime - tolerance) >> return ((xs2, ys1), myTime)
  -- Orthogonal collision y and x axis
  | otherwise  = let myTime    = timeAtCollision ts1 te1 ys1 ys2
                     otherTime = timeAtCollision ts2 te2 xs1 xs2
                 in guard (myTime >= otherTime - tolerance) >> return ((xs1, ys2), myTime)
  where
    fromI = fromIntegral

    m1 = if (xs1 == xe1) then Inf else Zero
    m2 = if (xs2 == xe2) then Inf else Zero

    headOnCollisionPointAndTime s1 e1 s2 e2 = let
        v1 = fromI (e1 - s1) / fromI (te1 - ts1)
        v2 = fromI (e2 - s2) / fromI (te2 - ts2)
        t  = (fromI s1 - v1 * fromI ts1 - fromI s2 + v2 * fromI ts2) / (v2 - v1)
      in (s1 + floor (v1 * (t - fromI ts1)), t)

    tailOnCollisionTime s1 e1 s2 = let
        v1 = fromI (e1 - s1) / fromI (te1 - ts1)
      in fromIntegral (s2 - s1) / v1

    tolerance = 0.1

    timeAtCollision ts te s1 s2 =
      fromI ts + (fromI (te - ts) / fromI (abs (s2 - s1)))

intersectPlayers :: (Player, [Projection]) -> (Player, [Projection]) -> Maybe (Point, Double)
intersectPlayers (player1, projections1) (player2, projections2) = listToMaybe $ do
  projection1 <- projections1
  projection2 <- projections2
  let intersection = projection1 `intersectProjections` projection2
  guard (isJust intersection)
  return $ fromJust intersection

tickGame :: Game -> Timestamp -> [(Timestamp, InEvent)] -> (Game, [OutEvent])
tickGame game@Game{ .. } tickTime inEvents = let
    events = sortBy (comparing fst) $ inEvents ++ idleEvents
    playerEvents = flip (`foldl` Map.empty) events $ \m (ts, event) ->
      case getPlayerId event of
        Nothing -> m
        Just playerId -> Map.insertWith (++) playerId [(ts, event)] m

    playerProjections :: Map.HashMap PlayerId (Player, [Projection])
    playerProjections = flip Map.mapWithKey playerEvents $ \playerId events ->
      stepGameEngine gameSettings gameMap (fromJust $ Map.lookup playerId gamePlayers) $
        sequence_ $ map stepPlayer events

    collisions :: [(Player, Player, Point, Double)]
    collisions = sortBy (comparing $ \(_, _, _, time) -> time)
      . catMaybes $ [
          fmap (\(point, time) -> (fst p1, fst p2, point, time)) $ p1 `intersectPlayers` p2
        | p1 <- Map.elems playerProjections
        , p2 <- Map.elems playerProjections
        , fst p1 /= fst p2 ]

    deadPlayers :: Map.HashMap PlayerId (PlayerState, Point, Timestamp)
    deadPlayers = flip (`foldl` Map.empty) collisions $ \m (p1, p2, position, time) ->
      let pId1 = playerId p1
          pId2 = playerId p2
      in case Map.lookup pId2 m of
          Nothing -> case Map.lookup pId1 m of
                       Nothing -> Map.insert pId1 (PlayerDead, position, floor time) m
                       _       -> m
          _       -> m

    allTheDeadPlayers = flip (`foldl` deadPlayers) (Map.elems playerProjections) $ \m (Player{..}, projections) ->
      if playerState == PlayerStopped && not (playerId `Map.member` m)
      then let
          (position, timestamp) = snd . last $ projections
        in Map.insert playerId (PlayerDead, position, timestamp) m
      else m

  in undefined
  where
    idleEvents = map (\pId -> (tickTime, InPlayerIdle pId)) $ Map.keys gamePlayers
