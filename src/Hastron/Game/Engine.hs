module Hastron.Game.Engine where

import Hastron.Game.Types

type GameStep = Game -> InEvent -> (Game, [OutEvent])

stepGame :: GameStep
stepGame game inEvent = undefined
