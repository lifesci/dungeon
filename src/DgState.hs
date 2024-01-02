module DgState where

import Parser(DgNode)
import Scope(Scope)
import qualified Scope as Scope
import System.Random(StdGen)

data DgState = DgState {
    currentRoom :: String,
    scope :: Scope,
    game :: DgNode,
    running :: Bool,
    rng :: StdGen
} deriving Show

buildState :: StdGen -> DgNode -> DgState
buildState gen dgn = DgState {
    scope=Scope.empty,
    currentRoom="test",
    game=dgn,
    running=True,
    rng=gen
}

updateGen :: StdGen -> DgState -> DgState
updateGen gen state = DgState {
    currentRoom=(currentRoom state),
    scope=(scope state),
    game=(game state),
    running=(running state),
    rng=gen
}

updateScopeAndGen :: Scope -> StdGen -> DgState -> DgState
updateScopeAndGen scp gen state = DgState {
    currentRoom=(currentRoom state),
    scope=scp,
    game=(game state),
    running=(running state),
    rng=gen
}

enterScope :: StdGen -> DgState -> DgState
enterScope gen state = DgState {
    currentRoom=(currentRoom state),
    scope=(Scope.push (scope state)),
    game=(game state),
    running=(running state),
    rng=gen
}

leaveScope :: DgState -> DgState
leaveScope state = DgState {
    currentRoom=(currentRoom state),
    scope=(Scope.parent (scope state)),
    game=(game state),
    running=(running state),
    rng=(rng state)
}

