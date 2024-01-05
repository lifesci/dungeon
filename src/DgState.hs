module DgState where

import Parser(DgNode, Entity, Room, dgPlayer, dgRooms)
import Scope(Scope)
import qualified Scope as Scope
import System.Random(StdGen)
import Data.Map(Map)

data DgState = DgState {
    currentRoom :: String,
    scope :: Scope,
    player :: Entity,
    rooms :: Map String Room,
    running :: Bool,
    rng :: StdGen
} deriving Show

buildState :: StdGen -> DgNode -> DgState
buildState gen dgn = DgState {
    scope=Scope.empty,
    currentRoom="test",
    player=(dgPlayer dgn),
    rooms=(dgRooms dgn),
    running=True,
    rng=gen
}

updateGen :: StdGen -> DgState -> DgState
updateGen gen state = DgState {
    currentRoom=(currentRoom state),
    scope=(scope state),
    player=(player state),
    rooms=(rooms state),
    running=(running state),
    rng=gen
}

updateScopeAndGen :: Scope -> StdGen -> DgState -> DgState
updateScopeAndGen scp gen state = DgState {
    currentRoom=(currentRoom state),
    scope=scp,
    player=(player state),
    rooms=(rooms state),
    running=(running state),
    rng=gen
}

enterScope :: StdGen -> DgState -> DgState
enterScope gen state = DgState {
    currentRoom=(currentRoom state),
    scope=(Scope.push (scope state)),
    player=(player state),
    rooms=(rooms state),
    running=(running state),
    rng=gen
}

leaveScope :: DgState -> DgState
leaveScope state = DgState {
    currentRoom=(currentRoom state),
    scope=(Scope.parent (scope state)),
    player=(player state),
    rooms=(rooms state),
    running=(running state),
    rng=(rng state)
}

setRunning :: Bool -> DgState -> DgState
setRunning newRunning state = DgState {
    currentRoom=(currentRoom state),
    scope=(scope state),
    player=(player state),
    rooms=(rooms state),
    running=newRunning,
    rng=(rng state)
}

