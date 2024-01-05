module DgState where

import Parser
import Scope(Scope)
import qualified Scope as Scope
import System.Random(StdGen)
import Data.Map(Map)
import qualified Data.Map as Map

data DgState = DgState {
    currentRoom :: String,
    scope :: Scope,
    source :: Maybe String,
    target :: Maybe String,
    player :: Entity,
    rooms :: Map String Room,
    running :: Bool,
    rng :: StdGen
} deriving Show

buildState :: StdGen -> DgNode -> DgState
buildState gen dgn = DgState {
    scope=Scope.empty,
    source=Nothing,
    target=Nothing,
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

updateScope :: Scope -> DgState -> DgState
updateScope scp state = DgState {
    currentRoom=(currentRoom state),
    scope=scp,
    player=(player state),
    rooms=(rooms state),
    running=(running state),
    rng=(rng state)
}

enterScope :: DgState -> DgState
enterScope state = DgState {
    currentRoom=(currentRoom state),
    scope=(Scope.push (scope state)),
    player=(player state),
    rooms=(rooms state),
    running=(running state),
    rng=(rng state)
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

getCurrentRoom :: DgState -> Room
getCurrentRoom state = case Map.lookup (currentRoom state) (rooms state) of
    Nothing -> error "Unable to locate current room"
    (Just x) -> x

getCurrentRoomEntity :: String -> DgState -> Entity
getCurrentRoomEntity entityName state =
    let
        room = getCurrentRoom state
    in
        case (Map.lookup entityName (roomEntities room)) of
            Nothing -> error "Unable to locate entity in room"
            (Just x) -> x

getPropFromEntity :: Entity -> String -> Int
getPropFromEntity entity prop = case Map.lookup prop (entityStats entity) of
    Nothing -> error "Unable to locate entity prop"
    (Just x) -> x

getPropFromOwner :: Maybe String -> String -> DgState -> Int
getPropFromOwner Nothing _ _ = error "Property owner is undefined"
getPropFromOwner (Just "player") name state = getPropFromEntity (player state) name
getPropFromOwner (Just enemy) name state = getPropFromEntity (getCurrentRoomEntity enemy state) name

getPropVal :: Prop -> DgState -> Int
getPropVal Prop{propVar="source", propName=name} state = getPropFromOwner (source state) name state
getPropVal Prop{propVar="target", propName=name} state = getPropFromOwner (target state) name state
getPropVal _ _ = error "Invalid property owner"

