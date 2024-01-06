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

toString :: DgState -> String
toString state =
    (currentRoom state) ++ "\n"
    ++ ""

buildState :: StdGen -> DgNode -> DgState
buildState gen dgn = DgState {
    currentRoom="test",
    scope=Scope.empty,
    source=Nothing,
    target=Nothing,
    player=(dgPlayer dgn),
    rooms=(dgRooms dgn),
    running=True,
    rng=gen
}

updateGen :: StdGen -> DgState -> DgState
updateGen gen state = DgState {
    currentRoom=(currentRoom state),
    scope=(scope state),
    source=(source state),
    target=(target state),
    player=(player state),
    rooms=(rooms state),
    running=(running state),
    rng=gen
}

updateScopeAndGen :: Scope -> StdGen -> DgState -> DgState
updateScopeAndGen scp gen state = DgState {
    currentRoom=(currentRoom state),
    scope=scp,
    source=(source state),
    target=(target state),
    player=(player state),
    rooms=(rooms state),
    running=(running state),
    rng=gen
}

updateScope :: Scope -> DgState -> DgState
updateScope scp state = DgState {
    currentRoom=(currentRoom state),
    scope=scp,
    source=(source state),
    target=(target state),
    player=(player state),
    rooms=(rooms state),
    running=(running state),
    rng=(rng state)
}

enterScope :: DgState -> DgState
enterScope state = DgState {
    currentRoom=(currentRoom state),
    scope=(Scope.push (scope state)),
    source=(source state),
    target=(target state),
    player=(player state),
    rooms=(rooms state),
    running=(running state),
    rng=(rng state)
}

leaveScope :: DgState -> DgState
leaveScope state = DgState {
    currentRoom=(currentRoom state),
    scope=(Scope.parent (scope state)),
    source=(source state),
    target=(target state),
    player=(player state),
    rooms=(rooms state),
    running=(running state),
    rng=(rng state)
}

setRunning :: Bool -> DgState -> DgState
setRunning newRunning state = DgState {
    currentRoom=(currentRoom state),
    scope=(scope state),
    source=(source state),
    target=(target state),
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
getCurrentRoomEntity name state =
    let
        room = getCurrentRoom state
    in
        case (Map.lookup name (roomEntities room)) of
            Nothing -> error "Unable to locate entity in current room"
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
getPropVal _ _ = error "Invalid property owner in update"

updateEntityProp :: Maybe String -> String -> Int -> DgState -> DgState
updateEntityProp Nothing _ _ _ = error "Property owner is undefined in update"
updateEntityProp (Just "player") name val state =
    let p = (player state) in
        DgState {
            currentRoom=(currentRoom state),
            scope=(scope state),
            source=(source state),
            target=(target state),
            player=Entity {
                entityType=(entityType p),
                entityName=(entityName p),
                entityArgs=(entityArgs p),
                entityStats=(Map.insert name val (entityStats p)),
                entityAlive=(entityAlive p),
                entityActions=(entityActions p),
                entityTriggers=(entityTriggers p),
                entityItems=(entityItems p)
            },
            rooms=(rooms state),
            running=(running state),
            rng=(rng state)
        }
updateEntityProp (Just enemy) name val state =
    let
        entity = getCurrentRoomEntity enemy state
        curRoom = getCurrentRoom state
    in
        DgState {
            currentRoom=(currentRoom state),
            scope=(scope state),
            source=(source state),
            target=(target state),
            player=(player state),
            rooms=(
                Map.insert
                    (currentRoom state)
                    Room {
                        roomName=(roomName curRoom),
                        roomEntities=(
                            Map.insert
                                enemy
                                Entity {
                                    entityType=(entityType entity),
                                    entityName=(entityName entity),
                                    entityArgs=(entityArgs entity),
                                    entityStats=(
                                        Map.insert
                                            name
                                            val
                                            (entityStats entity)),
                                    entityAlive=(entityAlive entity),
                                    entityActions=(entityActions entity),
                                    entityTriggers=(entityTriggers entity),
                                    entityItems=(entityItems entity)
                                }
                                (roomEntities curRoom)),
                        roomItems=(roomItems curRoom),
                        roomDoors=(roomDoors curRoom)
                    }
                    (rooms state)),
            running=(running state),
            rng=(rng state)
        }

updateProp :: Prop -> Int -> DgState -> DgState
updateProp Prop{propVar="source", propName=name} val state = updateEntityProp (source state) name val state
updateProp Prop{propVar="target", propName=name} val state = updateEntityProp (target state) name val state
updateProp _ _ _ = error "Invalid property owner in assign"

