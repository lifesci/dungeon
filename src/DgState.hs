module DgState (
    DgState(..),
    buildState,
    toString,
    updateGen,
    updateScopeAndGen,
    updateScope,
    enterScope,
    leaveScope,
    setRunning,
    getPropVal,
    updateProp,
    getCurrentRoom,
    takeItem,
    updateSourceAndTarget,
    updateSTS
) where

import Scope(Scope)
import qualified Stat
import qualified Dungeon
import qualified Entity
import qualified Room
import qualified Scope
import qualified Item
import Lib(listToMap, join)
import System.Random(StdGen)
import Data.Map(Map)
import qualified Data.Map as Map

data DgState = DgState {
    currentRoom :: String,
    scope :: Scope,
    source :: Maybe String,
    target :: Maybe String,
    player :: Entity.Entity,
    rooms :: Map String Room.Room,
    running :: Bool,
    rng :: StdGen
} deriving Show

toString :: DgState -> Int -> String
toString state t =
    (
        join
            "\n"
            [
                Entity.toString t (player state),
                Room.toString t (getCurrentRoom state)
            ]
    )
    ++ "\n"

buildState :: StdGen -> Dungeon.Dungeon -> DgState
buildState gen dgn = DgState {
    currentRoom="start",
    scope=Scope.empty,
    source=Nothing,
    target=Nothing,
    player=Entity.playerFromTemplate (Dungeon.playerTemplate dgn) (Dungeon.statblock dgn),
    rooms=listToMap
        (
            map
                (Room.fromTemplate
                    (Dungeon.enemyTemplates dgn)
                    (Dungeon.itemTemplates dgn)
                    (Dungeon.statblock dgn))
                (Dungeon.roomTemplates dgn)
        )
        Room.name
        id,
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

updateSTS :: Maybe String -> Maybe String -> Scope -> DgState -> DgState
updateSTS src trgt scp state = state { source=src, target=trgt, scope=scp }

updateSourceAndTarget :: Maybe String -> Maybe String -> DgState -> DgState
updateSourceAndTarget s t state = state { source=s, target=t }

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

takeItem :: Item.Item -> Room.Room -> DgState -> DgState
takeItem i r state = DgState {
    currentRoom=(currentRoom state),
    scope=(scope state),
    source=(source state),
    target=(target state),
    player=(Entity.takeItem i (player state)),
    rooms=(Map.insert (Room.name r) r (rooms state)),
    running=(running state),
    rng=(rng state)
}

getCurrentRoom :: DgState -> Room.Room
getCurrentRoom state = case Map.lookup (currentRoom state) (rooms state) of
    Nothing -> error "Unable to locate current room"
    (Just x) -> x

getCurrentRoomEntity :: String -> DgState -> Entity.Entity
getCurrentRoomEntity name state =
    let
        room = getCurrentRoom state
    in
        case (Map.lookup name (Room.entities room)) of
            Nothing -> error "Unable to locate entity in current room"
            (Just x) -> x

getPropFromEntity :: Entity.Entity -> String -> Int
getPropFromEntity entity prop = case Map.lookup prop (Entity.stats entity) of
    Nothing -> error "Unable to locate entity prop"
    (Just x) -> x

getPropFromOwner :: Maybe String -> String -> DgState -> Int
getPropFromOwner Nothing _ _ = error "Property owner is undefined"
getPropFromOwner (Just "player") name state = getPropFromEntity (player state) name
getPropFromOwner (Just enemy) name state = getPropFromEntity (getCurrentRoomEntity enemy state) name

getPropVal :: Stat.Stat -> DgState -> Int
getPropVal Stat.Stat{Stat.owner="source", Stat.name=name} state = getPropFromOwner (source state) name state
getPropVal Stat.Stat{Stat.owner="target", Stat.name=name} state = getPropFromOwner (target state) name state
getPropVal _ _ = error "Invalid property owner"

updateEntityProp :: Maybe String -> String -> Int -> DgState -> DgState
updateEntityProp Nothing _ _ _ = error "Property owner is undefined in update"
updateEntityProp (Just "player") name val state =
    let p = (player state) in
        DgState {
            currentRoom=(currentRoom state),
            scope=(scope state),
            source=(source state),
            target=(target state),
            player=Entity.Entity {
                Entity.eType=(Entity.eType p),
                Entity.name=(Entity.name p),
                Entity.args=(Entity.args p),
                Entity.stats=(Map.insert name val (Entity.stats p)),
                Entity.alive=(Entity.alive p),
                Entity.actions=(Entity.actions p),
                Entity.triggers=(Entity.triggers p),
                Entity.items=(Entity.items p)
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
                    Room.Room {
                        Room.name=(Room.name curRoom),
                        Room.entities=(
                            Map.insert
                                enemy
                                Entity.Entity {
                                    Entity.eType=(Entity.eType entity),
                                    Entity.name=(Entity.name entity),
                                    Entity.args=(Entity.args entity),
                                    Entity.stats=(
                                        Map.insert
                                            name
                                            val
                                            (Entity.stats entity)),
                                    Entity.alive=(Entity.alive entity),
                                    Entity.actions=(Entity.actions entity),
                                    Entity.triggers=(Entity.triggers entity),
                                    Entity.items=(Entity.items entity)
                                }
                                (Room.entities curRoom)),
                        Room.items=(Room.items curRoom),
                        Room.doors=(Room.doors curRoom)
                    }
                    (rooms state)),
            running=(running state),
            rng=(rng state)
        }

updateProp :: Stat.Stat -> Int -> DgState -> DgState
updateProp Stat.Stat{Stat.owner="source", Stat.name=name} val state = updateEntityProp (source state) name val state
updateProp Stat.Stat{Stat.owner="target", Stat.name=name} val state = updateEntityProp (target state) name val state
updateProp _ _ _ = error "Invalid property owner in assign"

