{
module Parser (parse) where

import Lexer
import Lib(rev, listToMap)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Dungeon
import qualified EntityTemplate
import qualified ItemTemplate
import qualified RoomTemplate
import qualified RoomTemplateEntity
import qualified RoomTemplateItem
import qualified Door
import qualified Action
import qualified Trigger
import qualified Declare
import qualified Assign
import qualified AssignStat
import qualified Func
import qualified If
import qualified While
import qualified Stmt
import qualified Expr
import qualified Dice
import qualified Stat
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    '+' { TAdd }
    '-' { TSub }
    '*' { TMul }
    '/' { TDiv }
    '%' { TMod }

    and { TAnd }
    or { TOr }
    not { TNot }
    eq { TEq }
    neq { TNeq }
    '>' { TGt }
    '<' { TLt }
    gte { TGte }
    lte { TLte }
    '(' { TLParen }
    ')' { TRParen }
    '{' { TLBrace }
    '}' { TRBrace }

    '=' { TAssign }

    ';' { TSemicolon }
    '.' { TDot }
    ',' { TComma }

    int { TInt $$ }
    id { TId $$ }
    dice { TRawDice $$ }

    let { TLet }
    if { TIf }
    else { TElse }
    while { TWhile }
    player { TPlayer }
    enemy { TEnemy }
    enemies { TEnemies }
    action { TAction }
    targets { TTargets }
    trigger { TTrigger }
    on { TOn }
    statblock { TStatblock }
    stats { TStats }
    item { TItem }
    items { TItems }
    doors { TDoors }
    to { TTo }
    requires { TRequires }
    room { TRoom }
    game { TGame }
    alive { TAlive }
-- Dungeon: GameName Statblock Player EnemyList ItemList RoomList {
%%

Dungeon: GameName Statblock Player EnemyList ItemList RoomList {
    Dungeon.Dungeon {
        Dungeon.name=$1,
        Dungeon.statblock=$2,
        Dungeon.playerTemplate=$3,
        Dungeon.enemyTemplates=(listToMap (rev $4) EntityTemplate.name id),
        Dungeon.itemTemplates=(listToMap (rev $5) ItemTemplate.name id),
        Dungeon.roomTemplates=(listToMap (rev $6) RoomTemplate.name id)
    }
}

GameName: game id ';' { $2 }

Statblock: statblock '{' StatList '}' { Map.fromList (rev $3) }

StatList
    : {- empty -} { []}
    | StatList Stat { $2 : $1 }

Stat: id '=' int ';' { ($1, $3) }

Alive: alive '=' Expr ';' { $3 }

Player
    : player id '{' stats '{' StatList '}' Alive ActionList TriggerList '}' {
        EntityTemplate.EntityTemplate {
            EntityTemplate.eType = EntityTemplate.Player,
            EntityTemplate.name=$2,
            EntityTemplate.args=[],
            EntityTemplate.stats=(Map.fromList (rev $6)),
            EntityTemplate.alive=$8,
            EntityTemplate.actions=(listToMap (rev $9) Action.name id),
            EntityTemplate.triggers=(listToMap (rev $10) Trigger.name id)
        }
    }

EnemyList
    : {- empty -} { [] }
    | EnemyList Enemy { $2 : $1 }

Enemy
    : enemy id '(' IdList ')' '{' stats '{' StatList '}' Alive ActionList TriggerList '}' {
        EntityTemplate.EntityTemplate {
            EntityTemplate.eType = EntityTemplate.Enemy,
            EntityTemplate.name=$2,
            EntityTemplate.args=(rev $4),
            EntityTemplate.stats=(Map.fromList (rev $9)),
            EntityTemplate.alive=$11,
            EntityTemplate.actions=(listToMap (rev $12) Action.name id),
            EntityTemplate.triggers=(listToMap (rev $13) Trigger.name id)
        }
    }

ItemList
    : {- empty -} { [] }
    | ItemList Item { $2 : $1 }

Item
    : item id '<' IdList '>' '(' IdList ')' '{' ActionList '}' {
        ItemTemplate.ItemTemplate {
            ItemTemplate.name=$2,
            ItemTemplate.attribs=(rev $4),
            ItemTemplate.args=(rev $7),
            ItemTemplate.actions=(listToMap (rev $10) Action.name id)
        }
    }

RoomList
    : {- empty -} { [] }
    | RoomList Room { $2 : $1 }

Room: room id '{' RoomEnemies RoomItems Doors '}' {
    RoomTemplate.RoomTemplate {
        RoomTemplate.name=$2,
        RoomTemplate.entities=$4,
        RoomTemplate.items=$5,
        RoomTemplate.doors=$6
    }
}

RoomEnemies: enemies '{' RoomEnemyList '}' { rev $3 }

RoomEnemyList
    : {- empty -} { [] }
    | RoomEnemyList RoomEnemy { $2 : $1 }

RoomEnemy: id '=' id '(' ExprList ')' ';' {
    RoomTemplateEntity.RoomTemplateEntity {
        RoomTemplateEntity.name=$1,
        RoomTemplateEntity.template=$3,
        RoomTemplateEntity.args=(rev $5)
    }
}

RoomItems: items '{' RoomItemList '}' { rev $3 }

RoomItemList
    : {- empty -} { [] }
    | RoomItemList RoomItem { $2 : $1 }

RoomItem: id '=' id '(' ExprList ')' ';' {
    RoomTemplateItem.RoomTemplateItem {
        RoomTemplateItem.name=$1,
        RoomTemplateItem.template=$3,
        RoomTemplateItem.args=(rev $5)
    }
}

Doors: doors '{' DoorList '}' { rev $3 }

DoorList
    : {- empty -} { [] }
    | DoorList Door { $2 : $1 }

Door: id to id DoorReq ';' {
    Door.Door {
        Door.name=$1,
        Door.to=$3,
        Door.req=$4
    }
}

DoorReq
    : {- empty -} { Expr.IntExpr 1 }
    | requires '(' Expr ')' { $3 }

IdList
    : {- empty -} { [] }
    | NonEmptyIdList { $1 }

NonEmptyIdList
    : id { [$1] }
    | NonEmptyIdList ',' id { $3 : $1 }

ActionList
    : {- empty -} { [] }
    | ActionList Action { $2 : $1 }

TriggerList
    : {- empty -} { [] }
    | TriggerList Trigger { $2 : $1 }

Action: action id targets '(' Expr ')' '{' StmtList '}' {
    Action.Action {
        Action.name=$2,
        Action.targets=$5,
        Action.stmts=(rev $8)
    }
}

Trigger: trigger id on '(' Expr ')' '{' StmtList '}' {
    Trigger.Trigger {
        Trigger.name=$2,
        Trigger.on=$5,
        Trigger.stmts=(rev $8)
    }
}

Stmt
    : let id '=' Expr ';' { Stmt.DeclareStmt Declare.Declare { Declare.var=$2, Declare.val=$4 } }
    | id '=' Expr ';' { Stmt.AssignStmt Assign.Assign { Assign.var=$1, Assign.val=$3 } }
    | PropLiteral '=' Expr ';' {
        Stmt.AssignStatStmt AssignStat.AssignStat {
            AssignStat.stat=$1,
            AssignStat.val=$3
        }
    }
    | id '(' ExprList ')' ';' { Stmt.FuncStmt Func.Func { Func.name=$1, Func.args=(rev $3) } }
    | while '(' Expr ')' '{' StmtList '}' {
        Stmt.WhileStmt While.While {
            While.cond=$3,
            While.stmts=(rev $6)
        }
    }
    | If { $1 }

If
    : if '(' Expr ')' '{' StmtList '}' ElseIfList Else {
        Stmt.IfStmt If.If {
            If.conds=((($3, $6):(rev $8))++[(Expr.IntExpr 1, $9)])
        }
    }

ElseIfList
    : {- empty -} { [] }
    | ElseIfList ElseIf { $2 : $1 }

ElseIf
    : else if '(' Expr ')' '{' StmtList '}' { ($4, rev $7) }

Else
    : else '{' StmtList '}' { rev $3 }

StmtList
    : {- empty -} { [] }
    | StmtList Stmt { $2 : $1 }

ExprList
    : {- empty -} { [] }
    | NonEmptyExprList { $1 }

NonEmptyExprList
    : Expr { [$1] }
    | NonEmptyExprList ',' Expr { $3 : $1 }

Expr
    : Expr '+' Factor { Expr.BinOpExpr Expr.Add $1 $3 }
    | Expr '-' Factor { Expr.BinOpExpr Expr.Sub $1 $3 }
    | Expr or Factor { Expr.BinOpExpr Expr.Or $1 $3 }
    | Expr '>' Factor { Expr.BinOpExpr Expr.Gt $1 $3 }
    | Expr '<' Factor { Expr.BinOpExpr Expr.Lt $1 $3 }
    | Expr gte Factor { Expr.BinOpExpr Expr.Gte $1 $3 }
    | Expr lte Factor { Expr.BinOpExpr Expr.Lte $1 $3 }
    | Factor { $1 }

Factor
    : Factor '*' Unary { Expr.BinOpExpr Expr.Mul $1 $3 }
    | Factor '/' Unary { Expr.BinOpExpr Expr.Div $1 $3 }
    | Factor '%' Unary { Expr.BinOpExpr Expr.Mod $1 $3 }
    | Factor and Unary { Expr.BinOpExpr Expr.And $1 $3 }
    | Unary { $1 }

Unary
    : '-' Term { Expr.UnOpExpr Expr.Neg $2 }
    | not Term { Expr.UnOpExpr Expr.Not $2 }
    | Term { $1 }

Term
    : '(' Expr ')' { $2 }
    | int { Expr.IntExpr $1 }
    | dice { Expr.DiceExpr (Dice.fromStr $1) }
    | id { Expr.VarExpr $1 }
    | PropLiteral { Expr.StatExpr $1 }

PropLiteral : id '.' id { Stat.Stat { Stat.owner=$1, Stat.name=$3 } }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

-- data DgNode = DgNode {
--     dgGame :: GameNode,
--     dgStatblock :: Map String Int,
--     dgPlayerTemplate :: EntityTemplateNode,
--     dgPlayer :: Entity,
--     dgEnemies :: [EntityTemplateNode],
--     dgItems :: Map String ItemTemplateNode,
--     dgRooms :: Map String Room
-- } deriving Show
-- 
-- data GameNode = GameNode String deriving Show
-- 
-- data StatblockNode = StatblockNode {
--     stats :: Map String Int
-- } deriving Show
-- 
-- data StatNode = StatNode {
--     statName :: String,
--     statVal :: Int
-- } deriving Show
-- 
-- data EntityType = Player | Enemy deriving Show
-- 
-- data EntityTemplateNode = EntityTemplateNode {
--     entityTemplateType :: EntityType,
--     entityTemplateName :: String,
--     entityTemplateArgs :: [String],
--     entityTemplateStats :: Map String Int,
--     entityTemplateAlive :: ExprNode,
--     entityTemplateActions :: Map String ActionNode,
--     entityTemplateTriggers :: Map String TriggerNode
-- } deriving Show
-- 
-- data ItemTemplateNode = ItemTemplateNode {
--     itemTemplateName :: String,
--     itemTemplateAttribs :: [String],
--     itemTemplateArgs :: [String],
--     itemTemplateActions :: Map String ActionNode
-- } deriving Show
-- 
-- data RoomTemplateNode = RoomTemplateNode {
--     roomTemplateName :: String,
--     roomTemplateEntities :: [RoomTemplateEntity],
--     roomTemplateItems :: [RoomTemplateItem],
--     roomTemplateDoors :: [Door]
-- } deriving Show
-- 
-- data RoomTemplateEntity = RoomTemplateEntity {
--     roomTemplateEntityName :: String,
--     roomTemplateEntityType :: String,
--     roomTemplateEntityArgs :: [ExprNode]
-- } deriving Show
-- 
-- data RoomTemplateItem = RoomTemplateItem {
--     roomTemplateItemName :: String,
--     roomTemplateItemType :: String,
--     roomTemplateItemArgs :: [ExprNode]
-- } deriving Show
-- 
-- -- data Door = Door {
-- --     doorName :: String,
-- --     doorTo :: String,
-- --     doorReq :: ExprNode
-- -- } deriving Show
-- 
-- data ActionNode = ActionNode {
--     actionName :: String,
--     actionTargets :: ExprNode,
--     actionStmts :: [StmtNode]
-- } deriving Show
-- 
-- data TriggerNode = TriggerNode {
--     triggerName :: String,
--     triggerOn :: ExprNode,
--     triggerStmts :: [StmtNode]
-- } deriving Show
-- 
-- data Declare = Declare {
--     declareVar :: String,
--     declareVal :: ExprNode
-- } deriving Show
-- 
-- data Assign = Assign {
--     assignVar :: String,
--     assignVal :: ExprNode
-- } deriving Show
-- 
-- data AssignProp = AssignProp {
--     assignPropProp :: Prop,
--     assignPropVal :: ExprNode
-- } deriving Show
-- 
-- data Func = Func {
--     funcName :: String,
--     funcArgs :: [ExprNode]
-- } deriving Show
-- 
-- data While = While {
--     whileCond :: ExprNode,
--     whileStmts :: [StmtNode]
-- } deriving Show
-- 
-- data If = If {
--     ifConds :: [(ExprNode, [StmtNode])]
-- } deriving Show
-- 
-- data ElseIf = ElseIf {
--     elseIfCond :: ExprNode,
--     elseIfStmts :: [StmtNode]
-- } deriving Show
-- 
-- data StmtNode
--     = DeclareNode Declare
--     | AssignNode Assign
--     | AssignPropNode AssignProp
--     | FuncNode Func
--     | WhileNode While
--     | IfNode If deriving Show
-- 
-- data Dice = Dice {
--     diceCount :: Int,
--     diceSize :: Int
-- } deriving Show
-- 
-- data Prop = Prop {
--     propVar :: String,
--     propName :: String
-- } deriving Show
-- 
-- data BinOp
--     = Add
--     | Sub
--     | Or
--     | Gt
--     | Lt
--     | Gte
--     | Lte
--     | Mul
--     | Div
--     | Mod
--     | And deriving Show
-- 
-- data UnOp
--     = Neg
--     | Not deriving Show
-- 
-- data ExprNode
--     = BinOpNode BinOp ExprNode ExprNode
--     | UnOpNode UnOp ExprNode
--     | IntNode Int
--     | DiceNode Dice
--     | IdNode String
--     | PropNode Prop deriving Show
-- 
-- data Room = Room {
--     roomName :: String,
--     roomEntities :: Map String Entity,
--     roomItems :: Map String Item,
--     roomDoors :: [Door]
-- } deriving Show
-- 
-- data Entity = Entity {
--     entityType :: EntityType,
--     entityName :: String,
--     entityArgs :: Map String ExprNode,
--     entityStats :: Map String Int,
--     entityAlive :: ExprNode,
--     entityActions :: Map String ActionNode,
--     entityTriggers :: Map String TriggerNode,
--     entityItems :: Map String Item
-- } deriving Show
-- 
-- data Item = Item {
--     itemName :: String,
--     itemAttribs :: Set String,
--     itemArgs :: Map String ExprNode,
--     itemActions :: Map String ActionNode
-- } deriving Show
-- 
-- populateRoom :: [EntityTemplateNode] -> [ItemTemplateNode] -> StatblockNode -> RoomTemplateNode -> Room
-- populateRoom ets its sb rt = Room {
--     roomName=(roomTemplateName rt),
--     roomEntities=(
--         let entityTemplateMap = (listToMap ets entityTemplateName id) in
--         listToMap
--             (
--                 map
--                     (
--                         entityFromTemplate
--                             entityTemplateMap
--                             sb
--                     )
--                     (roomTemplateEntities rt)
--             )
--             entityName
--             id
--         ),
--     roomItems=(
--         let itemTemplateMap = (listToMap its itemTemplateName id) in
--         listToMap
--             (
--                 map
--                     (itemFromTemplate itemTemplateMap)
--                     (roomTemplateItems rt)
--             )
--             itemName
--             id
--     ),
--     roomDoors=(roomTemplateDoors rt)
-- }
-- 
-- itemFromTemplate :: Map String ItemTemplateNode -> RoomTemplateItem -> Item
-- itemFromTemplate its rti =
--     let
--         template = case (Map.lookup (roomTemplateItemType rti) its) of
--             Just x -> x
--             Nothing -> error "Unknown item type"
--     in
--         Item {
--             itemName=(roomTemplateItemName rti),
--             itemAttribs=(Set.fromList (itemTemplateAttribs template)),
--             itemArgs=Map.fromList (
--                     zip
--                         (itemTemplateArgs template)
--                         (roomTemplateItemArgs rti)
--                 ),
--             itemActions=(itemTemplateActions template)
--         }
-- 
-- entityFromTemplate :: Map String EntityTemplateNode -> StatblockNode -> RoomTemplateEntity -> Entity
-- entityFromTemplate ets sb rte =
--     let
--         template = case (Map.lookup (roomTemplateEntityType rte) ets) of
--             Just x -> x
--             Nothing -> error "Unknown entity type"
--     in
--         Entity {
--             entityType=(entityTemplateType template),
--             entityName=(roomTemplateEntityName rte),
--             entityArgs=(
--                 Map.fromList (
--                     zip
--                         (entityTemplateArgs template)
--                         (roomTemplateEntityArgs rte)
--                 )
--             ),
--             entityStats=(getEntityStats sb template),
--             entityAlive=(entityTemplateAlive template),
--             entityActions=(entityTemplateActions template),
--             entityTriggers=(entityTemplateTriggers template),
--             entityItems=Map.empty
--         }
-- 
-- getEntityStats :: StatblockNode -> EntityTemplateNode -> Map String Int
-- getEntityStats sb template =
--     Map.union
--         (
--             Map.intersection
--                 (entityTemplateStats template)
--                 (stats sb)
--         )
--         (stats sb)
-- 
}

