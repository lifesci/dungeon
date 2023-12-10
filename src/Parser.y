{
module Parser where
import Lexer
import Lib(rev, listToMap)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
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
%%

Dungeon: GameName Statblock Player EnemyList ItemList RoomList {
    DgNode {
        dgGame=$1,
        dgStatblock=(stats $2),
        dgPlayer=$3,
        dgEnemies=(rev $4),
        dgItems=(listToMap (rev $5) itemTemplateName id),
        dgRooms=(rev $6)
    }
}

GameName: game id ';' { GameNode $2 }

Statblock: statblock '{' StatList '}' { StatblockNode { stats=(listToMap (rev $3) statName statVal) } }

StatList
    : {- empty -} { []}
    | StatList Stat { $2 : $1 }

Stat: id '=' int ';' { StatNode { statName=$1, statVal=$3 } }

Player
    : player id '{' stats '{' StatList '}' ActionList TriggerList '}' {
        EntityTemplateNode {
            entityTemplateType=Player,
            entityTemplateName=$2,
            entityTemplateArgs=[],
            entityTemplateStats=(listToMap (rev $6) statName statVal),
            entityTemplateActions=(listToMap (rev $8) actionName id),
            entityTemplateTriggers=(listToMap (rev $9) triggerName id)
        }
    }

EnemyList
    : {- empty -} { [] }
    | EnemyList Enemy { $2 : $1 }

Enemy
    : enemy id '(' IdList ')' '{' stats '{' StatList '}' ActionList TriggerList '}' {
        EntityTemplateNode {
            entityTemplateType=Enemy,
            entityTemplateName=$2,
            entityTemplateArgs=(rev $4),
            entityTemplateStats=(listToMap (rev $9) statName statVal),
            entityTemplateActions=(listToMap (rev $11) actionName id),
            entityTemplateTriggers=(listToMap (rev $12) triggerName id)
        }
    }

ItemList
    : {- empty -} { [] }
    | ItemList Item { $2 : $1 }

Item
    : item id '<' IdList '>' '(' IdList ')' '{' ActionList '}' { ItemTemplateNode { itemTemplateName=$2, itemTemplateAttribs=(rev $4), itemTemplateArgs=(rev $7), itemTemplateActions=(listToMap (rev $10) actionName id) } }

RoomList
    : {- empty -} { [] }
    | RoomList Room { $2 : $1 }

Room: room id '{' RoomEnemies RoomItems Doors '}' {
    RoomTemplateNode {
        roomTemplateName=$2,
        roomTemplateEntities=$4,
        roomTemplateItems=$5,
        roomTemplateDoors=$6
    }
}

RoomEnemies: enemies '{' RoomEnemyList '}' { rev $3 }

RoomEnemyList
    : {- empty -} { [] }
    | RoomEnemyList RoomEnemy { $2 : $1 }

RoomEnemy: id '=' id '(' ExprList ')' ';' {
    RoomTemplateEntity {
        roomTemplateEntityName=$1,
        roomTemplateEntityType=$3,
        roomTemplateEntityArgs=(rev $5)
    }
}

RoomItems: items '{' RoomItemList '}' { rev $3 }

RoomItemList
    : {- empty -} { [] }
    | RoomItemList RoomItem { $2 : $1 }

RoomItem: id '=' id '(' ExprList ')' ';' {
    RoomTemplateItem {
        roomTemplateItemName=$1,
        roomTemplateItemType=$3,
        roomTemplateItemArgs=(rev $5)
    }
}

Doors: doors '{' DoorList '}' { rev $3 }

DoorList
    : {- empty -} { [] }
    | DoorList Door { $2 : $1 }

Door: id to id DoorReq ';' { Door { doorName=$1, doorTo=$3, doorReq=$4 } }

DoorReq
    : {- empty -} { IntNode 1 }
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

Action
    : action id targets '(' Expr ')' '{' StmtList '}' { ActionNode { actionName=$2, actionTargets=$5, actionStmts=(rev $8) } }

Trigger
    : trigger id on '(' Expr ')' '{' StmtList '}' { TriggerNode { triggerName=$2, triggerOn=$5, triggerStmts=$8 } }

Stmt
    : let id '=' Expr ';' { DeclareNode Declare { declareVar=$2, declareVal=$4 } }
    | id '=' Expr ';' { AssignNode Assign { assignVar=$1, assignVal=$3 } }
    | PropLiteral '=' Expr ';' { AssignPropNode AssignProp { assignPropProp=$1, assignPropVal=$3 } }
    | id '(' ExprList ')' ';' { FuncNode Func { funcName=$1, funcArgs=(rev $3) } }
    | while '(' Expr ')' '{' StmtList '}' { WhileNode While { whileCond=$3, whileStmts=(rev $6) } }
    | If { $1 }

If
    : if '(' Expr ')' '{' StmtList '}' ElseIfList Else { IfNode If { ifCond=$3, ifStmts=(rev $6), elseIfs=(rev $8), elseStmts=(rev $9) } }

ElseIfList
    : {- empty -} { [] }
    | ElseIfList ElseIf { $2 : $1 }

ElseIf
    : else if '(' Expr ')' '{' StmtList '}' { ElseIf { elseIfCond=$4, elseIfStmts=(rev $7) } }

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
    : Expr '+' Factor { BinOpNode Add $1 $3 }
    | Expr '-' Factor { BinOpNode Sub $1 $3 }
    | Expr or Factor { BinOpNode Or $1 $3 }
    | Expr '>' Factor { BinOpNode Gt $1 $3 }
    | Expr '<' Factor { BinOpNode Lt $1 $3 }
    | Expr gte Factor { BinOpNode Gte $1 $3 }
    | Expr lte Factor { BinOpNode Lte $1 $3 }
    | Factor { $1 }

Factor
    : Factor '*' Unary { BinOpNode Mul $1 $3 }
    | Factor '/' Unary { BinOpNode Div $1 $3 }
    | Factor '%' Unary { BinOpNode Mod $1 $3 }
    | Factor and Unary { BinOpNode And $1 $3 }
    | Unary { $1 }

Unary
    : '-' Term { UnOpNode Neg $2 }
    | not Term { UnOpNode Not $2 }
    | Term { $1 }

Term
    : '(' Expr ')' { $2 }
    | int { IntNode $1 }
    | dice { DiceNode Dice { diceCount=1, diceSize=1 } }
    | id { IdNode $1 }
    | PropLiteral { PropNode $1 }

PropLiteral : id '.' id { Prop { propVar=$1, propName=$3 } }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data DgNode = DgNode {
    dgGame :: GameNode,
    dgStatblock :: Map String Int,
    dgPlayer :: EntityTemplateNode,
    dgEnemies :: [EntityTemplateNode],
    dgItems :: Map String ItemTemplateNode,
    dgRooms :: [RoomTemplateNode]
} deriving Show

data GameNode = GameNode String deriving Show

data StatblockNode = StatblockNode {
    stats :: Map String Int
} deriving Show

data StatNode = StatNode {
    statName :: String,
    statVal :: Int
} deriving Show

data EntityType = Player | Enemy deriving Show

data EntityTemplateNode = EntityTemplateNode {
    entityTemplateType :: EntityType,
    entityTemplateName :: String,
    entityTemplateArgs :: [String],
    entityTemplateStats :: Map String Int,
    entityTemplateActions :: Map String ActionNode,
    entityTemplateTriggers :: Map String TriggerNode
} deriving Show

data ItemTemplateNode = ItemTemplateNode {
    itemTemplateName :: String,
    itemTemplateAttribs :: [String],
    itemTemplateArgs :: [String],
    itemTemplateActions :: Map String ActionNode
} deriving Show

data RoomTemplateNode = RoomTemplateNode {
    roomTemplateName :: String,
    roomTemplateEntities :: [RoomTemplateEntity],
    roomTemplateItems :: [RoomTemplateItem],
    roomTemplateDoors :: [Door]
} deriving Show

data RoomTemplateEntity = RoomTemplateEntity {
    roomTemplateEntityName :: String,
    roomTemplateEntityType :: String,
    roomTemplateEntityArgs :: [ExprNode]
} deriving Show

data RoomTemplateItem = RoomTemplateItem {
    roomTemplateItemName :: String,
    roomTemplateItemType :: String,
    roomTemplateItemArgs :: [ExprNode]
} deriving Show

data Door = Door {
    doorName :: String,
    doorTo :: String,
    doorReq :: ExprNode
} deriving Show

data ActionNode = ActionNode {
    actionName :: String,
    actionTargets :: ExprNode,
    actionStmts :: [StmtNode]
} deriving Show

data TriggerNode = TriggerNode {
    triggerName :: String,
    triggerOn :: ExprNode,
    triggerStmts :: [StmtNode]
} deriving Show

data Declare = Declare {
    declareVar :: String,
    declareVal :: ExprNode
} deriving Show

data Assign = Assign {
    assignVar :: String,
    assignVal :: ExprNode
} deriving Show

data AssignProp = AssignProp {
    assignPropProp :: Prop,
    assignPropVal :: ExprNode
} deriving Show

data Func = Func {
    funcName :: String,
    funcArgs :: [ExprNode]
} deriving Show

data While = While {
    whileCond :: ExprNode,
    whileStmts :: [StmtNode]
} deriving Show

data If = If {
    ifCond :: ExprNode,
    ifStmts :: [StmtNode],
    elseIfs :: [ElseIf],
    elseStmts :: [StmtNode]
} deriving Show

data ElseIf = ElseIf {
    elseIfCond :: ExprNode,
    elseIfStmts :: [StmtNode]
} deriving Show

data StmtNode
    = DeclareNode Declare
    | AssignNode Assign
    | AssignPropNode AssignProp
    | FuncNode Func
    | WhileNode While
    | IfNode If deriving Show

data Dice = Dice {
    diceCount :: Int,
    diceSize :: Int
} deriving Show

data Prop = Prop {
    propVar :: String,
    propName :: String
} deriving Show

data BinOp
    = Add
    | Sub
    | Or
    | Gt
    | Lt
    | Gte
    | Lte
    | Mul
    | Div
    | Mod
    | And deriving Show

data UnOp
    = Neg
    | Not deriving Show

data ExprNode
    = BinOpNode BinOp ExprNode ExprNode
    | UnOpNode UnOp ExprNode
    | IntNode Int
    | DiceNode Dice
    | IdNode String
    | PropNode Prop deriving Show

data Room = Room {
    roomName :: String,
    roomItems :: Map String Item,
    roomEntities :: Map String Entity,
    roomDoors :: [Door]
} deriving Show

data Entity = Entity {
    entityType :: EntityType,
    entityName :: String,
    entityArgs :: Map String ExprNode,
    entityStats :: Map String Int,
    entityActions :: Map String ActionNode,
    entityTriggers :: Map String TriggerNode
} deriving Show

data Item = Item {
    itemName :: String,
    itemAttribs :: Set String,
    itemArgs :: Map String ExprNode,
    itemActions :: Map String ActionNode
} deriving Show

populateRoom :: RoomTemplateNode -> [EntityTemplateNode] -> [ItemTemplateNode] -> StatblockNode -> Room
populateRoom rt ets its sb = Room {
    roomName=(roomTemplateName rt),
    roomEntities=(
        listToMap
            (
                map
                    (
                        entityFromTemplate
                            (listToMap ets entityTemplateName id)
                            sb
                    )
                    (roomTemplateEntities rt)
            )
            entityName
            id
    ),
    roomItems=Map.empty,
    roomDoors=(roomTemplateDoors rt)
}

entityFromTemplate :: Map String EntityTemplateNode -> StatblockNode -> RoomTemplateEntity -> Entity
entityFromTemplate ets sb rte =
    let
        template = case (Map.lookup (roomTemplateEntityType rte) ets) of
            Just x -> x
            Nothing -> error "Unknown entity type"
    in
        Entity {
            entityType=(entityTemplateType template),
            entityName=(roomTemplateEntityName rte),
            entityArgs=(
                Map.fromList (
                    zip
                        (entityTemplateArgs template)
                        (roomTemplateEntityArgs rte)
                )
            ),
            entityStats=(
                Map.union
                    (
                        Map.intersection
                            (entityTemplateStats template)
                            (stats sb)
                    )
                    (stats sb)
            ),
            entityActions=(entityTemplateActions template),
            entityTriggers=(entityTemplateTriggers template)
        }

}

