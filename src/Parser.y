{
module Parser where
import Lexer
import Lib(rev)
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

Dungeon: GameName Statblock Player EnemyList ItemList RoomList { DgNode { dgGame=$1, dgStatblock=$2, dgPlayer=$3, dgEnemies=(rev $4), dgItems=(rev $5), dgRooms=(rev $6) } }

GameName: game id ';' { GameNode $2 }

Statblock: statblock '{' StatList '}' { StatblockNode { stats=(rev $3) } }

StatList
    : {- empty -} { []}
    | StatList Stat { $2 : $1 }

Stat: id '=' int ';' { StatNode { statName=$1, statVal=$3 } }

Player
    : player id '{' stats '{' StatList '}' ActionList TriggerList '}' { EntityNode { entityType=Player, entityName=$2, entityStats=(rev $6), entityActions=(rev $8), entityTriggers=(rev $9) } }

EnemyList
    : {- empty -} { [] }
    | EnemyList Enemy { $2 : $1 }

Enemy
    : enemy id '{' stats '{' StatList '}' ActionList TriggerList '}' { EntityNode { entityType=Enemy, entityName=$2, entityStats=(rev $6), entityActions=(rev $8), entityTriggers=(rev $9) } }

ItemList
    : {- empty -} { [] }
    | ItemList Item { $2 : $1 }

Item
    : item id '<' IdList '>' '{' ActionList '}' { ItemNode { itemName=$2, itemAttribs=(rev $4), itemActions=(rev $7) } }

RoomList
    : {- empty -} { [] }
    | RoomList Room { $2 : $1 }

Room: room id '{' RoomEnemies RoomItems Doors '}' { RoomNode { roomName=$2, roomEnemies=$4, roomItems=$5, doors=$6 } }

RoomEnemies: enemies '{' RoomEnemyList '}' { rev $3 }

RoomEnemyList
    : {- empty -} { [] }
    | RoomEnemyList RoomEnemy { $2 : $1 }

RoomEnemy: id '=' id ';' { RoomEnemy { roomEnemyName=$1, roomEnemyType=$3 } }

RoomItems: items '{' RoomItemList '}' { rev $3 }

RoomItemList
    : {- empty -} { [] }
    | RoomItemList RoomItem { $2 : $1 }

RoomItem: id '=' id { RoomItem { roomItemName=$1, roomItemType=$3 } }

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
    : action id targets '(' Expr ')' '{' StmtList '}' { ActionNode { actionName=$2, actionTargets=$5, actionStmts=$8 } }

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
    dgStatblock :: StatblockNode,
    dgPlayer :: EntityNode,
    dgEnemies :: [EntityNode],
    dgItems :: [ItemNode],
    dgRooms :: [RoomNode]
} deriving Show

data GameNode = GameNode String deriving Show

data StatblockNode = StatblockNode {
    stats :: [StatNode]
} deriving Show

data StatNode = StatNode {
    statName :: String,
    statVal :: Int
} deriving Show

data EntityType = Player | Enemy deriving Show

data EntityNode = EntityNode {
    entityType :: EntityType,
    entityName :: String,
    entityStats :: [StatNode],
    entityActions :: [ActionNode],
    entityTriggers :: [TriggerNode]
} deriving Show

data ItemNode = ItemNode {
    itemName :: String,
    itemAttribs :: [String],
    itemActions :: [ActionNode]
} deriving Show

data RoomNode = RoomNode {
    roomName :: String,
    roomEnemies :: [RoomEnemy],
    roomItems :: [RoomItem],
    doors :: [Door]
} deriving Show

data RoomEnemy = RoomEnemy {
    roomEnemyName :: String,
    roomEnemyType :: String
} deriving Show

data RoomItem = RoomItem {
    roomItemName :: String,
    roomItemType :: String
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

}
