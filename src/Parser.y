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
        Dungeon.roomTemplates=rev $6
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
    : item id '<' IdList '>' '(' IdList ')' '{' ActionList TriggerList'}' {
        ItemTemplate.ItemTemplate {
            ItemTemplate.name=$2,
            ItemTemplate.attribs=(rev $4),
            ItemTemplate.args=(rev $7),
            ItemTemplate.actions=(listToMap (rev $10) Action.name id),
            ItemTemplate.triggers=(listToMap (rev $11) Trigger.name id)
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
}

