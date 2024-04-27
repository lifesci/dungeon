{
module Parser (parse) where

import Lexer
import Lib(rev, listToMap)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Command
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
    '+' { TAdd _ }
    '-' { TSub _ }
    '*' { TMul _ }
    '/' { TDiv _ }
    '%' { TMod _ }

    and { TAnd _ }
    or { TOr _ }
    not { TNot _ }
    eq { TEq _ }
    neq { TNeq _ }
    '>' { TGt _ }
    '<' { TLt _ }
    gte { TGte _ }
    lte { TLte _ }
    '(' { TLParen _ }
    ')' { TRParen _ }
    '{' { TLBrace _ }
    '}' { TRBrace _ }

    '=' { TAssign _ }

    ';' { TSemicolon _ }
    '.' { TDot _ }
    ',' { TComma _ }

    int { TInt _ $$ }
    id { TId _ $$ }
    dice { TRawDice _ $$ }

    let { TLet _ }
    if { TIf _ }
    else { TElse _ }
    while { TWhile _ }
    player { TPlayer _ }
    enemy { TEnemy _ }
    enemies { TEnemies _ }
    action { TAction _ }
    trigger { TTrigger _ }
    on { TOn _ }
    statblock { TStatblock _ }
    stats { TStats _ }
    item { TItem _ }
    items { TItems _ }
    doors { TDoors _ }
    to { TTo _ }
    requires { TRequires _ }
    room { TRoom _ }
    game { TGame _ }
    alive { TAlive _ }
    behaviour { TBehaviour _ }
    default { TDefault _ }
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
            EntityTemplate.triggers=(listToMap (rev $10) Trigger.name id),
            EntityTemplate.behaviour=[]
        }
    }

EnemyList
    : {- empty -} { [] }
    | EnemyList Enemy { $2 : $1 }

Enemy
    : enemy id '(' IdList ')' '{' stats '{' StatList '}' Alive ActionList TriggerList Behaviour '}' {
        let
            (behaviour, defaultBehaviour) = $14
        in
            EntityTemplate.EntityTemplate {
                EntityTemplate.eType = EntityTemplate.Enemy,
                EntityTemplate.name=$2,
                EntityTemplate.args=(rev $4),
                EntityTemplate.stats=(Map.fromList (rev $9)),
                EntityTemplate.alive=$11,
                EntityTemplate.actions=(listToMap (rev $12) Action.name id),
                EntityTemplate.triggers=(listToMap (rev $13) Trigger.name id),
                EntityTemplate.behaviour=behaviour,
                EntityTemplate.defaultBehaviour=defaultBehaviour
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

Action: action id '{' StmtList '}' {
    Action.Action {
        Action.name=$2,
        Action.stmts=(rev $4)
    }
}

Trigger: trigger id on '(' Expr ')' '{' StmtList '}' {
    Trigger.Trigger {
        Trigger.name=$2,
        Trigger.on=$5,
        Trigger.stmts=(rev $8)
    }
}

Behaviour: behaviour '{' BehaviourList DefaultBehaviour '}' { ((rev $3), $4) }

BehaviourList
    : {- empty -} { [] }
    | BehaviourList BehaviourItem { $2 : $1 }

BehaviourItem: Expr '=' BehaviourCommand ';' { ($1, $3) }

DefaultBehaviour: default '=' BehaviourCommand ';' { $3 }

BehaviourCommand
    : id id { Command.Command { Command.name=$1, Command.target=$2, Command.using=Nothing } }
    | id player { Command.Command { Command.name=$1, Command.target="player", Command.using=Nothing } }

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
    : {- empty -} { [] }
    | else '{' StmtList '}' { rev $3 }

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
parseError [] = error "Parse error at end of file"
parseError (tk:tks) =
    let
        AlexPn _ line col = tok_pos tk
    in
        error ("Unexpected token at line " ++ (show line) ++ ", column " ++ (show col))
}

