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
    doors { TDoors }
    to { TTo }
    requires { TRequires }
    room { TRoom }
%%

Statblock: statblock '{' StatList '}' { StatBlockNode { stats=(rev $3) } }

StatList
    : {- empty -} { []}
    | StatList Stat { $2 : $1 }

Stat: id '=' int ';' { StatNode { statName=$1, statVal=$3 } }

Player
    : player id '{' stats '{' StatList '}' '}' { EntityNode { entityType=Player, entityName=$2, entityStats=(rev $6) } }

Enemy
    : enemy id '{' stats '{' StatList '}' '}' { EntityNode { entityType=Enemy, entityName=$2, entityStats=(rev $6) } }

Stmt
    : let id '=' Expr ';' { DeclareNode Declare { declareVar=$2, declareVal=$4 } }
    | id '=' Expr ';' { AssignNode Assign { assignVar=$1, assignVal=$3 } }
    | Prop '=' Expr ';' { AssignPropNode AssignProp { assignPropProp=$1, assignPropVal=$3 } }
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
    | ExprList ',' Expr { $3 : $1 }

Expr
    : Expr '+' Factor { AddNode $1 $3 }
    | Expr '-' Factor { SubNode $1 $3 }
    | Expr or Factor { OrNode $1 $3 }
    | Expr '>' Factor { GtNode $1 $3 }
    | Expr '<' Factor { LtNode $1 $3 }
    | Expr gte Factor { GteNode $1 $3 }
    | Expr lte Factor { LteNode $1 $3 }
    | Factor { $1 }

Factor
    : Factor '*' Unary { MulNode $1 $3 }
    | Factor '/' Unary { DivNode $1 $3 }
    | Factor '%' Unary { ModNode $1 $3 }
    | Factor and Unary { AndNode $1 $3 }
    | Unary { $1 }

Unary
    : '-' Term { NegNode $2 }
    | not Term { NotNode $2 }
    | Term { $1 }

Term
    : '(' Expr ')' { $2 }
    | int { IntNode $1 }
    | dice { DiceNode Dice { diceCount=1, diceSize=1 } }
    | id { IdNode $1 }
    | Prop { $1 }

Prop : id '.' id { PropNode Prop { propVar=$1, propName=$3 } }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data StatBlockNode = StatBlockNode {
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
    entityStats :: [StatNode]
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

data ExprNode
    = AddNode ExprNode ExprNode
    | SubNode ExprNode ExprNode
    | OrNode ExprNode ExprNode
    | GtNode ExprNode ExprNode
    | LtNode ExprNode ExprNode
    | GteNode ExprNode ExprNode
    | LteNode ExprNode ExprNode
    | MulNode ExprNode ExprNode
    | DivNode ExprNode ExprNode
    | ModNode ExprNode ExprNode
    | AndNode ExprNode ExprNode
    | NegNode ExprNode
    | NotNode ExprNode
    | IntNode Int
    | DiceNode Dice
    | IdNode String
    | PropNode Prop deriving Show

}
