{
module Parser where
import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    '+' { Add }
    '-' { Sub }
    '*' { Mul }
    '/' { Div }
    '%' { Mod }

    and { And }
    or { Or }
    not { Not }
    eq { Eq }
    neq { Neq }
    '>' { Gt }
    '<' { Lt }
    gte { Gte }
    lte { Lte }
    '(' { LParen }
    ')' { RParen }
    '{' { LBrace }
    '}' { RBrace }

    '=' { Assign }

    ';' { Semicolon }

    int { Int $$ }
    id { Id $$ }
    dice { Dice $$ }

    let { Let }
    if { If }
    else { Else }
    while { While }
    player { Player }
    enemy { Enemy }
    enemies { Enemies }
    action { Action }
    targets { Targets }
    trigger { Trigger }
    on { On }
    statblock { Statblock }
    stats { Stats }
    item { Item }
    doors { Doors }
    to { To }
    requires { Requires }
    room { Room }
%%

Statblock: statblock '{' StatList '}' { StatBlockNode { stats=$3 } }

StatList
    : {- empty -} { [] }
    | StatList Stat { $2 : $1 }

Stat: id '=' int ';' { StatNode { name=$1, val=$3 } }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data StatBlockNode = StatBlockNode {
    stats :: [StatNode]
} deriving Show

data StatNode = StatNode {
    name :: String,
    val :: Int
} deriving Show

}
