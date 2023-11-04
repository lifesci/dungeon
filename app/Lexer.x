{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

@id = ($alpha) ($alpha | $digit | \_)*
@int = 0 | (\-?[1-9]($digit)*)
@dice = (@int) "d" (@int)

tokens :-
    $white+;

    "+" { \s -> TAdd }
    "-" { \s -> TSub }
    "*" { \s -> TMul }
    "/" { \s -> TDiv }
    "%" { \s -> TMod }

    "and" { \s -> TAnd }
    "or" { \s -> TOr }
    "not" { \s -> TNot }
    "==" { \s -> TEq }
    "!=" { \s -> TNeq }
    ">" { \s -> TGt }
    "<" { \s -> TLt }
    ">=" { \s -> TGte }
    "<=" { \s -> TLte }

    "(" { \s -> TLParen }
    ")" { \s -> TRParen }
    "{" { \s -> TLBrace }
    "}" { \s -> TRBrace }

    "=" { \s -> TAssign }

    ";" { \s -> TSemicolon }
    "." { \s -> TDot }
    "," { \s -> TComma }

    "let" { \s -> TLet }
    "if" { \s -> TIf }
    "else" { \s -> TElse }
    "while" { \s -> TWhile }
    "player" { \s -> TPlayer }
    "enemy" { \s -> TEnemy }
    "enemies" { \s -> TEnemies }
    "action" { \s -> TAction }
    "targets" { \s -> TTargets }
    "trigger" { \s -> TTrigger }
    "on" { \s -> TOn }
    "statblock" { \s -> TStatblock }
    "stats" { \s -> TStats }
    "item" { \s -> TItem }
    "items" { \s -> TItems }
    "doors" { \s -> TDoors }
    "to" { \s -> TTo }
    "requires" { \s -> TRequires }
    "room" { \s -> TRoom }

    @int { \s -> TInt (read s) }
    @id { \s -> TId s }
    @dice { \s -> TRawDice s }

{

data Token
    = TAdd
    | TSub
    | TMul
    | TDiv
    | TMod

    | TAnd
    | TOr
    | TNot
    | TEq
    | TNeq
    | TGt
    | TLt
    | TGte
    | TLte

    | TLParen
    | TRParen
    | TLBrace
    | TRBrace

    | TAssign

    | TSemicolon
    | TDot
    | TComma

    | TInt Int
    | TRawDice String

    | TId String

    | TLet
    | TIf
    | TElse
    | TWhile
    | TPlayer
    | TEnemy
    | TEnemies
    | TAction
    | TTargets
    | TTrigger
    | TOn
    | TStatblock
    | TStats
    | TItem
    | TItems
    | TDoors
    | TTo
    | TRequires
    | TRoom

    deriving (Eq, Show)

main = do
    s <- getContents
    print (alexScanTokens s)
}
