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

    "+" { \_ -> TAdd }
    "-" { \_ -> TSub }
    "*" { \_ -> TMul }
    "/" { \_ -> TDiv }
    "%" { \_ -> TMod }

    "and" { \_ -> TAnd }
    "or" { \_ -> TOr }
    "not" { \_ -> TNot }
    "==" { \_ -> TEq }
    "!=" { \_ -> TNeq }
    ">" { \_ -> TGt }
    "<" { \_ -> TLt }
    ">=" { \_ -> TGte }
    "<=" { \_ -> TLte }

    "(" { \_ -> TLParen }
    ")" { \_ -> TRParen }
    "{" { \_ -> TLBrace }
    "}" { \_ -> TRBrace }

    "=" { \_ -> TAssign }

    ";" { \_ -> TSemicolon }
    "." { \_ -> TDot }
    "," { \_ -> TComma }

    "let" { \_ -> TLet }
    "if" { \_ -> TIf }
    "else" { \_ -> TElse }
    "while" { \_ -> TWhile }
    "player" { \_ -> TPlayer }
    "enemy" { \_ -> TEnemy }
    "enemies" { \_ -> TEnemies }
    "action" { \_ -> TAction }
    "targets" { \_ -> TTargets }
    "trigger" { \_ -> TTrigger }
    "on" { \_ -> TOn }
    "statblock" { \_ -> TStatblock }
    "stats" { \_ -> TStats }
    "item" { \_ -> TItem }
    "items" { \_ -> TItems }
    "doors" { \_ -> TDoors }
    "to" { \_ -> TTo }
    "requires" { \_ -> TRequires }
    "room" { \_ -> TRoom }
    "game" { \_ -> TGame }

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
    | TGame

    deriving (Eq, Show)

main = do
    s <- getContents
    print (alexScanTokens s)
}
