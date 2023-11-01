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

    "+" { \s -> Add }
    "-" { \s -> Sub }
    "*" { \s -> Mul }
    "/" { \s -> Div }
    "%" { \s -> Mod }

    "and" { \s -> And }
    "or" { \s -> Or }
    "not" { \s -> Not }
    "==" { \s -> Eq }
    "!=" { \s -> Neq }
    ">" { \s -> Gt }
    "<" { \s -> Lt }
    ">=" { \s -> Gte }
    "<=" { \s -> Lte }

    "(" { \s -> LParen }
    ")" { \s -> RParen }
    "{" { \s -> LBrace }
    "}" { \s -> RBrace }

    "=" { \s -> Assign }

    ";" { \s -> Semicolon }

    "let" { \s -> Let }
    "if" { \s -> If }
    "else" { \s -> Else }
    "while" { \s -> While }
    "player" { \s -> Player }
    "enemy" { \s -> Enemy }
    "enemies" { \s -> Enemies }
    "action" { \s -> Action }
    "targets" { \s -> Targets }
    "trigger" { \s -> Trigger }
    "on" { \s -> On }
    "statblock" { \s -> Statblock }
    "stats" { \s -> Stats }
    "item" { \s -> Item }
    "items" { \s -> Items }
    "doors" { \s -> Doors }
    "to" { \s -> To }
    "requires" { \s -> Requires }
    "room" { \s -> Room }

    @int { \s -> Int (read s) }
    @id { \s -> Id s }
    @dice { \s -> Dice s }

{

data Token
    = Add
    | Sub
    | Mul
    | Div
    | Mod

    | And
    | Or
    | Not
    | Eq
    | Neq
    | Gt
    | Lt
    | Gte
    | Lte

    | LParen
    | RParen
    | LBrace
    | RBrace

    | Assign

    | Semicolon

    | Int Int
    | Dice String
    
    | Id String

    | Let
    | If
    | Else
    | While
    | Player
    | Enemy
    | Enemies
    | Action
    | Targets
    | Trigger
    | On
    | Statblock
    | Stats
    | Item
    | Items
    | Doors
    | To
    | Requires
    | Room

    deriving (Eq, Show)

main = do
    s <- getContents
    print (alexScanTokens s)
}
