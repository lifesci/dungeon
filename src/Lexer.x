{
module Lexer (alexScanTokens, Token(..), AlexPosn(..), tok_pos) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

@id = ($alpha) ($alpha | $digit | \_)*
@int = 0 | (\-?[1-9]($digit)*)
@dice = (@int) "d" (@int)

tokens :-
    $white+;

    "+" { tok (\p _ -> TAdd p) }
    "-" { tok (\p _ -> TSub p) }
    "*" { tok (\p _ -> TMul p) }
    "/" { tok (\p _ -> TDiv p) }
    "%" { tok (\p _ -> TMod p) }

    "and" { tok (\p _ -> TAnd p) }
    "or" { tok (\p _ -> TOr p) }
    "not" { tok (\p _ -> TNot p) }
    "==" { tok (\p _ -> TEq p) }
    "!=" { tok (\p _ -> TNeq p) }
    ">" { tok (\p _ -> TGt p) }
    "<" { tok (\p _ -> TLt p) }
    ">=" { tok (\p _ -> TGte p) }
    "<=" { tok (\p _ -> TLte p) }

    "(" { tok (\p _ -> TLParen p) }
    ")" { tok (\p _ -> TRParen p) }
    "{" { tok (\p _ -> TLBrace p) }
    "}" { tok (\p _ -> TRBrace p) }

    "=" { tok (\p _ -> TAssign p) }

    ";" { tok (\p _ -> TSemicolon p) }
    "." { tok (\p _ -> TDot p) }
    "," { tok (\p _ -> TComma p) }

    "let" { tok (\p _ -> TLet p) }
    "if" { tok (\p _ -> TIf p) }
    "else" { tok (\p _ -> TElse p) }
    "while" { tok (\p _ -> TWhile p) }
    "player" { tok (\p _ -> TPlayer p) }
    "enemy" { tok (\p _ -> TEnemy p) }
    "enemies" { tok (\p _ -> TEnemies p) }
    "action" { tok (\p _ -> TAction p) }
    "targets" { tok (\p _ -> TTargets p) }
    "trigger" { tok (\p _ -> TTrigger p) }
    "on" { tok (\p _ -> TOn p) }
    "statblock" { tok (\p _ -> TStatblock p) }
    "stats" { tok (\p _ -> TStats p) }
    "item" { tok (\p _ -> TItem p) }
    "items" { tok (\p _ -> TItems p) }
    "doors" { tok (\p _ -> TDoors p) }
    "to" { tok (\p _ -> TTo p) }
    "requires" { tok (\p _ -> TRequires p) }
    "room" { tok (\p _ -> TRoom p) }
    "game" { tok (\p _ -> TGame p) }
    "alive" { tok (\p _ -> TAlive p) }
    "behaviour" { tok (\p _ -> TBehaviour p) }
    "default" { tok (\p _ -> TDefault p) }

    @int { tok (\p s -> TInt p (read s)) }
    @id { tok (\p s -> TId p s) }
    @dice { tok (\p s -> TRawDice p s) }

{
tok f p s = f p s

data Token
    = TAdd AlexPosn
    | TSub AlexPosn
    | TMul AlexPosn
    | TDiv AlexPosn
    | TMod AlexPosn

    | TAnd AlexPosn
    | TOr AlexPosn
    | TNot AlexPosn
    | TEq AlexPosn
    | TNeq AlexPosn
    | TGt AlexPosn
    | TLt AlexPosn
    | TGte AlexPosn
    | TLte AlexPosn

    | TLParen AlexPosn
    | TRParen AlexPosn
    | TLBrace AlexPosn
    | TRBrace AlexPosn

    | TAssign AlexPosn

    | TSemicolon AlexPosn
    | TDot AlexPosn
    | TComma AlexPosn

    | TInt AlexPosn Int
    | TRawDice AlexPosn String

    | TId AlexPosn String

    | TLet AlexPosn
    | TIf AlexPosn
    | TElse AlexPosn
    | TWhile AlexPosn
    | TPlayer AlexPosn
    | TEnemy AlexPosn
    | TEnemies AlexPosn
    | TAction AlexPosn
    | TTargets AlexPosn
    | TTrigger AlexPosn
    | TOn AlexPosn
    | TStatblock AlexPosn
    | TStats AlexPosn
    | TItem AlexPosn
    | TItems AlexPosn
    | TDoors AlexPosn
    | TTo AlexPosn
    | TRequires AlexPosn
    | TRoom AlexPosn
    | TGame AlexPosn
    | TAlive AlexPosn
    | TDefault AlexPosn
    | TBehaviour AlexPosn

    deriving (Eq, Show)

tok_pos :: Token -> AlexPosn
tok_pos (TAdd p) = p
tok_pos (TSub p) = p
tok_pos (TMul p) = p
tok_pos (TDiv p) = p
tok_pos (TMod p) = p

tok_pos (TAnd p) = p
tok_pos (TOr p) = p
tok_pos (TNot p) = p
tok_pos (TEq p) = p
tok_pos (TNeq p) = p
tok_pos (TGt p) = p
tok_pos (TLt p) = p
tok_pos (TGte p) = p
tok_pos (TLte p) = p

tok_pos (TLParen p) = p
tok_pos (TRParen p) = p
tok_pos (TLBrace p) = p
tok_pos (TRBrace p) = p

tok_pos (TAssign p) = p

tok_pos (TSemicolon p) = p
tok_pos (TDot p) = p
tok_pos (TComma p) = p

tok_pos (TInt p _) = p
tok_pos (TRawDice p _) = p

tok_pos (TId p _) = p

tok_pos (TLet p) = p
tok_pos (TIf p) = p
tok_pos (TElse p) = p
tok_pos (TWhile p) = p
tok_pos (TPlayer p) = p
tok_pos (TEnemy p) = p
tok_pos (TEnemies p) = p
tok_pos (TAction p) = p
tok_pos (TTargets p) = p
tok_pos (TTrigger p) = p
tok_pos (TOn p) = p
tok_pos (TStatblock p) = p
tok_pos (TStats p) = p
tok_pos (TItem p) = p
tok_pos (TItems p) = p
tok_pos (TDoors p) = p
tok_pos (TTo p) = p
tok_pos (TRequires p) = p
tok_pos (TRoom p) = p
tok_pos (TGame p) = p
tok_pos (TAlive p) = p
tok_pos (TDefault p) = p
tok_pos (TBehaviour p) = p
}

