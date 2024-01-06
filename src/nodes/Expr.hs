module Expr where

import Dice(Dice)
import Stat(Stat)

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

data Expr
    = BinOpExpr BinOp Expr Expr
    | UnOpExpr UnOp Expr
    | IntExpr Int
    | DiceExpr Dice
    | VarExpr String
    | StatExpr Stat deriving Show

