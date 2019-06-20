module Syntax where

import Prelude
import Data.Tuple (Tuple)

type Var = String

data Expr
  = Var Var
  | App Expr Expr
  | Lam Var Expr
  | Let Var Expr Expr
  | Lit Lit
  | If Expr Expr Expr
  | Fix Expr
  | Op Binop Expr Expr
  | RecOp Recop String
derive instance   eqExpr :: Eq   (Expr)
derive instance  ordExpr :: Ord  (Expr)


data Lit
  = LInt Int
  | LBool Boolean
derive instance   eqLit :: Eq   (Lit)
derive instance  ordLit :: Ord  (Lit)

data Binop = Add | Sub | Mul | Eql
derive instance   eqBinop :: Eq   (Binop)
derive instance  ordBinop :: Ord  (Binop)

data Recop = SetField | GetField | DelField
derive instance   eqRecop :: Eq   (Recop)
derive instance  ordRecop :: Ord  (Recop)

type Decl = Tuple String Expr

data Program = Program (Array Decl) Expr
