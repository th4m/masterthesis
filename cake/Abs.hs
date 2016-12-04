module Abs where

newtype Ident = Ident String deriving (Eq, Ord, Show)

data Stmt = Dec Ident Stmt
          | Exp Expr
          | Break
          | Seq Stmt Stmt
          | If Expr Stmt Stmt
          | For Expr Expr Stmt
          deriving (Show)

data Expr = Var Ident
          | Num Int
          | Add Expr Expr
          | Assign Ident Expr
          deriving (Show)

data R = Rval Int
       | Rbreak
       | Rfail
       deriving (Show)
