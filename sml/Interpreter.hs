module Interpreter where

import Abs
import Prelude hiding (lookup)

type State = [(Ident, R)]


-- | Lookup a variable from the state
lookup :: Ident -> State -> Maybe R
lookup y []          = Nothing
lookup y ((x, r):xs) = case y == x of
  True  -> Just r
  False -> lookup y xs

replace :: (Ident, R) -> State -> State
replace (x,_) [] = error $ "Could not find the identifier "
                   ++ getName x ++ " in the state."
replace id@(x,r) ((x',r'):xs) = if x == x' then
                             (x,r):xs
                           else
                             (x',r'):(replace id xs)

getName :: Ident -> String
getName (Ident n) = n

-- | Executes an expression
run_e :: State -> Expr -> (R, State)
run_e s (Var x)      = case lookup x s of
  Nothing -> (Rfail, s)
  Just a  -> (a, s)
run_e s (Num i)      = (Rval i, s)
run_e s (Add e1 e2)  = case run_e s e1 of
  (Rval n1, s1) -> case run_e s1 e2 of
    (Rval n2, s2) -> (Rval (n1 + n2), s2)
    r             -> r
  r             -> r
run_e s (Assign x e) = case run_e s e of
  (Rval n1, s1) -> (Rval n1, replace (x, (Rval n1)) s1)
  r             -> r


-- | Executes a statement
run_t :: State -> Stmt -> (R, State)
run_t s (Exp e)       = run_e s e
run_t s (Dec x t)     = run_t ((x,(Rval 0)):s) t
run_t s  Break        = (Rbreak, s)
run_t s (Seq t1 t2)   = case run_t s t1 of
  (Rval _, s1) -> run_t s1 t2
  r            -> r
run_t s (If e t1 t2)  = case run_e s e of
  (Rval n1, s1) -> if n1 == 0 then
                     run_t s1 t2
                   else
                     run_t s1 t1
  r             -> r
run_t s (For e1 e2 t) = case run_e s e1 of
  (Rval n1, s1) -> if n1 == 0 then
                     (Rval 0, s1)
                   else
                     case run_t s1 t of
                      (Rval _, s2) -> case run_e s2 e2 of
                        (Rval _, s3) -> run_t s3 (For e1 e2 t)
                        r            -> r
                      (Rbreak, s2) -> (Rval 0, s2)
                      r            -> r
  r             -> r
