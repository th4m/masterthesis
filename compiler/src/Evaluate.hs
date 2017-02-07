module Evaluate where

import AbsCakeML
import SemanticPrimitives
import Lib



list_result :: Result a b -> Result [a] b
list_result (RVal v) = RVal [v]
list_result (RErr e) = RErr e

evaluate :: State -> Environment V -> [Exp] -> (State, Result [V] V)
evaluate st _env []          = (st, RVal [])
evaluate st env (e1:e2:es)   =
  case evaluate st env [e1] of
    (st', RVal v1) ->
      case evaluate st' env (e2:es) of
        (st'', RVal vs) -> (st'', RVal (head v1:vs))
        res -> res
    res -> res
evaluate st env [Raise e]    =
  case evaluate st env [e] of
    (st', RVal v) -> (st', RErr (RRaise (head v)))
    res           -> res
evaluate st env [Handle e pes] = undefined
evaluate st env [Con cn es]    = undefined
evaluate st env [Var n]      =
  case lookup_var_id n env of
    Just v  -> (st, RVal [v])
    Nothing -> (st, RErr (RAbort RType_Error))
evaluate st env [Fun x e] = (st, RVal [Closure env x e])
evaluate st _env [Literal l] = (st, RVal [LitV l])
evaluate st env  [App op es] =
  case evaluate st env (reverse es) of
    (st', RVal vs) ->
      if op == OpApp then
        undefined
      else
        case do_app (refs st) op (reverse vs) of
          Just (refs', r) -> (st'{refs = refs'}, list_result r)
          Nothing         -> (st', RErr (RAbort RType_Error))
evaluate st env [Log lop e1 e2] =
  case evaluate st env [e1] of
    (st', RVal v1) ->
      case do_log lop (head v1) e2 of
        Just (Exp e) -> evaluate st' env [e]
        Just (Val v) -> (st', RVal [v])
        Nothing      -> (st', RErr (RAbort RType_Error))
    res -> res
evaluate st env [If e1 e2 e3] =
  case evaluate st env [e1] of
    (st', RVal v) ->
      case do_if (head v) e2 e3 of
        Just e  -> evaluate st' env [e]
        Nothing -> (st', RErr (RAbort RType_Error))
    res -> res
evaluate st env [Mat e pes]     = undefined
evaluate st env [Let xo e1 e2]  =
  case evaluate st env [e1] of
    (st', RVal v') ->
      evaluate st' env {v = opt_bind xo (head v') (v env)} [e2]
    res           -> res
evaluate st env [LetRec funs e] = undefined
evaluate st env [TAnnot e t]    = evaluate st env [e]
