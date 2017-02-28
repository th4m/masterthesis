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
evaluate st env [Handle e pes] =
  case evaluate st env [e] of
    (st', RErr (RRaise v)) -> evaluate_match st' env v pes v
    res                    -> res
evaluate st env [Con cn es]  =
  if do_con_check (c env) cn (fromIntegral (length es)) then
    case evaluate st env (reverse es) of
      (st', RVal vs) ->
        case build_conv (c env) cn (reverse vs) of
          Just v  -> (st', RVal [v])
          Nothing -> (st', RErr (RAbort RType_Error))
      res            -> res
  else
    (st, RErr (RAbort RType_Error))
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
        case do_opapp (reverse vs) of
          Just (env', e) ->
            (st', RErr (RAbort RTimeout_Error))
          Nothing ->
            (st', RErr (RAbort RType_Error))
      else
        case do_app (refs st) op (reverse vs) of
          Just (refs', r) -> (st'{refs = refs'}, list_result r)
          Nothing         -> (st', RErr (RAbort RType_Error))
    res -> res
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
evaluate st env [Mat e pes]     =
  case evaluate st env [e] of
    (st', RVal v) ->
      evaluate_match st' env (head v) pes bindv
    res           -> res
evaluate st env [Let xo e1 e2]  =
  case evaluate st env [e1] of
    (st', RVal v') ->
      evaluate st' env {v = opt_bind xo (head v') (v env)} [e2]
    res           -> res
evaluate st env [LetRec funs e] =
  if allDistinct (map (\(x,y,z) -> x) funs) then
    evaluate st (env {v = build_rec_env funs env (v env)}) [e]
  else
    (st, RErr (RAbort RType_Error))
evaluate st env [TAnnot e t]    = evaluate st env [e]

evaluate_match :: State -> Environment V -> V -> [(Pat, Exp)] -> V -> (State, Result [V] V)
evaluate_match st _env v []           err_v = (st, RErr (RRaise err_v))
evaluate_match st  env v' ((p,e):pes) err_v =
  if allDistinct (pat_bindings p []) then
    case pmatch (c env) (refs st) p v' (v env) of
      Match env_v'     -> evaluate st env {v = env_v'} [e]
      No_Match         -> evaluate_match st env v' pes err_v
      Match_Type_Error -> (st, RErr (RAbort RType_Error))
  else
    (st, RErr (RAbort RType_Error))


-- | evaluateSmall should take a Thunk and return a Thunk or a fully
--   evaluated value.
--   Represents small-steps semantics.
evaluateSmall :: State -> Environment V -> [Exp] -> (State, Result [V] V)
evaluateSmall st _env []             = (st, RVal [])
evaluateSmall st env (e1:e2:es)      =
  case evaluateSmall st env [e1] of
    (st', RVal v1) ->
      case evaluateSmall st' env (e2:es) of
        (st'', RVal vs) -> (st'', RVal (head v1:vs))
        res -> res
    res -> res
evaluateSmall st env [Raise e]       =
  case evaluateSmall st env [e] of
    (st', RVal v) -> case head v of
      Thunk env' e' -> (st', RVal [Thunk env' (Raise e')])
      v' -> (st', RErr (RRaise (head v)))
    res -> res
evaluateSmall st env [Handle e pes]  =
  case forceExpList st env [e] of
    (st', RErr (RRaise v)) -> undefined -- evaluate_match
    res                    -> res
evaluateSmall st env [Con cn es]     =
  if do_con_check (c env) cn (fromIntegral (length es)) then
    case forceExpList st env (reverse es) of
      (st', RVal vs) ->
        case build_conv (c env) cn (reverse vs) of
          Just v  -> (st', RVal [v])
          Nothing -> (st', RErr (RAbort RType_Error))
      res -> res
  else
    (st, RErr (RAbort RType_Error))
evaluateSmall st env [Var n]         =
  case lookup_var_id n env of
    Just v  -> (st, RVal [v])
    Nothing -> (st, RErr (RAbort RType_Error))
evaluateSmall st env [Fun x e]       = (st, RVal [Closure env x e])
evaluateSmall st env [Literal l]     = (st, RVal [LitV l])
evaluateSmall st env [App op es]     =
  case forceExpList st env es of
    (st', RVal vs) ->
      if op == OpApp then
        case do_opapp (reverse vs) of
          Just (env', e) ->
            (st', RErr (RAbort RTimeout_Error))
          Nothing ->
            (st', RErr (RAbort RType_Error))
      else
        case do_app (refs st) op (reverse vs) of
          Just (refs', r) -> (st'{refs = refs'}, list_result r)
          Nothing         -> (st', RErr (RAbort RType_Error))
evaluateSmall st env [Log lop e1 e2] =
  case evaluateSmall st env [e1] of
    (st', RVal v1) ->
      case do_log lop v1' e2 of
        Just (Exp e) -> (st'', RVal [Thunk env e2])
        Just (Val v) -> (st'', RVal [v])
        Nothing      -> (st'', RErr (RAbort RType_Error))
      where (st'', v1') = force st' (head v1)
    res -> res
evaluateSmall st env [Mat e pes]     = undefined
  -- case evaluateSmall st env [e] of
  --   (st', RVal v) -> case head v of 
  --     Thunk env' e' -> (st', RVal [Thunk env' (Mat e' pes)])
  --     val -> undefined --evaluate_match
  --   res -> res
evaluateSmall st env [Let xo e1 e2]  =
  case evaluateSmall st env [e1] of
    (st', RVal v') -> case force st' (head v') of
      (st'', val) -> evaluateSmall st' env {v = opt_bind xo val (v env)} [e2]
    res -> res
evaluateSmall st env [LetRec funs e] =
  if allDistinct (map (\(x,y,z) -> x) funs) then
    undefined
  else
    (st, RErr (RAbort RType_Error))
evaluateSmall st env [If e1 e2 e3]   =
  case evaluateSmall st env [e1] of
    (st', RVal v) ->
      case do_if v' e2 e3 of
        Just e  -> (st'', RVal [Thunk env e])
        Nothing -> (st'', RErr (RAbort RType_Error))
      where (st'', v') = force st' (head v)
    res -> res
evaluateSmall st env [TAnnot e t]    = evaluateSmall st env [e]

force :: State -> V -> (State, V)
force st (Thunk env e) = case evaluateSmall st env [e] of
  (st', RVal [Thunk env' e']) -> force st' (Thunk env' e')
  (st', RVal v) -> (st', head v)
force st v = (st, v)

forceExpList :: State -> Environment V -> [Exp] -> (State, Result [V] V)
forceExpList st _env []    = (st, RVal [])
forceExpList st env (e:es) =
  case evaluateSmall st env [e] of
    (st', RVal v') -> (st''', RVal (val:vals))
      where (st'' , val)  = force st' (head v')
            (st''', RVal vals) = forceExpList st'' env es
    res -> res
