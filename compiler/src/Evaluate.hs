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
            -- (st', RErr (RAbort RTimeout_Error))
            evaluate st' env' [e]
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
evaluate_match st _env _v []          err_v = (st, RErr (RRaise err_v))
evaluate_match st  env v' ((p,e):pes) err_v =
  if allDistinct (pat_bindings p []) then
    case pmatch (c env) (refs st) p v' (v env) of
      Match env_v'     -> evaluate st env {v = env_v'} [e]
      No_Match         -> evaluate_match st env v' pes err_v
      Match_Type_Error -> (st, RErr (RAbort RType_Error))
  else
    (st, RErr (RAbort RType_Error))



------- Lazy Semantics ------

-- | evaluateSmall should take a Thunk and return a Thunk or a fully
--   evaluated value.
evaluateSmall :: Environment V -> [Exp] -> Result [V] V
evaluateSmall _env []             = RVal []
evaluateSmall env (e1:e2:es)      =
  case evaluateSmall env [e1] of
    RVal v1 ->
      case evaluateSmall env (e2:es) of
        RVal vs -> RVal (head v1:vs)
        res -> res
    res -> res
evaluateSmall env [Raise e]       =
  case evaluateSmall env [e] of
    RVal v -> case head v of
      Thunk env' e' -> RVal [Thunk env' (Raise e')]
      v' -> RErr (RRaise v')
    res -> res
evaluateSmall env [Handle e pes]  =
  case forceExpList env [e] of
    RErr (RRaise v) -> evaluate_match_small env v pes v
    res             -> res
evaluateSmall env [Con cn es]     =
  if do_con_check (c env) cn (fromIntegral (length es)) then
    case forceExpList env (reverse es) of
      RVal vs ->
        case build_conv (c env) cn (reverse vs) of
          Just v  -> RVal [v]
          Nothing -> RErr (RAbort RType_Error)
      res -> res
  else
    RErr (RAbort RType_Error)
evaluateSmall env [Var n]         =
  case lookup_var_id n env of
    Just v  -> RVal [v]
    Nothing -> RErr (RAbort RType_Error)
evaluateSmall env [Fun x e]       = RVal [Closure env x e]
evaluateSmall env [Literal l]     = RVal [LitV l]
evaluateSmall env [App op es]     =
  case forceExpList env (reverse es) of
    RVal vs ->
      if op == OpApp then
        case do_opapp (reverse vs) of
          Just (env', e) ->
            evaluateSmall env' [e]
            -- (st', RErr (RAbort RTimeout_Error))
          Nothing ->
            RErr (RAbort RType_Error)
      else
        case doAppLazy op (reverse vs) of
          Just r   -> (list_result r)
          Nothing  -> RErr (RAbort RType_Error)
    res -> res
evaluateSmall env [Log lop e1 e2] =
  case forceExpList env [e1] of
    RVal v1 -> case do_log lop (head v1) e2 of
      Just (Exp e) -> RVal [Thunk env e]
      Just (Val v) -> RVal [v]
      Nothing      -> RErr (RAbort RType_Error)
    res -> res
evaluateSmall env [Mat e pes]     =
  case forceExpList env [e] of
    RVal v -> evaluate_match_small env (head v) pes bindv
    res -> res
evaluateSmall env [Let xo e1 e2]  =
  case forceExpList env [e1] of
    RVal v' ->  evaluateSmall env {v = opt_bind xo (head v') (v env)} [e2]
    res -> res
evaluateSmall env [LetRec funs e] =
  if allDistinct (map (\(x,y,z) -> x) funs) then
    evaluateSmall (env {v = build_rec_env funs env (v env)}) [e]
  else
    RErr (RAbort RType_Error)
evaluateSmall env [If e1 e2 e3]   =
  case forceExpList env [e1] of
    RVal v ->
      case do_if (head v) e2 e3 of
        Just e  -> RVal [Thunk env e]
        Nothing -> RErr (RAbort RType_Error)
    res -> res
evaluateSmall env [TAnnot e t]    = evaluateSmall env [e]

evaluate_match_small :: Environment V -> V -> [(Pat, Exp)] -> V -> Result [V] V
evaluate_match_small _env _v []          err_v = RErr (RRaise err_v)
evaluate_match_small  env v' ((p,e):pes) err_v =
  if allDistinct (pat_bindings p []) then
    case pmatchLazy (c env) p v' (v env) of
      Match env_v'     -> evaluateSmall env {v = env_v'} [e]
      No_Match         -> evaluate_match_small env v' pes err_v
      Match_Type_Error -> RErr (RAbort RType_Error)
  else
    RErr (RAbort RType_Error)



force :: V -> Result [V] V
force (Thunk env e) = case evaluateSmall env [e] of
  RVal [Thunk env' e'] -> force (Thunk env' e')
  res -> res
force v = RVal [v]

forceExpList :: Environment V -> [Exp] -> Result [V] V
forceExpList _env []    = RVal []
forceExpList env (e:es) =
  case evaluateSmall env [e] of
    RVal v' -> case force (head v') of
      RVal val ->
        case forceExpList env es of
          RVal vs -> RVal ((head val):vs)
          res -> res
      res -> res
    res -> res
