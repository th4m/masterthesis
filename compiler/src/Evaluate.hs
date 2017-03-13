module Evaluate where

import AbsCakeML
import SemanticPrimitives
import Lib

import Numeric.Natural

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

-- | evaluateLazy takes an expression and evaluates as little
--   as possible. 
evaluateLazy :: Environment V -> [Exp] -> Result [V] V
evaluateLazy _env []             = RVal []
evaluateLazy env (e1:e2:es)      =
  case evaluateLazy env [e1] of
    RVal v1 ->
      case evaluateLazy env (e2:es) of
        RVal vs -> RVal (head v1:vs)
        res -> res
    res -> res
evaluateLazy env [Raise e]       =
  case evaluateLazy env [e] of
    RVal v -> case head v of
      Thunk env' e' -> RVal [Thunk env' (Raise e')]
      v' -> RErr (RRaise v')
    res -> res
evaluateLazy env [Handle e pes]  =
  case evalAndForce env [e] of
    RErr (RRaise v) -> evaluate_match_small env v pes v
    res             -> res
evaluateLazy env [Con cn es]     =
  if do_con_check (c env) cn (fromIntegral (length es)) then
    case build_conv (c env) cn (reverse (map (Thunk env) es)) of
      Just v  -> RVal [v]
      Nothing -> RErr (RAbort RType_Error)
--res -> res
  else
    RErr (RAbort RType_Error)
evaluateLazy env [Var n]         =
  case lookup_var_id n env of
    Just v  -> RVal [v]
    Nothing -> RErr (RAbort RType_Error)
evaluateLazy env [Fun x e]       = RVal [Closure env x e]
evaluateLazy env [Literal l]     = RVal [LitV l]
evaluateLazy env [App op es]     =
  if op == OpApp then
    case evalAndForce env es of
      RVal vs ->
        case do_opapp vs of
          Just (env', e) ->
            evaluateLazy env' [e]
          Nothing ->
            RErr $ RAbort RType_Error
      res -> res
  else
    case op of
      OPN op ->
        evalOnOpn env op es
      VFromList ->
        case es of
          [e] ->
            case doAppLazy VFromList [Thunk env e] of
              Just r  -> list_result r
              Nothing -> RErr $ RAbort RType_Error
          _   -> RErr $ RAbort RType_Error -- Wrong amount of arguments
      _ ->
        case evalAndForce env es of
          RVal vs ->
            case doAppLazy op vs of
              Just r  -> list_result r
              Nothing -> RErr $ RAbort RType_Error
          res -> res
evaluateLazy env [Log lop e1 e2] =
  case evalAndForce env [e1] of
    RVal v1 -> case do_log lop (head v1) e2 of
      Just (Exp e) -> RVal [Thunk env e]
      Just (Val v) -> RVal [v]
      Nothing      -> RErr (RAbort RType_Error)
    res -> res
evaluateLazy env [Mat e pes]     =
  case evalAndForce env [e] of
    RVal v -> evaluate_match_small env (head v) pes bindv
    res -> res
evaluateLazy env [Let xo e1 e2]  =
  RVal [Thunk env {v = opt_bind xo (Thunk env e1) (v env)} e2]
  -- case evalAndForce env [e1] of
  --   RVal v' ->  evaluateLazy env {v = opt_bind xo (head v') (v env)} [e2]
  --   res -> res
evaluateLazy env [LetRec funs e] =
  if allDistinct (map (\(x,y,z) -> x) funs) then
    RVal [Thunk env {v = build_rec_env funs env (v env)} e]
    --evaluateLazy (env {v = build_rec_env funs env (v env)}) [e]
  else
    RErr (RAbort RType_Error)
evaluateLazy env [If e1 e2 e3]   =
  case evalAndForce env [e1] of
    RVal v ->
      case do_if (head v) e2 e3 of
        Just e  -> RVal [Thunk env e]
        Nothing -> RErr (RAbort RType_Error)
    res -> res
evaluateLazy env [TAnnot e t]    = evaluateLazy env [e]

evaluate_match_small :: Environment V -> V -> [(Pat, Exp)] -> V -> Result [V] V
evaluate_match_small _env _v []          err_v = RErr (RRaise err_v)
evaluate_match_small  env v' ((p,e):pes) err_v =
  if allDistinct (pat_bindings p []) then
    case pmatchLazy (c env) p v' (v env) of
      Match env_v'     -> evaluateLazy env {v = env_v'} [e]
      No_Match         -> evaluate_match_small env v' pes err_v
      Match_Type_Error -> RErr (RAbort RType_Error)
  else
    RErr (RAbort RType_Error)



force :: V -> Result [V] V
force (Thunk env e) = case evaluateLazy env [e] of
  RVal [Thunk env' e'] -> force (Thunk env' e')
  res -> res
force v = RVal [v]

evalAndForce :: Environment V -> [Exp] -> Result [V] V
evalAndForce _env []    = RVal []
evalAndForce env (e:es) =
  case evaluateLazy env [e] of
    RVal v' -> case force (head v') of
      RVal val ->
        case evalAndForce env es of
          RVal vs -> RVal ((head val):vs)
          res -> res
      res -> res
    res -> res

evalOnOpn :: Environment V -> Opn -> [Exp] -> Result [V] V
evalOnOpn _ _ [] = RVal []
evalOnOpn env Times  (e:es) =
  case evalAndForce env [e] of
    RVal v ->
      if (head v) == LitV (IntLit 0) then
        RVal v
      else
        case evalAndForce env es of
          RVal vs ->
            case doAppLazy (OPN Times) (head v: vs) of
              Just r  -> list_result r
              Nothing -> RErr $ RAbort RType_Error
          res     -> res
    res -> res
evalOnOpn env Divide (e:es) =
  case evalAndForce env [e] of
    RVal v ->
      if (head v) == LitV (IntLit 0) then
        RVal v
      else
        case evalAndForce env es of
          RVal vs ->
            case doAppLazy (OPN Divide) (head v: vs) of
              Just r  -> list_result r
              Nothing -> RErr $ RAbort RType_Error
          res     -> res
    res -> res
evalOnOpn env Modulo (e:es) =
  case evalAndForce env [e] of
    RVal v ->
      if (head v) == LitV (IntLit 0) then
        RVal v
      else
        case evalAndForce env es of
          RVal vs ->
            case doAppLazy (OPN Modulo) (head v: vs) of
              Just r  -> list_result r
              Nothing -> RErr $ RAbort RType_Error
          res     -> res
    res -> res
evalOnOpn env op      es    = -- Plus and Minus
  case evalAndForce env es of
    RVal vs ->
      case doAppLazy (OPN op) vs of
        Just r  -> list_result r
        Nothing -> RErr $ RAbort RType_Error
    res -> res



-- forceList :: Environment V -> [Exp] -> Result [V] V
-- forceList _   [] =
--   RVal []
-- forceList env [e@(Con (Just (Short cn)) es)] =
--   case evaluateLazy env [e] of
--     (RVal [ConV (Just ("::", TypeId (Short "nil"))) _]) ->
--       case forceList env es of
--         RVal vs -> RVal [ConV (Just (cn, TypeId (Short "nil"))) vs]
--         res -> res
--     res -> res
-- forceList _ _ =
--   RErr $ RAbort RType_Error

-- forceList :: Environment V -> [Exp] -> Natural -> V
-- forceList _   []     _ =
--   ConV (Just ("nil", TypeId (Short "list"))) []
-- --forceList env  es    0 = undefined
-- forceList env (e:es) n =
--   ConV (Just ("::", TypeId (Short "list"))) [head v, forceList env es (n-1)]
--   where RVal v = force (Thunk env e)
