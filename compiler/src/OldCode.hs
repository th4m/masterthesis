evaluateSmall :: State -> Environment V -> [Exp] -> (State, Result [V] V)
evaluateSmall st env [Handle e pes]  =
  case evaluateSmall st env [e] of
    (st', RVal v) -> case head v of
      Thunk env' e' -> (st', RVal [Thunk env' (Handle e' pes)])
      v' -> (st', RVal [v'])
    (st', RErr (RRaise v)) -> undefined --evaluate_match
    res -> res
evaluateSmall st env [App op es]     =
  if op == OpApp then
    undefined --do_opapp
  else
    case es of -- es should contain two elements
      [Literal l1, Literal l2] ->
        case do_app (refs st) op [LitV l1, LitV l2] of
          Just (refs', r) -> (st{refs = refs'}, list_result r)
          Nothing         -> (st, RErr (RAbort RType_Error))
      [Literal l1, e2'] -> case evaluateSmall st env [e2'] of
        (st', RVal v) ->
          case head v of
            Thunk env' e -> (st', RVal [Thunk env' (App op [Literal l1, e])])
            LitV l2      -> (st', RVal [Thunk env  (App op [Literal l1, Literal l2])])
            v -> (st', RErr (RAbort RType_Error))
      [e1', e2']         ->
        case evaluateSmall st env [e1'] of
          (st', RVal v) ->
            case head v of
              Thunk env' e -> (st', RVal [Thunk env' (App op [e, e2'])])
              LitV l1      -> (st', RVal [Thunk env  (App op [Literal l1, e2'])])
              v' -> (st', RErr (RAbort RType_Error))
evaluateSmall st env [Log lop e1 e2] =
  case evaluateSmall st env [e1] of
    (st', RVal v1) ->
      case head v1 of
        Thunk env' e1' -> (st', RVal [Thunk env' (Log lop e1' e2)])
        v1' -> case do_log lop v1' e2 of
          Just (Exp e) -> evaluateSmall st' env [e2]
          Just (Val v) -> (st', RVal [v])
          Nothing      -> (st', RErr (RAbort RType_Error))
evaluateSmall st env [If e1 e2 e3]   =
  case evaluateSmall st env [e1] of
    (st', RVal vs) -> case head vs of
      Thunk env' e -> (st', RVal [Thunk env' (If e e2 e3)])
      v            -> case do_if v e2 e3 of
        Just e  -> (st', RVal [Thunk env e])
        Nothing -> (st', RErr (RAbort RType_Error))
    res -> res
evaluateSmall st env [Mat e pes]     =
  case evaluateSmall st env [e] of
    (st', RVal v) -> case head v of 
      Thunk env' e' -> (st', RVal [Thunk env' (Mat e' pes)])
      val -> undefined --evaluate_match
    res -> res
evaluateSmall st env [Let xo e1 e2]  =
  case evaluateSmall st env [e1] of
    (st', RVal v') -> case head v' of
      Thunk env' e -> (st', RVal [Thunk env' (Let xo e e2)])
      val -> evaluateSmall st' env {v = opt_bind xo val (v env)} [e2]
    res -> res
