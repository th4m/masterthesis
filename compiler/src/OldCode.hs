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


----------- COMPILE -----------
compile :: Exp -> Exp
compile (Raise e) = Raise $ compile e
compile (Handle e pes) = Handle (compile e) pes
compile (Con cn es) = Con cn $ map compile es
compile (Var n) = Var n
compile (Fun x e) = Fun x $ compile e
compile (Literal l) = Literal l
compile (App op es) =
  if op == OpApp then
    case es of
      [Fun x e1, e2] ->
        compile $ replace x e2 e1 -- already handled by evaluate?
      -- Recursion?
      _ ->
        App OpApp $ map compile es
  else
    case op of
      OPN opn ->
        convArith opn es
      _ ->
        App op $ map compile es
compile (Log lop e1 e2) = Log lop (compile e1) (compile e2)
compile (If e1 e2 e3) = If (compile e1) (compile e2) (compile e3)
compile (Mat e pes) = Mat (compile e) pes
compile (Let xo e1 e2) = case xo of
  Just n ->
    replace n e1 $ compile e2
  Nothing ->
    undefined -- Just do a normal let with e1 and e2?
compile (LetRec funs e) = LetRec funs (compile e) -- Should compile funs too? Should use replace?
compile (TAnnot e t) = TAnnot (compile e) t


replace :: VarN -> Exp -> Exp -> Exp
replace vn e (Var n) =
  case n of
    Short n ->
      if vn == n then
        e
      else
        Var (Short n)
    Long mn n ->
      undefined -- Should it be possible to replace variables from other modules?
replace vn e (Raise e') =
  Raise $ replace vn e e'
replace vn e (Handle e' pes) =
  Handle (replace vn e e') pes
replace vn e (Con cn es) =
  Con cn $ map (replace vn e) es
replace vn e (Fun x e') =
  Fun x $ replace vn e e'
replace _  _ (Literal l) =
  Literal l
replace vn e (App op es) =
  App op $ map (replace vn e) es
replace vn e (Log lop e1 e2) =
  Log lop e1' e2'
  where [e1', e2'] = map (replace vn e) [e1, e2]
replace vn e (If e1 e2 e3) =
  If e1' e2' e3'
  where [e1', e2', e3'] = map (replace vn e) [e1, e2, e3]
replace vn e (Mat e' pes) =
  Mat (replace vn e e') pes
replace vn e (Let xo e1 e2) =
  Let xo e1' e2'
  where [e1', e2'] = map (replace vn e) [e1, e2]
replace vn e (LetRec funs e') =
  LetRec funs $ replace vn e e' -- Should it replace in each definition in funs?
replace vn e (TAnnot e' t) =
  TAnnot (replace vn e e') t


convArith :: Opn -> [Exp] -> Exp
convArith op [e1, e2]
  | op `elem` [Times, Divide, Modulo] =
      checkZero op e1 e2
  | otherwise =
      App (OPN op) [compile e1, compile e2]
  where checkZero op' e1' e2' =
          case compile e1' of
            Literal (IntLit 0) ->
              Literal $ IntLit 0
            e ->
              App (OPN op') [e, compile e2']
convArith op es =
  error $ "Arity for " ++ show op ++ " is 2. Got " ++ show (length es) ++ "."

