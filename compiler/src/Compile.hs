module Compile where

import AbsCakeML
-- import Evaluate


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
