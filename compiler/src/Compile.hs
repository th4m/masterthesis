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
    App OpApp $ map compile es
  else
    case op of
      OPN opn ->
        convArith opn es
compile (Log lop e1 e2) = Log lop (compile e1) (compile e2)
compile (If e1 e2 e3) = If (compile e1) (compile e2) (compile e3)
compile (Mat e pes) = Mat (compile e) pes
compile (Let xo e1 e2) = undefined
compile (LetRec funs e) = undefined
compile (TAnnot e t) = TAnnot (compile e) t


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
