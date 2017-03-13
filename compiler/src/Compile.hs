module Compile where

import AbsCakeML
-- import Evaluate


compile :: Exp -> Exp
compile (App op es) =
  if op == OpApp then
    undefined
  else
    case op of
      OPN opn ->
        convArith opn es

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
