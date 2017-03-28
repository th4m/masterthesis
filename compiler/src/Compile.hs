module Compile where

import AbsCakeML


compile :: Exp -> Exp
compile (Raise e) = undefined
compile (Handle e pes) = undefined
compile (Con cn es) = undefined
compile (Var n) = makeVal $ Var n
compile (Fun x e) = makeVal $ Fun x e
compile (Literal l) = makeVal $ Literal l
compile (App op es) = case op of
  OpApp  ->
    App OpApp es -- Should use thunk for second argument
  OPN op ->
    compOnOpn op es -- App (OPN op) $ map undefined undefined
  _ ->
    App op $ map (force . compile) es
compile (Log lop e1 e2) = undefined
compile (If e1 e2 e3) = undefined
compile (Mat e pes) = undefined
compile (Let xo e1 e2) =
  Let xo (makeThunk (compile e1)) (makeThunk (compile e2))
compile (LetRec funs e) = undefined
compile (TAnnot e t) = undefined


makeThunk :: Exp -> Exp
makeThunk e = Con (Just (Short "Thunk")) [Fun "" e]
makeVal :: Exp -> Exp
makeVal e = Con (Just (Short "Val")) [e]


force :: Exp -> Exp
force e =
  App OpApp [LetRec [("force", "exp"
                     , Mat (Var (Short "exp"))
                       [(thunkPat [PVar "Thunk"]
                        , App OpApp [Var (Short "force")
                                    , App OpApp [Var (Short "Thunk")
                                                , Literal (IntLit 0)]])
                       ,(valPat [PVar "Val"]
                        , Var (Short "Val"))]
                     )] (Var (Short "force"))
            , e]

thunkPat ps = PCon (Just (Short "Thunk")) ps
valPat   ps = PCon (Just (Short "Val"))   ps

thunkCon es = Con (Just (Short "Thunk")) es
litCon   es = Con (Just (Short "Val"))   es

expToPat :: Exp -> Pat
expToPat (Var n)      = case n of
  Short n  -> PVar n
  Long m n -> undefined
expToPat (Literal l)  = PLit l
expToPat (Con id ps)  = PCon id $ map expToPat ps
expToPat (TAnnot e t) = PTAnnot (expToPat e) t
-- ref-case?

compOnOpn :: Opn -> [Exp] -> Exp
compOnOpn op [e1, e2]
  | op `elem` [Times, Divide, Modulo] =
      -- let XV = (force (compile e1)) in
      Let (Just "XV") (force (compile e1)) $
      -- if XV == 0 then 0
      If (App Equality [Var (Short "XV"), Literal (IntLit 0)]) (Literal (IntLit 0)) $
      -- else let YV == (force (compile e2)) in XV + YV
      Let (Just "YV") (force (compile e2)) (App (OPN op) [Var (Short "XV"), Var (Short "YV")])
  | otherwise =
      -- let XV = (force (compile e1)) in
      Let (Just "XV") (force (compile e1)) $
      -- let YV = (force (compile e2)) in
      Let (Just "YV") (force (compile e2)) $
      -- XY + YV
      App (OPN op) [Var (Short "XV"), Var (Short "YV")]
