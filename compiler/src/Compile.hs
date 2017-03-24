module Compile where

import AbsCakeML


compile :: Exp -> Exp
compile (Raise e) = undefined
compile (Handle e pes) = undefined
compile (Con cn es) = undefined
compile (Var n) = makeVal $ Var n
compile (Fun x e) = undefined
compile (Literal l) = makeVal $ Literal l
compile (App op es) = case op of
  OpApp ->
    App OpApp es -- Should use thunk for second argument
  OPN op ->
    App (OPN op) $ map (force) es
  op' ->
    App op' es
compile (Log lop e1 e2) = undefined
compile (If e1 e2 e3) = undefined
compile (Mat e pes) = undefined
compile (Let xo e1 e2) =
  Let xo comp $ (force . compile) e2
  where comp = compile (App OpApp [Fun "" e1, Literal (IntLit 0)])
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
      undefined -- [force (makeThunk e1), force (makeThunk e2)]
  | otherwise =
      undefined
