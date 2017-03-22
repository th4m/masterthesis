module Compile where

import AbsCakeML
-- import Evaluate


compile :: Exp -> Exp
compile (Raise e) = undefined
compile (Handle e pes) = undefined
compile (Con cn es) = undefined
compile (Var n) = undefined
compile (Fun x e) = undefined
compile (Literal l) = Con (Just (Short "Val")) [Literal l]
compile (App op es) = case op of
  OPN op -> undefined
compile (Log lop e1 e2) = undefined
compile (If e1 e2 e3) = undefined
compile (Mat e pes) = undefined
compile (Let xo e1 e2) = undefined
compile (LetRec funs e) = undefined
compile (TAnnot e t) = undefined

makeThunk :: Exp -> Exp
makeThunk e = Con (Just (Short "Thunk")) [Fun "" e]

force = LetRec [("force", "exp"
               , Mat (Var (Short "exp"))
                 [(thunkPat [], undefined)
                  ,(litPat [] , undefined)]
               )]
        (Var (Short "force"))

thunkPat ps = PCon (Just (Short "Thunk")) ps
litPat ps = PCon (Just (Short "Val")) ps

compOnOpn :: Opn -> [Exp] -> Exp
compOnOpn op [e1, e2]
  | op `elem` [Times, Divide, Modulo] =
      undefined -- [force (makeThunk e1), force (makeThunk e2)]
  | otherwise =
      undefined
