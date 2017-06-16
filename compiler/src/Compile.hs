module Compile where

import AbsCakeML


compile :: Exp -> Exp
compile (Raise e) = makeThunk $ Raise $ forceCompile e
compile (Handle e pes) = makeThunk $ Handle (forceCompile e) (compilePats pes)
compile (Con cn es) = makeVal $ Con cn $ map thunkCompile es
compile (Var n) = Var n
compile (Fun x e) = makeVal $ Fun x (compile e)
compile (Literal l) = makeVal $ Literal l
compile (App op es) = case (op, es) of
  (OpApp, [e1, e2])  ->
    makeThunk $ App op [forceCompile e1, thunkCompile e2]
  _ ->
    makeVal $ App op $ map forceCompile es
compile (Log lop e1 e2) =
  makeVal $ Log lop (forceCompile e1) (forceCompile e2)
compile (If e1 e2 e3) = If (forceCompile e1) (thunkCompile e2) (thunkCompile e3)
compile (Mat e pes) = Mat (forceCompile e) (compilePats pes)
compile (Let xo e1 e2) = Let xo (thunkCompile e1) (thunkCompile e2)
compile (LetRec funs e) =
  LetRec (recVals funs) (repl (compile e) (fst3 funs))
  where
    fst3 []     = []
    fst3 ((f,_,_):fs) = (f:fst3 fs)
    repl e []     = e
    repl e (f:fs) =
      Let (Just f) (makeVal (Var (Short f))) (repl e fs)
    recVals []           = []
    recVals ((f,x,e):xs) =
      (f,x,
       Let (Just f)
       (makeVal (Var (Short f)))
       (compile e))
      :(recVals xs)
compile (TAnnot e t) = TAnnot (thunkCompile e) t

forceCompile = force . compile
thunkCompile = makeThunk . compile


-- Might be used for pattern matching?
mapPat :: (Exp -> Exp) -> (Pat, Exp) -> (Pat, Exp)
mapPat f (p,e) = (p, f e)

compilePat :: (Pat, Exp) -> (Pat, Exp)
compilePat  = mapPat compile
compilePats = map compilePat

makeThunk :: Exp -> Exp
makeThunk e = Con (Just (Short "Thunk"))
              [App OpRef [Con (Just (Short "RefExp")) [Fun "" e]]]
makeVal :: Exp -> Exp
makeVal e = Con (Just (Short "Val")) [e]

makeRefVal :: Exp -> Exp
makeRefVal e = Con (Just (Short "RefVal")) [e]

force :: Exp -> Exp
force e =
  App OpApp [LetRec [("force", "exp"
                     , Mat (Var (Short "exp"))
                       [(thunkPat [PVar "TPtr"]
                        ,refMat (App OpDeref [Var (Short "TPtr")]))
                       ,(valPat [PVar "Val"]
                        , Var (Short "Val"))]
                     )] (Var (Short "force"))
            , e]

refMat :: Exp -> Exp
refMat e =
  Mat (e)
  [(refValPat [PVar "RefV"]
   ,Var (Short "RefV"))
  ,(refExpPat [PVar "RefE"]
   ,Let Nothing (App OpAssign [Var (Short "TPtr"), makeRefVal sol])
     (getVal (App OpDeref [Var (Short "TPtr")]))
   )
  ]
  where sol = App OpApp [Var (Short "force"),
                         App OpApp [Var (Short "RefE")
                                   ,Literal (IntLit 0)]]
        getVal e' = Mat e'
                    [(refValPat [PVar "RVal"]
                     ,Var (Short "RVal"))]

thunkPat ps = PCon (Just (Short "Thunk")) ps
valPat   ps = PCon (Just (Short "Val"))   ps

refValPat ps = PCon (Just (Short "RefVal")) ps
refExpPat ps = PCon (Just (Short "RefExp")) ps
