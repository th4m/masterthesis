module Compile where

import AbsCakeML


compile :: Exp -> Exp
compile (Raise e) = makeThunk $ Raise $ forceCompile e
compile (Handle e pes) = makeThunk $ Handle (forceCompile e) (compilePats pes)
compile (Con cn es) = Con cn $ map thunkCompile es
compile (Var n) = makeThunk $ Var n
compile (Fun x e) = makeVal $ Fun x (compile e) -- compile e?
compile (Literal l) = makeVal $ Literal l
compile (App op es) = case (op, es) of
  (OpApp, [e1, e2])  ->
    -- makeThunk $ App OpApp $ map forceCompile es -- ?

    Let (Just "E1") (forceCompile e1) $
    Let (Just "E2") (thunkCompile e2) $
    makeThunk $ App op [Var (Short "E1"), Var (Short "E2")]
  (OPN op, [_, _]) ->
    compOnOpn op es
  _ ->
    makeVal $ App op $ map forceCompile es -- ?
    -- Let (Just "E1") (forceCompile e1) $
    -- Let (Just "E2") (forceCompile e2) $
    -- makeVal $ App op [Var (Short "E1"), Var (Short "E2")]

compile (Log lop e1 e2) = -- Log lop (forceCompile e1) $ thunkCompile e2
  Let (Just "E1") (forceCompile e1) $
  Let (Just "E2") (forceCompile e2) $
  makeVal $ Log lop (Var (Short "E1")) (Var (Short "E2"))
compile (If e1 e2 e3) = If (forceCompile e1) (thunkCompile e2) (thunkCompile e3)
compile (Mat e pes) = Mat (forceCompile e) (compilePats pes)
compile (Let xo e1 e2) = Let xo (thunkCompile e1) (thunkCompile e2)
compile (LetRec funs e) =
  LetRec (replace funs invalidNames) (reinstance invalidNames names (thunkCompile e))
  where names = getNames funs
        getNames [] = []
        getNames ((f,_,_):funs') = (f:getNames funs')
        invalidNames = map (++ " ") names
  -- makeVal $
  -- LetRec (funsCompile funs) (thunkCompile e)
  -- Let (Just "E") (thunkCompile e) $
  -- LetRec (funsCompile funs) (Var (Short "E"))
compile (TAnnot e t) = TAnnot (thunkCompile e) t

forceCompile = force . compile
thunkCompile = makeThunk . compile

-- | Replace variable names in funs
replace :: [(VarN, VarN, Exp)] -> [VarN] -> [(VarN, VarN, Exp)]
replace [] _ = []
replace ((f,x,e'):funs') (n:ns) =
  ((n,x,compile (subst f n e')):replace funs' ns)
replace fs ns = error $ "replace: xs - " ++ show (length fs) ++ ", ys - " ++ show (length ns)

-- | Reinstance x as y
reinstance :: [VarN] -> [VarN] -> Exp -> Exp
reinstance [] [] e = e
reinstance (x:xs) (y:ys) e = Let (Just y) (makeVal (Var (Short x))) (reinstance xs ys e)
reinstance xs ys _ = error $ "reinstance: xs - " ++ show (length xs) ++ ", ys - " ++ show (length ys)

mapFun :: (Exp -> Exp) -> [(VarN, VarN, Exp)] -> [(VarN, VarN, Exp)]
mapFun _ [] = []
mapFun f ((v1,v2,e):funs) = ((v1,v2,f e):(mapFun f funs))

-- funsCompile :: [(VarN, VarN, Exp)] -> [(VarN, VarN, Exp)]
-- funsCompile = mapFun thunkCompile
-- funsCompile []           = []
-- funsCompile ((f,x,e):fs) = ((f,x,thunkCompile e): funsCompile fs)

-- | Changes names of variables
subst :: VarN -> VarN -> Exp -> Exp
subst old new (Var n) =
  case n of
    Short name ->
      if name == old then
        Var (Short new)
      else
        Var (Short name)
    Long m n -> undefined
subst o n (Raise e) =
  Raise (subst o n e)
subst o n (Handle e pes) =
  Handle (subst o n e) (map (mapPat (subst o n)) pes)
subst o n (Con cn es) =
  Con cn (map (subst o n) es)
subst o n (Fun x e) =
  Fun x (subst o n e)
subst _ _ (Literal l) =
  Literal l
subst o n (App op es) =
  App op (map (subst o n) es)
subst o n (Log lop e1 e2) =
  Log lop e1' e2'
  where [e1',e2'] = map (subst o n) [e1,e2]
subst o n (If e1 e2 e3) =
  If e1' e2' e3'
  where [e1',e2',e3'] = map (subst o n) [e1,e2,e3]
subst o n (Mat e pes) =
  Mat (subst o n e) (map (mapPat (subst o n)) pes)
subst o n (Let xo e1 e2) =
  Let xo e1' e2'
  where [e1',e2'] = map (subst o n) [e1,e2]
subst o n (LetRec funs e) =
  LetRec (mapFun (subst o n) funs) (subst o n e)
subst o n (TAnnot e t) =
  TAnnot (subst o n e) t

-- Might be used for pattern matching?
mapPat :: (Exp -> Exp) -> (Pat, Exp) -> (Pat, Exp)
mapPat f (p,e) = (p, f e)

compilePat :: (Pat, Exp) -> (Pat, Exp)
compilePat  = mapPat compile
compilePats = map compilePat

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
      Let (Just "XV") (forceCompile e1) $
      -- if XV == 0 then 0
      If (App Equality [Var (Short "XV"), Literal (IntLit 0)]) (makeVal (Literal (IntLit 0))) $
      -- else let YV == (forceCompile e2) in
      Let (Just "YV") (forceCompile e2) $
      -- XV + YV
      makeVal $ (App (OPN op) [Var (Short "XV"), Var (Short "YV")])
  | otherwise =
      -- let XV = (forceCompile e1) in
      Let (Just "XV") (forceCompile e1) $
      -- let YV = (forceCompile e2) in
      Let (Just "YV") (forceCompile e2) $
      -- XY + YV
      makeVal $ App (OPN op) [Var (Short "XV"), Var (Short "YV")]
