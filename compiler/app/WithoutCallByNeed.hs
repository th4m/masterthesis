module WithoutCallByNeed where

import TestComp (mkList
                ,applyCT
                ,applyCR
                ,infList
                ,infList1
                ,stopAtZero
                ,forceCons)
import AbsCakeML
import SemanticPrimitives
import Evaluate (evaluate
                ,evaluateLazy
                ,evalAndForce)
import Compile (compile
               ,force)
import qualified Data.Set as S

empty_env = Env {
  v = [],
  c = ([],[]),
  m = []
  }

empty_st = St {
  refs = empty_store,
  defined_types = S.empty,
  defined_mods = S.empty
  }

ex_env = empty_env {c =([], [("::", (2, TypeId (Short "list"))),
                             ("nil", (0, TypeId (Short "list"))),
                             ("Thunk", (1, TypeId (Short "lazy"))),
                             ("Val", (1, TypeId (Short "lazy")))])
                   }

ex :: Exp -> (State, Result [V] V)
ex e = evaluate empty_st ex_env [e]

exLazy :: Exp -> Result [V] V
exLazy e = evaluateLazy ex_env [e]

exForce :: Exp -> Result [V] V
exForce e = evalAndForce ex_env [e]

efc :: Exp -> (State, Result [V] V)
efc = ex . force . compile

-- Demo for strict semantics

list1to5 = ex $ mkList [1,2,3,4,5]

take3 = ex $ applyCT 3 (mkList [1,2,3,4,5])

infLoop = ex $ Let (Just "var") (infList1) (Literal (IntLit 0))















-- Demo for lazy semantics

lazyEval = exLazy $
  Let
  (Just "var")
  (App (OPN Plus) [Literal (IntLit 5), Literal (IntLit 5)])
  (Var (Short "var"))
forceEval = exForce $
  Let
  (Just "var")
  (App (OPN Plus) [Literal (IntLit 5), Literal (IntLit 5)])
  (Var (Short "var"))

letInfList = exForce $
  Let
  (Just "var")
  (infList1)
  (Literal (IntLit 0))











-- Demo for compiler without call-by-need

letInfListComp = efc $
  Let
  (Just "var")
  (infList1)
  (Literal (IntLit 0))

take3fromInf = forceCons $ efc $ applyCT 3 infList1















-- Demo to compare call-by-name and call-by-need
-- This is call-by-name!

okList = forceCons $ efc $
  applyCR
  (App OpApp [stopAtZero, Literal (IntLit 100)])
  5
