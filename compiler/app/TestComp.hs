module TestComp where

import AbsCakeML
import SemanticPrimitives
import Evaluate
import Compile
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

apa = Var (Short "apa")
bepa = Var (Short "bepa")

true = App (OPB LEq) [Literal (IntLit 0), Literal (IntLit 1)]
false = App (OPB GEq) [Literal (IntLit 0), Literal (IntLit 1)]

insertVarIntoEnv :: Environment V -> VarN -> V -> Environment V
insertVarIntoEnv env varn v' =
  env {v = (varn, v'):(v env)}

-- | Returns an environment with variables "apa" = IntLit 7
--   and "bepa" = IntLit 5
ex_env = insertVarIntoEnv env1 "apa" (ConV (Just ("Val",TypeId (Short "lazy"))) [LitV (IntLit 7)])
  where env1 = insertVarIntoEnv env2 "bepa" (ConV (Just ("Val",TypeId (Short "lazy"))) [LitV (IntLit 5)])
        env2 = empty_env {c =
                          ([], [("::", (2, TypeId (Short "list"))),
                               ("nil", (0, TypeId (Short "list"))),
                               ("Thunk", (1, TypeId (Short "lazy"))),
                               ("Val", (1, TypeId (Short "lazy")))])
                         }

-- | Insert an expression, and this function will run it with an
--   example environment containing some variables
ex :: Exp -> (State, Result [V] V)
ex e = evaluate empty_st ex_env [e]

-- | Test list of expressions
exs :: [Exp] -> (State, Result [V] V)
exs es = evaluate empty_st ex_env es

exLazy :: Exp -> Result [V] V
exLazy e = evaluateLazy ex_env [e]

exLazys :: [Exp] -> Result [V] V
exLazys es = evaluateLazy ex_env es

exForce :: Exp -> Result [V] V
exForce e = evalAndForce ex_env [e]

exForces :: [Exp] -> Result [V] V
exForces es = evalAndForce ex_env es

efc :: Exp -> (State, Result [V] V)
efc = ex . force . compile
---------------------------------------------------------------------

compareEval :: Exp -> Bool
compareEval e = strict == lazy
  where (_, strict) = efc e
        lazy        = exForce e

-- Expressions to test

recEx1 =
  LetRec [("fun", "par", If (Var (Short "par")) appFun false)] (Var (Short "fun"))
  where appFun = (App OpApp [(Var (Short "fun")), true])
recEx2 =
  LetRec [("fun", "par", App (OPN Times) [Var (Short "par"), appFun])] (Var (Short "fun"))
  where appFun = (App OpApp [Var (Short "fun"), Var (Short "par")])
-- timesEx1 = App OpApp [recEx2, (Literal (IntLit 0))]

letEx1 = Let (Just "let1") (App (OPN Times) [Literal (IntLit 3), Literal (IntLit 523)]) (Var (Short "let1"))
letEx2 = Let (Just "let2") (App OpApp [recEx1, true]) (Literal (IntLit 0))

-- The following examples use undefined to test evaluation steps

-- If the second argument is evaluated, an error should be returned
mulEx1 = App (OPN Times) [Literal (IntLit 0), undefined]

-- Force needed after compile
letEx3 = Let (Just "let3") undefined (Literal (IntLit 0))

-- Force needed after compile
ifEx1  = If true (Literal (IntLit 0)) undefined
ifEx2  = If false undefined (Literal (IntLit 0))
