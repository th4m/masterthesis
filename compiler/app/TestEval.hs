import AbsCakeML
import SemanticPrimitives
import Evaluate
import Lib
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

insertVarIntoEnv :: Environment V -> VarN -> V -> Environment V
insertVarIntoEnv env varn v' =
  env {v = (varn, v'):(v env)}

-- | Returns an environment with variables "apa" = IntLit 7
--   and "bepa" = IntLit 5
ex_env = insertVarIntoEnv env1 "apa" (LitV(IntLit 7))
  where env1 = insertVarIntoEnv empty_env "bepa" (LitV(IntLit 5))

------- Some shortcuts -------

apa = Var (Short "apa")
bepa = Var (Short "bepa")

true = App (OPB LEq) [Literal (IntLit 0), Literal (IntLit 1)]
false = App (OPB GEq) [Literal (IntLit 0), Literal (IntLit 1)]

------------------------------

-- | Insert an expression, and this function will run it with an
--   example environment containing some variables
ex :: Exp -> (State, Result [V] V)
ex e = evaluate empty_st ex_env [e]

-- | Test list of expressions
exs :: [Exp] -> (State, Result [V] V)
exs es = evaluate empty_st ex_env es

ifEx1 = ex $ If true true false
ifEx2 = ex $ If false true false

andEx b1 b2 = ex $ Log And b1 b2
orEx  b1 b2 = ex $ Log Or b1 b2

recAndEx b1 = Log And b1 (recAndEx b1)

--recMulEx (Literal (IntLit 0)) = Literal (IntLit 0)
recMulEx n1 = App (OPN Times) [n1, (recMulEx n1)]

-- | let cepa = 3; cepa <= 3 && cepa >= 3
letEx = ex $
  Let (Just "cepa") (Literal (IntLit 3))
  (Log And (
      App (OPB LEq) [Var (Short "cepa"),
                     Literal (IntLit 3)])
    ((App (OPB GEq) [Var (Short "cepa"),
                     Literal (IntLit 3)]))
  )

