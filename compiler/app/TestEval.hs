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

-------------- Some expressions to test --------------

plusExp e1 e2 = App (OPN Plus)   [e1, e2]
subExp  e1 e2 = App (OPN Minus)  [e1, e2]
mulExp  e1 e2 = App (OPN Times)  [e1, e2]
divExp  e1 e2 = App (OPN Divide) [e1, e2]
modExp  e1 e2 = App (OPN Modulo) [e1, e2]

eqExp es = App Equality es
ordExp c = App Ord [Literal (Char c)]
chrExp i = App Chr [Literal (IntLit i)]
chopbExp op c1 c2 = App (ChOpb op) [Literal (Char c1), Literal (Char c2)]
implExp es = App Implode es
strsubExp str i = App StrSub [Literal (StrLit str), Literal (IntLit i)]
strlenExp str = App StrLen [Literal (StrLit str)]
vlistExp es = App VFromList es
vsubExp es i = App VSub [(App VFromList es), Literal (IntLit i)]
vlenExp es = App VLength [App VFromList es]

andExp  e1 e2 = Log And e1 e2
orExp   e1 e2 = Log Or  e1 e2

ifExp1 = If true true false
ifExp2 = If false true false

---------------------- Examples ----------------------

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


--------------- Test Laziness ---------------

exSmall :: Exp -> Result [V] V
exSmall e = evaluateSmall ex_env [e]

exForce :: Exp -> Result [V] V
exForce e = evalAndForce ex_env [e]

letExSmall = exSmall $
  Let (Just "cepa") (Literal (IntLit 3))
  (Log And (
      App (OPB LEq) [Var (Short "cepa"),
                     Literal (IntLit 3)])
    ((App (OPB GEq) [Var (Short "cepa"),
                     Literal (IntLit 3)]))
  )
---------------------------------------------
