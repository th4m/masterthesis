module TestEval where

import AbsCakeML
import SemanticPrimitives
import Evaluate
import Lib
import qualified Data.Char as C (ord, chr)
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
  where env1 = insertVarIntoEnv env2 "bepa" (LitV(IntLit 5))
        env2 = empty_env {c =
                          ([], [("::", (2, TypeId (Short "list"))),
                               ("nil", (0, TypeId (Short "list"))),
                               ("con", (1, TypeId (Short "test")))]
                            )
                         }

buildList' [] = Con (Just (Short "nil")) []
buildList' (e:es) = Con (Just (Short "::")) [e, buildList' es]

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

plusExp  e1 e2 = App (OPN Plus)   [e1, e2]
minusExp e1 e2 = App (OPN Minus)  [e1, e2]
timesExp e1 e2 = App (OPN Times)  [e1, e2]
divExp   e1 e2 = App (OPN Divide) [e1, e2]
modExp   e1 e2 = App (OPN Modulo) [e1, e2]

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

intListEx = buildList' [ Literal (IntLit 0)
                       , Literal (IntLit 1)
                       , Literal (IntLit 2)
                       , Literal (IntLit 3)]

--------------- Test Laziness ---------------

exLazy :: Exp -> Result [V] V
exLazy e = evaluateLazy ex_env [e]

exLazys :: [Exp] -> Result [V] V
exLazys es = evaluateLazy ex_env es

exForce :: Exp -> Result [V] V
exForce e = evalAndForce ex_env [e]

exForces :: [Exp] -> Result [V] V
exForces es = evalAndForce ex_env es

letExLazy = exLazy $
  Let (Just "cepa") (Literal (IntLit 3))
  (Log And (
      App (OPB LEq) [Var (Short "cepa"),
                     Literal (IntLit 3)])
    ((App (OPB GEq) [Var (Short "cepa"),
                     Literal (IntLit 3)]))
  )

---------------------------------------------
--               Unit Tests                --
---------------------------------------------

intLitA = Literal (IntLit intA)
intLitB = Literal (IntLit intB)

intA = 3
intB = 9

testPlus =
  plusExp intLitA intLitB

testMinus =
  minusExp intLitA intLitB

testTimes =
  timesExp intLitA intLitB
testDiv =
  divExp intLitA intLitB

testMod =
  modExp intLitA intLitB

testEq1 =
  eqExp [intLitA, intLitA]
testOrd =
  ordExp 'a'

testChr =
  chrExp 97

testChOpb =
  chopbExp LEq 'a' 'b'

strForTest = "testString"
testStrsub = strsubExp strForTest intA
  -- == RVal [LitV (Char (strForTest !! intA))]

testLength = strlenExp "1234567"
  --RVal [LitV (IntLit 7)]

compareEval = map exForce allExps == map (snd . ex) allExps
  where allExps = 
          [ testPlus
          , testMinus
          , testTimes
          , testDiv
          , testMod
          , testEq1
          , testOrd
          , testChr
          , testChOpb
          -- , testImplode
          , testStrsub
          , testLength
          ]


-- Testing termination (no formal proof, just try to run the functions)

-- Testing strict recursive semantics that should not terminate
testRec1 =
  ex $ App OpApp [recEx1, true]
testRec2 =
  ex $ App OpApp [recEx2, (Literal (IntLit 0))]
testRec3 =
  ex $ recEx3

-- Testing lazy recursive that semantics should not terminate
testRecLazy1 =
  exForce $ App OpApp [recEx1, true]

-- Testing lazy recursive semantics that should terminate
testRecLazy2 =
  exForce $ App OpApp [recEx2, (Literal (IntLit 0))]
testRecLazy3 =
  exForce $ recEx3


-- Recursive expressions to test
recEx1 =
  LetRec [("fun", "par", If (Var (Short "par")) appFun false)] (Var (Short "fun"))
  where appFun = (App OpApp [(Var (Short "fun")), true])
recEx2 =
  LetRec [("fun", "par", App (OPN Times) [Var (Short "par"), appFun])] (Var (Short "fun"))
  where appFun = (App OpApp [Var (Short "fun"), Var (Short "par")])
recEx3 =
  Let (Just "let") (App OpApp [recEx1, true]) (Literal (IntLit 0))

matEx1 e =
  Mat e [(PVar "apa", Literal (IntLit 0)), (PVar "bepa", Literal (IntLit 1))]

matEx2 e =
  Mat e [(PLit (IntLit 0), Literal (StrLit "first")), (PLit (IntLit 1), Literal (StrLit "second"))]

matEx3 e =
  Mat e [(PCon (Just (Short "con")) [PLit (IntLit 1)], Literal (IntLit 213))]
