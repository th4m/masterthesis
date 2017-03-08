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
                               ("nil", (0, TypeId (Short "list")))])
                         }

buildList [] = Con (Just (Short "nil")) []
buildList (e:es) = Con (Just (Short "::")) [e, buildList es]

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

intListEx = buildList [ Literal (IntLit 0)
                      , Literal (IntLit 1)
                      , Literal (IntLit 2)
                      , Literal (IntLit 3)]

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
--               Unit Tests                --
---------------------------------------------

intLitA = Literal (IntLit intA)
intLitB = Literal (IntLit intB)

intA = 3
intB = 9

testPlus :: Bool
testPlus =
  exForce (plusExp intLitA intLitB)
  == RVal [LitV (IntLit (intA + intB))]
  && RVal [LitV (IntLit (intA + intB))]
  == exForce (plusExp intLitB intLitA)

testMinus :: Bool
testMinus =
  exForce (minusExp intLitA intLitB)
  == RVal [LitV (IntLit (intA - intB))]
  && RVal [LitV (IntLit (intB - intA))]
  == exForce (minusExp intLitB intLitA)

testTimes :: Bool
testTimes =
  exForce (timesExp intLitA intLitB)
  == RVal [LitV (IntLit (intA * intB))]
  && RVal [LitV (IntLit (intB * intA))]
  == exForce (timesExp intLitB intLitA)

testDiv :: Bool
testDiv =
  exForce (divExp intLitA intLitB)
  == RVal [LitV (IntLit (intA `div` intB))]
  && RVal [LitV (IntLit (intB `div` intA))]
  == exForce (divExp intLitB intLitA)

testMod :: Bool
testMod =
  exForce (modExp intLitA intLitB)
  == RVal [LitV (IntLit (intA `mod` intB))]
  && RVal [LitV (IntLit (intB `mod` intA))]
  == exForce (modExp intLitB intLitA)

testEq1 :: Bool
testEq1 =
  exForce (eqExp [intLitA, intLitA])
  == RVal [boolv True]
  && RVal [boolv True]
  == exForce (eqExp [intLitB, intLitB])

testOrd :: Bool
testOrd =
  exForce (ordExp 'a')
  == RVal [LitV (IntLit (C.ord 'a'))]
  && RVal [LitV (IntLit (C.ord 'b'))]
  == exForce (ordExp 'b')

testChr :: Bool
testChr =
  exForce (chrExp 97)
  == RVal [LitV (Char (C.chr 97))]
  && RVal [LitV (Char (C.chr 98))]
  == exForce (chrExp 98)

testChOpb :: Bool
testChOpb =
  exForce (chopbExp LEq 'a' 'b')
  == RVal [boolv True]
  && RVal [boolv False]
  == exForce (chopbExp LEq 'b' 'a')

strForTest = "testString"
testStrsub :: Bool
testStrsub =
  exForce (strsubExp strForTest intA)
  == RVal [LitV (Char (strForTest !! intA))]

testLength :: Bool
testLength = exForce (strlenExp "1234567") == RVal [LitV (IntLit 7)]

allTests :: Bool
allTests = and
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
