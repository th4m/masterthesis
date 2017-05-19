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
-------------------------------------
-- Compare strict and lazy results --
-------------------------------------

compareEval :: Exp -> Bool
compareEval e = strict == lazy
  where strict = ex e
        lazy   = efc e

testAll =
  and $
  map compareEval
  [ plusExp intLitA intLitB
  , minusExp intLitA intLitB
  , timesExp intLitA intLitB
  , divExp intLitA intLitB
  , modExp intLitA intLitB
  , eqExp [intLitA, intLitB]
  , ordExp 'a'
  , chrExp 97
  , chopbExp LEq 'a' 'b'
  , chopbExp GEq 'b' 'a'
  , strsubExp strForTest intA
  , strlenExp strForTest
  , andExp true true
  , andExp false true
  , andExp true false
  , andExp false false
  , orExp true true
  , orExp false true
  , orExp true false
  , orExp false false
  , ifExp1
  , ifExp2
  , Mat (Literal (IntLit 1))
    [(PLit (IntLit 1), (Literal (StrLit "first"))), (PLit (IntLit 2), (Literal (StrLit "second")))]
  , Mat (Literal (IntLit 2))
    [(PLit (IntLit 1), (Literal (StrLit "first"))), (PLit (IntLit 2), (Literal (StrLit "second")))]
  , Raise (Literal (IntLit 0))
  , Handle (Raise (Literal (IntLit 1)))
    [(PLit (IntLit 1), (Literal (StrLit "first"))), (PLit (IntLit 2), (Literal (StrLit "second")))]
  , Handle (Raise (Literal (IntLit 2)))
    [(PLit (IntLit 1), (Literal (StrLit "first"))), (PLit (IntLit 2), (Literal (StrLit "second")))]
  , Let (Just "x") (Literal (StrLit "letTest")) (Var (Short "x"))
  ]

-- Expressions to test

intLitA = Literal $ IntLit intA
intLitB = Literal $ IntLit intB

intA = 3
intB = 9

strForTest = "testString"

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

-------------------------
-- Testing termination --
-------------------------

opapp e1 e2 = App OpApp [e1, e2]

recEx1 =
  LetRec [("fun", "par", If (Var (Short "par")) appFun false)] (Var (Short "fun"))
  where appFun = (App OpApp [(Var (Short "fun")), true])
recEx2 =
  LetRec [("fun", "par", App (OPN Times) [Var (Short "par"), appFun])] (Var (Short "fun"))
  where appFun = (App OpApp [Var (Short "fun"), Var (Short "par")])
-- timesEx1 = App OpApp [recEx2, (Literal (IntLit 0))]

-- Non recursive RecClosures

-- Returns the argument
recEx3 =
  LetRec [("fun", "par", Var (Short "par"))] (Var (Short "fun"))
-- Prints "recEx4"
recEx4 =
  LetRec [("fun", "par", Literal (StrLit "recEx4"))] (Var (Short "fun"))

letEx1 = Let (Just "let1") (App (OPN Times) [Literal (IntLit 3), Literal (IntLit 523)]) (Var (Short "let1"))
letEx2 = Let (Just "let2") (App OpApp [recEx1, true]) (Literal (IntLit 0))

testRecex =
  ex $ App OpApp [recEx2, (Literal (IntLit 0))]
testRecefc =
  efc $ App OpApp [recEx2, (Literal (IntLit 0))]

mulEx1 = App (OPN Times) [Literal (IntLit 0), App OpApp [recEx2, (Literal (IntLit 0))]]

-- The following examples use undefined to test evaluation steps

-- If the second argument is evaluated, an error should be returned
mulEx2 = App (OPN Times) [Literal (IntLit 0), undefined]

-- Force needed after compile
letEx3 = Let (Just "let3") undefined (Literal (IntLit 0))

-- Force needed after compile
ifEx1  = If true (Literal (IntLit 0)) undefined
ifEx2  = If false undefined (Literal (IntLit 0))


recLet = Let (Just "xo") (App OpApp [inf, true]) (Literal (StrLit "OK"))

inf = LetRec [("f","n", App OpApp [Var (Short "f"), true])] (Var (Short "f"))


-- takes a list and returns the first element
-- cakeHead = Fun "list" $ Mat (Var (Short "list")) $
--   [(PCon (Just (Short "nil")) [], Literal (StrLit "error: empty list"))
--   ,(PCon (Just (Short "::" )) [PVar "elem", PVar "rest"], Var (Short "elem"))]

mkList :: [Int] -> Exp
mkList []     = emptyList
mkList (x:xs) = consList x xs

emptyList     = Con (Just (Short "nil")) []
consList x xs = Con (Just (Short "::" )) [Literal (IntLit x), mkList xs]

cakeTake :: Exp
cakeTake =
  LetRec [("take", "n",
           Fun "ls" $
           If (App Equality [Var (Short "n"), Literal (IntLit 0)])
 {-then-}  emptyList
 {-else-}  (Mat (Var (Short "ls"))
            [(PCon (Just (Short "::" )) [PVar "elem", PVar "rest"]
             ,Con (Just (Short "::")) [Var (Short "elem")
                                      ,cakeTake2])
            ,(PCon (Just (Short "nil")) [], emptyList)]
           )
         )] $
  Var (Short "take")
--  Fun "n" $

cakeTake2 = App OpApp [App OpApp [Var (Short "take"), decr], Var (Short "rest")]
decr = App (OPN Minus) [Var (Short "n"), Literal (IntLit 1)]

applyCT :: Int -> Exp -> Exp
applyCT n ls = App OpApp [App OpApp [cakeTake, Literal (IntLit n)], ls]

infList :: Int -> Exp
infList i = LetRec [("infList", "N/A"
                  ,Con (Just (Short "::")) [Literal (IntLit i)
                                           , App OpApp [(Var (Short "infList"))
                                                       , zero]]
                  )] $
            Var (Short "infList")

infList1 = App OpApp [infList 1, zero]

zero = Literal (IntLit 0)

stopAtZero = LetRec [("stopAtZero", "n",
                      If (App Equality [Var (Short "n"), Literal (IntLit 0)])
                      (Literal (StrLit "OK"))
                      (App OpApp [Var (Short "stopAtZero"), decr])
                     )] (Var (Short "stopAtZero"))

-- repeats elem for n times
cakeRepeat = LetRec [("repeat", "elem",
                     Fun "n" $
                     If (App Equality [Var (Short "n"), Literal (IntLit 0)])
                     emptyList
                     (Con (Just (Short "::")) [Var (Short "elem"),
                                               App OpApp [App OpApp [Var (Short "repeat"),
                                                                     Var (Short "elem")],
                                                           decr]])
                     )] (Var (Short "repeat"))

applyCR elem n = App OpApp [App OpApp [cakeRepeat, elem], Literal (IntLit n)]
