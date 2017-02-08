module SemanticPrimitives where

import AbsCakeML
import Lib

import Numeric.Natural
import Data.Set
-- import qualified Data.Map as Map

data TId_or_Exn
  = TypeId (Id TypeN)
  | TypeExn (Id ConN)
  deriving (Eq)

type AList_Mod_Env k v = AList ModN (AList k v, AList k v) -- Map.Map ModN (Map.Map k v, Map.Map k v)

merge_alist_mod_env (menv1, env1) (menv2, env2) =
  (menv1 ++ menv2, env1 ++ env2)

lookup_alist_mod_env id (mcenv, cenv) =
  case id of
    Short x  -> alist_lookup x cenv
    Long x y ->
      case alist_lookup x mcenv of
        Nothing    -> Nothing
        Just cenv' -> alist_lookup y cenv

type Flat_Env_CTor = AList ConN (Natural, TId_or_Exn)
type Env_CTor      = AList_Mod_Env ConN (Natural, TId_or_Exn)

data Environment v' = Env {
  v :: AList VarN V, -- Map.Map VarN V,
  c :: AList_Mod_Env ConN (Natural, TId_or_Exn),
  m :: AList ModN (AList VarN V) -- Map.Map ModN (Map.Map VarN V)
  }
  deriving (Eq)

-- | Value Forms
data V
  = LitV Lit
  -- Constructor Application
  | ConV (Maybe (ConN, TId_or_Exn)) [V]
  -- Function Closures
  | Closure (Environment V) VarN Exp
  deriving (Eq)
  -- More in the future?

data Abort
  = RType_Error
  | RTimeout_Error

data Error_Result a
  = RRaise a
  | RAbort Abort

data Result a b
  = RVal a
  | RErr (Error_Result b)

-- | Stores
data Store_V a
  = RefV a
  -- W8Array [Word8]
  | VArray [a]
  deriving Show

store_v_same_type :: Store_V a -> Store_V a -> Bool
store_v_same_type v1 v2 =
  case (v1, v2) of
    (RefV _, RefV _)     -> True
    -- (W8Array _, W8Array _) -> True
    (VArray _, VArray _) -> True
    _                    -> False

type Store a
  = [Store_V a]

empty_store :: Store a
empty_store = []

store_lookup :: Natural -> Store a -> Maybe (Store_V a)
store_lookup l st =
  if fromIntegral l < length st then
    Just (st !! fromIntegral l)
  else
    Nothing

store_alloc :: Store_V a -> Store a -> (Store a, Natural)
store_alloc v st =
  ((st ++ [v]), toEnum (length st))

store_assign :: Natural -> Store_V a -> Store a -> Maybe (Store a)
store_assign n v st =
  if fromIntegral n < length st && store_v_same_type v (st !! (fromIntegral n)) then
    Just $ updateList st n v
  else
    Nothing

-- Helper function
updateList :: [a] -> Natural -> a -> [a]
updateList st n v =
  case splitAt ((fromIntegral n)) st of
    (first, (_:rest)) -> first ++ (v:rest)
    _                 -> st

lookup_var_id :: Id VarN -> Environment V -> Maybe V
lookup_var_id id env =
  case id of
    Short x  -> alist_lookup x (v env)
    Long x y ->
      case alist_lookup x (m env) of
        Nothing   -> Nothing
        Just env' -> alist_lookup y env'

data Match_Result a 
  = No_Match
  | Match_Type_Error
  | Match a

same_tid :: TId_or_Exn -> TId_or_Exn -> Bool
same_tid (TypeId tn1) (TypeId tn2) = tn1 == tn2
same_tid (TypeExn _ ) (TypeExn _ ) = True
same_tid _            _            = False

same_ctor :: (ConN, TId_or_Exn) -> (ConN, TId_or_Exn) -> Bool
same_ctor (cn1, TypeExn mn1) (cn2, TypeExn mn2) = cn1 == cn2 && mn1 == mn2
same_ctor (cn1, _)           (cn2, _)           = cn1 == cn2

ctor_same_type :: Maybe (ConN, TId_or_Exn) -> Maybe (ConN, TId_or_Exn) -> Bool
ctor_same_type c1 c2 =
  case (c1, c2) of
    (Nothing, Nothing)           -> True
    (Just (_, t1), Just (_, t2)) -> same_tid t1 t2
    _                            -> False
  


data State = St {
  refs          :: Store V,
  defined_types :: Set TId_or_Exn,
  defined_mods  :: Set ModN
 }

data Exp_or_Val
  = Exp Exp
  | Val V

boolv :: Bool -> V
boolv b = if b
  then ConV (Just ("true", TypeId (Short "bool"))) []
  else ConV (Just ("false", TypeId (Short "bool"))) []

do_app :: Store V -> Op -> [V] -> Maybe (Store V, Result V V)
do_app s op vs =
  case (op, vs) of
    (OPN op, [LitV (IntLit n1), LitV (IntLit n2)]) ->
      Just (s, RVal (LitV (IntLit (opn_lookup op n1 n2))))
    (OPB op, [LitV (IntLit n1), LitV (IntLit n2)]) ->
      Just (s, RVal (boolv (opb_lookup op n1 n2)))

do_log :: LOp -> V -> Exp -> Maybe Exp_or_Val
do_log l v e =
  case (l, v) of
    (And, ConV (Just ("true", TypeId (Short "bool"))) []) -> Just (Exp e)
    (Or, ConV (Just ("false", TypeId (Short "bool"))) []) -> Just (Exp e)
    (_, ConV (Just ("true",  TypeId (Short "bool"))) [])  -> Just (Val v)
    (_, ConV (Just ("false", TypeId (Short "bool"))) [])  -> Just (Val v)
    _                                                     -> Nothing

do_if :: V -> Exp -> Exp -> Maybe Exp
do_if v e1 e2 =
  if v == boolv True then
    Just e1
  else if v == boolv False then
    Just e2
  else
    Nothing

do_opapp :: [V] -> Maybe (Environment V, Exp)
do_opapp vs =
  case vs of
    [Closure env n e, v'] ->
      Just (env {v = ((n, v'):(v env))}, e)

opn_lookup :: Opn -> (Int -> Int -> Int)
opn_lookup n =
  case n of
    Plus   -> (+)
    Minus  -> (-)
    Times  -> (*)
    Divide -> (div)
    Modulo -> (mod)

opb_lookup :: Opb -> (Int -> Int -> Bool)
opb_lookup n =
  case n of
    Lt  -> (<)
    Gt  -> (>)
    LEq -> (<=)
    GEq -> (>=)
