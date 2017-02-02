module SemanticPrimitives where

import AbsCakeML
import Numeric.Natural
import Data.Set
import qualified Data.Map as Map

data TId_or_Exn
  = TypeId (Id TypeN)
  | TypeExn (Id ConN)


type AList_Mod_Env k v = Map.Map ModN (Map.Map k v, Map.Map k v)

data Environment v' = Env {
  v :: Map.Map VarN V,
  c :: AList_Mod_Env ConN (Natural, TId_or_Exn),
  m :: Map.Map ModN (Map.Map VarN V)
  }

-- | Value Forms
data V
  = LitV Lit
  | ConV (Maybe (ConN, TId_or_Exn)) [V]
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
    Short x  -> Map.lookup x (v env)
    Long x y ->
      case Map.lookup x (m env) of
        Nothing   -> Nothing
        Just env' -> Map.lookup y env'

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
