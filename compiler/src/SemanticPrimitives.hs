module SemanticPrimitives where

import AbsCakeML
import Numeric.Natural
import Data.Set
import qualified Data.Map as Map

data TId_or_Exn
  = TypeId (Id TypeN)
  | TypeExn (Id ConN)

-- | Value Forms
data V
  = Litv Lit
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

updateList :: [a] -> Natural -> a -> [a]
updateList st n v =
  case splitAt ((fromIntegral n)) st of
    (first, (_:rest)) -> first ++ (v:rest)
    _                 -> st


type AList_Mod_Env k v = Map.Map ModN (Map.Map k v, Map.Map k v)

data Environment v' = Env {
  v :: Map.Map VarN V,
  c :: AList_Mod_Env ConN (Natural, TId_or_Exn),
  m :: Map.Map ModN (Map.Map VarN V)
  }


data State = St {
  refs          :: Store V,
  defined_types :: Set TId_or_Exn,
  defined_mods  :: Set ModN
 }


do_app :: Store V -> Op -> [V] -> Maybe (Store V, Result V V)
do_app s op vs =
  case (op, vs) of
    (OPN op, [Litv (IntLit n1), Litv (IntLit n2)]) ->
      Just (s, RVal (Litv (IntLit (opn_lookup op n1 n2))))

opn_lookup :: Opn -> (Int -> Int -> Int)
opn_lookup n =
  case n of
    Plus   -> (+)
    Minus  -> (-)
    Times  -> (*)
    Divide -> (div)
    Modulo -> (mod)
