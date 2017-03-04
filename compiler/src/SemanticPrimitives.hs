module SemanticPrimitives where

import AbsCakeML
import Lib

import Numeric.Natural
import Data.Word
import Data.Bits
import qualified Data.Char as C
import qualified Data.Set as S
-- import qualified Data.Map as Map

data TId_or_Exn
  = TypeId (Id TypeN)
  | TypeExn (Id ConN)
  deriving (Eq, Ord, Show)

type AList_Mod_Env k v = (AList ModN (AList k v), AList k v) -- Map.Map ModN (Map.Map k v, Map.Map k v)

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
  deriving (Eq, Ord, Show)

-- | Value Forms
data V
  = LitV Lit
  -- Constructor Application
  | ConV (Maybe (ConN, TId_or_Exn)) [V]
  -- Function Closures
  | Closure (Environment V) VarN Exp
  | RecClosure (Environment V) [(VarN, VarN, Exp)] VarN
  | Loc Natural
  | VectorV [V]
  | Thunk (Environment V) Exp
  deriving (Eq, Ord, Show)

bindv = ConV (Just ("Bind", TypeExn (Short "Bind"))) []

type Env_Val = AList VarN V
type Env_Mod = AList ModN Env_Val

data Abort
  = RType_Error
  | RTimeout_Error
  deriving (Eq, Ord, Show)

data Error_Result a
  = RRaise a
  | RAbort Abort
  deriving (Eq, Ord, Show)

data Result a b
  = RVal a
  | RErr (Error_Result b)
  deriving (Eq, Ord, Show)

-- | Stores
data Store_V a
  = RefV a
  | W8Array [Word8]
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

lit_same_type :: Lit -> Lit -> Bool
lit_same_type l1 l2 =
  case (l1, l2) of
    (IntLit _, IntLit _) -> True
    (Char   _, Char   _) -> True
    (StrLit _, StrLit _) -> True
    (Word8  _, Word8  _) -> True
    (Word64 _, Word64 _) -> True
    _                    -> False

data Match_Result a
  = No_Match
  | Match_Type_Error
  | Match a
  deriving (Eq, Ord, Show)

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

pmatch :: Env_CTor -> Store V -> Pat -> V -> Env_Val -> Match_Result Env_Val
pmatch envC s (PVar x) v' env = Match $ (x, v'):env
pmatch envC s (PLit l) (LitV l') env =
  if l == l' then
    Match env
  else if lit_same_type l l' then
    No_Match
  else
    Match_Type_Error
pmatch envC s (PCon (Just n) ps) (ConV (Just (n', t')) vs) env =
  case lookup_alist_mod_env n envC of
    Just (l, t) ->
      if same_tid t t' && length ps == 1 then
        if same_ctor (id_to_n n, t) (n', t') then
          pmatch_list envC s ps vs env
        else
          No_Match
      else
        Match_Type_Error
    _           -> Match_Type_Error
pmatch envC s (PCon Nothing ps) (ConV Nothing vs) env =
  if length ps == length vs then
    pmatch_list envC s ps vs env
  else
    Match_Type_Error
pmatch envC s (PRef p) (Loc lnum) env =
  case store_lookup lnum s of
    Just (RefV v) -> pmatch envC s p v env
    Just _        -> Match_Type_Error
    Nothing       -> Match_Type_Error
pmatch envC s (PTAnnot p t) v env =
  pmatch envC s p v env
pmatch envC _ _ _ env = Match_Type_Error


pmatch_list envC s [] [] env = Match env
pmatch_list envC s (p:ps) (v:vs) env =
  case pmatch envC s p v env of
    No_Match -> No_Match
    Match_Type_Error -> Match_Type_Error
    Match env' -> pmatch_list envC s ps vs env'
pmatch_list _envC _s _ _ _env = Match_Type_Error

data State = St {
  refs          :: Store V,
  defined_types :: S.Set TId_or_Exn,
  defined_mods  :: S.Set ModN
 }
  deriving Show

build_rec_env :: [(VarN, VarN, Exp)] -> Environment V -> Env_Val -> Env_Val
build_rec_env funs cl_env add_to_env =
  foldr f' add_to_env funs
  where f' = \(f,x,e) env' -> (f, RecClosure cl_env funs f):env'

find_recfun :: VarN -> [(VarN, a, b)] -> Maybe (a, b)
find_recfun n funs =
  case funs of
    []            -> Nothing
    (f,x,e):funs' ->
      if f == n then
        Just (x,e)
      else
        find_recfun n funs

-- | Check that a constructor is properly applied
do_con_check :: Env_CTor -> Maybe (Id ConN) -> Natural -> Bool
do_con_check cenv n_opt l =
  case n_opt of
    Nothing -> True
    Just n  ->
      case lookup_alist_mod_env n cenv of
        Nothing       -> False
        Just (l', ns) -> l == l'

build_conv :: Env_CTor -> Maybe (Id ConN) -> [V] -> Maybe V
build_conv envC cn vs =
  case cn of
    Nothing ->
      Just (ConV Nothing vs)
    Just id ->
      case lookup_alist_mod_env id envC of
        Nothing       -> Nothing
        Just (len, t) -> Just (ConV (Just (id_to_n id, t))vs)

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

data Eq_Result
  = Eq_Val Bool
  | Eq_Type_Error

do_eq :: V -> V -> Eq_Result
do_eq (LitV l1) (LitV l2) =
  if lit_same_type l1 l2 then
    Eq_Val (l1 == l2)
  else
    Eq_Type_Error
do_eq (Loc l1)           (Loc l2)           = Eq_Val (l1 == l2)
do_eq (ConV cn1 vs1)     (ConV cn2 vs2)     =
  if cn1 == cn2 && (length vs1 == length vs2) then
    do_eq_list vs1 vs2
  else if ctor_same_type cn1 cn2 then
    Eq_Val False
  else
    Eq_Type_Error
do_eq (VectorV vs1)      (VectorV vs2)      =
  if length vs1 == length vs2 then
    do_eq_list vs1 vs2
  else
    Eq_Val False
do_eq (Closure _ _ _)    (Closure _ _ _)    = Eq_Val True
do_eq (Closure _ _ _)    (RecClosure _ _ _) = Eq_Val True
do_eq (RecClosure _ _ _) (Closure _ _ _)    = Eq_Val True
do_eq (RecClosure _ _ _) (RecClosure _ _ _) = Eq_Val True
do_eq _                  _                  = Eq_Type_Error

do_eq_list :: [V] -> [V] -> Eq_Result
do_eq_list [] [] = Eq_Val True
do_eq_list (v1:vs1) (v2:vs2) =
  case do_eq v1 v2 of
    Eq_Type_Error -> Eq_Type_Error
    Eq_Val r ->
      if not r then
        Eq_Val False
      else
        do_eq_list vs1 vs2
do_eq_list _ _ = Eq_Val False

prim_exn :: ConN -> V
prim_exn cn = ConV (Just (cn, TypeExn (Short cn))) []

do_opapp :: [V] -> Maybe (Environment V, Exp)
do_opapp vs =
  case vs of
    [Closure env n e, v'] ->
      Just (env {v = ((n, v'):(v env))}, e)
    [RecClosure env funs n, v'] ->
      if allDistinct (map (\(f,x,e) -> f) funs) then
        case find_recfun n funs of
          Just (n,e) -> Just (env {v=(n,v'):(build_rec_env funs env (v env))}, e)
          Nothing    -> Nothing
      else
        Nothing
    _ -> Nothing

v_to_list :: V -> Maybe [V]
v_to_list (ConV (Just (cn, TypeId (Short tn))) []) =
  if cn == "nil" && tn == "list" then
    Just []
  else
    Nothing
v_to_list (ConV (Just (cn, TypeId (Short tn))) [v1,v2]) =
  if cn == "::" && tn == "list" then
    case v_to_list v2 of
      Just vs -> Just (v1:vs)
      Nothing -> Nothing
  else
    Nothing
v_to_list _ = Nothing

v_to_char_list :: V -> Maybe [Char]
v_to_char_list (ConV (Just (cn, TypeId (Short tn))) []) =
  if cn == "nil" && tn == "list" then
    Just []
  else
    Nothing
v_to_char_list (ConV (Just (cn, TypeId (Short tn))) [LitV (Char c), v]) =
  if cn == "::" && tn == "list" then
    case v_to_char_list v of
      Just cs -> Just (c:cs)
      Nothing -> Nothing
  else
    Nothing
v_to_char_list _ = Nothing

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

opw8_lookup :: Opw -> Word8 -> Word8 -> Word8
opw8_lookup op =
  case op of
    AndW -> (.&.)
    OrW  -> (.|.)
    XOr  -> xor
    Add  -> (+)
    Sub  -> (-)

opw64_lookup :: Opw -> Word64 -> Word64 -> Word64
opw64_lookup op =
  case op of
    AndW -> (.&.)
    OrW  -> (.|.)
    XOr  -> xor
    Add  -> (+)
    Sub  -> (-)

-- Unsure if ASR and LSR are the same. They should be when working with unsigned?
shift8_lookup :: Shift -> Word8 -> Natural -> Word8
shift8_lookup sh = case sh of
  ASR -> \w n -> shiftR w (fromIntegral n)
  LSR -> \w n -> shiftR w (fromIntegral n)
  LSL -> \w n -> shiftL w (fromIntegral n)

shift64_lookup :: Shift -> Word64 -> Natural -> Word64
shift64_lookup sh = case sh of
  ASR -> \w n -> shiftR w (fromIntegral n)
  LSR -> \w n -> shiftR w (fromIntegral n)
  LSL -> \w n -> shiftL w (fromIntegral n)

------ Lazy Semantics ------


doAppLazy :: Op -> [V] -> Maybe (Result V V)
doAppLazy op vs =
  case (op, vs) of
    (OPN op, [LitV (IntLit n1), LitV (IntLit n2)]) ->
      Just (RVal (LitV (IntLit (opn_lookup op n1 n2))))
    (OPB op, [LitV (IntLit n1), LitV (IntLit n2)]) ->
      Just (RVal (boolv (opb_lookup op n1 n2)))
    (OPW W8 op, [LitV (Word8 w1), LitV (Word8 w2)]) ->
      Just $ RVal $ LitV $ Word8 $ opw8_lookup op w1 w2
    (OPW W64 op, [LitV (Word64 w1), LitV (Word64 w2)]) ->
      Just $ RVal $ LitV $ Word64 $ opw64_lookup op w1 w2
    (Shift W8 op n, [LitV (Word8 w)]) ->
      Just $ RVal $ LitV $ Word8 $ shift8_lookup op w n
    (Shift W64 op n, [LitV (Word64 w)]) ->
      Just $ RVal $ LitV $ Word64 $ shift64_lookup op w n
    (Equality, [v1, v2]) ->
      case do_eq v1 v2 of
        Eq_Type_Error -> Nothing
        Eq_Val b      -> Just $ RVal $ boolv b
    (WordFromInt W8, [LitV (IntLit i)]) ->
      Just $ RVal $ LitV $ Word8 $ toEnum i
    (WordFromInt W64, [LitV (IntLit i)]) ->
      Just $ RVal $ LitV $ Word8 $ toEnum i
    (WordToInt W8, [LitV (IntLit i)]) ->
      Just $ RVal $ LitV $ Word8 $ fromIntegral i
    (WordToInt W64, [LitV (IntLit i)]) ->
      Just $ RVal $ LitV $ Word64 $ fromIntegral i
    (Ord, [LitV (Char c)]) ->
      Just $ RVal $ LitV $ IntLit $ C.ord c
    (Chr, [LitV (IntLit i)]) ->
      Just $ RVal $ LitV $ Char $ C.chr i
    (ChOpb op, [LitV (Char c1), LitV (Char c2)]) ->
      Just $ RVal $ boolv $ opb_lookup op (C.ord c1) (C.ord c2)
    (Implode, [v]) ->
      case v_to_char_list v of
        Just ls -> Just $ RVal $ LitV $ StrLit ls
        Nothing -> Nothing
    (StrSub, [LitV (StrLit str), LitV (IntLit i)]) ->
      if i < 0 then
        Just $ RErr $ RRaise $ prim_exn "SubScript"
      else
        if i >= length str then
          Just $ RErr $ RRaise $ prim_exn "SubScript"
        else
          Just $ RVal $ LitV $ Char $ str !! i
    (StrLen, [LitV (StrLit str)]) ->
      Just $ RVal $ LitV $ IntLit $ length str
    (VFromList, [v]) ->
      case v_to_list v of
        Just vs -> Just $ RVal $ VectorV vs
        Nothing -> Nothing
    (VSub, [VectorV vs, LitV (IntLit i)]) ->
      if i < 0 then
        Just $ RErr $ RRaise $ prim_exn "Subscript"
      else
        if i >= length vs then
          Just $ RErr $ RRaise $ prim_exn "Subscript"
        else
          Just $ RVal $ vs !! i
    (VLength, [VectorV vs]) ->
      Just $ RVal $ LitV $ IntLit $ length vs
    _ -> Nothing

pmatchLazy :: Env_CTor -> Pat -> V -> Env_Val -> Match_Result Env_Val
pmatchLazy envC (PVar x) v' env = Match $ (x, v'):env
pmatchLazy envC (PLit l) (LitV l') env =
  if l == l' then
    Match env
  else if lit_same_type l l' then
    No_Match
  else
    Match_Type_Error
pmatchLazy envC (PCon (Just n) ps) (ConV (Just (n', t')) vs) env =
  case lookup_alist_mod_env n envC of
    Just (l, t) ->
      if same_tid t t' && length ps == 1 then
        if same_ctor (id_to_n n, t) (n', t') then
          pmatchListLazy envC ps vs env
        else
          No_Match
      else
        Match_Type_Error
    _           -> Match_Type_Error
pmatchLazy envC (PCon Nothing ps) (ConV Nothing vs) env =
  if length ps == length vs then
    pmatchListLazy envC ps vs env
  else
    Match_Type_Error
pmatchLazy envC (PRef p) (Loc lnum) env = undefined
  -- case store_lookup lnum s of
  --   Just (RefV v) -> pmatch envC s p v env
  --   Just _        -> Match_Type_Error
  --   Nothing       -> Match_Type_Error
pmatchLazy envC (PTAnnot p t) v env =
  pmatchLazy envC p v env
pmatchLazy envC _ _ env = Match_Type_Error

pmatchListLazy envC [] [] env = Match env
pmatchListLazy envC (p:ps) (v:vs) env =
  case pmatchLazy envC p v env of
    No_Match -> No_Match
    Match_Type_Error -> Match_Type_Error
    Match env' -> pmatchListLazy envC ps vs env'
pmatchListLazy _envC _ _ _env = Match_Type_Error
