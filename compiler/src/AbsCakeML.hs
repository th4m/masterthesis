module AbsCakeML where

import Numeric.Natural
import Data.Word


-- | Literal constants
data Lit
  = IntLit Int
  | Char Char
  | StrLit String
  | Word8 Word8
  | Word64 Word64
  deriving (Eq, Show, Ord)

-- | Identifiers
data Id a
  = Short a
  | Long ModN a
  deriving (Eq, Show, Ord)

-- | Variable Names
type VarN = String

-- | Constructor Names
type ConN = String

-- | Type Names
type TypeN = String

-- | Type Variable Names
type TVarN = String

-- | Module Names
type ModN = String

mk_id :: Maybe ModN -> a -> Id a
mk_id mn_opt n =
  case mn_opt of
    Nothing -> Short n
    Just mn -> Long mn n

id_to_n :: Id a -> a
id_to_n id =
  case id of
    Short n  -> n
    Long _ n -> n

-- | Type Constructors
data TCtor
  = TC_int
  | TC_char
  | TC_string
  | TC_fn
  deriving (Eq, Show, Ord)

-- | Types
data T
  = TVar TVarN
  | TVar_DB Natural
  | TApp [T] TCtor
  deriving (Eq, Show, Ord)

-- | Patterns
data Pat
  = PVar VarN
  | PLit Lit
  | PCon (Maybe (Id ConN)) [Pat]
  | PRef Pat
  | PTAnnot Pat T
  deriving (Eq, Show, Ord)

-- | Expressions
data Exp
  = Raise Exp
  | Handle Exp [(Pat, Exp)]
  | Literal Lit
  | Con (Maybe (Id ConN)) [Exp]
  | Var (Id VarN)
  | Fun VarN Exp
  | App Op [Exp]
  | Log LOp Exp Exp
  | If Exp Exp Exp
  | Mat Exp [(Pat, Exp)]
  | Let (Maybe VarN) Exp Exp
  | LetRec [(VarN, VarN, Exp)] Exp
  | TAnnot Exp T
  deriving (Eq, Show, Ord)

data Word_Size = W8 | W64
  deriving (Eq, Show, Ord)

data Op
  -- Integer operations
  = OPN Opn
  | OPB Opb
  -- Word operations
  | OPW Word_Size Opw
  | Shift Word_Size Shift Natural
  | Equality
  -- Function application
  | OpApp
  -- Reference operations
  | OpAssign
  | OpRef
  | OpDeref
  -- Word/Integer conversions
  | WordFromInt Word_Size
  | WordToInt Word_Size
  -- Char operations
  | Ord
  | Chr
  | ChOpb Opb
  -- String operations
  | Implode
  | StrSub
  | StrLen
  -- Vector operations
  | VFromList
  | VSub
  | VLength
  deriving (Eq, Show, Ord)

-- | Built-in binary operations
data Opn
  = Plus
  | Minus
  | Times
  | Divide
  | Modulo
  deriving (Eq, Show, Ord)

data Opb
  = Lt
  | Gt
  | LEq
  | GEq
  deriving (Eq, Show, Ord)

data Opw
  = AndW
  | OrW
  | XOr
  | Add
  | Sub
  deriving (Eq, Show, Ord)

data Shift
  = LSL
  | LSR
  | ASR
  deriving (Eq, Show, Ord)

data LOp
  = And
  | Or
  deriving (Eq, Show, Ord)



pat_bindings :: Pat -> [VarN] -> [VarN]
pat_bindings (PVar n)      already_bound =
  n:already_bound
pat_bindings (PLit l)      already_bound =
  already_bound
pat_bindings (PCon _ ps)   already_bound =
  pats_bindings ps already_bound
pat_bindings (PRef p)      already_bound =
  pat_bindings p already_bound
pat_bindings (PTAnnot p _) already_bound =
  pat_bindings p already_bound

pats_bindings :: [Pat] -> [VarN] -> [VarN]
pats_bindings []     already_bound =
  already_bound
pats_bindings (p:ps) already_bound =
  pats_bindings ps (pat_bindings p already_bound)
