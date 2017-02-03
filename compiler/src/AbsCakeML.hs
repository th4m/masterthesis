module AbsCakeML where

import Numeric.Natural

-- | Literal constants
data Lit
  = IntLit Int
  | CharLit Char
  | StrLit String
  deriving (Eq)

-- | Identifiers
data Id a
  = Short a
  | Long ModN a
  deriving (Eq)

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
  deriving (Eq)

-- | Types
data T
  = TVar TVarN
  | TVar_DB Natural
  | TApp [T] TCtor
  deriving (Eq)

-- | Patterns
data Pat
  = PVar VarN
  | PLit Lit
  | PCon (Maybe (Id ConN)) [Pat]
  | PRef Pat
  | PTAnnot Pat T
  deriving (Eq)

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
  deriving (Eq)

data Op
  = OPN Opn
  | OPB Opb
  | OpApp
  deriving (Eq)

-- | Built-in binary operations
data Opn
  = Plus
  | Minus
  | Times
  | Divide
  | Modulo
  deriving (Eq)

data Opb
  = Lt
  | Gt
  | LEq
  | GEq
  deriving (Eq)

data LOp
  = And
  | Or
  deriving (Eq)

