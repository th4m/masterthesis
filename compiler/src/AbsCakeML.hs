module AbsCakeML where

-- | Literal constants
data Lit
  = IntLit Int
  | CharLit Char
  | StrLit String
  deriving (Eq)

-- | Built-in binary operations
data Opn
  = Plus
  | Minus
  | Times
  | Divide
  | Modulo

data Opb
  = Lt
  | Gt
  | LEq
  | GEq

-- | Expressions
data Exp
  = Raise Exp
  | Literal Lit
  | App Op [Exp]
  | Var (Id VarN)
  | Log LOp Exp Exp
  | If Exp Exp Exp

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

data Op
  = OPN Opn
  | OPB Opb
  | OpApp

data LOp
  = And
  | Or
