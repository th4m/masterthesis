module AbsCakeML where

-- | Literal constants
data Lit
  = IntLit Int
  | CharLit Char
  | StrLit String

-- | Built-in binary operations
data Opn
  = Plus
  | Minus

data Exp
  = Literal Lit
  | App Op [Exp]
  | Var VarN -- Should probably be changed in future

-- | Identifiers
data Id a
  = Short a
  | Long (ModN, a)

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
    Just mn -> Long (mn, n)






-- | Type Constructors
data TCtor
  = TC_int
  | TC_char
  | TC_string
  | TC_fn





data Op
  = OPN Opn

