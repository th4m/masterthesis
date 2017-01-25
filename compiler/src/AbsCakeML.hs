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

data Op
  = OPN Opn

data Exp
  = Literal Lit
  | App Op [Exp]
