module SemanticPrimitives where

import AbsCakeML

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

-- data Environment v = Env {
--   v ::
-- Temporary until language can handle more than literals
type Environment = String
type State = String


-- Replace Strings with Store and FFI in future
do_app :: (String, String) -> Op -> [V] -> Maybe ((String, String), Result V V)
do_app (s, t) op vs =
  case (op, vs) of
    (OPN op, [Litv (IntLit n1), Litv (IntLit n2)]) ->
      Just ((s, t), RVal (Litv (IntLit (opn_lookup op n1 n2))))

opn_lookup :: Opn -> (Int -> Int -> Int)
opn_lookup n =
  case n of
    Plus  -> (+)
    Minus -> (-)
