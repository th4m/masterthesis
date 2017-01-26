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
