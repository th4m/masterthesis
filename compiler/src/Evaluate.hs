module Evaluate where

import AbsCakeML
import SemanticPrimitives


evaluate :: State -> Environment -> [Exp] -> (State, Result [V] V)
evaluate st _env []          = (st, RVal [])
evaluate st _env [Literal l] = (st, RVal [Litv l])
evaluate st _env [App op es] = undefined
evaluate st  env [Var vname] = undefined
