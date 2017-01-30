module Evaluate where

import AbsCakeML
import SemanticPrimitives


evaluate :: State -> Environment -> [Exp] -> (State, Result [V] V)
evaluate st _env []          = (st, RVal [])
evaluate st env (e1:e2:es)   =
  case evaluate st env [e1] of
    (st', RVal v1) ->
      case evaluate st' env (e1:es) of
        (st'', RVal vs) -> (st'', RVal (head v1:vs))
        res -> res
    res -> res
evaluate st _env [Literal l] = (st, RVal [Litv l])
evaluate st env  [App op es] =
  case evaluate st env (reverse es) of
    (st', RVal vs) ->
      case op of
        OpApp -> undefined
        other ->
          case do_app ("apa", "bepa") op (reverse vs) of
            Just ((refs, ffi), r) -> undefined
            Nothing               -> undefined
evaluate st  env [Var vname] = undefined
