module Evaluate where

import AbsCakeML
import SemanticPrimitives




list_result :: Result a b -> Result [a] b
list_result (RVal v) = RVal [v]
list_result (RErr e) = RErr e

evaluate :: State -> Environment V -> [Exp] -> (State, Result [V] V)
evaluate st _env []          = (st, RVal [])
evaluate st env (e1:e2:es)   =
  case evaluate st env [e1] of
    (st', RVal v1) ->
      case evaluate st' env (e2:es) of
        (st'', RVal vs) -> (st'', RVal (head v1:vs))
        res -> res
    res -> res
evaluate st env [Raise e]    =
  case evaluate st env [e] of
    (st', RVal v) -> (st', RErr (RRaise (head v)))
    res           -> res
evaluate st _env [Literal l] = (st, RVal [Litv l])
evaluate st env  [App op es] =
  case evaluate st env (reverse es) of
    (st', RVal vs) ->
      case op of
        OpApp -> undefined
        other ->
          case do_app (refs st) op (reverse vs) of
            Just (refs', r) -> (st'{refs = refs'}, list_result r)
            Nothing         -> (st', RErr (RAbort RType_Error))
evaluate st  env [Var vname] = undefined



