import AbsCakeML
import Compile



compareEval :: Exp -> Bool
compareEval e = strict == lazy
  where (_, strict) = ex $ compile e
        lazy        = exForce e


