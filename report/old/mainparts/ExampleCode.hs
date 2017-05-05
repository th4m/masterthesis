module ExampleCode where

-- Should check n too
-- takeML n e =
--   LetRec [("take", "arg"
--           , Mat e
--           [(PCon (Just "::")  [(PVar "l"), (PVar "ls")]
--            , Con (Just "::") [Var (Short "l")
--                              , App OpApp [Var (Short "take")
--                                          , Var (Short "ls")]])
--           ,(PCon (Just "nil") []
--            , Con (Just "nil") [])])]


-------

data List' a = Con a (List a) | Nil

takeHS n e =
  if n == 0 then
    Nil
  else
    case e of
      Con a as -> Con a (takeHs n-1 as)
      Nil -> Nil
