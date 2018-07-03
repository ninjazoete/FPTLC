module Playground where
  
import Reduction
import Parse
import Parsec
import Expression

origShow :: Expr -> String
origShow (Func n e) = "\\" ++ n ++ "." ++ (origShow e)
origShow (FuncApp e e1) = "(" ++ origShow e ++ " " ++ origShow e1 ++ ")"
origShow (Name s) = s