module Reduction (reduce) where
  
import Expression

reduce :: Expr -> Expr
reduce (FuncApp (Func n e) e1) = reduce $ findAndSubstitue n e e1
reduce (Func n e) = reduce e
reduce x = x

findAndSubstitue :: String -> Expr -> Expr -> Expr
findAndSubstitue s v@(Name n) e
                       | n == s = e
                       | otherwise = v
findAndSubstitue s (Func n e) e1 = Func n $ findAndSubstitue s e e1
findAndSubstitue s (FuncApp e e1) e2 = FuncApp (findAndSubstitue s e e2) (findAndSubstitue s e1 e2)