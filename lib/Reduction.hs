module Reduction (reduce) where
  
import Expression

reduce :: Expr -> Expr
reduce e = if needsApply e
              then reduce $ reduceSinglePass e 
              else e

needsApply :: Expr -> Bool
needsApply (FuncApp e e1) = True
needsApply (Name n) = False
needsApply (Func n e) = needsApply e
  
reduceSinglePass :: Expr -> Expr
reduceSinglePass (FuncApp (Func n e) e1) = reduceSinglePass $ findAndSubstitue n e e1
reduceSinglePass (FuncApp e e1) = FuncApp (reduceSinglePass e) (reduceSinglePass e1)
reduceSinglePass (Func n e) = Func n $ reduceSinglePass e
reduceSinglePass (Name n) = Name n

findAndSubstitue :: String -> Expr -> Expr -> Expr
findAndSubstitue s v@(Name n) e
                       | n == s = e
                       | otherwise = v
findAndSubstitue s (Func n e) e1 = Func n $ findAndSubstitue s e e1
findAndSubstitue s (FuncApp e e1) e2 = FuncApp (findAndSubstitue s e e2) (findAndSubstitue s e1 e2)
