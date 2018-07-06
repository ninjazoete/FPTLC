module Expression 
  (
  Expr (
    Name
  , Func
  , FuncApp)
  )
where
  
-- Expression in Lambda Calculus:
-- <expression> ::= <function> | <name> | <application>
-- <function> ::= \<name>.<body>
-- <name> ::= alpha numeric characters
-- <body> ::= <expression>
-- <application> ::= (<function expression> <argument expression>)
-- <function expression> ::= <expression>
-- <argument expression> ::= <expression>

data Expr = Func String Expr
 | FuncApp Expr Expr
 | Name String deriving (Eq)

-- Pretty Show Expr

instance Show Expr where 
  show (Func n e) = "𝞴" ++ show n ++ "." ++ show e
  show (FuncApp e e1) = "(" ++ show e ++ " " ++ show e1 ++ ")"
  show (Name s) = show s
