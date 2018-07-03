{-# LANGUAGE FlexibleContexts #-}

module Parse
 ( name
 , function
 , application
 , expression
 )
where
  
import Expression
import Parsec
import Reduction

import Text.Parsec
import Data.Char
import Control.Monad(void)
 
-- Atomic Parsers 

identifier :: Parser String
identifier = lexeme $ (:) <$> start <*> many rest
    where 
      start = satisfy (\x -> isLetter x && isLower x)
      rest = satisfy (\x -> (isLetter x) && isLower x || isDigit x)

-- Expression Parsers

name :: Parser Expr
name = Name <$> (lexeme identifier)
      
function :: Parser Expr
function = do
  void $ lexeme $ char '\\'
  n <- identifier <?> "name here"
  void $ lexeme $ char '.'
  e <- expression
  return $ Func n e

application :: Parser Expr
application = lexeme $ do
  void $ lexeme $ char '('
  e1 <- expression
  void $ (char ' ' <?> "a whitespace between expressions")
  e2 <- expression
  void $ lexeme $ char ')'
  return $ FuncApp e1 e2

expression :: Parser Expr
expression = function <|> name <|> application
  

      
