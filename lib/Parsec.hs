{-# LANGUAGE FlexibleContexts #-}

module Parsec (
    Parser
  , regularParse
  , lexeme
) where

import Text.Parsec (Parsec, parse, ParseError, many, oneOf)
import Control.Monad (void)

-- Parsec Convenience

type Parser = Parsec String ()

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

lexeme :: Parser a -> Parser a
lexeme p = whitespace >> p
  where
    whitespace = void $ many $ oneOf " \n\t"