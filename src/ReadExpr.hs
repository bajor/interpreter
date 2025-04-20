module ReadExpr (readExpr) where

import Text.ParserCombinators.Parsec (Parser, oneOf, parse, skipMany1, space, noneOf, char, many)
import LispTypes


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space


parseLispString :: Parser LispVal
parseLispString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ LispTypes.LispString x
  -- A string is a double quote mark, followed by any number of non-quote characters, followed by a closing quote mark


readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value" ++ show val

