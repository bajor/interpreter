module ReadExpr (readExpr) where

import Control.Applicative ((<|>))
import Text.ParserCombinators.Parsec (Parser, oneOf, parse, skipMany1, space, noneOf, char, many, letter, char, digit, many1)
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
  return (LispString x)
  -- A string is a double quote mark, followed by any number of non-quote characters, followed by a closing quote mark


parseLispAtom :: Parser LispVal
parseLispAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let parsedList = first:rest
  return (
    case parsedList of
      "#t" -> LispBool True
      "#f" -> LispBool False
      _ -> LispAtom parsedList
    )
  -- An atom is a letter or symbol, followed by any number of letters, digits, or symbols
  --
  -- We use a case expression to determine which LispVal to create and return, matching against the literal strings for true and false. If that is not the case, we return LispAtom.


parseLispNumber :: Parser LispVal
parseLispNumber = do
  numStr <- many1 digit
  return (LispNumber (read numStr))


readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value" ++ show val

