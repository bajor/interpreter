module ReadExpr (readExpr) where

import Control.Applicative ((<|>))
import Text.ParserCombinators.Parsec (Parser, oneOf, parse, noneOf, char, many, letter, char, digit, many1)
import LispTypes


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


escapedChar :: Parser Char
escapedChar =
  char '\\' >>
  (   char '"'  >> return '"'
  <|> char '\\' >> return '\\'
  <|> char 'n'  >> return '\n'
  <|> char 't'  >> return '\t'
  <|> char 'r'  >> return '\r'
  )


parseLispString :: Parser LispVal
parseLispString =
  char '"' >>
  many (escapedChar <|> noneOf "\"") >>= \content->
  char '"' >>
  return (LispString content)
  -- A string is a double quote mark, followed by any number characters, followed by a closing quote mark


parseLispAtom :: Parser LispVal
parseLispAtom =
  (letter <|> symbol) >>= \first ->
  many (letter <|> digit <|> symbol) >>= \rest ->
  let parsedList = first:rest in 
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
parseLispNumber = many1 digit >>= \numStr -> return (LispNumber (read numStr))


parseExpr :: Parser LispVal
parseExpr = parseLispAtom
  <|> parseLispString
  <|> parseLispNumber 


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found value"

