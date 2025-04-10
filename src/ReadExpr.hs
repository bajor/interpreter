module ReadExpr (readExpr) where

import Text.ParserCombinators.Parsec (Parser, oneOf, parse, skipMany1, space)


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value" ++ show val


