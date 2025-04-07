module ReadExpr (readExpr) where

import Text.ParserCombinators.Parsec (Parser, oneOf, parse)


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

