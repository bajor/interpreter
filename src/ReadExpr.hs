module ReadExpr (readExpr) where

import Data.Char (toLower)
import Control.Applicative ((<|>))
import Text.ParserCombinators.Parsec (Parser, oneOf, parse, noneOf, char, many, letter, digit, many1, string)
import LispTypes
import Numeric (readOct, readBin, readHex, readFloat)


delimiter :: Parser Char
delimiter = oneOf " ()\"'`,"

parseNewLineOrSpace :: Parser LispVal
parseNewLineOrSpace =
    string "#\\" >>
    many1 letter >>= \name ->
        case map toLower name of
            "space" -> delimiter >> return (LispChar ' ')
            "newline" -> delimiter >> return (LispChar '\n')

parseChar :: Parser LispVal
parseChar =
    string "#\\" >>
    (letter <|> symbol) >>=
    \ls -> delimiter >>
    return (LispChar ls)


parseLispChar :: Parser LispVal
parseLispChar = parseNewLineOrSpace <|> parseChar

escapedChar :: Parser Char
escapedChar =
    char '\\' >> (
        char '"'  >> return '"'
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


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseLispAtom :: Parser LispVal
parseLispAtom =
    (letter <|> symbol) >>= \first ->
    many (letter <|> digit <|> symbol) >>= \second ->
    let parsedList = first:second in 
        return (
            case parsedList of
                "#t" -> LispBool True
                "#f" -> LispBool False
                _ -> LispAtom parsedList
               )
  -- An atom is a letter or symbol, followed by any number of letters, digits, or symbols
  --
  -- We use a case expression to determine which LispVal to create and return, matching against the literal strings for true and false. If that is not the case, we return LispAtom.


parseRadixNumbers :: Parser LispVal
parseRadixNumbers =
    char '#' >> 
    oneOf "bodx" >>= \second ->
    many1 digit >>= \rest ->
    return (
        LispNumber(
            case second of
                'b' -> fst . head $ readBin rest
                'o' -> fst . head $ readOct rest
                'd' -> read rest
                'x' -> fst . head $ readHex rest
        )
    )


parseManyDigits :: Parser LispVal
parseManyDigits = many1 digit >>= \numStr -> return (LispNumber (read numStr))

parseLispNumber :: Parser LispVal
parseLispNumber = parseManyDigits <|> parseRadixNumbers


parseLispFloat :: Parser LispVal
parseLispFloat = 
    many1 digit >>= \intPart -> 
    char '.' >> many1 digit >>= \floatPart ->
    return (LispFloat (fst . head $ readFloat (intPart ++ "." ++ floatPart)))


parseExpr :: Parser LispVal
parseExpr = parseLispAtom
    <|> parseLispChar
    <|> parseLispString
    <|> parseLispNumber 
    <|> parseLispFloat


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

