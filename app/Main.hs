module Main (main) where

import ReadExpr

import System.Environment (getArgs)

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)

-- run this with `stack run <expression>`
