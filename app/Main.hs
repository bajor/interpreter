module Main (main) where

import ReadExpr

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let name = args !! 0

    -- let symbol :: Parser Char
    --     symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
    --
    putStrLn ("hello " ++ name)

-- How to run it: `stack run Andrzej` will say `hello Andrzej`

