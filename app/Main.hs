module Main (main) where

import System.Environment (getArgs)
-- import Lib

main :: IO ()
-- main = someFunc
main = do
    args <- getArgs
    putStrLn ("hello " ++ args !! 0)

-- How to run it: `stack run Andrzej` will say `hello Andrzej`
