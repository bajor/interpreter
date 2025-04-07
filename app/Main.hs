module Main (main) where

import System.Environment (getArgs)
-- import Lib

main :: IO ()
-- main = someFunc
main = do
    args <- getArgs
    let name = args !! 0
    putStrLn ("hello " ++ name)

-- How to run it: `stack run Andrzej` will say `hello Andrzej`
