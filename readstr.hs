module Main where
import System.Environment
 
main :: IO ()
main = do
    args <- getLine
    putStrLn ("Hello, " ++ args)