module Main where

import Day1.Day1
import System.Environment

main :: IO ()
main = do
    file : args <- getArgs
    putStrLn $ "Reading file " ++ file
    file_content <- readFile file
--    putStrLn file_content
    let ll = lines file_content
    putStrLn "Result:"
    putStrLn $ Day1.Day1.result1 ll
