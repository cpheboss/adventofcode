module Main where

import Day1.Day1
import System.Environment
import qualified Data.Text as T

runDayPart :: String -> String -> [T.Text] -> T.Text
runDayPart day part lines =
    case (day, part) of
        ("1", "1") -> Day1.Day1.result1 lines
        ("1", "2") -> Day1.Day1.result2 lines

main :: IO ()
main = do
    day : part : file : args <- getArgs
    putStrLn $ "Reading file " ++ file
    file_content <- readFile file
    let ll = map T.pack $ lines file_content
    putStrLn "Result:"
    putStrLn $ T.unpack $ runDayPart day part ll

