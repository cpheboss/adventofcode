module Day1.Day1 where

import System.Environment
import Text.Regex.TDFA
import Debug.Trace

getDigits :: String -> [String]
getDigits s = getAllTextMatches (s =~ "[0-9]") :: [String]

toInt :: String -> Int
toInt = read::String->Int

joinFirstAndLast :: [String] -> String
joinFirstAndLast [] = "0"
joinFirstAndLast x = head x ++ last x

result1 :: [String] -> String
result1 (lines) = do
    let stringDigits = map getDigits lines
    let intStrings = map joinFirstAndLast stringDigits
    let ints = map toInt intStrings
    let res = sum ints
    traceShow res ""

    show res

