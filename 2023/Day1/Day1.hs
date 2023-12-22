module Day1.Day1 where

import System.Environment
import Text.Regex.TDFA
import Debug.Trace
import qualified Data.Map.Strict as Map

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


result2 :: [String] -> String
result2 (lines) = do
    let numbers = Map.fromList [("one", "1"), ("two", "2"), ("three", "3"), ("four","4"), ("five","5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9"), ("zero", "0")]
    traceShow numbers ""
    ""
