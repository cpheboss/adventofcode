module Day1.Day1 where

import System.Environment
import Text.Regex.TDFA
import Debug.Trace
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

getDigits :: T.Text -> [T.Text]
getDigits s = getAllTextMatches (s =~ "[0-9]") :: [T.Text]

toInt :: T.Text -> Int
toInt text = (read::String->Int) $ T.unpack text


joinFirstAndLast :: [T.Text] -> T.Text
joinFirstAndLast [] = T.pack "0"
joinFirstAndLast x = head x <> last x

result1 :: [T.Text] -> T.Text
result1 lines = T.pack $ show res
    where
        stringDigits = map getDigits lines
        intStrings = map joinFirstAndLast stringDigits
        ints = map toInt intStrings
        res = sum ints
        

result2 :: [T.Text] -> T.Text
result2 (lines) = do
    let numbers = Map.fromList [("one", "o1e"), ("two", "t2w"), ("three", "t3e"), ("four","f4r"), ("five","f5e"), ("six", "s6x"), ("seven", "s7n"), ("eight", "e8t"), ("nine", "n9e"), ("zero", "z0o")]
    traceShow numbers (T.pack "")
